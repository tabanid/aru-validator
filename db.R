# =============================================================================
# db.R  —  Data layer for the validation-only tool.
#
# SQLite ONLY (RSQLite). The tool opens a pipeline-built SQLite validation
# database, reads its clips, and writes labels back. It never touches DuckDB:
# that is the upstream scores warehouse (allScores.ddb / the `vets` pool), read
# only by the R pipeline (R1/R2/R4). The contract surface here is the per-species
# SQLite `.db` file — schema fixed by VALIDATION_TOOL_CONTRACT.md v2 (PK = sample_id).
#
# The five write-back columns (classification, classified, vocalization_type,
# classified_at, notes) are NON-NEGOTIABLE — R3/R6 key off them. Do not rename,
# revalue, or repurpose them without routing a change through the pipeline chat.
# =============================================================================

library(DBI)
library(RSQLite)

# ---- shared/codes.R: the ONE definition of validation codes (contract v3 §3) ----
# ALL_CODES is the write gate; DETECTION, NOT_TARGET (N/O/I/P, counted as N
# downstream), UNCERTAIN (U: excluded from fitting, never blocks an absence),
# VOC_TYPES, TIMESTAMP_FORMAT. deploy.sh puts codes.R beside the app; in the repo
# it is ../../shared/codes.R (cwd = this folder) or shared/codes.R (cwd = repo root).
local({
  cands <- c("codes.R", "../../shared/codes.R", "shared/codes.R",
             file.path(Sys.getenv("ARU_REPO", unset = ""), "shared", "codes.R"))
  hit <- cands[nzchar(cands) & file.exists(cands)]
  if (!length(hit))
    stop("shared/codes.R not found; looked in: ", paste(cands, collapse = ", "),
         "\nRun from the app folder, the repo root, or set ARU_REPO.")
  source(hit[1], local = FALSE)
})

# Display colour per code, keyed by the shared names (a code added or dropped in
# codes.R fails here, not silently in the UI).
CODE_COLOUR <- c(Y = "#4caf50", N = "#dc3545", O = "#2196f3",   # code-map
                 I = "#2196f3", P = "#9c27b0", U = "#ff9800")   # code-map
stopifnot(setequal(names(CODE_COLOUR), ALL_CODES))

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0) return(b)
  if (length(a) == 1 && is.na(a))   return(b)
  a
}

# ---- open / close ----------------------------------------------------------

#' Open a pipeline-built SQLite validation database.
#' Guards against being pointed at a non-SQLite file (e.g. the DuckDB scores
#' store) or a SQLite file that isn't a validation DB.
open_validation_db <- function(db_path) {
  if (!file.exists(db_path)) stop("Database not found: ", db_path)

  con <- tryCatch(
    dbConnect(RSQLite::SQLite(), db_path),
    error = function(e)
      stop("Could not open as SQLite: ", basename(db_path),
           "\n(The upstream scores store is DuckDB '.ddb' and is not opened here.)\n",
           conditionMessage(e))
  )

  tabs <- tryCatch(dbListTables(con), error = function(e) character(0))
  if (!"clips" %in% tabs) {
    dbDisconnect(con)
    stop("No 'clips' table in ", basename(db_path),
         " — this does not look like a validation database.")
  }

  vdb <- structure(
    list(
      conn   = con,
      path   = normalizePath(db_path),
      db_dir = dirname(normalizePath(db_path)),
      cols   = dbListFields(con, "clips"),
      tables = tabs
    ),
    class = "validation_db"
  )
  vdb$spcd       <- db_species(vdb)
  vdb$audio_dirs <- audio_dirs(vdb)
  vdb
}

close_validation_db <- function(vdb) {
  if (!is.null(vdb$conn)) try(dbDisconnect(vdb$conn), silent = TRUE)
  invisible(NULL)
}

# ---- validation_sessions (contract v3 §5.1): who validated, when; append-only ----
# Ported from V4 (legacy/V4_0/validation_database_V4_0.R). Rows are only ever
# INSERTed, and each is UPDATEd once, on close; older rows are never touched.

#' Append a validation_sessions row for this app session on an open DB. Creates the
#' table if the DB predates it (schema_version 1). Returns the new rowid.
open_validation_session_row <- function(conn, validator, app_version) {
  stopifnot(is.character(validator), length(validator) == 1L, nzchar(validator))
  aru_ensure_validation_sessions(conn)
  dbExecute(conn, "INSERT INTO validation_sessions (validator, app_version, session_start)
                   VALUES (?, ?, ?)",
            params = list(validator, as.character(app_version), .iso_now()))
  dbGetQuery(conn, "SELECT last_insert_rowid() AS id")$id
}

#' Close a validation_sessions row: set session_end and n_classified (clips whose
#' classified_at falls in [session_start, session_end]). A row already closed is
#' left alone, so a second close is a no-op.
close_validation_session_row <- function(conn, rowid) {
  if (is.null(rowid) || !DBI::dbIsValid(conn)) return(invisible(NULL))
  now <- .iso_now()
  dbExecute(conn, "
    UPDATE validation_sessions
       SET session_end  = ?,
           n_classified = (SELECT COUNT(*) FROM clips
                            WHERE classified_at >= validation_sessions.session_start
                              AND classified_at <= ?)
     WHERE rowid = ? AND session_end IS NULL",
    params = list(now, now, as.integer(rowid)))
  invisible(NULL)
}

has_col <- function(vdb, col) col %in% vdb$cols

# ---- mode detection --------------------------------------------------------
# Order per contract §5:
#   1. validation_meta.mode (authoritative when present)
#   2. flag inference: any sample2_active = 1 -> occupancy; else sample1_nominee
#      = 1 -> calibration
#   3. default to calibration with a visible warning
detect_mode <- function(vdb) {
  con <- vdb$conn

  if ("validation_meta" %in% vdb$tables) {
    m <- tryCatch(dbGetQuery(con, "SELECT * FROM validation_meta LIMIT 1"),
                  error = function(e) NULL)
    if (!is.null(m) && nrow(m) >= 1 &&
        !is.na(m$mode) && nzchar(as.character(m$mode))) {
      return(list(
        mode    = as.character(m$mode),
        grain   = if ("grain" %in% names(m)) as.character(m$grain) else NA_character_,
        meta    = m,
        source  = "validation_meta",
        warning = NULL
      ))
    }
  }

  n_s2 <- if (has_col(vdb, "sample2_active"))
    dbGetQuery(con, "SELECT COUNT(*) AS n FROM clips WHERE sample2_active = 1")$n else 0L
  n_s1 <- if (has_col(vdb, "sample1_nominee"))
    dbGetQuery(con, "SELECT COUNT(*) AS n FROM clips WHERE sample1_nominee = 1")$n else 0L

  if (n_s2 > 0)
    return(list(mode = "occupancy", grain = NA_character_, meta = NULL,
                source = "flag_inference", warning = NULL))
  if (n_s1 > 0)
    return(list(mode = "calibration", grain = NA_character_, meta = NULL,
                source = "flag_inference", warning = NULL))

  list(mode = "calibration", grain = NA_character_, meta = NULL,
       source = "default",
       warning = paste0("No validation_meta and no sample flags found — ",
                        "defaulting to calibration mode (validate all, flat)."))
}

# ---- read clips ------------------------------------------------------------
# Validators always work high -> low score.

load_all_clips <- function(vdb) {
  dbGetQuery(vdb$conn, "SELECT * FROM clips ORDER BY score DESC")
}

# The work queue: clips not yet classified, highest score first. "Top scores"
# view shows the top of this; "Next view" reloads it (the ones just done drop out).
load_unclassified_clips <- function(vdb) {
  dbGetQuery(vdb$conn,
    "SELECT * FROM clips WHERE classified IS NULL OR classified = 0 ORDER BY score DESC")
}

load_group_clips <- function(vdb, unit, time_period) {
  dbGetQuery(vdb$conn,
    "SELECT * FROM clips WHERE unit = ? AND time_period = ? ORDER BY score DESC",
    params = list(unit, time_period))
}

# View: the single highest-scoring still-unexamined clip from each cell that has
# no confirmed Y yet, score-ordered. Validating these resolves the most cells per
# look. Uses a window function (SQLite >= 3.25, bundled with RSQLite).
load_best_per_cell <- function(vdb) {
  dbGetQuery(vdb$conn, paste0(
    "WITH resolved AS (
       SELECT DISTINCT unit, time_period FROM clips WHERE ", SQL_IS_DETECTION, "
     ),
     ranked AS (
       SELECT c.*,
              ROW_NUMBER() OVER (PARTITION BY c.unit, c.time_period
                                 ORDER BY c.score DESC) AS rn
       FROM clips c
       LEFT JOIN resolved r
         ON c.unit = r.unit AND c.time_period = r.time_period
       WHERE r.unit IS NULL
         AND (c.classified IS NULL OR c.classified = 0)
     )
     SELECT * FROM ranked WHERE rn = 1 ORDER BY score DESC"))
}

# View (brief F4): every clip from one unit, all weeks, highest score first, so one
# AM's candidates can be heard together. Reads clips only.
load_unit_clips <- function(vdb, unit) {
  dbGetQuery(vdb$conn, "SELECT * FROM clips WHERE unit = ? ORDER BY score DESC",
             params = list(unit))
}

# Units for the unit view, west -> east when lon is known (both modes).
db_units <- function(vdb) .unit_order_by_lon(derive_groups(vdb))

get_clip <- function(vdb, sample_id) {
  dbGetQuery(vdb$conn, "SELECT * FROM clips WHERE sample_id = ?",
             params = list(as.integer(sample_id)))
}

# ---- write back (the five fixed columns) -----------------------------------

# TIMESTAMP_FORMAT (codes.R, contract §5.1): the same form as session_start/end, so
# validation_sessions.n_classified can compare them as text.
.iso_now <- function() format(Sys.time(), TIMESTAMP_FORMAT, tz = "GMT")

#' Persist one classification. Always sets classified, classification,
#' vocalization_type, classified_at. Writes `notes` only when update_notes=TRUE
#' (so re-classifying or bulk-marking never silently blanks an existing note).
save_classification <- function(vdb, sample_id, classification,
                                vocalization_type = NULL, notes = NULL,
                                update_notes = FALSE) {
  if (!classification %in% ALL_CODES)
    stop("Invalid classification '", classification, "'. Must be one of ",
         paste(ALL_CODES, collapse = "/"), ".")

  if (is.null(vocalization_type)) vocalization_type <- NA_character_
  if (!is.na(vocalization_type) && vocalization_type == "")
    vocalization_type <- NA_character_
  if (!is.na(vocalization_type) && !vocalization_type %in% VOC_TYPES)
    stop("vocalization_type must be one of ", paste(VOC_TYPES, collapse = "/"), ", or NA.")
  # vocalization_type is meaningful only for Y; clear it otherwise.
  if (classification != DETECTION) vocalization_type <- NA_character_

  if (update_notes) {
    if (is.null(notes) || (length(notes) == 1 && !is.na(notes) && notes == ""))
      notes <- NA_character_
    dbExecute(vdb$conn, "
      UPDATE clips SET classified = 1, classification = ?, vocalization_type = ?,
             classified_at = ?, notes = ? WHERE sample_id = ?",
      params = list(classification, vocalization_type, .iso_now(),
                    notes, as.integer(sample_id)))
  } else {
    dbExecute(vdb$conn, "
      UPDATE clips SET classified = 1, classification = ?, vocalization_type = ?,
             classified_at = ? WHERE sample_id = ?",
      params = list(classification, vocalization_type, .iso_now(),
                    as.integer(sample_id)))
  }
  invisible(TRUE)
}

#' Un-classify a clip (explicit user re-open). Blanks the five fields.
clear_classification <- function(vdb, sample_id) {
  dbExecute(vdb$conn, "
    UPDATE clips
       SET classified        = 0,
           classification    = NULL,
           vocalization_type = NULL,
           notes             = NULL,
           classified_at     = NULL
     WHERE sample_id = ?",
    params = list(as.integer(sample_id)))
  invisible(TRUE)
}

#' Save just a note without altering classification state.
save_note <- function(vdb, sample_id, notes) {
  if (is.null(notes) || is.na(notes) || notes == "") notes <- NA_character_
  dbExecute(vdb$conn, "UPDATE clips SET notes = ? WHERE sample_id = ?",
            params = list(notes, as.integer(sample_id)))
  invisible(TRUE)
}

# ---- "Other species" (brief R8) -------------------------------------------
# Species heard in a clip besides the target, kept in `notes` as a first line
# `other=BTNW,NAWA`, then the free-text comment (VALIDATION_TOOL_CONTRACT.md, notes).
# Only a first line matching NOTES_OTHER_RE is read as codes; anything else is free text.
NOTES_OTHER_RE <- "^other=[A-Z0-9-]+(,[A-Z0-9-]+)*$"

#' Codes typed in the Other species box -> clean vector: upper-cased, split on spaces,
#' commas or semicolons, characters outside A-Z 0-9 - dropped, de-duplicated, the
#' target's own code removed.
parse_other_codes <- function(txt, target = NULL) {
  if (is.null(txt) || length(txt) == 0 || is.na(txt[1])) return(character(0))
  tok <- strsplit(toupper(txt[1]), "[[:space:],;]+")[[1]]
  tok <- gsub("[^A-Z0-9-]", "", tok)
  tok <- unique(tok[nzchar(tok)])
  if (!is.null(target) && length(target) == 1 && !is.na(target)) tok <- setdiff(tok, toupper(target))
  tok
}

#' notes -> list(other = codes, free = comment text).
split_notes <- function(notes) {
  if (is.null(notes) || length(notes) == 0 || is.na(notes[1]) || !nzchar(notes[1]))
    return(list(other = character(0), free = ""))
  lines <- strsplit(notes[1], "\n", fixed = TRUE)[[1]]
  if (length(lines) && grepl(NOTES_OTHER_RE, lines[1]))
    return(list(other = strsplit(sub("^other=", "", lines[1]), ",", fixed = TRUE)[[1]],
                free  = paste(lines[-1], collapse = "\n")))
  list(other = character(0), free = notes[1])
}

#' codes + comment -> the notes value (NA when both are empty). No codes -> no other= line.
compose_notes <- function(other, free) {
  free <- if (is.null(free) || length(free) == 0 || is.na(free[1])) "" else free[1]
  other <- other[!is.na(other) & nzchar(other)]
  out <- if (length(other)) paste0("other=", paste(other, collapse = ","),
                                   if (nzchar(free)) paste0("\n", free) else "")
         else free
  if (nzchar(out)) out else NA_character_
}

#' HawkEars species codes (spcdHE of allBNHsp.csv), found like codes.R: beside the app,
#' in the repo, or under ARU_REPO. NULL if not found: then nothing is flagged.
load_known_species_codes <- function() {
  rel <- file.path("projects", "_shared", "inputs", "allBNHsp.csv")
  cands <- c("allBNHsp.csv", file.path("..", "..", rel), rel,
             file.path(Sys.getenv("ARU_REPO", unset = ""), rel))
  hit <- cands[nzchar(cands) & file.exists(cands)]
  if (!length(hit)) return(NULL)
  x <- tryCatch(read.csv(hit[1], stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(x) || !"spcdHE" %in% names(x)) return(NULL)
  v <- toupper(trimws(x$spcdHE)); unique(v[!is.na(v) & nzchar(v)])
}

#' Codes not in the known list (never blocks a save; the app shows them in amber).
unknown_codes <- function(codes, known) {
  if (is.null(known) || !length(codes)) return(character(0))
  setdiff(codes, known)
}

#' Mark a set of clips at once (mark-all). The server passes the in-view
#' UNCLASSIFIED ids; each is written via save_classification.
mark_clips <- function(vdb, sample_ids, classification, vocalization_type = NULL) {
  for (sid in sample_ids)
    save_classification(vdb, sid, classification, vocalization_type)
  invisible(length(sample_ids))
}

# ---- grouping derived on the fly (no reliance on stale groups/group_order) --
# Occupancy DBs have groups/group_order cleared by the pipeline; calibration
# DBs may carry stale ones. We always compute the unit x time_period grid live.

derive_groups <- function(vdb) {
  lon_sel <- if (has_col(vdb, "lon")) "MIN(lon) AS lon" else "NULL AS lon"
  q <- sprintf("
    SELECT unit, time_period,
           COUNT(*)                                            AS n_total,
           SUM(CASE WHEN classified = 1 THEN 1 ELSE 0 END)     AS n_examined,
           SUM(CASE WHEN %s THEN 1 ELSE 0 END)                AS n_yes,
           MAX(score)                                          AS max_score,
           %s
      FROM clips
     GROUP BY unit, time_period", SQL_IS_DETECTION, lon_sel)
  g <- dbGetQuery(vdb$conn, q)
  g$status <- ifelse(g$n_yes > 0, "yes",
              ifelse(g$n_examined > 0, "examined", "unexamined"))
  g
}

#' Occupancy stop rule (contract §5): one Y resolves a unit x time_period cell;
#' a cell worked to the bottom with no Y is an absence.
group_complete <- function(group_row) {
  if (group_row$n_yes > 0)
    return(list(complete = TRUE, reason = "resolved_by_Y"))
  if (group_row$n_examined >= group_row$n_total)
    return(list(complete = TRUE, reason = "examined_no_Y_absence"))
  list(complete = FALSE, reason = NA_character_)
}

#' Calibration progress: validate all, no early stop.
calibration_progress <- function(vdb) {
  dbGetQuery(vdb$conn, "
    SELECT COUNT(*) AS n_total,
           SUM(CASE WHEN classified = 1 THEN 1 ELSE 0 END) AS n_examined
      FROM clips")
}

# ---- audio resolution (brief R5; one layout for all projects) ---------------
# Clips are looked for, in order:
#   1. beside the DB                           (contract §4.3 package layout)
#   2. dirname(db)/../clips/{SPCD}/            (the flat working layout:
#                                               …/dbs/{SPCD}-occupancy.db + …/clips/{SPCD}/)
#   3. session_config.audio_folder             (old calibration DBs only)
# audio_folder is read, never rewritten; old absolute paths in it (C:/Contracts/...)
# simply don't exist here and are skipped.

# Resource prefix the app serves each audio dir under (names match audio_dirs()).
AUDIO_PREFIXES <- c(beside = "audio_files", clips = "audio_files_clips",
                    fallback = "audio_files_fb")

#' Species code of a DB: validation_meta.species, else the DB file name up to the
#' first '-' or '_' (AMRE-occupancy.db, ACFL-sampleHE_....db -> AMRE, ACFL).
db_species <- function(vdb) {
  if ("validation_meta" %in% vdb$tables) {
    sp <- tryCatch(dbGetQuery(vdb$conn, "SELECT species FROM validation_meta LIMIT 1")$species,
                   error = function(e) NULL)
    if (length(sp) && !is.na(sp[1]) && nzchar(sp[1])) return(as.character(sp[1]))
  }
  sub("[-_].*$", "", tools::file_path_sans_ext(basename(vdb$path)))
}

#' The audio dirs that exist for this DB, in resolution order, named as in
#' AUDIO_PREFIXES. Computed once at open (open_validation_db stores it on vdb).
audio_dirs <- function(vdb) {
  cand <- c(beside = vdb$db_dir,
            clips  = file.path(dirname(vdb$db_dir), "clips", vdb$spcd %||% db_species(vdb)),
            fallback = .audio_folder_fallback(vdb))
  cand[!is.na(cand) & dir.exists(cand)]
}

.audio_folder_fallback <- function(vdb) {
  if (!"session_config" %in% vdb$tables) return(NA_character_)
  af <- tryCatch(
    dbGetQuery(vdb$conn, "SELECT audio_folder FROM session_config WHERE id = 1")$audio_folder,
    error = function(e) NA_character_)
  if (length(af) == 0) return(NA_character_)
  if (is.na(af) || !nzchar(af)) return(NA_character_)
  af
}

#' Where a clip WAV is: list(path, which), `which` naming the audio dir (see
#' AUDIO_PREFIXES). If no dir holds it, the DB-relative path with which = "beside",
#' so the UI can show a clear "file not found" state.
resolve_audio <- function(vdb, file_name) {
  dirs <- vdb$audio_dirs %||% audio_dirs(vdb)
  for (k in names(dirs)) {
    p <- file.path(dirs[[k]], file_name)
    if (file.exists(p)) return(list(path = p, which = k))
  }
  list(path = file.path(vdb$db_dir, file_name), which = "beside")
}

resolve_clip_path <- function(vdb, file_name) resolve_audio(vdb, file_name)$path

# ---- optional effort-grid backdrop -----------------------------------------
# Companion effort_grid.csv (columns: unit, period) sits in the DB folder, as
# emitted by build_effort_grid.R. period matches clips.time_period exactly
# (same .period_key, e.g. 2025-W24). Cells in the grid with no clip group are
# "deployed but silent" (surveyed, no detections for this species).

load_effort_grid <- function(vdb) {
  f <- file.path(vdb$db_dir, "effort_grid.csv")
  if (!file.exists(f)) return(NULL)
  eg <- tryCatch(read.csv(f, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(eg) || !all(c("unit", "period") %in% names(eg))) return(NULL)
  unique(eg[, c("unit", "period")])
}

# ---- occupancy group navigation (pure helpers over a derive_groups() frame) -

#' Row order for plotting/navigation: west -> east by lon when available.
.unit_order_by_lon <- function(groups) {
  if (is.null(groups) || nrow(groups) == 0) return(character(0))
  if (!"lon" %in% names(groups) || all(is.na(groups$lon)))
    return(sort(unique(as.character(groups$unit))))
  agg <- aggregate(lon ~ unit, data = groups, FUN = function(x) min(x, na.rm = TRUE))
  as.character(agg$unit[order(agg$lon)])
}

.group_done <- function(row) isTRUE(group_complete(row)$complete)

.first_unresolved <- function(g) {
  for (i in seq_len(nrow(g))) if (!.group_done(g[i, ])) return(i)
  1L
}

#' Next unresolved group after `from`, scanning cyclically; NA if all resolved.
.next_unresolved <- function(g, from) {
  n <- nrow(g)
  for (step in seq_len(n)) {
    i <- ((from - 1L + step) %% n) + 1L
    if (!.group_done(g[i, ])) return(i)
  }
  NA_integer_
}

# ---- summary / stats helpers (for the sidebar info panels) ------------------

#' Per-class tallies over the whole DB (named int vector, e.g. c(Y=12, N=30, ...)).
class_counts <- function(vdb) {
  q <- dbGetQuery(vdb$conn,
    "SELECT classification AS c, COUNT(*) AS n FROM clips
      WHERE classification IS NOT NULL AND classification <> '' GROUP BY classification")
  if (nrow(q) == 0) return(integer(0))
  stats::setNames(as.integer(q$n), q$c)
}

#' Per-class tallies within one unit x time_period cell.
group_class_counts <- function(vdb, unit, time_period) {
  q <- dbGetQuery(vdb$conn,
    "SELECT classification AS c, COUNT(*) AS n FROM clips
      WHERE unit = ? AND time_period = ? AND classification IS NOT NULL
        AND classification <> '' GROUP BY classification",
    params = list(unit, time_period))
  if (nrow(q) == 0) return(integer(0))
  stats::setNames(as.integer(q$n), q$c)
}

#' Count for one code from a class_counts() vector (0 if absent).
cc_get <- function(counts, code) if (code %in% names(counts)) as.integer(counts[[code]]) else 0L

#' Score-distribution quantile bins for the magma summary bar.
score_distribution <- function(vdb) {
  s <- dbGetQuery(vdb$conn, "SELECT score FROM clips")$score
  s <- s[!is.na(s)]
  if (length(s) == 0) return(NULL)
  th   <- c(min(s), stats::quantile(s, 0.20), stats::quantile(s, 0.50),
            stats::quantile(s, 0.75), stats::quantile(s, 0.90), max(s))
  th   <- unname(th)
  bins <- vapply(seq_len(length(th) - 1L),
                 function(i) sum(s >= th[i] & s < th[i + 1L]), integer(1))
  list(thresholds = th, bins = bins, total = length(s), range = range(s))
}

#' Export all classified clips to a CSV (mirrors the old consolidated export).
#' Returns the number of rows written.
export_results <- function(vdb, output_path) {
  want <- c("spcd", "fn", "newfn", "file_name", "unit", "date", "time_period",
            "score", "BNscore", "solar_label", "solar_period",
            "sample1_nominee", "sample2_active", "nominating_model",
            "classification", "vocalization_type", "notes", "classified",
            "classified_at")
  cols <- want[want %in% vdb$cols]
  q <- sprintf("SELECT %s FROM clips WHERE classified = 1
                ORDER BY unit, time_period, score DESC",
               paste(cols, collapse = ", "))
  res <- dbGetQuery(vdb$conn, q)
  utils::write.csv(res, output_path, row.names = FALSE)
  nrow(res)
}

# ---- directory scan: per-species subfolders, each with a .db + clips --------
# The old tool pointed at a high-level folder holding one subdirectory per
# species (validation_databases/<SPCD>/...db). This mirrors that: given a root,
# scan the root itself and each immediate subdirectory for .db files that are
# validation DBs (have a `clips` table), and report species / mode / clip count.

#' Cheap read-only probe. Returns NULL if the file isn't a validation DB.
.probe_validation_db <- function(db_path) {
  con <- tryCatch(
    dbConnect(RSQLite::SQLite(), db_path, flags = RSQLite::SQLITE_RO),
    error = function(e) tryCatch(dbConnect(RSQLite::SQLite(), db_path),
                                 error = function(e2) NULL))
  if (is.null(con)) return(NULL)
  on.exit(try(dbDisconnect(con), silent = TRUE), add = TRUE)
  tabs <- tryCatch(dbListTables(con), error = function(e) character(0))
  if (!"clips" %in% tabs) return(NULL)
  spcd <- tryCatch(dbGetQuery(con, "SELECT spcd FROM clips LIMIT 1")$spcd,
                   error = function(e) NA_character_)
  n <- tryCatch(dbGetQuery(con, "SELECT COUNT(*) AS n FROM clips")$n,
                error = function(e) NA_integer_)
  mode <- NA_character_
  if ("validation_meta" %in% tabs) {
    m <- tryCatch(dbGetQuery(con, "SELECT mode FROM validation_meta LIMIT 1")$mode,
                  error = function(e) NA_character_)
    if (length(m) && !is.na(m) && nzchar(as.character(m))) mode <- as.character(m)
  }
  list(species = if (length(spcd) && !is.na(spcd) && nzchar(spcd)) as.character(spcd) else NULL,
       mode = mode, n_clips = if (length(n) && !is.na(n)) as.integer(n) else NA_integer_)
}

#' Scan `root` (and its immediate subdirectories) for validation DBs.
#' Returns a data.frame (species, db_path, db_name, subdir, mode, n_clips),
#' species/db sorted, or NULL if none found. Clip WAVs are assumed to sit beside
#' each .db (the contract layout), so a species subdir is self-contained.
scan_species_dbs <- function(root) {
  if (is.null(root) || !dir.exists(root)) return(NULL)
  lvl1 <- list.dirs(root, full.names = TRUE, recursive = FALSE)
  lvl2 <- unlist(lapply(lvl1, function(d)
    list.dirs(d, full.names = TRUE, recursive = FALSE)), use.names = FALSE)
  search_dirs <- unique(c(root, lvl1, lvl2))
  rows <- list()
  for (d in search_dirs) {
    dbs <- list.files(d, pattern = "\\.db$", full.names = TRUE, ignore.case = TRUE)
    for (db in dbs) {
      info <- .probe_validation_db(db)
      if (is.null(info)) next
      sp <- info$species %||% basename(d)
      rows[[length(rows) + 1L]] <- data.frame(
        species = sp, db_path = normalizePath(db), db_name = basename(db),
        subdir = basename(d), mode = info$mode %||% NA_character_,
        n_clips = info$n_clips %||% NA_integer_, stringsAsFactors = FALSE)
    }
  }
  if (!length(rows)) return(NULL)
  res <- do.call(rbind, rows)
  res <- res[!duplicated(res$db_path), , drop = FALSE]
  res[order(tolower(res$species), tolower(res$db_name)), , drop = FALSE]
}

#' List immediate sub-directories and .db files in `dir`, for the UI browser.
#' Returns data.frame(name, is_dir) with directories first, then .db files.
list_dir_entries <- function(dir) {
  if (is.null(dir) || !dir.exists(dir))
    return(data.frame(name = character(0), is_dir = logical(0), stringsAsFactors = FALSE))
  ents <- tryCatch(list.files(dir, all.files = FALSE, include.dirs = TRUE,
                              no.. = TRUE), error = function(e) character(0))
  if (!length(ents))
    return(data.frame(name = character(0), is_dir = logical(0), stringsAsFactors = FALSE))
  full  <- file.path(dir, ents)
  isdir <- dir.exists(full)
  isdb  <- !isdir & grepl("\\.db$", ents, ignore.case = TRUE)
  keep  <- isdir | isdb
  ents  <- ents[keep]; isdir <- isdir[keep]
  ord   <- order(!isdir, tolower(ents))
  data.frame(name = ents[ord], is_dir = isdir[ord], stringsAsFactors = FALSE)
}
