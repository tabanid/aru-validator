# =============================================================================
# shared/other.R — the `other=` reader and the period key, one copy for the
# pipeline and the validator (ARCHITECTURE.md §1.1; brief
# docs/briefs_done/R6_OTHER_HARVEST_BRIEF_2026-09-30.md).
#
# Validators label other species heard in a clip as rows of `clip_others` (contract
# v4 §4.4, schema 3; brief docs/briefs_done/CC_OTHER_LIST_BRIEF.md). A `Y` row (any source) is a
# borrowed detection: R6 counts it as an occupancy detection of that species in the
# clip's unit x period, and the app shows such cells as resolved; R3 never reads it.
# Older DBs (no clip_others table) are read the old way: a first line of notes
# `other=BTNW,NAWA` (contract v3 §4.2, superseded). Finished, unconverted projects
# keep working; nothing may write other= lines any more.
#
#   period_key(dates, grain, anchor)  Date -> period key (moved from core/build_effort_grid.R)
#   parse_other(notes)         one notes value -> the codes on a valid first line
#                              (legacy fallback and the v3 converter only)
#   harvest_other(db_paths)    every borrowed detection in the DBs, one row per code x clip
#   OTHER_DONOR_MODES          DB modes whose other= codes count
#
# Pure definitions: sourcing twice is harmless. DBI/RSQLite are used by namespace
# inside harvest_other() only. A change here is a `shared:` commit decided with Phil.
# =============================================================================

# Modes whose other-species detections count ("census" added 2026-10-26, Part 3 addendum b:
# BITH census clips carried into one-round Sample 2 DBs). Drop "calibration" to stop Sample 1
# donating (one line; brief decision 2).
OTHER_DONOR_MODES <- c("calibration", "occupancy", "census")

# The first-line format (contract §4.2). Same pattern as the app's NOTES_OTHER_RE.
OTHER_LINE_RE <- "^other=[A-Z0-9-]+(,[A-Z0-9-]+)*$"

#' Collapse Dates to the chosen period key.
#' @param dates a Date (or "YYYY-MM-DD") vector.
#' @param grain "week" (ISO week, e.g. 2025-W23), "5day" (5-day blocks within a
#'   year, e.g. 2025-P34), "day" (the date), or "anchored_week" (7-day weeks
#'   counted from `anchor`: W1 = anchor .. anchor + 6, as sample1_draw's occasions).
#' @param anchor the first day of W1 (Date or "YYYY-MM-DD"); "anchored_week" only.
#' Note: "%Y-W%V" pairs the calendar year with the ISO week; at a year boundary the
#' ISO year (%G) can differ. Kept as is: R4 wrote the DBs' time_period with it.
period_key <- function(dates, grain, anchor = NULL) {
  dates <- as.Date(dates)
  if (grain == "day")  return(as.character(dates))
  if (grain == "anchored_week") {
    if (is.null(anchor) || length(anchor) != 1L || is.na(as.Date(anchor)))
      stop("period_key: grain 'anchored_week' needs one anchor date")
    k <- as.integer(dates - as.Date(anchor)) %/% 7L + 1L
    return(ifelse(is.na(k), NA_character_, paste0("W", k)))
  }
  if (grain == "5day") {
    doy <- as.integer(format(dates, "%j"))
    blk <- ((doy - 1L) %/% 5L) + 1L
    return(sprintf("%s-P%02d", format(dates, "%Y"), blk))
  }
  format(dates, "%Y-W%V")   # default: ISO year-week
}

#' One notes value -> the species codes on a valid first `other=` line, else
#' character(0). Anything else (not the first line, lower case, spaces) is free text.
#' Reads old (schema < 3) DBs only: the legacy fallback below and core/convert_db_v3.R.
parse_other <- function(notes) {
  if (is.null(notes) || length(notes) == 0 || is.na(notes[1]) || !nzchar(notes[1]))
    return(character(0))
  first <- strsplit(notes[1], "\n", fixed = TRUE)[[1]][1]
  if (is.na(first) || !grepl(OTHER_LINE_RE, first)) return(character(0))
  unique(strsplit(sub("^other=", "", first), ",", fixed = TRUE)[[1]])
}

# Mode of an open DB, in the contract §5 order (as the app's detect_mode()):
# validation_meta.mode; else flag inference (sample2_active -> occupancy,
# sample1_nominee -> calibration); else calibration by default.
.other_db_mode <- function(con, tabs, cols) {
  if ("validation_meta" %in% tabs) {
    m <- tryCatch(DBI::dbGetQuery(con, "SELECT mode FROM validation_meta LIMIT 1")$mode,
                  error = function(e) NULL)
    if (length(m) && !is.na(m[1]) && nzchar(m[1]))
      return(list(mode = as.character(m[1]), source = "validation_meta"))
  }
  n <- function(col) if (col %in% cols)
    DBI::dbGetQuery(con, paste0("SELECT COUNT(*) AS n FROM clips WHERE ", col, " = 1"))$n else 0L
  if (n("sample2_active")  > 0) return(list(mode = "occupancy",   source = "flag_inference"))
  if (n("sample1_nominee") > 0) return(list(mode = "calibration", source = "flag_inference"))
  list(mode = "calibration", source = "default")
}

# Species of an open DB: validation_meta.species, else clips.spcd.
.other_db_species <- function(con, tabs) {
  if ("validation_meta" %in% tabs) {
    s <- tryCatch(DBI::dbGetQuery(con, "SELECT species FROM validation_meta LIMIT 1")$species,
                  error = function(e) NULL)
    if (length(s) && !is.na(s[1]) && nzchar(s[1])) return(as.character(s[1]))
  }
  s <- DBI::dbGetQuery(con, "SELECT spcd FROM clips WHERE spcd IS NOT NULL LIMIT 1")$spcd
  if (length(s)) as.character(s[1]) else NA_character_
}

# schema_version of an open DB (as shared/codes.R aru_schema_version(); repeated so
# this file also works sourced on its own).
.other_schema_version <- function(con, tabs) {
  if (!"validation_meta" %in% tabs) return(1L)
  if (!"schema_version" %in% DBI::dbListFields(con, "validation_meta")) return(1L)
  v <- DBI::dbGetQuery(con, "SELECT schema_version FROM validation_meta LIMIT 1")$schema_version
  if (!length(v) || is.na(v[1])) 1L else as.integer(v[1])
}

# One DB, read-only: its mode, species, which reader, and one row per code x clip
# (sample_id, unit, date, file_name, classified_at, code). The reader is chosen by the
# table, not the stamp: clip_others present -> its Y rows (notes ignored); a schema >= 3
# stamp without the table is an error (skipped, never read as legacy); otherwise the
# legacy other= first line of notes. `n_lines` / `n_invalid` feed the scan tally.
.other_read_db <- function(p) {
  if (!file.exists(p)) stop("file not found")
  con <- DBI::dbConnect(RSQLite::SQLite(), p, flags = RSQLite::SQLITE_RO, synchronous = NULL)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  tabs <- DBI::dbListTables(con)
  if (!"clips" %in% tabs) stop("no clips table")
  cols <- DBI::dbListFields(con, "clips")
  opt <- function(col) if (col %in% cols) paste0("c.", col) else paste0("NULL AS ", col)
  out <- list(md = .other_db_mode(con, tabs, cols), sp = .other_db_species(con, tabs))
  if ("clip_others" %in% tabs) {
    cl <- DBI::dbGetQuery(con, paste0(
      "SELECT c.sample_id, c.unit, c.date, ", opt("file_name"), ", o.classified_at, o.code",
      " FROM clip_others o JOIN clips c ON c.sample_id = o.sample_id",
      " WHERE o.label = 'Y' ORDER BY c.sample_id, o.code"))
    return(c(out, list(reader = "clip_others", cl = cl,
                       n_lines = length(unique(cl$sample_id)), n_invalid = 0L)))
  }
  if (.other_schema_version(con, tabs) >= 3L) stop("schema 3 without clip_others")
  if (!"notes" %in% cols) stop("clips has no notes column")
  nl <- DBI::dbGetQuery(con, paste0(
    "SELECT c.sample_id, c.unit, c.date, c.notes, ", opt("file_name"), ", ", opt("classified_at"),
    " FROM clips c WHERE c.notes LIKE 'other=%' ORDER BY c.sample_id"))
  codes <- lapply(nl$notes, parse_other)
  k <- rep(seq_len(nrow(nl)), lengths(codes))
  cl <- nl[k, c("sample_id", "unit", "date", "file_name", "classified_at"), drop = FALSE]
  cl$code <- as.character(unlist(codes))
  c(out, list(reader = "legacy_notes", cl = cl, n_lines = nrow(nl),
              n_invalid = sum(lengths(codes) == 0L)))
}

#' Every borrowed detection in the DBs, read-only: clip_others Y rows (schema 3), or
#' other= first lines of notes in older DBs.
#' @param db_paths validation DB paths (any mode, any species).
#' @param modes    donor modes kept (default OTHER_DONOR_MODES).
#' @param quiet    TRUE suppresses the per-DB skip messages.
#' @return data.frame, one row per recipient code x donor clip: recipient_species,
#'   unit, date (text as stored), source_species, source_db, source_mode,
#'   mode_source, source_sample_id, source_file_name, classified_at. Attribute
#'   "scan": one row per DB (db, species, mode, mode_source, status ok/skipped/
#'   excluded_mode, reason, reader clip_others/legacy_notes/NA when skipped,
#'   n_other_lines = donor clips, n_invalid_lines (legacy only), n_rows).
#' A DB that fails to open or has no clips table is skipped with a message; no DB
#' is ever opened read-write.
harvest_other <- function(db_paths, modes = OTHER_DONOR_MODES, quiet = FALSE) {
  empty <- data.frame(recipient_species = character(0), unit = character(0), date = character(0),
                      source_species = character(0), source_db = character(0),
                      source_mode = character(0), mode_source = character(0),
                      source_sample_id = integer(0), source_file_name = character(0),
                      classified_at = character(0), stringsAsFactors = FALSE)
  rows <- list(); scan <- list()
  for (p in unique(db_paths)) {
    sc <- list(db = p, species = NA_character_, mode = NA_character_, mode_source = NA_character_,
               status = "ok", reason = NA_character_, reader = NA_character_, n_other_lines = 0L,
               n_invalid_lines = 0L, n_rows = 0L)
    res <- tryCatch(.other_read_db(p), error = function(e) e)
    if (inherits(res, "error")) {
      sc$status <- "skipped"; sc$reason <- conditionMessage(res)
      if (!quiet) message("harvest_other: skipped ", p, " (", sc$reason, ")")
      scan[[length(scan) + 1L]] <- sc; next
    }
    sc$species <- res$sp; sc$mode <- res$md$mode; sc$mode_source <- res$md$source
    sc$reader <- res$reader
    cl <- res$cl
    sc$n_other_lines <- as.integer(res$n_lines)
    sc$n_invalid_lines <- as.integer(res$n_invalid)
    if (!res$md$mode %in% modes) {
      sc$status <- "excluded_mode"
    } else if (nrow(cl)) {
      rows[[length(rows) + 1L]] <- data.frame(
        recipient_species = as.character(cl$code), unit = as.character(cl$unit),
        date = as.character(cl$date), source_species = res$sp, source_db = p,
        source_mode = res$md$mode, mode_source = res$md$source,
        source_sample_id = as.integer(cl$sample_id),
        source_file_name = as.character(cl$file_name),
        classified_at = as.character(cl$classified_at), stringsAsFactors = FALSE)
      sc$n_rows <- nrow(cl)
    }
    scan[[length(scan) + 1L]] <- sc
  }
  out <- if (length(rows)) do.call(rbind, rows) else empty
  rownames(out) <- NULL
  sdf <- if (length(scan)) do.call(rbind, lapply(scan, as.data.frame, stringsAsFactors = FALSE))
         else data.frame(db = character(0), species = character(0), mode = character(0),
                         mode_source = character(0), status = character(0), reason = character(0),
                         reader = character(0), n_other_lines = integer(0), n_invalid_lines = integer(0),
                         n_rows = integer(0), stringsAsFactors = FALSE)
  attr(out, "scan") <- sdf
  out
}
