# =============================================================================
# shared/other.R — the `other=` reader and the period key, one copy for the
# pipeline and the validator (ARCHITECTURE.md §1.1; brief
# docs/R6_OTHER_HARVEST_BRIEF_2026-09-30.md).
#
# Validators record other species they are sure they heard as a first line of a
# clip's notes: `other=BTNW,NAWA` (contract §4.2). R6 counts each code as an
# occupancy detection of that species in the clip's unit x period; R3 never reads
# notes. The app will reuse these to show such cells as resolved.
#
#   period_key(dates, grain)   Date -> period key (moved from core/build_effort_grid.R)
#   parse_other(notes)         one notes value -> the codes on a valid first line
#   harvest_other(db_paths)    every other= code in the DBs, one row per code x clip
#   OTHER_DONOR_MODES          DB modes whose other= codes count
#
# Pure definitions: sourcing twice is harmless. DBI/RSQLite are used by namespace
# inside harvest_other() only. A change here is a `shared:` commit decided with Phil.
# =============================================================================

# Modes whose other= codes count as detections. Drop "calibration" to stop Sample 1
# donating (one line; brief decision 2).
OTHER_DONOR_MODES <- c("calibration", "occupancy")

# The first-line format (contract §4.2). Same pattern as the app's NOTES_OTHER_RE.
OTHER_LINE_RE <- "^other=[A-Z0-9-]+(,[A-Z0-9-]+)*$"

#' Collapse Dates to the chosen period key.
#' @param dates a Date (or "YYYY-MM-DD") vector.
#' @param grain "week" (ISO week, e.g. 2025-W23), "5day" (5-day blocks within a
#'   year, e.g. 2025-P34), or "day" (the date).
#' Note: "%Y-W%V" pairs the calendar year with the ISO week; at a year boundary the
#' ISO year (%G) can differ. Kept as is: R4 wrote the DBs' time_period with it.
period_key <- function(dates, grain) {
  dates <- as.Date(dates)
  if (grain == "day")  return(as.character(dates))
  if (grain == "5day") {
    doy <- as.integer(format(dates, "%j"))
    blk <- ((doy - 1L) %/% 5L) + 1L
    return(sprintf("%s-P%02d", format(dates, "%Y"), blk))
  }
  format(dates, "%Y-W%V")   # default: ISO year-week
}

#' One notes value -> the species codes on a valid first `other=` line, else
#' character(0). Anything else (not the first line, lower case, spaces) is free text.
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

# One DB, read-only: its mode, species and the clips whose notes start "other=".
.other_read_db <- function(p) {
  if (!file.exists(p)) stop("file not found")
  con <- DBI::dbConnect(RSQLite::SQLite(), p, flags = RSQLite::SQLITE_RO, synchronous = NULL)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  tabs <- DBI::dbListTables(con)
  if (!"clips" %in% tabs) stop("no clips table")
  cols <- DBI::dbListFields(con, "clips")
  if (!"notes" %in% cols) stop("clips has no notes column")
  opt <- function(col) if (col %in% cols) col else paste0("NULL AS ", col)
  list(md = .other_db_mode(con, tabs, cols), sp = .other_db_species(con, tabs),
       cl = DBI::dbGetQuery(con, paste0(
         "SELECT sample_id, unit, date, notes, ", opt("file_name"), ", ", opt("classified_at"),
         " FROM clips WHERE notes LIKE 'other=%' ORDER BY sample_id")))
}

#' Every other= code in the DBs, read-only.
#' @param db_paths validation DB paths (any mode, any species).
#' @param modes    donor modes kept (default OTHER_DONOR_MODES).
#' @param quiet    TRUE suppresses the per-DB skip messages.
#' @return data.frame, one row per recipient code x donor clip: recipient_species,
#'   unit, date (text as stored), source_species, source_db, source_mode,
#'   mode_source, source_sample_id, source_file_name, classified_at. Attribute
#'   "scan": one row per DB (db, species, mode, mode_source, status ok/skipped/
#'   excluded_mode, reason, n_other_lines, n_invalid_lines, n_rows).
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
               status = "ok", reason = NA_character_, n_other_lines = 0L,
               n_invalid_lines = 0L, n_rows = 0L)
    res <- tryCatch(.other_read_db(p), error = function(e) e)
    if (inherits(res, "error")) {
      sc$status <- "skipped"; sc$reason <- conditionMessage(res)
      if (!quiet) message("harvest_other: skipped ", p, " (", sc$reason, ")")
      scan[[length(scan) + 1L]] <- sc; next
    }
    sc$species <- res$sp; sc$mode <- res$md$mode; sc$mode_source <- res$md$source
    cl <- res$cl
    sc$n_other_lines <- nrow(cl)
    codes <- lapply(cl$notes, parse_other)
    sc$n_invalid_lines <- sum(lengths(codes) == 0L)
    if (!res$md$mode %in% modes) {
      sc$status <- "excluded_mode"
    } else if (nrow(cl)) {
      k <- rep(seq_len(nrow(cl)), lengths(codes))
      if (length(k)) {
        rows[[length(rows) + 1L]] <- data.frame(
          recipient_species = unlist(codes), unit = as.character(cl$unit[k]),
          date = as.character(cl$date[k]), source_species = res$sp, source_db = p,
          source_mode = res$md$mode, mode_source = res$md$source,
          source_sample_id = as.integer(cl$sample_id[k]),
          source_file_name = as.character(cl$file_name[k]),
          classified_at = as.character(cl$classified_at[k]), stringsAsFactors = FALSE)
        sc$n_rows <- length(k)
      }
    }
    scan[[length(scan) + 1L]] <- sc
  }
  out <- if (length(rows)) do.call(rbind, rows) else empty
  rownames(out) <- NULL
  sdf <- if (length(scan)) do.call(rbind, lapply(scan, as.data.frame, stringsAsFactors = FALSE))
         else data.frame(db = character(0), species = character(0), mode = character(0),
                         mode_source = character(0), status = character(0), reason = character(0),
                         n_other_lines = integer(0), n_invalid_lines = integer(0),
                         n_rows = integer(0), stringsAsFactors = FALSE)
  attr(out, "scan") <- sdf
  out
}
