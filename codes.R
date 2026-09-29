# =============================================================================
# shared/codes.R — the ONE definition of validation codes, flag/column names and
# the validation_meta / validation_sessions schema (ARCHITECTURE.md §1.1).
#
# Sourced by core/ (RUN_WORKFLOW.R, and a guard in every stage script) and by
# apps/validation/ (validation_database_V4_0.R; deploy.sh copies this file beside
# the app). Pure assignments: sourcing it twice is harmless. No library() calls;
# DBI is referenced by namespace inside the two helpers only.
#
# Decided 2026-09-24 (docs/WORKFLOW_DESIGN_DECISIONS.md Q2, Q5, Q6). A change here
# is a `shared:` commit decided with Phil first — never a side effect of a stage edit.
# =============================================================================

ARU_CODES_VERSION <- 1L     # bump only when a name below changes meaning

# ---- the six classification codes (contract §3, §4.2) ------------------------------
CODE_MEANING <- c(Y = "target species",
                  N = "not target",
                  O = "other bird",
                  I = "insect",
                  P = "peeper (frog)",
                  U = "unsure")
ALL_CODES      <- names(CODE_MEANING)             # "Y","N","O","I","P","U" — the write gate
DETECTION      <- "Y"                             # the only code that is a detection (Q2)
NOT_TARGET     <- c("N", "O", "I", "P")           # counted as N by R3 (O/I/P = N)
UNCERTAIN      <- "U"                             # reviewed, undecided; excluded from fits
RESOLVED_CODES <- c(DETECTION, NOT_TARGET)        # Y vs not-target: R3 fit set, R4 bake-in

# Pre-2026-06 "done" set. Kept ONLY so two legacy counters (register_validation_dbs
# n_yn, project_status_app) keep today's numbers; R5 no longer uses it (2026-09-24).
# Not a detection rule. Retire with those counters (Q12, Q18).
LEGACY_DONE_CODES <- c("Y", "N")

# ---- vocalisation type (recorded on Y clips) ----------------------------------------
VOC_SONG  <- "S"
VOC_CALL  <- "C"
VOC_TYPES <- c(VOC_SONG, VOC_CALL)                # NA when not given or not Y

# ---- clips flags and write-back columns (contract §4.1, §4.2) -----------------------
COL_SAMPLE1_NOMINEE <- "sample1_nominee"
COL_SAMPLE2_ACTIVE  <- "sample2_active"
COL_CLASSIFICATION  <- "classification"
COL_VOCALIZATION    <- "vocalization_type"
COL_CLASSIFIED      <- "classified"
COL_CLASSIFIED_AT   <- "classified_at"
WRITEBACK_COLS      <- c(COL_CLASSIFICATION, COL_CLASSIFIED, COL_VOCALIZATION,
                         COL_CLASSIFIED_AT, "notes")
TIMESTAMP_FORMAT    <- "%Y-%m-%d %H:%M:%S"        # GMT; classified_at, session_start/end

# ---- SQL fragments (RSQLite cannot bind an IN list) ---------------------------------
sql_in <- function(x) paste0("(", paste0("'", x, "'", collapse = ","), ")")
SQL_IN_ALL         <- sql_in(ALL_CODES)           # "('Y','N','O','I','P','U')"
SQL_IN_RESOLVED    <- sql_in(RESOLVED_CODES)      # "('Y','N','O','I','P')"
SQL_IN_LEGACY_DONE <- sql_in(LEGACY_DONE_CODES)   # "('Y','N')"
SQL_IS_DETECTION   <- paste0(COL_CLASSIFICATION, " = '", DETECTION, "'")   # "classification = 'Y'"

# ---- validation_meta (contract §5): build provenance, one row per DB ---------------
SCHEMA_VERSION       <- 2L                        # 2 = adds schema_version + validation_sessions
VALIDATION_META_COLS <- c("mode", "grain", "species", "top_k", "threshold",
                          "build_date", "pipeline_version", "schema_version")

# ---- validation_sessions (contract §5.1): who validated, when; append-only ----------
VALIDATION_SESSIONS_COLS <- c("validator", "app_version", "session_start",
                              "session_end", "n_classified")
VALIDATION_SESSIONS_DDL  <- "
  CREATE TABLE IF NOT EXISTS validation_sessions (
    validator     TEXT    NOT NULL,
    app_version   TEXT,
    session_start TEXT    NOT NULL,
    session_end   TEXT,
    n_classified  INTEGER
  )"

# ---- two helpers ----------------------------------------------------------------------
#' Schema version of an open validation DB. Absent table, absent column or NULL -> 1L.
aru_schema_version <- function(con) {
  if (!"validation_meta" %in% DBI::dbListTables(con)) return(1L)
  if (!"schema_version" %in% DBI::dbListFields(con, "validation_meta")) return(1L)
  v <- DBI::dbGetQuery(con, "SELECT schema_version FROM validation_meta LIMIT 1")$schema_version
  if (!length(v) || is.na(v[1])) 1L else as.integer(v[1])
}

#' Create validation_sessions if it does not exist (idempotent).
aru_ensure_validation_sessions <- function(con) {
  DBI::dbExecute(con, VALIDATION_SESSIONS_DDL)
  invisible(NULL)
}
