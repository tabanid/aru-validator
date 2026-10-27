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

ARU_CODES_VERSION <- 2L     # bump only when a name below changes meaning
                            # 2 (2026-10-26): other-list constants, PARKED_CODES, schema 3

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

# Parked target codes (brief docs/briefs_done/CC_OTHER_LIST_BRIEF.md D8): still in CODE_MEANING so
# old data reads, but the app's write gate refuses them for new labels (Part 3; unused
# until then). What they stood for now goes in the other-species list.
PARKED_CODES <- c("O", "I", "P")

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
SCHEMA_VERSION       <- 3L                        # 2 = adds schema_version + validation_sessions
                                                  # 3 = adds clip_others, clip_candidates,
                                                  #     clips.others_complete (contract v4)
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

# ---- other species (contract v4 §4.4): one labelled row per species per clip ----------
OTHER_LABELS  <- c("Y", "N", "U")                  # the target's own codes; no row = not assessed
OTHER_SOURCES <- c("listed", "added", "legacy")    # in clip_candidates / typed / converter only
# HawkEars non-bird classes, valid other-species codes beside spcdHE bird codes (D6).
HE_NONBIRD_CODES <- c("Squirrel", "Canine", "Insects", "FROG-PEEP", "FROG-WOOD", "FROG-GREN",
                      "FROG-GRTR", "FROG-BULL", "FROG-LEOP", "FROG-PICK", "TOAD-AMER",
                      "Noise", "Speech", "Rooster", "Other")

# Pipeline-owned, written at build/convert time, read-only for the app: the list shown.
CLIP_CANDIDATES_DDL <- "
  CREATE TABLE IF NOT EXISTS clip_candidates (
    sample_id INTEGER NOT NULL REFERENCES clips(sample_id),
    code      TEXT    NOT NULL,
    he_score  REAL    NOT NULL,
    PRIMARY KEY (sample_id, code)
  )"

# App-owned: the validator's answers. Blank = no row.
CLIP_OTHERS_DDL <- "
  CREATE TABLE IF NOT EXISTS clip_others (
    sample_id     INTEGER NOT NULL REFERENCES clips(sample_id),
    code          TEXT    NOT NULL,
    label         TEXT    NOT NULL CHECK (label IN ('Y','N','U')),
    source        TEXT    NOT NULL CHECK (source IN ('listed','added','legacy')),
    classified_at TEXT    NOT NULL,
    PRIMARY KEY (sample_id, code)
  )"
COL_OTHERS_COMPLETE <- "others_complete"           # clips: NULL / 0 / 1

# ---- one-round species classes (docs/VALIDATION_WORKFLOW.md §W2) ----------------------
# Read by core/R2_species_classes.R, R3_occupancy_draw.R, R6 and R7. Ladder rare_vet -> sweep ->
# standard (Phil's edits to the approved file). "rare" (the site sweep's name before
# 2026-10-27) is read as "sweep" in an approved file.
OCC_CLASSES       <- c("standard", "sweep", "rare_vet", "no_signal", "no_cluster", "skip", "untriaged")
OCC_DRAWN_CLASSES <- c("standard", "sweep", "rare_vet")   # the classes that get a DB
OCC_CLASS_ALIASES <- c(rare = "sweep")

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
