# =============================================================================
# install_validator.R — install or update the ARU validation app.
#
# Paste this one line into the R console (RGui or RStudio):
#
#   source("https://raw.githubusercontent.com/tabanid/aru-validator/main/install_validator.R")
#
# It asks where to put the app (first time only; a folder "ARU-validator" is made
# there), downloads the app files and checks each one, installs any missing R
# packages, puts an "ARU Validator" shortcut on the Desktop (Windows), and starts
# the app. Run the same line again to update; your databases, clips and settings
# are never touched.
#
# For testing: ARU_VALIDATOR_BASE (download from here instead, e.g. file:///...),
# ARU_VALIDATOR_DIR (install here, no questions), ARU_VALIDATOR_NO_LAUNCH=1.
# =============================================================================
local({
  base <- Sys.getenv("ARU_VALIDATOR_BASE",
                     "https://raw.githubusercontent.com/tabanid/aru-validator/main/")
  if (!grepl("/$", base)) base <- paste0(base, "/")
  say <- function(...) cat(..., "\n", sep = "")
  is_win <- .Platform$OS.type == "windows"

  # ---- where to install (asked once, remembered) ----------------------------
  conf_dir  <- tools::R_user_dir("aru-validator", "config")
  conf_file <- file.path(conf_dir, "install_dir.txt")
  dest <- Sys.getenv("ARU_VALIDATOR_DIR", "")
  if (!nzchar(dest) && file.exists(conf_file)) {
    prev <- trimws(readLines(conf_file, n = 1L, warn = FALSE))
    if (length(prev) && dir.exists(prev)) { dest <- prev; say("Updating the copy in ", dest) }
  }
  if (!nzchar(dest)) {
    docs <- normalizePath("~", winslash = "/", mustWork = FALSE)   # Documents on Windows
    parent <- NA_character_
    if (is_win && interactive()) {
      say("A window will ask where to put the validator (a folder 'ARU-validator' is made there).")
      parent <- utils::choose.dir(default = docs, caption = "Where should the ARU validator go?")
    } else if (interactive()) {
      a <- readline(sprintf("Install in which folder? [Enter = %s] ", docs))
      parent <- if (nzchar(a)) a else docs
    }
    if (is.na(parent) || !nzchar(parent)) parent <- docs
    dest <- file.path(normalizePath(parent, winslash = "/", mustWork = FALSE), "ARU-validator")
  }
  dest <- normalizePath(dest, winslash = "/", mustWork = FALSE)
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(dest)) stop("Could not create the folder ", dest)

  # ---- download every file and check it ---------------------------------------
  say("Downloading the app ...")
  lst <- tempfile(fileext = ".txt")
  utils::download.file(paste0(base, "FILES.txt"), lst, mode = "wb", quiet = TRUE)
  man <- read.table(lst, col.names = c("md5", "file"), stringsAsFactors = FALSE)
  for (i in seq_len(nrow(man))) {
    f <- man$file[i]; tmp <- tempfile()
    utils::download.file(paste0(base, f), tmp, mode = "wb", quiet = TRUE)
    if (unname(tools::md5sum(tmp)) != man$md5[i]) stop("Download of ", f, " was damaged; please run the line again.")
    if (!file.copy(tmp, file.path(dest, f), overwrite = TRUE)) stop("Could not write ", file.path(dest, f))
  }
  version <- trimws(readLines(file.path(dest, "APP_VERSION.txt"), n = 1L, warn = FALSE))
  say("  ", nrow(man), " files, version ", version)

  # ---- R packages ---------------------------------------------------------------
  pkgs <- c("shiny", "bslib", "DBI", "RSQLite", "ggplot2", "tuneR", "seewave", "viridis")
  need <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(need)) {
    say("Installing R packages: ", paste(need, collapse = ", "), " (a few minutes, once) ...")
    repos <- getOption("repos")
    if (is.null(repos) || any(repos %in% "@CRAN@")) repos <- c(CRAN = "https://cloud.r-project.org")
    utils::install.packages(need, repos = repos)
    still <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
    if (length(still)) stop("These packages did not install: ", paste(still, collapse = ", "))
  }

  # ---- remember where, and a Desktop shortcut (Windows) --------------------------
  dir.create(conf_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(dest, conf_file)
  launch <- sprintf("shiny::runApp('%s', launch.browser = TRUE)", dest)
  writeLines(c("# Start the ARU validator:  source() this file, or open it in RStudio and click Source.",
               launch), file.path(dest, "run_validator.R"))
  if (is_win) {
    desk <- tryCatch(utils::readRegistry("Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\Shell Folders",
                                         hive = "HCU")$Desktop, error = function(e) NULL)
    if (is.null(desk) || !dir.exists(desk)) desk <- file.path(Sys.getenv("USERPROFILE"), "Desktop")
    rscript <- normalizePath(file.path(R.home("bin"), "Rscript.exe"), winslash = "\\", mustWork = FALSE)
    bat <- c("@echo off",
             "title ARU Validator - close this window to stop the app",
             sprintf("\"%s\" -e \"%s\"", rscript, launch),
             "if errorlevel 1 pause")
    if (dir.exists(desk)) {
      writeLines(bat, file.path(desk, "ARU Validator.bat"))
      say("Desktop shortcut: ", file.path(desk, "ARU Validator.bat"))
    }
    writeLines(bat, file.path(dest, "ARU Validator.bat"))
  }

  say("")
  say("Installed in: ", dest)
  say("Version:      ", version)
  say("Please send this 'Installed in' line to Phil.")
  say("")
  if (Sys.getenv("ARU_VALIDATOR_NO_LAUNCH") != "1") {
    say("Starting the app in your browser (press Esc or close the R window to stop it) ...")
    shiny::runApp(dest, launch.browser = TRUE)
  }
})
