# =============================================================================
# spectrogram.R  —  Spectrogram rendering. Lifted verbatim from the working
# tool (validation_shinyInterface_V4_0.R) — the validation experience is kept.
# Reads a clip WAV from disk and returns a cached render() closure.
# =============================================================================

library(tuneR)
library(seewave)

spec_cache <- new.env()

generate_spectrogram <- function(file_path, wl = 512, ovlp = 50, wn = "hanning",
                                  flim = NULL, contrast = 5, color_scheme = "magma") {
  cache_key <- paste(file_path, wl, ovlp, wn,
                     paste(flim, collapse = ","),
                     contrast, color_scheme, sep = "_")

  if (exists(cache_key, envir = spec_cache)) {
    return(get(cache_key, envir = spec_cache))
  }

  result <- tryCatch({
    wave <- readWave(file_path)
    nyquist_freq <- wave@samp.rate / 2000

    if (!is.null(flim)) {
      flim[1] <- max(0, min(flim[1], nyquist_freq))
      flim[2] <- min(flim[2], nyquist_freq)
      if (flim[1] >= flim[2]) flim <- NULL
    }

    spec <- spectro(wave, plot = FALSE, dB = "max0", wl = wl, ovlp = ovlp,
                    wn = wn, flim = flim)

    mat <- spec$amp
    lower_q <- contrast / 100
    limits  <- quantile(mat, c(lower_q, 0.999), na.rm = TRUE)
    mat[mat < limits[1] - 5] <- limits[1] - 5
    mat[mat > limits[2]]     <- limits[2]

    if (color_scheme == "magma") {
      pal <- viridis::magma(100)
    } else if (color_scheme == "viridis") {
      pal <- viridis::viridis(100)
    } else {
      pal <- gray.colors(100, start = 0, end = 1)
    }

    list(
      success = TRUE,
      nyquist = nyquist_freq,
      render  = function() {
        par(mar = c(0, 0, 0, 0))
        image(x = spec$time, y = spec$freq, z = t(mat), col = pal,
              useRaster = TRUE, axes = FALSE, xlab = "", ylab = "")
        usr      <- par("usr")
        freq_ticks <- seq(0, usr[4], by = 1)
        tick_len   <- (usr[2] - usr[1]) * 0.015
        for (f in freq_ticks) {
          lines(x = c(usr[1], usr[1] + tick_len), y = c(f, f),
                col = "gray80", lwd = 1)
          text(x = usr[1] + tick_len * 1.5, y = f, labels = as.character(f),
               col = "gray80", cex = 0.6, adj = c(0, 0.5))
          lines(x = c(usr[2] - tick_len, usr[2]), y = c(f, f),
                col = "gray80", lwd = 1)
        }
      }
    )
  }, error = function(e) {
    list(success = FALSE, render = function() {
      suppressWarnings(plot.new())
      text(0.5, 0.5, "audio not found", col = "#b00", cex = 1.1)
    })
  })

  assign(cache_key, result, envir = spec_cache)
  return(result)
}

# Short clip-time label for the grid caption (lifted).
format_clip_datetime_short <- function(clip_datetime) {
  if (is.null(clip_datetime) || is.na(clip_datetime) || clip_datetime == "") return("")
  dt <- as.POSIXct(clip_datetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  if (is.na(dt)) return("")
  format(dt, "%b-%d %H:%M")
}
