# =============================================================================
# app.R  —  PAM Validation Tool (validation-only surface).
#
# Opens a pipeline-built SQLite validation DB and lets a human classify clips,
# writing labels back via the fixed five-field contract. NO CSV import, NO
# sampling, NO DB creation, NO threshold logic. Mode (calibration | occupancy)
# is read from validation_meta (flag fallback). Layout, button colours, and
# spectrogram controls mirror the previous tool.
#
# Run:  shiny::runApp("validator")
# Needs: shiny, DBI, RSQLite, tuneR, seewave, viridis, ggplot2
# =============================================================================

library(shiny)
library(bslib)
local({
  cand <- c(getwd(), tryCatch(dirname(sys.frame(1)$ofile), error = function(e) NA))
  dir <- NULL
  for (d in cand) if (!is.na(d) && file.exists(file.path(d, "db.R"))) { dir <- d; break }
  if (is.null(dir)) stop("Cannot locate db.R/spectrogram.R/plot.R — run via shiny::runApp() from the app folder.")
  for (f in c("db.R", "spectrogram.R", "plot.R")) source(file.path(dir, f), local = FALSE)
})

# Single-code buttons -> the code they write (Y/Song/Call are wired by name below).
CODE_BUTTON <- c(btn_no = "N", btn_other = "O", btn_insect = "I",   # code-map
                 btn_peeper = "P", btn_unsure = "U")                # code-map
stopifnot(setequal(CODE_BUTTON, c(NOT_TARGET, UNCERTAIN)))   # buttons for parked codes stay, greyed
# O, I and P are parked as target labels (brief D8; O since 2026-10-20). O is now the
# key into the other-species list (D9); it saves nothing. Mark O stays off.
# Mark O and Mark U are not on screen (Phil 2026-10-27).
# IP_BUTTONS_ON = TRUE brings back the I / P / Mark I / Mark P buttons and their keys
# (the write gate, WRITE_CODES in db.R, still refuses the codes).
IP_BUTTONS_ON <- FALSE
stopifnot(all(PARKED_CODES %in% CODE_BUTTON))
`%|na|%` <- function(a, b) if (length(a) == 0 || is.na(a)) b else a

# HawkEars codes for the list's Add box: unknown codes are flagged, never blocked. The
# non-bird classes (Squirrel, FROG-PEEP, ...) and the quick-add codes count as known (D6).
KNOWN_SPECIES_CODES <- local({ k <- load_known_species_codes(); if (is.null(k)) NULL else c(k, HE_NONBIRD_CODES, OTHER_QUICK_CODES) })

# ---- display-settings persistence (A5) — same file as the previous tool ------
# ARU_VALIDATOR_SETTINGS overrides the location (tests point it at a temp file).
SETTINGS_FILE <- Sys.getenv("ARU_VALIDATOR_SETTINGS", unset = "audio_classifier_settings.rds")
load_settings <- function() {
  defaults <- list(wl = 512, ovlp = 50, wn = "hanning", freq_range = c(0, 12),
                   color_scheme = "magma", contrast = 5, spec_height = 260,
                   grid_rows = 5, grid_cols = 4, last_dir = path.expand("~"),
                   validator = "",   # remembered validator name (contract §5.1)
                   show_p = FALSE,   # p(correct) pill: display only, hidden by default (brief F2)
                   auto_next_page = FALSE)   # last clip answered -> next page (off: Phil 2026-10-20)
  if (file.exists(SETTINGS_FILE)) {
    tryCatch({
      s <- readRDS(SETTINGS_FILE)
      for (k in names(defaults)) if (is.null(s[[k]])) s[[k]] <- defaults[[k]]
      s
    }, error = function(e) defaults)
  } else defaults
}
save_settings <- function(settings) saveRDS(settings, SETTINGS_FILE)
# Persist just the last-used directory without disturbing other saved settings.
.persist_last_dir <- function(d) {
  s <- tryCatch(load_settings(), error = function(e) list())
  s$last_dir <- d; try(save_settings(s), silent = TRUE)
}
.persist_validator <- function(v) {
  s <- tryCatch(load_settings(), error = function(e) list())
  s$validator <- v; try(save_settings(s), silent = TRUE)
}

# App version, recorded in validation_sessions (contract §5.1). APP_VERSION.txt beside
# the app wins (deploy.sh stamps it, brief R6); otherwise "5.0", plus the git short
# hash when run inside the repo.
APP_VERSION <- local({
  v <- "5.0"
  if (file.exists("APP_VERSION.txt")) {
    f <- trimws(readLines("APP_VERSION.txt", n = 1L, warn = FALSE))
    if (length(f) == 1L && nzchar(f)) return(f)
  }
  h <- tryCatch(suppressWarnings(system2("git", c("rev-parse", "--short", "HEAD"),
                                          stdout = TRUE, stderr = FALSE))[1],
                error = function(e) "")
  if (is.character(h) && length(h) == 1L && !is.na(h) && grepl("^[0-9a-f]{7,}$", h))
    paste0(v, "+", h) else v
})

# Key under the coverage plot (brief F1). Colours match plot.R temporal_tiles. White
# stays ambiguous until effort_grid.csv is wired in: no clip at all for that cell.
.coverage_legend <- function() {
  sw <- function(col, txt) tags$span(style = "margin-right:10px; white-space:nowrap;",
    tags$span(style = sprintf("display:inline-block;width:10px;height:10px;background:%s;border:1px solid #bbb;vertical-align:middle;margin-right:3px;", col)),
    txt)
  div(class = "coverage-legend", style = "font-size:11px; color:#444; margin-top:3px;",
      sw("#cccccc", "grey = to do"), sw("#dc3545", "red = examined, no Y"),
      sw("#28a745", "green = Y"),
      sw("#17a2b8", "teal = present, heard in another species' clip (skip it)"),
      sw("#ffffff", "white = no clip in this DB (no detection \u2265 \u03c4, or not recorded)"))
}

# I / P / Mark I / Mark P are left off the screen unless IP_BUTTONS_ON (Phil 2026-10-27);
# their keys then do nothing (pressBtn finds no button).
.ip_switch <- function(btn) if (IP_BUTTONS_ON) btn
# Place a control at a fixed cell of the button grid.
at <- function(row, col, tag) tagAppendAttributes(tag, style = sprintf("grid-row:%d; grid-column:%d;", row, col))

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
ui <- page_sidebar(
  theme = bs_theme(version = 5),
  title = paste0("PAM Validation Tool ", APP_VERSION),
  window_title = "PAM Validation Tool",
  fillable = FALSE,

  tags$head(
    tags$style(HTML("
      .spec-plot { border-style:solid; border-width:4px; padding:2px; cursor:pointer; margin:2px; position:relative; }
      .spec-plot-selected { box-shadow:0 0 0 5px black; z-index:2; }
      .box-clip { font-size:14px; line-height:18px; color:#333; white-space:nowrap; overflow:hidden;
                  text-overflow:ellipsis; }
      .seek-line { position:absolute; top:4px; bottom:22px; width:2px; background:#ff3b30;
                   pointer-events:none; z-index:5; }
      .spectrogram-container { height:55vh; min-height:160px; overflow:auto; }
      .clip-resizer { height:14px; margin:2px 0 8px; cursor:row-resize;
                      display:flex; align-items:center; justify-content:center; }
      .clip-resizer::before { content:''; width:70px; height:4px; border-radius:2px; background:#cbd3da; }
      .clip-resizer:hover::before, .clip-resizer.dragging::before { background:#0d6efd; }
      #notes { height:38px !important; }
      #other_add { height:28px !important; font-size:13px; }
      .other-list { font-size:13px; margin-top:4px; }
      .other-head { font-size:11px; color:#333; }
      .other-hint { display:none; color:#0d6efd; font-weight:600; }
      .other-list:focus-within .other-hint { display:inline; }
      .other-row { display:flex; align-items:center; gap:4px; padding:1px 3px; border-radius:3px; outline:none; }
      .other-row:focus { box-shadow:0 0 0 2px #0d6efd; background:#eef6ff; }
      .other-code { min-width:76px; font-family:monospace; }
      .other-code.unknown { background:#ffb300; padding:0 3px; border-radius:3px; }
      .other-btn { width:26px; height:22px; padding:0; font-size:12px; border:1px solid #bbb; background:#fff; border-radius:3px; }
      .other-quick { display:flex; gap:4px; margin-top:3px; }
      .other-quick button { height:22px; padding:0 6px; font-size:11px; font-family:monospace; border:1px solid #bbb; background:#fff; border-radius:3px; }
      .other-quick button:hover { background:#eef6ff; border-color:#0d6efd; }
      .other-btn.on-Y { background:#4caf50; color:#fff; border-color:#4caf50; }
      .other-btn.on-N { background:#dc3545; color:#fff; border-color:#dc3545; }
      .other-btn.on-U { background:#ff9800; color:#fff; border-color:#ff9800; }
      .other-x { border:none; background:none; color:#888; padding:0 4px; }
      .other-none { color:#888; font-size:12px; }
      .species-pick-wrap select { width:100% !important; font-size:14px; line-height:1.5; }
      .species-pick-wrap select option { padding:6px 8px; }
      .mode-cal { color:#6a1b9a; font-weight:bold; }
      .mode-occ { color:#1565c0; font-weight:bold; }
      .dir-entry:hover { background:#eef6ff; }
      .sb-card { margin-bottom:.6rem; }
      .sb-card > .card-header { padding:.4rem .7rem; font-weight:600; font-size:.86rem; background:#f7f7f9; }
      .sb-card > .card-body { padding:.55rem .7rem; }
      .sb-details > summary { cursor:pointer; font-weight:600; padding:.35rem 0; }
      .controls-container { width:100%; min-width:640px; margin-top:10px; display:grid; gap:8px;
                            grid-template-columns:repeat(5, minmax(60px, 1fr)) minmax(200px, 2fr)
                                                  minmax(0, 2fr) minmax(70px, 110px);
                            grid-auto-rows:80px; }
      .ctl-btn { width:100%; min-width:0; height:80px; font-weight:bold; font-size:17px; padding:0;
                 display:flex; align-items:center; justify-content:center; border-radius:4px; }
      .ctl-notes { grid-column:6; grid-row:1 / span 3; min-width:0; overflow-y:auto; }
      .ctl-notes #notes { height:60px; }
      .temporal-scroll { overflow-x:auto; }
      #nav_buttons { display:contents; }
      .bslib-sidebar-layout { position:relative; }
      .sidebar-resizer { position:absolute; top:0; bottom:0; width:9px; margin-left:-4px;
                         cursor:col-resize; z-index:30; }
      .sidebar-resizer:hover, .sidebar-resizer.dragging { background:rgba(13,110,253,0.28); }
      .form-group, .shiny-input-container { margin-bottom:.5rem; }
    ")),
    tags$script(HTML("
      $(document).on('keydown', function(e) {
        // Typing in Notes or Other species never fires a shortcut (R8). Enter saves the
        // boxes to the clip they belong to and stays there; Esc puts back what was saved.
        // Both leave the box (blur), which gives the keys back to the shortcuts. The
        // save itself happens on leaving the box (the focus/blur handlers below).
        var ae = document.activeElement.id;
        if (ae === 'db_path') {
          if (e.key === 'Enter') {
            Shiny.setInputValue('db_path_enter', {path: $('#db_path').val(), n: Math.random()},
                                {priority:'event'});
            e.preventDefault();
          }
          return;
        }
        // The other-species list (brief Part 3): O puts the focus on a row. There y / n / u
        // label that row (pressing the lit one again clears it), arrows move between rows,
        // a goes to the Add box, Space plays, Esc or Enter leave. Every other key is ignored,
        // so a stray key never labels or clears the target while in the list.
        var ael = document.activeElement;
        if (ael && $(ael).hasClass('other-row')) {
          var rows = $('#other_list .other-row'), i = rows.index(ael), lk = e.key.toLowerCase();
          if (e.key === 'Tab') return;
          if (e.key === 'ArrowDown')     { if (i < rows.length - 1) rows.eq(i + 1).focus(); }
          else if (e.key === 'ArrowUp')  { if (i > 0) rows.eq(i - 1).focus(); }
          else if (lk === 'y' || lk === 'n' || lk === 'u') otherSet(ael.dataset.code, lk.toUpperCase(), true);
          else if (lk === 'a')           $('#other_add').focus();
          else if (e.key === ' ')        $('#btn_play').click();
          else if (e.key === 'Escape' || e.key === 'Enter') { window.otherFocus = null; ael.blur(); }
          e.preventDefault(); return;
        }
        // Add box: Enter adds the typed code(s) to the clip the list belongs to and stays
        // (empty: leaves); Esc empties it and leaves.
        if (ae === 'other_add') {
          if (e.key === 'Enter') {
            var v = $('#other_add').val();
            if (v.trim()) {
              Shiny.setInputValue('other_add_go', {txt: v, sid: otherSid(), n: Math.random()}, {priority:'event'});
              $('#other_add').val('').trigger('input');
            } else document.activeElement.blur();
            e.preventDefault();
          } else if (e.key === 'Escape') { $('#other_add').val('').trigger('input'); document.activeElement.blur(); e.preventDefault(); }
          return;
        }
        if (ae === 'notes') {
          if (e.key === 'Enter') { document.activeElement.blur(); e.preventDefault(); }
          else if (e.key === 'Escape') {
            var b = document.activeElement;
            if (b.dataset.orig !== undefined) { b.value = b.dataset.orig; $(b).trigger('input'); }
            b.blur(); e.preventDefault();
          }
          return;
        }
        // Any other place you can type (validator name, DB path, number boxes, the
        // select search boxes): the key is text, never a shortcut.
        var el = document.activeElement;
        // A dropdown's search box: text only while its list is open. Closed, it gives
        // the keys back (it keeps the cursor after a pick, even of the same value).
        if (el && el.id && /-selectized$/.test(el.id)) {
          if ($(el).closest('.selectize-input').hasClass('dropdown-active')) return;
          el.blur(); el = document.body;
        }
        if (el && (el.isContentEditable || el.tagName === 'TEXTAREA' || el.tagName === 'SELECT' ||
            (el.tagName === 'INPUT' && /^(text|search|number|email|password|url|tel|)$/i.test(el.type || '')))) return;
        if (e.altKey) {
          var m = {y:'btn_mark_y', s:'btn_mark_s', c:'btn_mark_c', n:'btn_mark_n',
                   u:'btn_mark_u', o:'btn_mark_o', i:'btn_mark_i', p:'btn_mark_p'};
          var k = e.key.toLowerCase();
          if (m[k]) { pressBtn(m[k]); e.preventDefault(); }
          return;
        }
        var s = {s:'btn_yes_song', c:'btn_yes_call', y:'btn_yes', n:'btn_no',
                 u:'btn_unsure', o:'btn_other', i:'btn_insect', p:'btn_peeper', x:'btn_clear'};
        var k = e.key.toLowerCase();
        if (s[k]) { pressBtn(s[k]); e.preventDefault(); }
        else if (e.key === ' ')          { $('#btn_play').click(); e.preventDefault(); }
        else if (e.key === 'ArrowLeft')  { Shiny.setInputValue('arrow_key','left', {priority:'event'}); e.preventDefault(); }
        else if (e.key === 'ArrowRight') { Shiny.setInputValue('arrow_key','right',{priority:'event'}); e.preventDefault(); }
        else if (e.key === 'ArrowUp')    { Shiny.setInputValue('arrow_key','up',   {priority:'event'}); e.preventDefault(); }
        else if (e.key === 'ArrowDown')  { Shiny.setInputValue('arrow_key','down', {priority:'event'}); e.preventDefault(); }
      });
      // A shortcut presses its button, if it is on screen (not I, P, Mark O/U/I/P).
      function pressBtn(id) { var b = $('#'+id); if (b.length && !b.prop('disabled')) b.click(); }
      // The clip the list belongs to (data-sid on the rendered list).
      function otherSid() { var l = $('#other_list .other-list'); return l.length ? parseInt(l.attr('data-sid')) : null; }
      // Set a list row. The server toggles: the lit label again clears the row. From the
      // keyboard, the focus comes back to the same row after the list redraws.
      function otherSet(code, label, fromKey) {
        window.otherFocus = fromKey ? {code: code, sid: otherSid()} : null;
        Shiny.setInputValue('other_set', {code: code, label: label, sid: otherSid(), n: Math.random()}, {priority:'event'});
      }
      $(document).on('shiny:value', function(ev) {
        if (ev.name !== 'other_list' || !window.otherFocus) return;
        var f = window.otherFocus;
        setTimeout(function() {
          var r = $('#other_list .other-row').filter(function() { return this.dataset.code === f.code; });
          if (r.length && otherSid() === f.sid) r.focus(); else window.otherFocus = null;
        }, 30);
      });
      // Notes belong to one clip, named beside the box (box_clip). On entering the box,
      // remember what it held; on leaving it (Enter, Tab, a click on a clip or a button),
      // save it if it changed. The server saves to the clip the box was loaded for, so
      // the entry cannot land on the next clip. Free text only (contract v4).
      $(document).on('focus', '#notes', function() { this.dataset.orig = this.value; });
      $(document).on('blur', '#notes', function() {
        if (this.value === this.dataset.orig) return;
        this.dataset.orig = this.value;
        Shiny.setInputValue('save_note', {notes: this.value, n: Math.random()}, {priority:'event'});
      });
      // After picking from a dropdown (view, unit, group...), give the keys back: its
      // search box would otherwise keep the cursor and swallow every shortcut.
      $(document).on('change', 'select', function() {
        setTimeout(function() {
          var a = document.activeElement;
          if (a && a.id && /-selectized$/.test(a.id)) a.blur();
        }, 0);
      });
      // Species list: a click puts that DB's path in the path box, a double-click
      // opens it. Only real user picks fire (a redraw of the list does not).
      $(document).on('change', '#species_pick', function() {
        Shiny.setInputValue('species_clicked', this.value, {priority:'event'});
      });
      $(document).on('dblclick', '#species_pick option', function() {
        Shiny.setInputValue('species_dblclick', this.value, {priority:'event'});
      });
      // O moves the focus into the other-species list: its first row, or the Add box
      // when the clip has no rows (D9). Nothing is saved.
      Shiny.addCustomMessageHandler('focus_other', function(m) {
        setTimeout(function(){
          var r = $('#other_list .other-row').first();
          if (r.length) r.focus(); else $('#other_add').focus();
        }, 50);
      });
      Shiny.addCustomMessageHandler('play_audio_client', function(message) {
        var a = document.getElementById('audio_player'); if (!a) return;
        var frac = 0;
        var active = document.querySelector('.spec-plot-selected');
        if (active) { var ln = active.querySelector('.seek-line');
          if (ln && ln.dataset.frac) frac = parseFloat(ln.dataset.frac) || 0; }
        var dur = (typeof message.dur === 'number' && message.dur > 0) ? message.dur : 0;
        if (a._objurl) { try { URL.revokeObjectURL(a._objurl); } catch(e){} a._objurl = null; }
        var startPlay = function(){
          a.removeEventListener('canplay', startPlay);
          var d = dur || (isFinite(a.duration) ? a.duration : 0);
          if (frac > 0 && d > 0) { try { a.currentTime = frac * d; } catch(e){} }
          a.play().catch(function(err){ console.log('audio play failed', err); });
        };
        // Fetch the whole clip into memory; a blob URL is always seekable,
        // unlike a streamed file served without HTTP range support.
        // Unique query + no-store: every species folder holds clip-0001.wav ..., so
        // the same URL would otherwise replay the previous species' cached bytes.
        var nocache = message.url + (message.url.indexOf('?') < 0 ? '?' : '&') + 't=' + Date.now();
        fetch(nocache, { cache: 'no-store' }).then(function(r){ return r.arrayBuffer(); }).then(function(buf){
          a._objurl = URL.createObjectURL(new Blob([buf], { type: 'audio/wav' }));
          a.src = a._objurl;
          a.addEventListener('canplay', startPlay);
          a.load();
        }).catch(function(err){
          console.log('audio fetch failed, streaming instead', err);
          a.src = message.url + '?t=' + new Date().getTime();
          a.addEventListener('canplay', startPlay);
          a.load();
        });
      });
      // Click a clip: if it's already active, just set the start point + line
      // (audio does NOT start). Otherwise select it. Play/space starts the audio.
      function clipSeekLine(el, x, frac){
        document.querySelectorAll('.seek-line').forEach(function(n){ n.remove(); });
        var line = document.createElement('div'); line.className = 'seek-line';
        line.style.left = x + 'px'; line.dataset.frac = frac; el.appendChild(line);
      }
      function clipClick(el, idx, ev){
        var ae = document.activeElement;              // clicking a clip leaves the text boxes
        if (ae && (ae.id === 'notes' || ae.id === 'other_add' || $(ae).hasClass('other-row'))) {
          window.otherFocus = null; ae.blur(); }
        var rect = el.getBoundingClientRect();
        var x = ev.clientX - rect.left;
        var w = el.clientWidth || rect.width;
        var frac = Math.max(0, Math.min(1, x / w));
        if (el.classList.contains('spec-plot-selected')) {
          clipSeekLine(el, x, frac);          // set start point only
        } else {
          document.querySelectorAll('.seek-line').forEach(function(n){ n.remove(); });
          Shiny.setInputValue('grid_click', idx, {priority:'event'});
        }
      }
      // Drag-to-resize the sidebar divider.
      $(document).ready(function(){
        setTimeout(function(){
          var layout = document.querySelector('.bslib-sidebar-layout');
          if (!layout) return;
          var handle = document.createElement('div');
          handle.className = 'sidebar-resizer';
          layout.appendChild(handle);
          var dragging = false;
          function cur(){ var v = getComputedStyle(layout).getPropertyValue('--_sidebar-width'); return parseInt(v) || 540; }
          function place(){ handle.style.left = cur() + 'px'; }
          function setW(px){ px = Math.max(280, Math.min(820, px));
            layout.style.setProperty('--_sidebar-width', px + 'px'); handle.style.left = px + 'px'; }
          place();
          handle.addEventListener('mousedown', function(e){ dragging = true; handle.classList.add('dragging');
            document.body.style.userSelect = 'none'; e.preventDefault(); });
          document.addEventListener('mousemove', function(e){ if (!dragging) return;
            setW(e.clientX - layout.getBoundingClientRect().left); });
          document.addEventListener('mouseup', function(){ if (!dragging) return; dragging = false;
            handle.classList.remove('dragging'); document.body.style.userSelect = ''; });
          window.addEventListener('resize', place);
        }, 350);
      });
      // Drag the bar to resize the clip window height (moves the buttons up/down).
      $(document).ready(function(){
        setTimeout(function(){
          var handle = document.querySelector('.clip-resizer');
          var cont = document.querySelector('.spectrogram-container');
          if (!handle || !cont) return;
          var dragging = false;
          handle.addEventListener('mousedown', function(e){ dragging = true;
            handle.classList.add('dragging'); document.body.style.userSelect = 'none'; e.preventDefault(); });
          document.addEventListener('mousemove', function(e){ if (!dragging) return;
            var top = cont.getBoundingClientRect().top;
            var h = Math.max(140, Math.min(window.innerHeight - 120, e.clientY - top));
            cont.style.height = h + 'px'; });
          document.addEventListener('mouseup', function(){ if (!dragging) return; dragging = false;
            handle.classList.remove('dragging'); document.body.style.userSelect = ''; });
        }, 400);
      });
    ")),
    tags$audio(id = "audio_player", style = "display:none;")
  ),

  sidebar = sidebar(
    width = 820,
    uiOutput("mode_panel"),
    conditionalPanel("output.data_loaded == true",
      # F5: in occupancy group view the group box and its navigation sit at the top,
      # so the coverage and score plots stay in view without scrolling.
      conditionalPanel("output.is_occupancy == true && input.view_mode == 'group'",
        card(class = "sb-card",
          card_header("Group (unit \u00d7 occasion)"),
          card_body(
            uiOutput("group_box"),
            uiOutput("group_select_ui"),
            div(style = "display:flex; gap:6px; flex-wrap:wrap;",
              actionButton("btn_prev_group", "\u25c4 Prev", class = "btn-warning btn-sm"),
              actionButton("btn_next_group", "Next \u25ba", class = "btn-warning btn-sm"),
              actionButton("btn_next_unresolved", "Next unresolved", class = "btn-info btn-sm"))
          )
        )
      ),
      conditionalPanel("output.is_occupancy == true",
        card(class = "sb-card",
          card_header("Coverage (unit \u00d7 occasion)"),
          card_body(div(class = "temporal-scroll",
                        plotOutput("temporal_plot", height = "180px",
                                   click = "temporal_click",
                                   hover = hoverOpts("temporal_hover", delay = 150))),
                    div(style = "font-size:11px; color:#333; min-height:15px;",
                        textOutput("temporal_hover_label", inline = TRUE)),
                    .coverage_legend(), padding = 4)
        )
      ),
      card(class = "sb-card",
        card_header("Score vs truth"),
        card_body(plotOutput("classification_plot", height = "180px"), padding = 4)
      ),
      card(class = "sb-card",
        card_header("Progress"),
        card_body(
          uiOutput("summary_table"),
          uiOutput("confidence_legend"),
          conditionalPanel("!(output.is_occupancy == true && input.view_mode == 'group')",
            hr(), uiOutput("group_stats_table"))
        )
      ),
      card(class = "sb-card",
        card_body(
          actionButton("save_settings", "Save settings", class = "btn-success btn-sm w-100"),
          div(style = "font-size:11px; color:#777; margin-top:4px;",
              "Saves spectrogram settings, layout, and the open database path.")
        )
      ),
      card(class = "sb-card",
        card_header("View & layout"),
        card_body(
          selectInput("view_mode", "View",
            c("Occupancy groups"           = "group",
              "Top clips by score"         = "topscore",
              "Best clip per cell"         = "bestcell",
              "All clips (review)"         = "allclips",
              "All clips for one unit"     = "unit")),
          conditionalPanel("input.view_mode == 'unit'", uiOutput("unit_select_ui")),
          radioButtons("show_codes", "Show", SHOW_CHOICES, selected = "all", inline = TRUE),
          checkboxInput("auto_next_page", "Go to the next page after the last clip", value = FALSE),
          div(style = "display:flex; gap:8px;",
            numericInput("grid_rows", "Rows", 5, min = 1, max = 12, step = 1),
            numericInput("grid_cols", "Cols", 4, min = 1, max = 8,  step = 1)),
          sliderInput("spec_height", "Clip height (px)", min = 120, max = 500,
                      value = 260, step = 20),
          checkboxInput("show_p", "Show p(correct) (pipeline estimate)", value = FALSE)
        )
      ),
      card(class = "sb-card",
        card_body(
          div(style = "display:flex; gap:6px; align-items:center;",
            actionButton("btn_prev_page", "\u25c4 Page", class = "btn-sm"),
            actionButton("btn_next_page", "Page \u25ba", class = "btn-sm"),
            conditionalPanel("input.view_mode == 'bestcell' || input.view_mode == 'topscore'",
              actionButton("btn_next_view", "Reload", class = "btn-info btn-sm",
                           title = "Pull the next batch of unclassified clips")),
            span(style = "font-size:12px; color:#555;",
                 textOutput("page_label", inline = TRUE)))
        )
      )
    ),
    conditionalPanel("output.has_scanned == true",
      card(class = "sb-card",
        card_header("Choose species"),
        card_body(uiOutput("species_select_ui"))
      )
    ),
    card(class = "sb-card",
      card_header("Open validation database"),
      card_body(
        # The box is the one "where": the folder list writes the folder it shows into
        # it, Open / Scan (or Enter) acts on it. No browser form history (old paths).
        htmltools::tagQuery(textInput("db_path", NULL,
                  placeholder = "/path/to/species-folder  or  SPCD-occupancy.db"))$
          find("input")$addAttrs(autocomplete = "off", spellcheck = "false")$allTags(),
        div(style = "display:flex; gap:6px; flex-wrap:wrap; margin-bottom:6px;",
            actionButton("btn_go", "Open / Scan", class = "btn-primary btn-sm"),
            actionButton("btn_browse_up", "\u2191 Up", class = "btn-sm"),
            actionButton("btn_browse_home", "Home", class = "btn-sm")),
        conditionalPanel("output.show_browser == true", uiOutput("dir_browser")),
        div(style = "margin-top:6px; font-size:12px;", uiOutput("open_status"))
      )
    ),
    conditionalPanel("output.data_loaded == true",
      tags$details(class = "sb-details",
        tags$summary("Spectrogram settings"),
        div(style = "padding-top:.3rem;",
          sliderInput("freq_range", "Frequency range (kHz)", min = 0, max = 12,
                      value = c(0, 12), step = 0.5),
          sliderInput("contrast", "Contrast (%)", min = 0, max = 50, value = 5, step = 1),
          radioButtons("wl", "Window length",
                       c("128"=128,"256"=256,"512"=512,"1024"=1024),
                       selected = 512, inline = TRUE),
          div(style = "display:flex; gap:8px;",
            numericInput("ovlp", "Overlap (%)", value = 50, min = 0, max = 95, step = 5),
            selectInput("wn", "Window",
                        c("hanning","hamming","blackman","bartlett"), selected = "hanning")),
          selectInput("color_scheme", "Colour",
                      c("Magma"="magma","Viridis"="viridis","Grayscale"="gray"), selected = "magma"),
          actionButton("update_spec", "Update spectrograms", class = "btn-info btn-sm w-100")
        )
      )
    )
  ),

  # ---- main display area ----
  uiOutput("warn_banner"),
  card(
    full_screen = TRUE,
    card_header("Clips"),
    div(class = "spectrogram-container", uiOutput("spectrogram_grid"))
  ),
  div(class = "clip-resizer", title = "Drag to resize the clip window"),
  # Buttons sit in a 5 x 3 grid, each at a fixed cell (at(row, col)) so nothing shifts
  # while the nav buttons (drawn once a DB is open) are missing; the notes / other-species
  # panel is column 6 over all three rows; column 7 is space; Quit is column 8, bottom row,
  # against the right edge (Phil 2026-10-27).
  div(class = "controls-container",
    at(1, 1, actionButton("btn_play", "\u25b6 Play", class = "btn-secondary ctl-btn")),
    uiOutput("nav_buttons"),                                     # row 1, columns 2-3
    at(1, 4, actionButton("btn_clear", "Clear", class = "ctl-btn",
                 style = "background-color:#f44336;border-color:#f44336;color:#fff;", title = "Clear (x)")),
    at(1, 5, actionButton("export_consolidated", "Export", class = "btn-primary ctl-btn")),
    at(2, 1, actionButton("btn_yes", "Yes", class = "ctl-btn",
                 style = "background-color:#1b5e20;border-color:#1b5e20;color:#fff;font-size:15px;", title = "Yes (unspecified)")),
    at(2, 2, actionButton("btn_yes_song", "Song", class = "ctl-btn",
                 style = "background-color:#4caf50;border-color:#4caf50;color:#fff;font-size:15px;", title = "Yes + Song")),
    at(2, 3, actionButton("btn_yes_call", "Call", class = "ctl-btn",
                 style = "background-color:#aed581;border-color:#aed581;color:#000;font-size:15px;", title = "Yes + Call")),
    at(2, 4, actionButton("btn_no", "N", class = "btn-danger ctl-btn", style = "font-size:18px;")),
    at(2, 5, actionButton("btn_other", "O", class = "btn-info ctl-btn", style = "font-size:18px;",
                 title = "Other species list (O)")),
    at(3, 5, actionButton("btn_unsure", "U", class = "btn-warning ctl-btn", style = "font-size:18px;")),
    at(3, 1, actionButton("btn_mark_y", "Mark Y", class = "btn-success ctl-btn", title = "Mark remaining as Yes (Alt+Y)")),
    at(3, 2, actionButton("btn_mark_s", "Mark S", class = "ctl-btn",
                 style = "background-color:#4caf50;border-color:#4caf50;color:#fff;", title = "Mark remaining as Song (Alt+S)")),
    at(3, 3, actionButton("btn_mark_c", "Mark C", class = "ctl-btn",
                 style = "background-color:#aed581;border-color:#aed581;color:#000;", title = "Mark remaining as Call (Alt+C)")),
    at(3, 4, actionButton("btn_mark_n", "Mark N", class = "btn-danger ctl-btn", title = "Mark remaining as No (Alt+N)")),
    at(3, 8, actionButton("btn_quit", "Quit", class = "btn-dark ctl-btn")),
    # IP_BUTTONS_ON only: these fill the free cells of column 7 and below
    .ip_switch(actionButton("btn_insect", "I", class = "ctl-btn",
                 style = "background-color:#2196f3;border-color:#2196f3;color:#fff;font-size:18px;", title = "Insect")),
    .ip_switch(actionButton("btn_peeper", "P", class = "ctl-btn",
                 style = "background-color:#9c27b0;border-color:#9c27b0;color:#fff;font-size:18px;", title = "Peeper")),
    .ip_switch(actionButton("btn_mark_i", "Mark I", class = "ctl-btn",
                 style = "background-color:#2196f3;border-color:#2196f3;color:#fff;", title = "Mark remaining as Insect (Alt+I)")),
    .ip_switch(actionButton("btn_mark_p", "Mark P", class = "ctl-btn",
                 style = "background-color:#9c27b0;border-color:#9c27b0;color:#fff;", title = "Mark remaining as Peeper (Alt+P)")),
    div(class = "ctl-notes",
        uiOutput("box_clip"),
        # no browser form history on the free-text boxes (it filled old code combos, JW)
        htmltools::tagQuery(textInput("notes", NULL, placeholder = "Notes..."))$
          find("input")$addAttrs(autocomplete = "off")$allTags(),
        uiOutput("other_list"),
        div(style = "display:flex; gap:4px; align-items:center;",
            htmltools::tagQuery(textInput("other_add", NULL, placeholder = "Add (e.g. BTNW)"))$
              find("input")$addAttrs(autocomplete = "off", spellcheck = "false")$allTags(),
            uiOutput("other_add_flag", inline = TRUE)),
        # one click adds the code as Y through the Add path; focus stays where it was
        div(class = "other-quick", lapply(OTHER_QUICK_CODES, function(k)
          tags$button(type = "button", tabindex = "-1", k, title = paste("Add", k, "as Y"),
            onmousedown = "event.preventDefault();",
            onclick = sprintf("Shiny.setInputValue('other_add_go',{txt:'%s',sid:otherSid(),n:Math.random()},{priority:'event'})", k)))),
        uiOutput("other_complete"))
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  saved_settings <- load_settings()

  observe({
    updateRadioButtons(session, "wl",          selected = saved_settings$wl)
    updateNumericInput(session, "ovlp",        value    = saved_settings$ovlp)
    updateSelectInput(session,  "wn",          selected = saved_settings$wn)
    updateSliderInput(session,  "freq_range",  value    = saved_settings$freq_range)
    updateSelectInput(session,  "color_scheme",selected = saved_settings$color_scheme)
    updateSliderInput(session,  "contrast",    value    = saved_settings$contrast)
    updateSliderInput(session,  "spec_height", value    = saved_settings$spec_height)
    updateNumericInput(session, "grid_rows",   value    = saved_settings$grid_rows)
    updateNumericInput(session, "grid_cols",   value    = saved_settings$grid_cols)
    updateCheckboxInput(session, "show_p",     value    = isTRUE(saved_settings$show_p))
    updateCheckboxInput(session, "auto_next_page", value = isTRUE(saved_settings$auto_next_page))
    if (!is.null(saved_settings$last_dir) && dir.exists(saved_settings$last_dir)) {
      rv$browse_dir <- saved_settings$last_dir
      isolate(.scan_into(saved_settings$last_dir))     # species list ready at launch
    }
  })

  rv <- reactiveValues(
    vdb = NULL, mode = NULL, mode_info = NULL, species = NULL, grain = NULL,
    groups = NULL, group_idx = 1L, unit_order = NULL, units = NULL, view_unit = NULL,
    all_samples = NULL, page = 1L, grid_samples = NULL,
    active_position = 1L, previous_active_position = NULL,
    last_updated_clip = NULL, classification_counter = 0L, bulk_update = FALSE,
    status_message = "", loading_status = "", warn_message = NULL, data_loaded = FALSE,
    grid_rebuild_trigger = 0L, last_audio_url = NULL,
    validator = trimws(saved_settings$validator %||% ""), session_row = NULL, pending_open = NULL,
    browse_dir = path.expand("~"), show_browser = FALSE, scanned = NULL, scan_root = NULL,
    panel_msg = NULL,  # latest Open / Scan / browse result, shown in the Open panel
    borrowed = NULL,   # other= surfacing for the open species (occupancy only)
    view_mode = "group",
    show = "all",      # Show filter (SHOW_CHOICES); back to all on every open
    box_sid = NULL,    # sample_id the Notes box and the other-species list belong to
    other_rev = 0L,    # bumped on every list write: redraws the list
    anchor = NULL,     # validation_meta.anchor_date (grain anchored_week)
    empty_note = NULL, # why the grid is empty (Show filter), shown in its place
    spec_params = list(wl = 512, ovlp = 50, wn = "hanning", flim = NULL,
                       contrast = 5, color_scheme = "magma")
  )

  # ---- who is validating (contract §5.1): asked at launch, remembered -----
  .ask_validator <- function() showModal(modalDialog(
    title = "Who is validating?",
    textInput("validator_name", "Your name or initials (recorded with every session)",
              value = isolate(rv$validator)),
    footer = actionButton("confirm_validator", "OK"), easyClose = FALSE))
  .ask_validator()
  observeEvent(input$confirm_validator, {
    v <- trimws(input$validator_name %||% "")
    if (!nzchar(v)) {
      showNotification("A validator name is required", type = "error", duration = 4)
      return()
    }
    rv$validator <- v; .persist_validator(v); removeModal()
    p <- rv$pending_open; rv$pending_open <- NULL
    if (!is.null(p)) .do_open(p)
  })

  # Close this app session's validation_sessions row on the open DB (no-op if none).
  .close_session_row <- function() {
    vdb <- isolate(rv$vdb); rid <- isolate(rv$session_row)
    if (!is.null(vdb) && !is.null(rid))
      try(close_validation_session_row(vdb$conn, rid), silent = TRUE)
    rv$session_row <- NULL
  }

  page_size <- reactive({
    r <- input$grid_rows %||% 4; c <- input$grid_cols %||% 4
    max(1L, as.integer(r) * as.integer(c))
  })

  output$data_loaded   <- reactive({ isTRUE(rv$data_loaded) })
  output$is_occupancy  <- reactive({ isTRUE(rv$mode == "occupancy") })
  outputOptions(output, "data_loaded",  suspendWhenHidden = FALSE)
  outputOptions(output, "is_occupancy", suspendWhenHidden = FALSE)

  # ---- spec params: initialised on open, refreshed on Update Spectrograms ----
  .read_spec_params <- function() {
    fl <- input$freq_range
    if (is.null(fl) || length(fl) != 2) fl <- NULL
    list(wl = as.numeric(input$wl %||% 512), ovlp = input$ovlp %||% 50,
         wn = input$wn %||% "hanning", flim = fl,
         contrast = input$contrast %||% 5, color_scheme = input$color_scheme %||% "magma")
  }
  observeEvent(input$update_spec, {
    rv$spec_params <- .read_spec_params()
  })

  # ---- open (folder browser + per-species scan, or a direct .db) --------
  .do_open <- function(path) {
    path <- trimws(path %||% "")
    if (!nzchar(path)) { rv$status_message <- "Choose a database."; return(invisible()) }
    if (!nzchar(rv$validator %||% "")) {             # contract §5.1: no anonymous sessions
      rv$pending_open <- path
      rv$status_message <- "\u26a0 Enter the validator name first"
      .ask_validator(); return(invisible())
    }
    .close_session_row()
    if (!is.null(rv$vdb)) try(close_validation_db(rv$vdb), silent = TRUE)

    vdb <- tryCatch(open_validation_db(path),
                    error = function(e) { rv$warn_message <- conditionMessage(e); NULL })
    if (is.null(vdb)) { rv$vdb <- NULL; rv$data_loaded <- FALSE; return(invisible()) }
    rv$vdb <- vdb; rv$warn_message <- NULL
    rv$session_row <- tryCatch(open_validation_session_row(vdb$conn, rv$validator, APP_VERSION),
      error = function(e) {
        rv$warn_message <- paste("Could not record the validation session:", conditionMessage(e))
        NULL })

    mi <- detect_mode(vdb)
    rv$mode_info <- mi; rv$mode <- mi$mode
    if (!is.null(mi$warning)) rv$warn_message <- mi$warning
    rv$species <- if (!is.null(mi$meta) && "species" %in% names(mi$meta) &&
                      !is.na(mi$meta$species))
      as.character(mi$meta$species) else
      paste(unique(dbGetQuery(vdb$conn, "SELECT DISTINCT spcd FROM clips")$spcd), collapse = ", ")
    rv$grain <- if (!is.null(mi$meta) && "grain" %in% names(mi$meta) && !is.na(mi$meta$grain))
      as.character(mi$meta$grain) else "week"
    rv$anchor <- if (!is.null(mi$meta) && "anchor_date" %in% names(mi$meta) && !is.na(mi$meta$anchor_date))
      as.character(mi$meta$anchor_date) else NULL

    # one resource prefix per audio dir that exists (beside / clips/{SPCD} / fallback)
    for (k in names(vdb$audio_dirs)) addResourcePath(AUDIO_PREFIXES[[k]], vdb$audio_dirs[[k]])
    rv$clip_dur <- tryCatch({
      f <- NA_character_
      for (d in vdb$audio_dirs) {
        f <- list.files(d, pattern = "\\.wav$", full.names = TRUE, ignore.case = TRUE)[1]
        if (!is.na(f)) break
      }
      if (!is.na(f) && file.exists(f)) {
        h <- tuneR::readWave(f, header = TRUE); as.numeric(h$samples) / as.numeric(h$sample.rate)
      } else NA_real_
    }, error = function(e) NA_real_)

    rv$spec_params <- .read_spec_params()
    rv$units <- tryCatch(db_units(vdb), error = function(e) character(0))   # F4 unit view
    rv$view_unit <- rv$units[1] %|na|% NULL

    rv$borrowed <- NULL
    if (rv$mode == "occupancy") {
      g <- derive_groups(vdb); ord <- .unit_order_by_lon(g); rv$unit_order <- ord
      g <- g[order(match(g$unit, ord), g$time_period), , drop = FALSE]; rownames(g) <- NULL
      rv$borrowed <- .build_borrowed(vdb, g)
      g <- apply_borrowed(g, rv$borrowed$cells)
      rv$groups <- g; rv$group_idx <- .first_unresolved(g)
      rv$view_mode <- "bestcell"; updateSelectInput(session, "view_mode", selected = "bestcell")
    } else {
      rv$groups <- NULL
      rv$view_mode <- "topscore"; updateSelectInput(session, "view_mode", selected = "topscore")
    }
    rv$show <- "all"; updateRadioButtons(session, "show_codes", selected = "all")
    rv$data_loaded <- TRUE
    .load_view()
    # A fully validated DB leaves the work views empty: open on the review view instead.
    if (!is.null(rv$all_samples) && nrow(rv$all_samples) == 0 &&
        isTRUE(dbGetQuery(vdb$conn, "SELECT COUNT(*) AS n FROM clips")$n > 0)) {
      rv$view_mode <- "allclips"; updateSelectInput(session, "view_mode", selected = "allclips")
      .load_view()
      rv$status_message <- "\u2713 Every clip is classified \u2014 showing all clips for review."
    }
    rv$show_browser <- FALSE
    rv$panel_msg <- NULL
    updateTextInput(session, "db_path", value = vdb$path)   # the box shows the open DB
    .persist_last_dir(vdb$db_dir)
    rv$loading_status <- sprintf("Opened %s \u2014 %s mode (%s)",
      basename(vdb$path), rv$mode, rv$mode_info$source)
    invisible()
  }

  # other= surfacing (brief 2026-09-30): rebuilt read-only on every occupancy open, so
  # other= lines written earlier in this session count. Index folder: the scanned
  # folder when this DB came from the scan, else the DB's own folder (a dbs/ folder
  # steps up to the project). Off, with a warning, if the grain check fails.
  .build_borrowed <- function(vdb, groups) {
    from_scan <- !is.null(rv$scan_root) && !is.null(rv$scanned) && vdb$path %in% rv$scanned$db_path
    root <- other_index_root(if (from_scan) rv$scan_root else vdb$path)
    chk <- grain_check(vdb, rv$grain, rv$anchor)
    if (!isTRUE(chk$ok)) {
      msg <- sprintf("Other-species surfacing is off for this DB: its time_period does not match its dates at grain '%s' (%s).",
                     rv$grain, if (is.na(chk$n_mismatch)) chk$example
                               else sprintf("%d of %d clips, e.g. %s", chk$n_mismatch, chk$n_checked, chk$example))
      rv$warn_message <- paste(c(rv$warn_message, msg), collapse = " ")
      return(list(off = TRUE, cells = NULL, n_out = 0L, n_nocell = 0L, n_offweek = 0L, n_rows = 0L, n_dbs = 0L, root = root))
    }
    idx <- tryCatch(build_other_index(root, vdb$spcd), error = function(e) NULL)
    if (is.null(idx)) return(list(off = FALSE, cells = NULL, n_out = 0L, n_nocell = 0L, n_offweek = 0L, n_rows = 0L, n_dbs = 0L, root = root))
    b <- borrowed_cells(idx, rv$grain, groups, rv$anchor)
    list(off = FALSE, cells = b$cells, n_out = b$n_out, n_nocell = b$n_nocell, n_offweek = b$n_offweek, n_rows = nrow(idx),
         n_dbs = sum(attr(idx, "scan")$status != "skipped"), root = root)
  }

  # Panel feedback is time-stamped, so a repeat Open / Scan of the same folder
  # still visibly answers.
  .panel_note <- function(msg) {
    rv$panel_msg <- sprintf("%s (%s)", msg, format(Sys.time(), "%H:%M:%S"))
    rv$status_message <- msg
  }

  .scan_into <- function(dir) {
    rv$scanned <- tryCatch(scan_species_dbs(dir), error = function(e) NULL)
    if (is.null(rv$scanned))
      .panel_note(sprintf("No validation databases found in %s or its subfolders.", basename(dir)))
    else {
      rv$scan_root <- normalizePath(dir)
      .panel_note(sprintf("Found %d database(s) under %s \u2014 choose species above",
                          nrow(rv$scanned), basename(dir)))
      .persist_last_dir(dir)
    }
  }

  # "Open / Scan": a .db path opens directly; a folder is scanned for species DBs.
  .go <- function(p) {
    p <- trimws(p %||% "")
    if (!nzchar(p)) { .panel_note("Enter a folder or a .db path."); return() }
    if (grepl("\\.db$", p, ignore.case = TRUE) && file.exists(p)) { .do_open(p); return() }
    if (dir.exists(p)) {
      rv$browse_dir <- normalizePath(p); rv$show_browser <- TRUE; .scan_into(rv$browse_dir)
    } else .panel_note(paste("Path not found:", p))
  }
  observeEvent(input$btn_go, .go(input$db_path))
  # A folder pasted or typed into the box is scanned once typing pauses, as if Open / Scan
  # were pressed. A .db path still waits for Open / Scan (or Enter): opening is a choice.
  db_path_typed <- debounce(reactive(input$db_path), 600)
  observeEvent(db_path_typed(), {
    p <- trimws(db_path_typed() %||% "")
    if (nzchar(p) && dir.exists(p) && !identical(normalizePath(p), rv$browse_dir)) .go(p)
  }, ignoreInit = TRUE)
  observeEvent(input$db_path_enter, .go(input$db_path_enter$path))

  # The folder list and the box stay in step: every folder change (into a folder,
  # Up, Home, Open / Scan, launch) writes that folder into the box. Opening a DB
  # does not change the folder, so the box then keeps the DB's path.
  observeEvent(rv$browse_dir, updateTextInput(session, "db_path", value = rv$browse_dir))

  observeEvent(input$btn_browse_home, {
    rv$browse_dir <- path.expand("~"); rv$show_browser <- TRUE
  })
  observeEvent(input$btn_browse_up, {
    parent <- dirname(rv$browse_dir)
    if (nzchar(parent) && parent != rv$browse_dir) rv$browse_dir <- parent
    rv$show_browser <- TRUE
  })

  dir_listing <- reactive({ rv$browse_dir; list_dir_entries(rv$browse_dir) })

  observeEvent(input$browse_pick, {                       # clicked a sub-folder
    ents <- dir_listing(); i <- as.integer(input$browse_pick)
    if (!is.na(i) && i >= 1 && i <= nrow(ents) && ents$is_dir[i])
      rv$browse_dir <- normalizePath(file.path(rv$browse_dir, ents$name[i]))
  })
  observeEvent(input$browse_open, {                       # clicked a .db file
    ents <- dir_listing(); i <- as.integer(input$browse_open)
    if (!is.na(i) && i >= 1 && i <= nrow(ents) && !ents$is_dir[i])
      .do_open(file.path(rv$browse_dir, ents$name[i]))
  })
  .scanned_row <- function(v) {
    i <- suppressWarnings(as.integer(v))
    if (!is.null(rv$scanned) && length(i) == 1 && !is.na(i) && i >= 1 && i <= nrow(rv$scanned)) i else NA_integer_
  }
  observeEvent(input$species_clicked, {
    i <- .scanned_row(input$species_clicked)
    if (!is.na(i)) updateTextInput(session, "db_path", value = rv$scanned$db_path[i])
  })
  observeEvent(input$species_dblclick, {
    i <- .scanned_row(input$species_dblclick)
    if (!is.na(i)) .do_open(rv$scanned$db_path[i])
  })
  observeEvent(input$btn_open_species, {
    req(rv$scanned, input$species_pick)
    i <- as.integer(input$species_pick)
    if (!is.na(i) && i >= 1 && i <= nrow(rv$scanned)) .do_open(rv$scanned$db_path[i])
  })

  # ---- context loaders --------------------------------------------------
  .attach_paths <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(df)
    df$file_path <- vapply(df$file_name, function(fn) resolve_clip_path(rv$vdb, fn), character(1))
    df
  }
  # Every view's clip list goes through the Show filter, then to page 1.
  .set_samples <- function(df) {
    n0 <- if (is.null(df)) 0L else nrow(df)
    df <- filter_by_answer(df, rv$show)
    rv$empty_note <- if (nrow(df) > 0 || identical(rv$show, "all")) NULL
    else if (!identical(rv$show, "none") && rv$view_mode %in% c("topscore", "bestcell"))
      "No clips to display. This view holds unclassified clips only; to review answers, choose View: All clips (review)."
    else if (n0 > 0)
      sprintf("No clips to display: none of the %d clip(s) in this view are '%s'. Set Show to All.",
              n0, names(SHOW_CHOICES)[SHOW_CHOICES == rv$show])
    rv$all_samples <- .attach_paths(df); rv$page <- 1L; .recompute_page()
  }
  .load_all_context <- function() .set_samples(load_all_clips(rv$vdb))
  .load_group_context <- function() {
    req(rv$groups); cur <- rv$groups[rv$group_idx, ]
    .set_samples(load_group_clips(rv$vdb, cur$unit, cur$time_period))
  }
  # Switch what clips are shown, by view. Per-clip labels and cell resolution are
  # unchanged by the view — it only re-orders/filters what's on screen.
  .load_view <- function() {
    vm <- rv$view_mode %||% "group"
    if (rv$mode == "occupancy" && vm == "group") {
      .load_group_context()
    } else if (vm == "unit") {                        # F4: one unit, all weeks, by score
      u <- rv$view_unit %||% rv$units[1]
      .set_samples(if (is.null(u) || is.na(u)) NULL else load_unit_clips(rv$vdb, u))
    } else if (vm == "allclips") {                    # browse everything, paginated
      .load_all_context()
    } else {
      .load_batch()            # topscore / bestcell / calibration: work queue
    }
  }
  # Load the next chunk of the highest-value UNCLASSIFIED work for the current
  # view. The clips just classified drop out, so this surfaces the next batch.
  .load_batch <- function() {
    df <- if (isTRUE(rv$view_mode == "bestcell")) load_best_per_cell(rv$vdb, exclude = rv$borrowed$cells)
          else                                    load_unclassified_clips(rv$vdb)
    .set_samples(df)
    if (is.null(rv$all_samples) || nrow(rv$all_samples) == 0)
      rv$status_message <- "\u2713 Nothing left to classify in this view."
  }
  # The work queues hold unclassified clips only, so Show there would filter a list of
  # what is left to do: picking any Show option moves to All clips (review) instead.
  observeEvent(input$show_codes, {
    if (identical(input$show_codes, rv$show)) return()
    rv$show <- input$show_codes
    if (!isTRUE(rv$data_loaded)) return()
    if (rv$view_mode %in% c("topscore", "bestcell")) {
      rv$view_mode <- "allclips"; updateSelectInput(session, "view_mode", selected = "allclips")
    }
    .load_view()
  }, ignoreInit = TRUE)
  observeEvent(input$view_mode, {
    if (!isTRUE(rv$data_loaded)) return()
    rv$view_mode <- input$view_mode
    if (rv$view_mode == "bestcell" && rv$mode == "occupancy") .refresh_groups_keep_idx()
    .load_view()
  }, ignoreInit = TRUE)
  .recompute_page <- function() {
    ps <- page_size(); n <- if (is.null(rv$all_samples)) 0L else nrow(rv$all_samples)
    rv$box_sid <- NULL       # set again when the new page's active clip loads its notes
    if (n == 0) { rv$grid_samples <- rv$all_samples; rv$active_position <- 1L
      rv$grid_rebuild_trigger <- rv$grid_rebuild_trigger + 1L; return() }
    start <- (rv$page - 1L) * ps + 1L
    if (start > n) { rv$page <- ceiling(n / ps); start <- (rv$page - 1L) * ps + 1L }
    end <- min(start + ps - 1L, n)
    rv$grid_samples <- rv$all_samples[start:end, , drop = FALSE]
    rv$active_position <- 1L
    rv$grid_rebuild_trigger <- rv$grid_rebuild_trigger + 1L
  }
  observeEvent(list(input$grid_rows, input$grid_cols), {
    if (isTRUE(rv$data_loaded)) .recompute_page()
  }, ignoreInit = TRUE)

  # ---- unit view (F4) ---------------------------------------------------
  output$unit_select_ui <- renderUI({
    req(rv$data_loaded, length(rv$units) > 0)
    selectInput("unit_pick", "Unit (west \u2192 east)", choices = rv$units, selected = rv$view_unit)
  })
  observeEvent(input$unit_pick, {
    if (!isTRUE(rv$data_loaded) || identical(input$unit_pick, rv$view_unit)) return()
    rv$view_unit <- input$unit_pick
    if (isTRUE(rv$view_mode == "unit")) .load_view()
  })

  # ---- group navigation -------------------------------------------------
  output$group_select_ui <- renderUI({
    req(rv$groups)
    labs <- sprintf("%s | %s | %s (%d/%d)", rv$groups$unit, rv$groups$time_period,
                    rv$groups$status, rv$groups$n_examined, rv$groups$n_total)
    selectInput("group_pick", NULL, choices = stats::setNames(seq_len(nrow(rv$groups)), labs),
                selected = rv$group_idx)
  })
  observeEvent(input$group_pick, {
    idx <- as.integer(input$group_pick)
    if (!is.na(idx) && idx != rv$group_idx) { rv$group_idx <- idx; .load_group_context() }
  })
  .go_prev_group <- function() { req(rv$groups, rv$view_mode == "group"); rv$group_idx <- max(1L, rv$group_idx - 1L); .load_group_context() }
  .go_next_group <- function() { req(rv$groups, rv$view_mode == "group"); rv$group_idx <- min(nrow(rv$groups), rv$group_idx + 1L); .load_group_context() }
  observeEvent(input$btn_prev_group,  .go_prev_group())
  observeEvent(input$btn_next_group,  .go_next_group())
  observeEvent(input$btn_prev_group2, .go_prev_group())
  observeEvent(input$btn_next_group2, .go_next_group())
  observeEvent(input$btn_next_unresolved, {
    req(rv$groups); nx <- .next_unresolved(rv$groups, rv$group_idx)
    if (is.na(nx)) rv$status_message <- "All groups resolved \u2713"
    else { rv$group_idx <- nx; .load_group_context() }
  })
  # "Next view": in group view -> next cell; in the work-queue views -> next batch.
  observeEvent(input$btn_next_view, { req(rv$data_loaded); .load_batch() })
  # Forward/back control in the button row, labelled per view.
  output$nav_buttons <- renderUI({
    req(rv$data_loaded)
    if (isTRUE(rv$view_mode == "group")) {
      tagList(
        at(1, 2, actionButton("btn_prev_group2", "\u25c4", class = "btn-warning ctl-btn", title = "Previous group")),
        at(1, 3, actionButton("btn_next_group2", "\u25ba", class = "btn-warning ctl-btn", title = "Next group")))
    } else {
      tagList(
        at(1, 2, actionButton("btn_prev_page2", "\u25c4", class = "btn-warning ctl-btn", title = "Previous page")),
        at(1, 3, actionButton("btn_next_page2", "\u25ba", class = "btn-warning ctl-btn", title = "Next page")))
    }
  })

  # ---- page navigation --------------------------------------------------
  .page_next <- function() {
    req(rv$all_samples); ps <- page_size()
    if (rv$page * ps < nrow(rv$all_samples)) { rv$page <- rv$page + 1L; .recompute_page() }
  }
  .page_prev <- function() { if (rv$page > 1L) { rv$page <- rv$page - 1L; .recompute_page() } }
  observeEvent(input$btn_next_page,  .page_next())
  observeEvent(input$btn_prev_page,  .page_prev())
  observeEvent(input$btn_next_page2, .page_next())
  observeEvent(input$btn_prev_page2, .page_prev())
  output$page_label <- renderText({
    req(rv$all_samples); ps <- page_size()
    sprintf("page %d / %d", rv$page, max(1L, ceiling(nrow(rv$all_samples) / ps)))
  })

  # ---- spectrogram grid -------------------------------------------------
  output$spectrogram_grid <- renderUI({
    req(rv$data_loaded); rv$grid_rebuild_trigger
    isolate({
      s <- rv$grid_samples
      if (is.null(s) || nrow(s) == 0)
        return(div(style = "padding:20px; color:#666;", rv$empty_note %||% "No clips to display."))
      cols <- input$grid_cols %||% 4; n <- nrow(s); nrw <- ceiling(n / cols)
      pos <- 1L; rows <- list()
      for (r in seq_len(nrw)) {
        rp <- list()
        for (cc in seq_len(cols)) {
          if (pos <= n) {
            w <- floor(100 / cols)
            rp[[cc]] <- div(style = sprintf("flex:0 0 %d%%; max-width:%d%%;", w, w),
                            uiOutput(paste0("spec_container_", pos)))
          }
          pos <- pos + 1L
        }
        rows[[r]] <- div(style = "display:flex; flex-wrap:nowrap;", rp)
      }
      do.call(tagList, rows)
    })
  })

  observe({
    req(rv$data_loaded); rv$grid_rebuild_trigger
    gs <- isolate(rv$grid_samples); req(gs)
    for (i in seq_len(nrow(gs))) local({
      idx <- i
      output[[paste0("spec_", idx)]] <- renderPlot({
        sp <- rv$spec_params   # refreshes only when Update Spectrograms is clicked
        spec <- generate_spectrogram(isolate(rv$grid_samples)$file_path[idx],
          wl = sp$wl, ovlp = sp$ovlp, wn = sp$wn, flim = sp$flim,
          contrast = sp$contrast, color_scheme = sp$color_scheme)
        spec$render()
      })
      output[[paste0("spec_container_", idx)]] <- renderUI({
        cur_last <- rv$last_updated_clip; cur_act <- rv$active_position
        prev_act <- isolate(rv$previous_active_position); rv$classification_counter
        if (!is.null(cur_last) && idx == cur_last) rv$grid_samples
        else if (!is.null(cur_act) && (idx == cur_act ||
                 (!is.null(prev_act) && idx == prev_act))) rv$grid_samples
        else if (isTRUE(isolate(rv$bulk_update))) rv$grid_samples
        else isolate(rv$grid_samples)

        s <- isolate(rv$grid_samples)
        if (is.null(s) || idx > nrow(s)) return(div())
        smp <- s[idx, ]
        is_active <- !is.null(rv$active_position) && idx == rv$active_position
        ph <- max(120, input$spec_height %||% 260)
        border <- if (is.na(smp$classified) || smp$classified == 0 || is.na(smp$classification))
          "#e0e0e0" else unname(CODE_COLOUR[as.character(smp$classification)] %|na|% "#6c757d")
        cls <- if (is_active) "spec-plot spec-plot-selected" else "spec-plot"
        div(class = cls, style = paste0("border-color:", border, ";"),
            onclick = sprintf("clipClick(this, %d, event)", idx),
            plotOutput(paste0("spec_", idx), height = paste0(ph, "px")),
            div(style = "text-align:center; font-size:11px; font-weight:bold;",
              HTML(.clip_label(smp, show_p = isTRUE(input$show_p)))))
      })
    })
  })

  observeEvent(input$grid_click, { rv$active_position <- input$grid_click })
  observeEvent(input$arrow_key, {
    req(rv$grid_samples); n <- nrow(rv$grid_samples); cols <- input$grid_cols %||% 4
    rv$previous_active_position <- rv$active_position; a <- rv$active_position
    a <- switch(input$arrow_key,
      "left"  = if (a - 1 < 1) n else a - 1,
      "right" = if (a + 1 > n) 1 else a + 1,
      "up"    = if (a - cols < 1) a else a - cols,
      "down"  = if (a + cols > n) a else a + cols, a)
    rv$active_position <- a
  })

  # TRUE when the grid has a clip at the active position (an empty view has none:
  # Play, code keys and note saves then do nothing instead of acting on an NA row).
  .has_active <- function() {
    s <- rv$grid_samples; a <- rv$active_position
    !is.null(s) && !is.null(a) && length(a) == 1 && !is.na(a) && a >= 1 && a <= nrow(s)
  }
  # Load the active clip's notes into the box and point the list at it. Also on every
  # grid rebuild: a new page keeps active_position at 1, and the box must not carry the
  # previous clip's text onto the new one.
  observeEvent(list(rv$active_position, rv$grid_rebuild_trigger), {
    req(.has_active(), rv$vdb)
    if (rv$active_position > nrow(rv$grid_samples)) return()
    smp <- rv$grid_samples[rv$active_position, ]
    r <- dbGetQuery(rv$vdb$conn, "SELECT notes FROM clips WHERE sample_id = ?",
                    params = list(smp$sample_id))
    updateTextInput(session, "notes", value = if (nrow(r) && !is.na(r$notes)) r$notes else "")
    rv$box_sid <- smp$sample_id      # the box and the list now belong to this clip
  })
  output$box_clip <- renderUI({
    req(rv$box_sid, rv$grid_samples)
    fn <- rv$grid_samples$file_name[rv$grid_samples$sample_id == rv$box_sid][1]
    if (is.na(fn)) return(NULL)
    span(class = "box-clip", title = paste("Notes and other species are saved to", fn),
         "for ", strong(tools::file_path_sans_ext(fn)))
  })
  # Leaving the Notes box (Enter, Tab, a click) saves it to the clip it belongs to
  # (rv$box_sid), not to whichever clip is active by then; the selection stays put.
  observeEvent(input$save_note, {
    req(rv$vdb, rv$box_sid)
    ev <- input$save_note
    save_note(rv$vdb, rv$box_sid, if (is.list(ev) && !is.null(ev$notes)) ev$notes else input$notes)
    rv$status_message <- "\u2713 Note saved"
  })

  # ---- the other-species list (brief Part 3; contract v4 §4.4) -------------
  # Always the active clip's (rv$box_sid). Every click writes at once to the open DB,
  # never advances, and is independent of the target label (D7).
  .other_row <- function(r, unk) {
    btn <- function(l) tags$button(type = "button", tabindex = "-1", l,
      class = paste("other-btn", if (identical(r$label, l)) paste0("on-", l)),
      onmousedown = "event.preventDefault();",
      onclick = sprintf("otherSet('%s','%s',false)", r$code, l))
    div(class = "other-row", tabindex = "0", `data-code` = r$code,
        title = if (r$source == "legacy") "converted from an old other= note" else NULL,
        span(class = paste("other-code", if (r$code %in% unk) "unknown"), r$code),
        lapply(OTHER_LABELS, btn),
        if (!r$listed) tags$button(type = "button", tabindex = "-1", class = "other-x", "\u00d7",
          title = "remove", onmousedown = "event.preventDefault();",
          onclick = sprintf("Shiny.setInputValue('other_set',{code:'%s',label:null,sid:otherSid(),n:Math.random()},{priority:'event'})", r$code)))
  }
  output$other_list <- renderUI({
    rv$other_rev
    req(rv$vdb, rv$box_sid)
    sid <- rv$box_sid
    ol <- other_list(rv$vdb, sid)
    unk <- unknown_codes(ol$code[!ol$listed], KNOWN_SPECIES_CODES)
    fn <- rv$grid_samples$file_name[rv$grid_samples$sample_id == sid][1]
    div(class = "other-list", `data-sid` = sid,
        div(class = "other-head", "Other species", if (!is.na(fn)) paste0(" \u2014 ", tools::file_path_sans_ext(fn)),
            span(class = "other-hint", " \u00b7 keys label this list, Esc leaves")),
        if (nrow(ol)) lapply(seq_len(nrow(ol)), function(i) .other_row(ol[i, ], unk))
        else div(class = "other-none", "no HawkEars candidates: O, then type in Add"))
  })
  # COMPLETE, under the Add box, looks like a list row: Y = every species heard on this
  # clip is labelled or added (1), N = not (0); the lit button again blanks it (NULL).
  # No U (Phil 2026-10-27): the column holds only 1 / 0 / NULL.
  output$other_complete <- renderUI({
    rv$other_rev
    req(rv$vdb, rv$box_sid)
    v <- others_complete_value(rv$vdb, rv$box_sid)
    lit <- if (is.na(v)) "" else if (v == 1L) "Y" else "N"                  # code-map: COMPLETE 1/0
    btn <- function(l) tags$button(type = "button", tabindex = "-1", l,
      class = paste("other-btn", if (identical(lit, l)) paste0("on-", l)),
      onmousedown = "event.preventDefault();",
      onclick = sprintf("Shiny.setInputValue('other_complete_set',{v:'%s',sid:%d,n:Math.random()},{priority:'event'})", l, rv$box_sid))
    div(class = "other-row other-complete", title = "Y: every species heard on this clip is labelled or added",
        span(class = "other-code", "COMPLETE"), btn("Y"), btn("N"))         # code-map: COMPLETE buttons
  })
  .other_sid <- function(ev) { s <- suppressWarnings(as.integer(ev$sid)); if (length(s) == 1 && !is.na(s)) s else rv$box_sid }
  observeEvent(input$other_set, {
    req(rv$vdb); ev <- input$other_set; sid <- .other_sid(ev); req(sid, ev$code)
    cur <- dbGetQuery(rv$vdb$conn, "SELECT label FROM clip_others WHERE sample_id = ? AND code = ?",
                      params = list(sid, ev$code))$label
    lab <- if (is.null(ev$label) || identical(cur, ev$label)) NULL else ev$label   # lit again -> blank
    set_other(rv$vdb, sid, ev$code, lab)
    rv$other_rev <- rv$other_rev + 1L
    rv$status_message <- sprintf("\u2713 %s: %s", ev$code, lab %||% "cleared")
  })
  observeEvent(input$other_add_go, {
    req(rv$vdb); ev <- input$other_add_go; sid <- .other_sid(ev); req(sid)
    codes <- parse_other_codes(ev$txt, rv$species)
    if (!length(codes)) { rv$status_message <- "Nothing to add (the target's own code is dropped)."; return() }
    for (k in codes) set_other(rv$vdb, sid, k, DETECTION)   # a typed code is heard: Y
    rv$other_rev <- rv$other_rev + 1L
    unk <- unknown_codes(codes, KNOWN_SPECIES_CODES)
    rv$status_message <- sprintf("\u2713 Added %s as Y%s", paste(codes, collapse = " "),
                                 if (length(unk)) paste0(" (not a known code, saved anyway: ", paste(unk, collapse = " "), ")") else "")
  })
  observeEvent(input$other_complete_set, {
    req(rv$vdb); ev <- input$other_complete_set; sid <- .other_sid(ev); req(sid)
    want <- if (identical(ev$v, "Y")) 1L else if (identical(ev$v, "N")) 0L else return()   # code-map: COMPLETE 1/0
    cur <- others_complete_value(rv$vdb, sid)
    set_others_complete(rv$vdb, sid, if (identical(cur, want)) NA else want == 1L)   # lit again -> blank
    rv$other_rev <- rv$other_rev + 1L
  })
  # Live flag for codes typed in the Add box that are not HawkEars codes (saved anyway).
  output$other_add_flag <- renderUI({
    unk <- unknown_codes(parse_other_codes(input$other_add, rv$species), KNOWN_SPECIES_CODES)
    if (!length(unk)) return(NULL)
    span(class = "other-unknown", title = "not a known code (saved anyway)",
         style = "background:#ffb300;color:#000;padding:1px 6px;border-radius:3px;font-size:11px;white-space:nowrap;",
         paste(unk, collapse = " "))
  })

  # ---- audio ------------------------------------------------------------
  observeEvent(input$btn_play, {
    req(.has_active())
    smp <- rv$grid_samples[rv$active_position, ]
    base <- AUDIO_PREFIXES[[resolve_audio(rv$vdb, smp$file_name)$which]]
    dur <- if (!is.null(rv$clip_dur) && !is.na(rv$clip_dur)) rv$clip_dur else NULL
    rv$last_audio_url <- paste0(base, "/", URLencode(smp$file_name, reserved = TRUE))
    session$sendCustomMessage("play_audio_client", list(url = rv$last_audio_url, dur = dur))
  })

  # ---- classification ---------------------------------------------------
  .save_active <- function(cls, voc = NULL, advance = TRUE) {
    req(.has_active(), rv$vdb)
    smp <- rv$grid_samples[rv$active_position, ]
    # Notes are not written here: the boxes save themselves on leaving them (.save_boxes),
    # and the box text seen by the server can lag what was typed.
    save_classification(rv$vdb, smp$sample_id, cls, voc)
    .update_local(smp$sample_id, cls, voc)
    rv$last_updated_clip <- rv$active_position
    rv$classification_counter <- rv$classification_counter + 1L
    if (rv$mode == "occupancy") .refresh_groups_keep_idx()
    if (advance) .advance()
  }
  .update_local <- function(sample_id, cls, voc) {
    for (nm in c("all_samples", "grid_samples")) {
      df <- rv[[nm]]; if (is.null(df)) next
      j <- which(df$sample_id == sample_id)
      if (length(j)) {
        df$classification[j] <- cls; df$classified[j] <- 1L
        if ("vocalization_type" %in% names(df))
          df$vocalization_type[j] <- if (is.null(voc)) NA_character_ else voc
        rv[[nm]] <- df
      }
    }
  }
  .advance <- function() {
    n <- nrow(rv$grid_samples); ps <- page_size()
    if (rv$active_position < n) {
      rv$previous_active_position <- rv$active_position
      rv$active_position <- rv$active_position + 1L
    } else if (rv$page * ps < nrow(rv$all_samples)) {
      if (isTRUE(input$auto_next_page)) {
        rv$page <- rv$page + 1L; .recompute_page()   # end of screen -> next page
      } else showNotification("Last clip on this page: \u25ba for the next page.",
                              type = "message", duration = 3)
    }
  }
  .refresh_groups_keep_idx <- function() {
    g <- apply_borrowed(derive_groups(rv$vdb), rv$borrowed$cells)
    ord <- rv$unit_order %||% .unit_order_by_lon(g)
    g <- g[order(match(g$unit, ord), g$time_period), , drop = FALSE]; rownames(g) <- NULL
    cur <- rv$groups[rv$group_idx, ]; rv$groups <- g
    ni <- which(g$unit == cur$unit & g$time_period == cur$time_period)
    if (length(ni)) rv$group_idx <- ni[1]
  }
  observeEvent(input$btn_yes,      .save_active(DETECTION))
  observeEvent(input$btn_yes_song, .save_active(DETECTION, VOC_SONG))
  observeEvent(input$btn_yes_call, .save_active(DETECTION, VOC_CALL))
  for (b in names(CODE_BUTTON)) local({
    id <- b; code <- CODE_BUTTON[[b]]
    if (identical(code, "O")) {   # code-map. O: focus into the other-species list, saves nothing (D9)
      observeEvent(input[[id]], {
        req(.has_active(), rv$box_sid)
        session$sendCustomMessage("focus_other", list())
      })
    } else if (code %in% PARKED_CODES && !IP_BUTTONS_ON) {
      return()                    # code-map: I / P parked (D8)
    } else observeEvent(input[[id]], .save_active(code))
  })

  observeEvent(input$btn_clear, {
    req(.has_active(), rv$vdb)
    smp <- rv$grid_samples[rv$active_position, ]
    clear_classification(rv$vdb, smp$sample_id)     # also the clip's list rows and complete
    rv$other_rev <- rv$other_rev + 1L
    for (nm in c("all_samples", "grid_samples")) {
      df <- rv[[nm]]; if (is.null(df)) next
      j <- which(df$sample_id == smp$sample_id)
      if (length(j)) { df$classification[j] <- NA; df$classified[j] <- 0L
        if ("vocalization_type" %in% names(df)) df$vocalization_type[j] <- NA
        rv[[nm]] <- df }
    }
    rv$last_updated_clip <- rv$active_position
    rv$classification_counter <- rv$classification_counter + 1L
    if (rv$mode == "occupancy") .refresh_groups_keep_idx()
    .advance()
  })

  # ---- mark-all ---------------------------------------------------------
  .mark_all <- function(cls, voc = NULL) {
    req(rv$grid_samples, rv$vdb)
    df <- rv$grid_samples
    ids <- df$sample_id[is.na(df$classified) | df$classified == 0]
    if (!length(ids)) { rv$status_message <- "Nothing unclassified on screen."; return() }
    rv$bulk_update <- TRUE
    mark_clips(rv$vdb, ids, cls, voc)
    for (sid in ids) .update_local(sid, cls, voc)
    rv$classification_counter <- rv$classification_counter + 1L
    if (rv$mode == "occupancy") .refresh_groups_keep_idx()
    rv$status_message <- sprintf("\u2713 Marked %d clip(s) as %s%s", length(ids), cls,
                                 if (!is.null(voc)) paste0("/", voc) else "")
    isolate({ rv$bulk_update <- FALSE })
  }
  observeEvent(input$btn_mark_y, .mark_all(DETECTION))
  observeEvent(input$btn_mark_s, .mark_all(DETECTION, VOC_SONG))
  observeEvent(input$btn_mark_c, .mark_all(DETECTION, VOC_CALL))
  for (k in NOT_TARGET) local({                        # btn_mark_n / _i / _p (no Mark U, Mark O)
    code <- k
    if (code %in% PARKED_CODES && (code == CODE_BUTTON[["btn_other"]] || !IP_BUTTONS_ON)) return()   # Mark O/I/P off
    observeEvent(input[[paste0("btn_mark_", tolower(code))]], .mark_all(code))
  })

  # ---- save settings / quit / export ------------------------------------
  observeEvent(input$save_settings, {
    save_settings(list(
      wl = as.numeric(input$wl), ovlp = input$ovlp, wn = input$wn,
      freq_range = input$freq_range, color_scheme = input$color_scheme,
      contrast = input$contrast, spec_height = input$spec_height,
      grid_rows = input$grid_rows, grid_cols = input$grid_cols,
      show_p = isTRUE(input$show_p), auto_next_page = isTRUE(input$auto_next_page),
      last_db  = if (!is.null(rv$vdb)) rv$vdb$path   else saved_settings$last_db,
      last_dir = if (!is.null(rv$vdb)) rv$vdb$db_dir else saved_settings$last_dir,
      validator = rv$validator))
    msg <- if (!is.null(rv$vdb)) "\u2713 Settings + database path saved" else "\u2713 Settings saved"
    showNotification(msg, type = "message", duration = 3)
  })
  observeEvent(input$btn_quit, {
    .close_session_row()
    isolate({ if (!is.null(rv$vdb)) try(close_validation_db(rv$vdb), silent = TRUE) })
    stopApp()
  })
  observeEvent(input$export_consolidated, {
    req(rv$vdb)
    sp <- gsub("[^A-Za-z0-9_-]", "", rv$species %||% "species")
    out <- file.path(rv$vdb$db_dir,
      paste0(tools::file_path_sans_ext(basename(rv$vdb$path)), "_consolidated.csv"))
    if (file.exists(out)) {
      showModal(modalDialog(title = "File exists",
        paste0(basename(out), " already exists. Overwrite?"),
        footer = tagList(modalButton("Cancel"),
                         actionButton("confirm_overwrite", "Overwrite", class = "btn-danger"))))
      return()
    }
    n <- tryCatch(export_results(rv$vdb, out), error = function(e) { rv$status_message <- paste("\u2717 Export error:", conditionMessage(e)); NA })
    if (!is.na(n)) rv$status_message <- sprintf("\u2713 Exported %d clips to %s", n, out)
  })
  observeEvent(input$confirm_overwrite, {
    removeModal(); req(rv$vdb)
    out <- file.path(rv$vdb$db_dir,
      paste0(tools::file_path_sans_ext(basename(rv$vdb$path)), "_consolidated.csv"))
    n <- tryCatch(export_results(rv$vdb, out), error = function(e) { rv$status_message <- paste("\u2717 Export error:", conditionMessage(e)); NA })
    if (!is.na(n)) rv$status_message <- sprintf("\u2713 Exported %d clips to %s", n, out)
  })

  # ---- sidebar panels ---------------------------------------------------
  # p(correct) is a pipeline estimate: display only, never a decision rule, and
  # hidden unless the validator ticks "Show p(correct)" (brief F2).
  output$confidence_legend <- renderUI({
    req(rv$data_loaded)
    if (!isTRUE(input$show_p) || is.null(rv$vdb) ||
        !any(c("p_correct", "score_precision", "p") %in% rv$vdb$cols)) return(NULL)
    reps <- list(c(0.97, "\u22650.95"), c(0.90, "0.85"), c(0.78, "0.70"),
                 c(0.60, "0.50"), c(0.40, "<0.50"))
    pills <- lapply(reps, function(z) {
      b <- .p_band(as.numeric(z[1]))
      tags$span(style = sprintf("background:%s;color:%s;padding:1px 6px;border-radius:8px;font-size:10px;font-weight:bold;margin-right:3px;",
                                b$bg, b$fg), z[2])
    })
    div(style = "margin-top:6px;",
        div(style = "font-size:11px; color:#555;", "Clip confidence p(correct) \u2014 pipeline estimate, display only:"),
        div(style = "margin-top:2px;", pills))
  })

  .browser_on <- function() !isTRUE(rv$data_loaded) || isTRUE(rv$show_browser)
  output$show_browser <- reactive({ .browser_on() })
  outputOptions(output, "show_browser", suspendWhenHidden = FALSE)
  output$has_scanned <- reactive({ !is.null(rv$scanned) })
  outputOptions(output, "has_scanned", suspendWhenHidden = FALSE)

  output$dir_browser <- renderUI({
    req(.browser_on())
    ents <- dir_listing()
    if (nrow(ents) == 0)
      return(div(style = "font-size:12px; color:#888; padding:4px;",
                 "(no sub-folders or .db files here)"))
    items <- lapply(seq_len(nrow(ents)), function(i) {
      isd <- ents$is_dir[i]; nm <- ents$name[i]
      evt <- if (isd) "browse_pick" else "browse_open"
      div(class = "dir-entry",
          style = sprintf("padding:2px 6px; cursor:pointer; white-space:nowrap; overflow:hidden; text-overflow:ellipsis; %s",
                          if (isd) "font-weight:500;" else "color:#1565c0;"),
          onclick = sprintf("Shiny.setInputValue('%s', %d, {priority:'event'})", evt, i),
          if (isd) paste0("\u25b8 ", nm, "/") else paste0("\u2009\u2009", nm))
    })
    div(style = "max-height:170px; overflow:auto; border:1px solid #ddd; border-radius:4px; background:#fff;",
        do.call(tagList, items))
  })

  output$species_select_ui <- renderUI({
    req(rv$scanned)
    labs <- sprintf("%s \u2014 %s%s%s", rv$scanned$species, rv$scanned$db_name,
                    ifelse(is.na(rv$scanned$mode), "",
                           sprintf("  (%s%s)", rv$scanned$mode,
                                   ifelse(is.na(rv$scanned$n_clips), "",
                                          sprintf(", %d clips", rv$scanned$n_clips)))),
                    ifelse(rv$scanned$v3 %in% FALSE, "  \u2014 needs conversion (core/convert_db_v3.R)", ""))
    tagList(
      div(style = "margin-top:8px; font-weight:500;",
          sprintf("Found %d database(s):", nrow(rv$scanned))),
      div(class = "species-pick-wrap",
          selectInput("species_pick", NULL,
                      choices = stats::setNames(seq_len(nrow(rv$scanned)), labs),
                      selectize = FALSE,
                      size = max(4, min(12, nrow(rv$scanned))),
                      width = "100%")),
      actionButton("btn_open_species", "Open selected", class = "btn-success btn-sm btn-block"),
      div(style = "font-size:11px; color:#777; margin-top:3px;", "Tip: double-click a species to open it.")
    )
  })

  # Which DB is open (name, species, folder; full path on hover), then the latest
  # Open / Scan / browse result. Both lines show together: once a DB is open, a
  # scan or a bad path must still answer here.
  output$open_status <- renderUI({
    open_line <- if (isTRUE(rv$data_loaded) && !is.null(rv$vdb))
      div(title = rv$vdb$path,
          span(style = "color:#2e7d32;", "\u2713 Open: "), strong(basename(rv$vdb$path)),
          span(style = "color:#555;", sprintf(" \u2014 %s, %s", rv$species, rv$mode)),
          div(style = "font-size:11px; color:#777; word-break:break-all;", rv$vdb$db_dir))
    else if (!is.null(rv$warn_message))
      div(style = "color:#b00;", rv$warn_message)
    else div(style = "color:#888;", "No database open")
    note <- if (!is.null(rv$panel_msg))
      div(style = "color:#1565c0; margin-top:3px;", rv$panel_msg)
    tagList(open_line, note)
  })
  output$warn_banner <- renderUI({
    req(rv$warn_message); if (!isTRUE(rv$data_loaded)) return(NULL)
    div(style = "background:#fff3cd;border:1px solid #ffe69c;padding:6px 10px;border-radius:4px;margin-bottom:6px;",
        strong("\u26a0 "), rv$warn_message)
  })
  output$loading_status <- renderText({ paste(rv$loading_status, rv$status_message) })
  output$mode_panel <- renderUI({
    req(rv$data_loaded)
    cls <- if (rv$mode == "calibration") "mode-cal" else "mode-occ"
    wellPanel(div(span("Mode: "), span(class = cls, toupper(rv$mode))),
              div("Species: ", strong(rv$species)),
              if (rv$mode == "occupancy") div("Grain: ", strong(rv$grain)))
  })

  .scheme_label <- function() {
    m <- rv$mode_info$meta
    if (!is.null(m)) {
      lbl <- rv$mode
      if (!is.na(rv$grain)) lbl <- paste(lbl, rv$grain, sep = " / ")
      extra <- c()
      if ("top_k" %in% names(m) && !is.na(m$top_k)) extra <- c(extra, paste0("top_k=", m$top_k))
      if ("threshold" %in% names(m) && !is.na(m$threshold)) extra <- c(extra, paste0("\u03c4=", round(m$threshold, 3)))
      if (length(extra)) lbl <- paste0(lbl, " (", paste(extra, collapse = ", "), ")")
      return(lbl)
    }
    sc <- tryCatch(dbGetQuery(rv$vdb$conn, "SELECT time_period_type, n1, n2, n3 FROM session_config WHERE id=1"),
                   error = function(e) NULL)
    if (!is.null(sc) && nrow(sc))
      return(sprintf("%s (nY=%s, nMax=%s, minThresh=%s)", sc$time_period_type, sc$n1, sc$n2, sc$n3))
    rv$mode
  }
  .class_badges <- function(cc) {
    sp <- function(col, lab, n) sprintf("<span style='background:%s;color:white;padding:2px 6px;border-radius:3px;margin-right:4px;font-weight:bold;'>%s:%d</span>", col, lab, n)
    paste0(vapply(ALL_CODES, function(k) sp(CODE_COLOUR[[k]], k, cc_get(cc, k)), character(1)),
           collapse = "")
  }

  output$summary_table <- renderUI({
    req(rv$data_loaded); rv$classification_counter
    cc <- class_counts(rv$vdb); prog <- calibration_progress(rv$vdb)
    n_groups_total <- if (!is.null(rv$groups)) nrow(rv$groups) else NA
    n_groups_done  <- if (!is.null(rv$groups))
      sum(vapply(seq_len(nrow(rv$groups)), function(i) .group_done(rv$groups[i, ]), logical(1))) else NA
    grp_split <- if (!is.null(rv$groups)) {
      why <- vapply(seq_len(nrow(rv$groups)), function(i) group_complete(rv$groups[i, ])$reason, character(1))
      sprintf(" (own Y %d, heard in other clips %d, absence %d)", sum(why %in% "resolved_by_Y"),
              sum(why %in% "resolved_by_other"), sum(why %in% "examined_no_Y_absence"))
    } else ""
    bor <- rv$borrowed
    bor_line <- if (rv$mode == "occupancy" && !is.null(bor)) {
      txt <- if (isTRUE(bor$off)) "off (see the warning above)" else
        sprintf("%d cell(s) resolved; %s; %d DB(s) read",
                if (is.null(bor$cells)) 0L else nrow(bor$cells), borrowed_out_text(bor, rv$mode_info$meta), bor$n_dbs)
      sprintf("<tr><td style='font-weight:bold;'>Other species:</td><td>%s</td></tr>", txt)
    } else ""
    grp_line <- if (rv$mode == "occupancy" && !is.null(rv$groups)) {
      cur <- rv$groups[rv$group_idx, ]
      sprintf("Group #%d/%d: %s - %s", rv$group_idx, n_groups_total, cur$unit, cur$time_period)
    } else "(flat list)"

    sd <- tryCatch(score_distribution(rv$vdb), error = function(e) NULL)
    score_html <- ""
    if (!is.null(sd) && sd$total > 0) {
      magma <- c("#000004","#3b0f70","#8c2981","#de4968","#fe9f6d")
      seg <- paste(vapply(seq_along(sd$bins), function(i) {
        w <- if (sd$total > 0) (sd$bins[i] / sd$total) * 100 else 0
        lab <- if (i < length(sd$bins)) {
          tv <- sd$thresholds[i + 1]
          fv <- if (diff(sd$range) > 10) round(tv, 0) else round(tv, 2)
          sprintf("<div style='position:absolute;right:4px;top:50%%;transform:translateY(-50%%);font-size:9px;font-weight:bold;color:white;'>%s</div>", fv)
        } else ""
        sprintf("<div style='width:%s%%;height:100%%;background:%s;display:inline-block;position:relative;'>%s</div>", w, magma[i], lab)
      }, character(1)), collapse = "")
      score_html <- sprintf("<tr><td style='font-weight:bold;'>Score:</td><td><div style='width:100%%;height:14px;border-radius:2px;overflow:hidden;display:flex;'>%s</div></td></tr>", seg)
    }

    HTML(sprintf("
      <table style='width:100%%; font-size:13px;'>
        <tr><td style='font-weight:bold;'>Species:</td><td>%s</td></tr>
        <tr><td style='font-weight:bold;'>Scheme:</td><td>%s</td></tr>
        <tr><td style='font-weight:bold;'>Group:</td><td>%s</td></tr>
        %s%s
        <tr><td style='font-weight:bold;'>Clips examined:</td><td>%d / %d</td></tr>
        %s
        <tr><td style='font-weight:bold;'>Classifications:</td><td>%s</td></tr>
      </table>",
      rv$species, .scheme_label(), grp_line,
      if (!is.na(n_groups_total)) sprintf("<tr><td style='font-weight:bold;'>Groups complete:</td><td>%d / %d%s</td></tr>", n_groups_done, n_groups_total, grp_split) else "",
      bor_line,
      prog$n_examined, prog$n_total, score_html, .class_badges(cc)))
  })

  .group_stats_html <- function() {
    if (rv$mode != "occupancy" || is.null(rv$groups)) {
      prog <- calibration_progress(rv$vdb)
      return(HTML(sprintf("<table style='width:100%%;font-size:13px;'>
        <tr><td style='font-weight:bold;'>Calibration:</td><td>%d / %d examined</td></tr></table>",
        prog$n_examined, prog$n_total)))
    }
    g <- rv$groups[rv$group_idx, ]
    cc <- group_class_counts(rv$vdb, g$unit, g$time_period)
    comp <- group_complete(g)
    if (isTRUE(comp$complete)) {
      txt <- switch(comp$reason,
        "resolved_by_Y" = paste0("\u2713 DETECTED (Y found, ", cc_get(cc, DETECTION), ")"),
        "resolved_by_other" = paste0("\u2713 PRESENT (heard in another species' clip: ",
                                     htmltools::htmlEscape(.borrowed_provenance(g$unit, g$time_period)), ")"),
        "examined_no_Y_absence" = "\u2713 ABSENCE (all examined, no Y)", comp$reason)
      style <- "background:#28a745;color:white;padding:4px;border-radius:3px;font-weight:bold;"
    } else { txt <- "\u25cb In progress"; style <- "background:#dc3545;color:white;padding:4px;border-radius:3px;font-weight:bold;" }
    HTML(sprintf("
      <table style='width:100%%; font-size:13px;'>
        <tr><td style='font-weight:bold;'>Group:</td><td>%s - %s</td></tr>
        <tr><td colspan='2' style='%s'>%s</td></tr>
        <tr><td style='font-weight:bold;'>Clips:</td><td>%d / %d</td></tr>
        <tr><td style='font-weight:bold;'>Classifications:</td><td>%s</td></tr>
      </table>", g$unit, g$time_period, style, txt, g$n_examined, g$n_total, .class_badges(cc)))
  }
  .borrowed_provenance <- function(unit, period) {
    cl <- rv$borrowed$cells
    if (is.null(cl)) return("")
    p <- cl$provenance[cl$unit == unit & cl$time_period == period]
    if (length(p)) p[1] else ""
  }
  output$group_stats_table <- renderUI({ req(rv$data_loaded); rv$classification_counter; .group_stats_html() })
  output$group_box         <- renderUI({ req(rv$data_loaded); rv$classification_counter; .group_stats_html() })

  output$classification_plot <- renderPlot({
    req(rv$data_loaded); rv$classification_counter
    df <- dbGetQuery(rv$vdb$conn, "SELECT score, classification FROM clips")
    classification_plot_pY(df)
  })
  # F3: the cell on screen is outlined — the open group in group view, else the
  # active clip's cell.
  .active_cell <- function() {
    if (isTRUE(rv$view_mode == "group") && !is.null(rv$groups) && nrow(rv$groups) >= rv$group_idx) {
      g <- rv$groups[rv$group_idx, ]; return(list(unit = g$unit, period = g$time_period))
    }
    s <- rv$grid_samples; a <- rv$active_position
    if (is.null(s) || is.null(a) || a > nrow(s)) return(NULL)
    list(unit = s$unit[a], period = s$time_period[a])
  }
  output$temporal_plot <- renderPlot({
    req(rv$data_loaded, rv$mode == "occupancy"); rv$classification_counter
    temporal_tiles(rv$groups, rv$unit_order, active = .active_cell())
  })
  # F3: hover names the cell; click opens it as a group (same path as the picker).
  output$temporal_hover_label <- renderText({
    h <- input$temporal_hover
    if (is.null(h) || is.null(rv$groups)) return("")
    t <- tile_at(rv$groups, rv$unit_order, h$x, h$y)
    if (is.null(t)) return("")
    g <- rv$groups[rv$groups$unit == t$unit & rv$groups$time_period == t$period, ][1, ]
    if (identical(g$status, "borrowed"))
      return(sprintf("%s \u00b7 %s \u00b7 present, heard in: %s", t$unit, t$period,
                     .borrowed_provenance(t$unit, t$period)))
    sprintf("%s \u00b7 %s \u00b7 %s (%d/%d examined)", t$unit, t$period, g$status, g$n_examined, g$n_total)
  })
  observeEvent(input$temporal_click, {
    req(rv$data_loaded, rv$mode == "occupancy", rv$groups)
    cl <- input$temporal_click
    t <- tile_at(rv$groups, rv$unit_order, cl$x, cl$y)
    if (is.null(t)) return()
    idx <- which(rv$groups$unit == t$unit & rv$groups$time_period == t$period)[1]
    if (is.na(idx)) return()
    rv$group_idx <- idx
    if (!isTRUE(rv$view_mode == "group")) {
      rv$view_mode <- "group"; updateSelectInput(session, "view_mode", selected = "group")
    }
    .load_group_context()
  })

  session$onSessionEnded(function() {
    isolate(.close_session_row())
    if (!is.null(isolate(rv$vdb))) try(close_validation_db(isolate(rv$vdb)), silent = TRUE)
  })
}

# ---- UI-only label helper --------------------------------------------------

# Per-clip confidence p(correct) supplied by the pipeline. Tolerant of column
# name; returns NA if no such column / value.
.clip_p <- function(smp) {
  for (nm in c("p_correct", "score_precision", "p")) {
    if (nm %in% names(smp)) {
      v <- suppressWarnings(as.numeric(smp[[nm]]))
      if (length(v) == 1 && !is.na(v)) return(v)
    }
  }
  NA_real_
}

# Banded, colour-blind-safe ramp (blue = confident -> orange = uncertain, with a
# neutral grey middle). Avoids red/green entirely. bg + matched readable fg.
.p_band <- function(p) {
  if (is.null(p) || length(p) != 1 || is.na(p)) return(NULL)
  p <- as.numeric(p)
  if      (p >= 0.95) list(bg = "#08519c", fg = "#ffffff", lab = "very high")
  else if (p >= 0.85) list(bg = "#6baed6", fg = "#08306b", lab = "high")
  else if (p >= 0.70) list(bg = "#bdbdbd", fg = "#252525", lab = "moderate")
  else if (p >= 0.50) list(bg = "#fdae6b", fg = "#7f2704", lab = "low")
  else                list(bg = "#e6550d", fg = "#ffffff", lab = "very low")
}

# A coloured pill carrying the confidence number itself.
.p_pill_html <- function(p) {
  b <- .p_band(p); if (is.null(b)) return("")
  sprintf(paste0("<span title='p(correct), pipeline estimate (display only): %s' style='background:%s;color:%s;",
                 "padding:1px 7px;border-radius:9px;font-weight:bold;font-size:11px;'>p&nbsp;%.2f</span>"),
          b$lab, b$bg, b$fg, as.numeric(p))
}

.clip_label <- function(smp, show_p = FALSE) {
  orig_unit <- if (!is.null(smp$fn) && !is.na(smp$fn) && nzchar(smp$fn))
    sub("_.*", "", basename(smp$fn)) else smp$unit
  parts <- c("<b>", round(smp$score, 2), "</b>")          # HE score first
  pill <- if (isTRUE(show_p)) .p_pill_html(.clip_p(smp)) else ""
  if (nzchar(pill)) parts <- c(parts, " ", pill)           # then the p pill
  parts <- c(parts, " | ", orig_unit, " | ", smp$file_name)
  if (!is.null(smp$vocalization_type) && !is.na(smp$vocalization_type) && smp$vocalization_type != "") {
    v <- smp$vocalization_type
    col <- if (v == VOC_SONG) "#4caf50" else if (v == VOC_CALL) "#aed581" else "#1b5e20"
    lab <- if (v == VOC_SONG) "Song" else if (v == VOC_CALL) "Call" else "Yes"
    parts <- c(parts, sprintf(" <span style='background:%s;color:#fff;padding:2px 6px;border-radius:3px;font-size:10px;font-weight:bold;'>%s</span>", col, lab))
  }
  if (!is.null(smp$clip_datetime) && !is.na(smp$clip_datetime) && smp$clip_datetime != "") {
    parts <- c(parts, " | ", format_clip_datetime_short(smp$clip_datetime))
    if (!is.null(smp$solar_label) && !is.na(smp$solar_label) && smp$solar_label != "")
      parts <- c(parts, " ", smp$solar_label)
  }
  paste0(parts, collapse = "")
}

shinyApp(ui, server, options = list(launch.browser = TRUE))
