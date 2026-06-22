#!/usr/bin/env Rscript
# Build a spoiler-free list of FIFA World Cup 2026 match highlights — from FIFA.
#
# R port of fetch_highlights.py. Source = fifa.com's own backend (the same APIs
# its website calls), so the list matches the official scores-fixtures page
# exactly: every match that has a highlight, in kickoff order.
#
# The crucial property: the score is never fetched, parsed, stored or shown. We
# keep only team names + kickoff date from the fixtures API and the highlight's
# video id; scores / posters / match-report text are never touched.
#
# Pipeline:
#   1. fixtures   api.fifa.com/api/v3/calendar/matches?idCompetition=17&idSeason=…
#   2. per match  cxm-api.fifa.com/fifaplusweb/api/sections/matchdetails/videos…  -> entryId
#
# Usage:
#   Rscript fetch_highlights.R          # rebuild data/matches.json

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
})

# null-coalescing helper (base R gained %||% only in 4.4; define for portability)
`%||%` = function(x, y) if (is.null(x)) y else x

HERE = tryCatch({
  a = commandArgs(trailingOnly = FALSE)
  f = sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
}, error = function(e) getwd())

DATA_FILE   = file.path(HERE, "data", "matches.json")
VIDEOS_FILE = file.path(HERE, "data", "fifa_videos.json")  # matchId -> {entryId,url} cache

COMPETITION = Sys.getenv("FIFA_COMPETITION", "17")          # FIFA World Cup
SEASON      = Sys.getenv("FIFA_SEASON", "285023")           # 2026 edition
FIFA_API    = "https://api.fifa.com/api/v3"
CXM_API     = "https://cxm-api.fifa.com/fifaplusweb/api"

# Variant highlight reels we never want to pick — we want the standard one.
VARIANTS = c("sign language", "(is)", "gamified", "alt cast", "alternative")


get_json = function(url, timeout = 30) {
  request(url) |>
    req_headers(`User-Agent` = "Mozilla/5.0", Accept = "application/json") |>
    req_timeout(timeout) |>
    req_perform() |>
    resp_body_json(simplifyVector = FALSE)
}


# --------------------------------------------------------------------------- #
# 1. Fixtures (the canonical list + ordering)
# --------------------------------------------------------------------------- #
fetch_fixtures = function() {
  url = sprintf(
    "%s/calendar/matches?idCompetition=%s&idSeason=%s&from=2026-06-01T00:00:00Z&to=2026-08-01T00:00:00Z&count=500&language=en",
    FIFA_API, COMPETITION, SEASON)
  data = get_json(url)
  matches = list()
  for (m in (data$Results %||% list())) {
    home = tryCatch(m$Home$TeamName[[1]]$Description, error = function(e) NULL)
    away = tryCatch(m$Away$TeamName[[1]]$Description, error = function(e) NULL)
    if (is.null(home) || is.null(away)) next  # placeholder fixture (teams TBD)
    matches[[length(matches) + 1]] = list(
      matchId = as.character(m$IdMatch),
      stageId = as.character(m$IdStage),
      kickoff = m$Date %||% NA_character_,
      home = home,
      away = away
    )
  }
  # sort ascending by kickoff
  keys = vapply(matches, function(x) x$kickoff %||% "", character(1))
  matches[order(keys)]
}


# --------------------------------------------------------------------------- #
# 2. Per-match highlight lookup
# --------------------------------------------------------------------------- #
pick_highlight = function(items) {
  cand = Filter(function(it) !is.null(it$entryId), items)
  title = function(it) trimws(it$title %||% "")
  is_variant = function(it) {
    t = tolower(title(it))
    any(vapply(VARIANTS, function(v) grepl(v, t, fixed = TRUE), logical(1)))
  }
  ends_highlights = function(it) grepl("highlights$", tolower(title(it)))
  plain = Filter(function(it) !is_variant(it) && ends_highlights(it), cand)
  if (length(plain)) return(plain[[1]])
  rest = Filter(function(it) !is_variant(it), cand)
  if (length(rest)) return(rest[[1]])
  if (length(cand)) return(cand[[1]])
  NULL
}

fetch_match_video = function(stage_id, match_id) {
  url = sprintf(
    "%s/sections/matchdetails/videos?locale=en&competitionId=%s&seasonId=%s&stageId=%s&matchId=%s",
    CXM_API, COMPETITION, SEASON, stage_id, match_id)
  data = tryCatch(get_json(url), error = function(e) NULL)
  if (is.null(data)) return(NULL)
  vb = data$vodVideosBaseCarousel
  items = if (is.list(vb)) vb$items else NULL
  if (!is.list(items)) items = list()
  it = pick_highlight(items)
  if (is.null(it)) return(NULL)
  page = it$readMorePageUrl %||% paste0("/en/watch/", it$entryId)
  list(entryId = it$entryId, url = paste0("https://www.fifa.com", page))
}


# --------------------------------------------------------------------------- #
# Build the spoiler-free list
# --------------------------------------------------------------------------- #
load_videos_cache = function() {
  if (!file.exists(VIDEOS_FILE)) return(list())
  tryCatch(fromJSON(VIDEOS_FILE, simplifyVector = FALSE), error = function(e) list())
}

save_videos_cache = function(cache) {
  dir.create(dirname(VIDEOS_FILE), showWarnings = FALSE, recursive = TRUE)
  write(toJSON(cache, auto_unbox = TRUE, null = "null"), VIDEOS_FILE)
}

date_label = function(iso) {
  if (is.null(iso) || is.na(iso) || !nzchar(iso)) return(NULL)
  dt = tryCatch(
    as.POSIXct(sub("Z$", "", iso), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
    error = function(e) NA)
  if (is.na(dt)) return(NULL)
  paste(as.integer(format(dt, "%d", tz = "UTC")), format(dt, "%b", tz = "UTC"))
}

build_matches = function() {
  fixtures = fetch_fixtures()
  cache = load_videos_cache()

  # Look up only matches we haven't already found a highlight for.
  todo = Filter(function(m) is.null(cache[[m$matchId]]), fixtures)
  if (length(todo)) {
    for (m in todo) {
      entry = fetch_match_video(m$stageId, m$matchId)
      if (!is.null(entry)) cache[[m$matchId]] = entry  # store only hits
    }
    save_videos_cache(cache)
  }

  out = list()
  for (m in fixtures) {
    info = cache[[m$matchId]]
    if (is.null(info)) next
    out[[length(out) + 1]] = list(
      teams = c(m$home, m$away),
      video_id = info$entryId,
      page_url = info$url,
      match_id = m$matchId,
      kickoff = m$kickoff,
      date = date_label(m$kickoff) %||% NA_character_
    )
  }

  # `out` is ascending by kickoff. Show newest day first, but keep each day's
  # matches in chronological (kickoff) order — reverse by day-group, not item.
  groups = list()
  for (m in out) {
    key = substr(m$kickoff %||% "", 1, 10)  # calendar day
    n = length(groups)
    if (n && groups[[n]]$key == key) {
      groups[[n]]$items = c(groups[[n]]$items, list(m))
    } else {
      groups[[n + 1]] = list(key = key, items = list(m))
    }
  }
  groups = rev(groups)
  out = list()
  for (g in groups) for (m in g$items) out[[length(out) + 1]] = m
  for (i in seq_along(out)) out[[i]]$order = i
  out
}

refresh = function() {
  matches = build_matches()
  payload = list(
    generated_at = format(as.POSIXlt(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%S+00:00"),
    source = sprintf("fifa.com (competition %s, season %s)", COMPETITION, SEASON),
    count = length(matches),
    matches = matches
  )
  dir.create(dirname(DATA_FILE), showWarnings = FALSE, recursive = TRUE)
  write(toJSON(payload, auto_unbox = TRUE, pretty = TRUE, null = "null"), DATA_FILE)
  payload
}


main = function() {
  payload = refresh()
  cat(sprintf("Wrote %d matches with highlights to %s\n", payload$count, DATA_FILE))
  for (m in payload$matches) {
    cat(sprintf("  %3d. %6s  %s v %s\n",
                m$order,
                if (is.na(m$date)) "   ?  " else m$date,
                m$teams[1], m$teams[2]))
  }
}

if (sys.nframe() == 0) {
  tryCatch(main(), error = function(e) {
    message(sprintf("error: %s", conditionMessage(e)))
    quit(status = 1)
  })
}
