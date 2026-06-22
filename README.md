# World Cup 2026 — Spoiler-Free Highlights

Lists every WC2026 match that has a highlight, **without showing the score**. Click a
match and it opens FIFA's own highlight page. No scores, no thumbnails, no spoilers.

> Unofficial fan project — not affiliated with or endorsed by FIFA. Highlights are
> hosted by FIFA.com; this only links to them. Non-commercial, personal use.

## Run it locally

Needs **R** with `httr2` + `jsonlite` (`install.packages(c("httr2","jsonlite"))`).

```bash
Rscript fetch_highlights.R     # build/refresh data/matches.json
python3 -m http.server 8000    # then open http://localhost:8000
```

## Put it online (free)

It's a static site, so it deploys to **GitHub Pages**:

1. Push to a **public** GitHub repo.
2. **Settings → Pages → Source = "GitHub Actions"**.

`.github/workflows/deploy.yml` then rebuilds the list hourly and publishes to
`https://<user>.github.io/<repo>/`. Share that link with anyone.

## How it stays spoiler-free

The score is never fetched or stored — only team names + kickoff date. Each match links
to FIFA's clean `/en/watch/<id>` page (score-free title), never the match report.

## Files

```
fetch_highlights.R            builds data/matches.json from FIFA's public API (R)
index.html                    the page (reads matches.json, links out to FIFA)
.github/workflows/deploy.yml  rebuild + deploy to GitHub Pages
```

Config: `FIFA_COMPETITION` (default `17`), `FIFA_SEASON` (default `285023`).
