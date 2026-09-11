# org-grid.el

A visual grid for Emacs org-mode notes, images, videos, and PDFs.

## What it does

`org-grid` scans a directory (or a Dired buffer) and shows every Org heading with an `:ID:` property, plus every image/video/PDF file, as cards in a scrollable grid. Notes show a text snippet; media shows a thumbnail.

<img width="1334" height="820" alt="gridd" src="https://github.com/user-attachments/assets/3f134f62-e1cb-4a04-9685-0adb88e9c0ce" />

## Features

- Works on org files, any heading with `:ID:` counts as a note
- Media thumbnails for images, video (via `ffmpeg`), and PDF (via `pdftoppm`)
- Query by text or `#tag`
- Sort by date / title / tags / type
- Cluster view (group by shared links) and orphan view (unlinked items)
- Color bars for tags. (minimum 2 notes should share the same tag for color bar to appear)
- Fast full-text filtering via `ripgrep` (optional)
- Open from Dired, jump back to Dired, or jump straight to a note's heading
- File based id's supported so it works with org-roam as well.

## Requirements

- Emacs 27.1+
- Optional: `ffmpeg` (video thumbnails), `pdftoppm` (PDF thumbnails), `rg` (fast search)

## Usage

```elisp
(require 'org-grid)
(setq org-grid-directory "~/org")
```

- `M-x org-grid-open` — open the grid for `org-grid-directory`
- `M-x org-grid-from-dired` — open the grid for the current Dired listing

## Keybindings (in the grid buffer)

| Key | Command |
|---|---|
| `RET` / mouse-1 | Open item at point |
| `d` | Jump to item in Dired |
| `/` | Filter |
| `s` | Cycle sort key |
| `r` | Reverse sort order |
| `c` | Toggle cluster view |
| `o` | Toggle orphan view |
| `g` | Refresh |
| `n` / `p` / `TAB` | Next / previous card |
| `↑` `↓` `←` `→` | Move between cards |
| `q` | Quit |

## Media filenames

Media files use `TITLE.ext` or `TITLE__tag1_tag2.ext` (underscores separate tags, no id/timestamp prefix needed).

