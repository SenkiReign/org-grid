;;; org-grid.el --- Grid view for org notes -*- lexical-binding: t; -*-

;; Author:  Senki R.
;; Keywords: notes, multimedia, moodboard, emacs, org-mode
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.6.0

;;; Code:

(require 'cl-lib)
(require 'svg)
(require 'dired)
(require 'color)
(require 'subr-x)
(require 'xml)
(require 'seq)

(defgroup org-grid nil
  "A local grid view for org notes, images, pdfs, and videos."
  :group 'convenience
  :prefix "org-grid-")

(defcustom org-grid-directory
  (if (bound-and-true-p org-directory) org-directory "~/org")
  "Default directory (or list of directories) scanned for notes and media.
A \"note\" is any org heading with an :ID: property; every other file
in the tree (image/video/pdf) is a media item."
  :type '(choice directory (repeat directory))
  :group 'org-grid)

(defcustom org-grid-thumbnail-size 380
  "Card width in pixels."
  :type 'integer
  :group 'org-grid)

(defcustom org-grid-card-aspect 0.8
  "Card height as a fraction of `org-grid-thumbnail-size'."
  :type 'float
  :group 'org-grid)

(defcustom org-grid-card-radius 0
  "Corner radius of cards in pixels.  0 gives sharp corners."
  :type 'integer
  :group 'org-grid)

(defcustom org-grid-gutter 12
  "Pixel gap between cards, both horizontally and vertically."
  :type 'integer
  :group 'org-grid)

(defcustom org-grid-max-columns 5
  "Maximum number of cards per row."
  :type 'integer
  :group 'org-grid)

(defcustom org-grid-image-fit 'contain
  "How image, video and PDF thumbnails sit in their card.
`contain' shows the whole picture with padding, which stays sharp for
tall or wide media.  `cover' crops to fill the card edge to edge, which
looks best for pictures close to the card's own proportions."
  :type '(choice (const contain) (const cover))
  :group 'org-grid)

(defcustom org-grid-note-snippet-length 600
  "How many characters of a note's body to show on its card."
  :type 'integer
  :group 'org-grid)

(defcustom org-grid-note-font-size 14
  "Font size in pixels for text inside note cards.  Line height follows it."
  :type 'integer
  :group 'org-grid)

(defcustom org-grid-note-font-family "sans-serif"
  "Font family used for text inside cards."
  :type 'string
  :group 'org-grid)

(defcustom org-grid-thumbnail-oversample 1
  "Resolution multiplier for generated video/pdf thumbnail files."
  :type 'integer
  :group 'org-grid)

(defcustom org-grid-lazy-batch-size 6
  "How many offscreen thumbnails to generate per idle tick."
  :type 'integer
  :group 'org-grid)

(defcustom org-grid-default-sort-key 'date
  "Initial sort key when opening a grid.
`date' sorts by file modification time (per-file for notes sharing a
file). `created' sorts by each note's :ID: timestamp when it looks
like a ts-style org-id (e.g. 20260911-160239 or 20260911T160239),
falling back to file mtime per-item when it doesn't; media items
always use their file mtime under `created' too, so both interleave
on the same time scale."
  :type '(choice (const date) (const created) (const title)
                  (const tags) (const type))
  :group 'org-grid)

(defcustom org-grid-default-sort-desc t
  "Initial sort direction when opening a grid.
Non-nil means descending (newest/last first)."
  :type 'boolean
  :group 'org-grid)

(defcustom org-grid-ripgrep-executable "rg"
  "Path to ripgrep executable used for fast full-text search."
  :type '(choice (const :tag "Disable ripgrep" nil) string)
  :group 'org-grid)

(defcustom org-grid-ffmpeg-executable "ffmpeg"
  "ffmpeg executable used for video thumbnails. Optional."
  :type 'string
  :group 'org-grid)

(defcustom org-grid-ffprobe-executable "ffprobe"
  "ffprobe executable used to read video durations for badges. Optional."
  :type 'string
  :group 'org-grid)

(defcustom org-grid-pdftoppm-executable "pdftoppm"
  "pdftoppm executable used for PDF thumbnails. Optional."
  :type 'string
  :group 'org-grid)

(defcustom org-grid-pdfinfo-executable "pdfinfo"
  "pdfinfo executable used to read PDF page counts for badges. Optional."
  :type 'string
  :group 'org-grid)

(defcustom org-grid-image-extensions '("jpg" "jpeg" "png" "gif" "webp" "bmp" "svg")
  "File extensions treated as images."
  :type '(repeat string) :group 'org-grid)

(defcustom org-grid-video-extensions '("mp4" "webm" "mov" "mkv")
  "File extensions treated as videos."
  :type '(repeat string) :group 'org-grid)

(defcustom org-grid-pdf-extensions '("pdf")
  "File extensions treated as PDFs."
  :type '(repeat string) :group 'org-grid)

(defcustom org-grid-ignore-regexp "~\\'"
  "Regexp matching file names that org-grid skips entirely.
Matched against the file name without its directory.  The default
skips Emacs backup files (`foo.org~' and numbered `foo.org.~1~'),
which would otherwise show up as duplicates of the real note."
  :type 'regexp :group 'org-grid)

(defun org-grid--ignored-file-p (file)
  "Non-nil if FILE's name matches `org-grid-ignore-regexp'."
  (and org-grid-ignore-regexp
       (string-match-p org-grid-ignore-regexp (file-name-nondirectory file))))

(defface org-grid-title-face
  '((t :inherit bold))
  "Face for the current card's title in the header line.")

(defface org-grid-tag-face
  '((t :inherit shadow))
  "Face for tags in the header line.")

(defface org-grid-selection-face
  '((((background dark)) :foreground "#8a9bff")
    (t :foreground "#3b4cc0"))
  "Face whose foreground is the border color of the selected card.")

(defface org-grid-chip-face
  '((t :inherit org-grid-tag-face :underline t))
  "Face for clickable tag chips in the header line.")

(defvar org-grid--cache-dir nil)

(defvar-local org-grid--items nil)
(defvar-local org-grid--filter "")
(defvar-local org-grid--sort-key 'date)
(defvar-local org-grid--sort-desc t)
(defvar-local org-grid--cluster-p nil)
(defvar-local org-grid--orphan-p nil)
(defvar-local org-grid--current-item nil)
(defvar-local org-grid--card-starts nil)
(defvar-local org-grid--card-rows nil
  "Vector: visual row index of each card, parallel to `org-grid--card-starts'.
A new cluster always starts a fresh row, so rows stay accurate even when
a cluster's last row is short.")
(defvar-local org-grid--card-cols nil
  "Vector: column index (0-based, within its row) of each card, parallel
to `org-grid--card-starts'.")
(defvar-local org-grid--source-directory nil)
(defvar-local org-grid--source-dired-buffer nil)
(defvar-local org-grid--selected-start nil
  "Buffer position of the card currently drawn as selected.")
(defvar-local org-grid--last-win-width nil)
(defvar-local org-grid--clusters-cache nil)
(defvar-local org-grid--clusters-cache-key nil)
(defvar-local org-grid--links-cache nil)
(defvar-local org-grid--links-cache-key nil)
(defvar-local org-grid--link-counts nil
  "Hash table: item id -> number of notes/media linked to or from that item.")
(defvar-local org-grid--pending-fill nil)
(defvar-local org-grid--fill-timer nil)
(defvar-local org-grid--image-cache nil)

(defun org-grid--cache-dir-for (root)
  (let ((dir (expand-file-name ".org-grid-thumbs/" root)))
    (unless (file-directory-p dir) (make-directory dir t))
    dir))

;;; Theme-safe colors
;;
;; SVG cannot parse Emacs-only color names (e.g. "gray50"), nor
;; "unspecified-fg"/"unspecified-bg", and falls back to black.  Every color
;; that goes into an SVG is therefore resolved to a #rrggbb string here.

(defun org-grid--hex (color)
  "Return COLOR as a #rrggbb string, or nil if it cannot be resolved."
  (when (and (stringp color)
             (not (string-prefix-p "unspecified" color)))
    (when-let ((rgb (color-name-to-rgb color)))
      (apply #'color-rgb-to-hex (append rgb '(2))))))

(defun org-grid--bg ()
  "Default background as #rrggbb."
  (or (org-grid--hex (face-background 'default nil t))
      (org-grid--hex (frame-parameter nil 'background-color))
      (if (eq (frame-parameter nil 'background-mode) 'dark) "#000000" "#ffffff")))

(defun org-grid--fg ()
  "Default foreground as #rrggbb."
  (or (org-grid--hex (face-foreground 'default nil t))
      (org-grid--hex (frame-parameter nil 'foreground-color))
      (if (eq (frame-parameter nil 'background-mode) 'dark) "#ffffff" "#000000")))

(defun org-grid--muted ()
  "Foreground blended toward background: always readable, always dimmer."
  (let ((fg (color-name-to-rgb (org-grid--fg)))
        (bg (color-name-to-rgb (org-grid--bg))))
    (apply #'color-rgb-to-hex
           (append (cl-mapcar (lambda (f b) (+ b (* 0.65 (- f b)))) fg bg)
                   '(2)))))

(defun org-grid--mix (from to amount)
  "Blend color FROM toward TO by AMOUNT (0..1), as #rrggbb."
  (apply #'color-rgb-to-hex
         (append (cl-mapcar (lambda (a b) (+ a (* amount (- b a))))
                            (color-name-to-rgb from) (color-name-to-rgb to))
                 '(2))))

(defun org-grid--card-height ()
  (round (* org-grid-thumbnail-size org-grid-card-aspect)))

(defvar-local org-grid--file-cache nil
  "path -> file content string, so a file with many headings is only
ever read from disk once per refresh, no matter how many notes it
holds or how many times links/snippets/search touch it.")

(defun org-grid--file-content (path)
  (unless (hash-table-p org-grid--file-cache)
    (setq org-grid--file-cache (make-hash-table :test 'equal)))
  (or (gethash path org-grid--file-cache)
      (puthash path
               (condition-case nil
                   (with-temp-buffer
                     (insert-file-contents path)
                     (buffer-string))
                 (error ""))
               org-grid--file-cache)))

(defconst org-grid--media-name-re "\\`\\(.*?\\)__\\(.+\\)\\'"
  "Matches a stripped media filename stem: TITLE__tag1_tag2.")

(defconst org-grid--heading-re "^\\(\\*+\\)[ \t]+\\(.*\\)$")

(defconst org-grid--tags-re "[ \t]+\\(:[[:alnum:]_@#%:]+:\\)[ \t]*\\'"
  "Matches a trailing org tags block on a heading line.")

(defconst org-grid--todo-keywords
  '("TODO" "DONE" "NEXT" "WAITING" "CANCELLED" "HOLD" "IN-PROGRESS"))

(defconst org-grid--link-re "\\[\\[\\([^]\\[]+\\)\\]"
  "Matches the target portion of an org link, [[TARGET]] or [[TARGET][desc]].")

(cl-defstruct org-grid-item
  id title tags path type mtime snippet-fetched snippet
  ;; pos/end: char positions of a note's heading and subtree end within
  ;; its file. Both nil for media items (which have no subtree).
  pos end
  ;; Number of items inside a folder card (type `channel' only).
  count)

(defun org-grid--file-type (ext)
  (cond
   ((member ext org-grid-image-extensions) 'image)
   ((member ext org-grid-video-extensions) 'video)
   ((member ext org-grid-pdf-extensions) 'pdf)
   ((equal ext "org") 'text)
   (t 'other)))

(defun org-grid--clean-line (line)
  "Turn an org LINE into plain display text.
Headings are prefixed with a ^A marker so the card can draw them bold."
  (let ((s line) (heading nil))
    (when (string-match "\\`\\*+[ \t]+" s)
      (setq heading t s (substring s (match-end 0)))
      (when (string-match org-grid--tags-re s)
        (setq s (substring s 0 (match-beginning 0))))
      (when (and (string-match "\\`\\([A-Z-]+\\)[ \t]+" s)
                 (member (match-string 1 s) org-grid--todo-keywords))
        (setq s (substring s (match-end 0)))))
    (setq s (replace-regexp-in-string "\\[\\[id:[^]]*\\]\\]" "" s))
    (setq s (replace-regexp-in-string "\\[\\[[^]]*\\]\\[\\([^]]*\\)\\]\\]" "\\1" s))
    (setq s (replace-regexp-in-string "\\[\\[\\(?:file:\\)?\\([^]]*\\)\\]\\]" "\\1" s))
    (setq s (replace-regexp-in-string "\\`\\([ \t]*\\)[-+] \\[ \\]" "\\1☐" s))
    (setq s (replace-regexp-in-string "\\`\\([ \t]*\\)[-+] \\[[xX]\\]" "\\1☑" s))
    (setq s (replace-regexp-in-string "\\`\\([ \t]*\\)[-+] " "\\1• " s))
    (setq s (replace-regexp-in-string "\\*\\([^* \t\n][^*\n]*?\\)\\*" "\\1" s))
    (setq s (replace-regexp-in-string "=\\([^= \t\n][^=\n]*?\\)=" "\\1" s))
    (setq s (replace-regexp-in-string "~\\([^~ \t\n][^~\n]*?\\)~" "\\1" s))
    (if heading (concat (string 1) s) s)))

(defun org-grid--snippet (item)
  "Return a cleaned plain-text excerpt of ITEM's subtree (a note), or \"\"."
  (condition-case nil
      (let ((content (org-grid--file-content (org-grid-item-path item))))
        (with-temp-buffer
          (insert content)
          (let ((end (min (or (org-grid-item-end item) (point-max)) (point-max)))
                (in-drawer nil)
                lines)
            (goto-char (org-grid-item-pos item))
            (forward-line 1)
            (while (< (point) end)
              (let ((line (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position))))
                (cond
                 (in-drawer
                  (when (string-match-p "\\`[ \t]*:END:[ \t]*\\'" line)
                    (setq in-drawer nil)))
                 ((string-match-p "\\`[ \t]*:END:[ \t]*\\'" line))
                 ((string-match-p "\\`[ \t]*:[[:alnum:]_-]+:[ \t]*\\'" line)
                  (setq in-drawer t))
                 ((string-match-p
                   "\\`[ \t]*\\(#\\(\\+\\| \\)\\|:[[:alnum:]_-]+:\\|\\(SCHEDULED\\|DEADLINE\\|CLOSED\\):\\|---\\)"
                   line))
                 (t (push (org-grid--clean-line line) lines))))
              (forward-line 1))
            (let ((body (string-trim (mapconcat #'identity (nreverse lines) "\n"))))
              (substring body 0 (min (length body) org-grid-note-snippet-length))))))
    (error "")))

(defun org-grid--get-snippet (item)
  (unless (org-grid-item-snippet-fetched item)
    (setf (org-grid-item-snippet item)
          (if (and (eq (org-grid-item-type item) 'text) (org-grid-item-pos item))
              (org-grid--snippet item)
            ""))
    (setf (org-grid-item-snippet-fetched item) t))
  (org-grid-item-snippet item))

(defun org-grid--path-id (path)
  "A stable synthetic id for a media file, which has no :ID: of its own."
  (secure-hash 'md5 (expand-file-name path)))

(defun org-grid--parse-media-file (path)
  "Parse PATH as a media item, or nil if its extension isn't recognized.
Filename convention: TITLE.ext or TITLE__tag1_tag2.ext (no id or
timestamp prefix)."
  (let* ((name (file-name-nondirectory path))
         (ext (downcase (or (file-name-extension name) "")))
         (type (org-grid--file-type ext)))
    (when (memq type '(image video pdf))
      (let* ((stem (file-name-sans-extension name))
             (mtime (float-time (file-attribute-modification-time (file-attributes path))))
             (title stem) (tags nil))
        (when (string-match org-grid--media-name-re stem)
          (setq title (match-string 1 stem)
                tags (split-string (match-string 2 stem) "_" t)))
        (make-org-grid-item
         :id (org-grid--path-id path)
         :title (replace-regexp-in-string "-" " " title)
         :tags tags :path path :type type :mtime mtime
         :snippet-fetched t :snippet "")))))

(defun org-grid--parse-org-file (path)
  "Parse PATH and return a list of note items.
Recognizes both file-level Org-Roam notes (top-level :ID: and #+TITLE:)
and heading-level notes with an :ID: property."
  (let (items (content (org-grid--file-content path)))
    (condition-case nil
        (with-temp-buffer
          (insert content)
          (let ((mtime (float-time (file-attribute-modification-time (file-attributes path)))))
            (goto-char (point-min))
            
            ;; 1. Check for File-Level Node (e.g. Org-Roam v2 file node)
            (let (file-id file-title file-tags file-end)
              ;; Extract #+TITLE if present
              (when (re-search-forward "^[ \t]*#\\+TITLE:[ \t]*\\(.*\\)$" nil t)
                (setq file-title (string-trim (match-string 1))))
              
              ;; Extract #+FILETAGS if present
              (goto-char (point-min))
              (when (re-search-forward "^[ \t]*#\\+FILETAGS:[ \t]*\\(.*\\)$" nil t)
                (let ((raw-tags (match-string 1)))
                  (setq file-tags (split-string raw-tags ":\\|[ \t]+" t))))

              ;; Search for top-level :ID: (before the first heading)
              (goto-char (point-min))
              (let ((first-heading-pos (save-excursion
                                         (if (re-search-forward org-grid--heading-re nil t)
                                             (match-beginning 0)
                                           (point-max)))))
                (when (re-search-forward "^[ \t]*:PROPERTIES:[ \t]*$" first-heading-pos t)
                  (forward-line 1)
                  (while (and (< (point) first-heading-pos)
                              (not (looking-at "^[ \t]*:END:[ \t]*$")))
                    (when (looking-at "^[ \t]*:ID:[ \t]+\\(.+?\\)[ \t]*$")
                      (setq file-id (match-string 1)))
                    (forward-line 1)))
                
                (when file-id
                  (let ((note-mtime mtime))
                    (push (make-org-grid-item
                           :id file-id
                           :title (if (and file-title (not (string-empty-p file-title)))
                                      file-title
                                    (file-name-base path))
                           :tags file-tags
                           :path path
                           :type 'text
                           :mtime note-mtime
                           :pos (point-min)
                           :end first-heading-pos
                           :snippet-fetched nil
                           :snippet "")
                          items)))))

            ;; 2. Parse Heading-Level Nodes
            (goto-char (point-min))
            (while (re-search-forward org-grid--heading-re nil t)
              (let* ((level (length (match-string 1)))
                     (raw (string-trim (match-string 2)))
                     (hstart (line-beginning-position))
                     (tags nil) (title raw))
                (when (string-match org-grid--tags-re raw)
                  (let ((tagstr (match-string 1 raw))
                        (cut (match-beginning 0)))
                    (setq tags (split-string tagstr ":" t)
                          title (string-trim (substring raw 0 cut)))))
                (when (string-match "\\`\\([A-Z-]+\\)[ \t]+\\(.*\\)\\'" title)
                  (when (member (match-string 1 title) org-grid--todo-keywords)
                    (setq title (match-string 2 title))))
                (goto-char hstart)
                (forward-line 1)
                (when (looking-at "^[ \t]*\\(SCHEDULED\\|DEADLINE\\|CLOSED\\):.*$")
                  (forward-line 1))
                (let (id)
                  (when (looking-at "^[ \t]*:PROPERTIES:[ \t]*$")
                    (forward-line 1)
                    (while (and (not (eobp)) (not (looking-at "^[ \t]*:END:[ \t]*$")))
                      (when (looking-at "^[ \t]*:ID:[ \t]+\\(.+?\\)[ \t]*$")
                        (setq id (match-string 1)))
                      (forward-line 1))
                    (when (looking-at "^[ \t]*:END:[ \t]*$") (forward-line 1)))
                  (when id
                    (let* ((body-start (point))
                           (body-end (save-excursion
                                       (if (re-search-forward
                                            (format "^\\*\\{1,%d\\}[ \t]" level) nil t)
                                           (line-beginning-position)
                                         (point-max))))
                           (note-mtime mtime))
                      (ignore body-start)
                      (push (make-org-grid-item
                             :id id
                             :title (if (string-empty-p title) "(untitled)" title)
                             :tags tags :path path :type 'text :mtime note-mtime
                             :pos hstart :end body-end
                             :snippet-fetched nil :snippet "")
                            items))))
                (goto-char hstart)
                (forward-line 1)))))
      (error nil))
    (nreverse items)))

(defun org-grid--collect-items (root)
  "Collect all note and media items under ROOT.
ROOT can be a directory path string or a list of directory path strings."
  (let ((dirs (if (listp root) root (list root)))
        items)
    (dolist (dir0 dirs)
      (let ((dir (and dir0 (expand-file-name dir0))))
        (when (and dir (file-directory-p dir))
          (dolist (f (directory-files-recursively
                      dir ".*" nil
                      (lambda (d) (not (string-prefix-p "." (file-name-nondirectory d))))))
            (let ((base (file-name-nondirectory f)))
              (unless (or (string-prefix-p "." base)
                          (org-grid--ignored-file-p f))
                (if (equal (downcase (or (file-name-extension base) "")) "org")
                    (dolist (it (org-grid--parse-org-file f)) (push it items))
                  (when-let ((item (org-grid--parse-media-file f)))
                    (push item items)))))))))
    (nreverse items)))

(defun org-grid--dired-visible-files ()
  (unless (derived-mode-p 'dired-mode)
    (user-error "org-grid: not in a dired buffer"))
  (let (files)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((f (dired-get-filename nil t)))
          (when (and f (not (file-directory-p f))
                     (not (org-grid--ignored-file-p f)))
            (push f files)))
        (forward-line 1)))
    (nreverse files)))

(defun org-grid--collect-items-from-dired ()
  (let (items)
    (dolist (f (org-grid--dired-visible-files))
      (if (equal (downcase (or (file-name-extension f) "")) "org")
          (dolist (it (org-grid--parse-org-file f)) (push it items))
        (when-let ((item (org-grid--parse-media-file f)))
          (push item items))))
    (nreverse items)))

(defun org-grid--file-contains-p (path query &optional beg end)
  "Search PATH for QUERY. With BEG/END, restrict the search to that region
of the file (used to scope a search to a single note's subtree)."
  (let ((rg (and org-grid-ripgrep-executable
                 (null beg)
                 (executable-find org-grid-ripgrep-executable))))
    (if rg
        (zerop (call-process rg nil nil nil "-q" "-i" "-F" query path))
      (let ((content (org-grid--file-content path)))
        (with-temp-buffer
          (insert content)
          (goto-char (or beg (point-min)))
          (let ((case-fold-search t))
            (search-forward query (and end (min end (point-max))) t)))))))

(defun org-grid--links (items)
  "Build a symmetric id->id link table from [[id:...]] and file/path links.
Each note is only scanned within its own subtree, and file links are
resolved relative to the note's file to connect notes to media items.
Notes are grouped by file first, so a file is read from disk (or cache)
exactly once no matter how many of its headings are notes."
  (let ((ids (make-hash-table :test 'equal))
        (paths (make-hash-table :test 'equal))
        (links (make-hash-table :test 'equal))
        (by-file (make-hash-table :test 'equal)))
    (dolist (it items)
      (puthash (org-grid-item-id it) t ids)
      (when (memq (org-grid-item-type it) '(image video pdf))
        (puthash (ignore-errors (file-truename (org-grid-item-path it)))
                 (org-grid-item-id it) paths))
      (when (and (eq (org-grid-item-type it) 'text) (org-grid-item-pos it))
        (push it (gethash (org-grid-item-path it) by-file))))
    (maphash
     (lambda (path notes)
       (condition-case nil
           (let ((content (org-grid--file-content path)))
             (with-temp-buffer
               (insert content)
               (let ((dir (file-name-directory path)))
                 (dolist (it notes)
                   (let ((end (min (or (org-grid-item-end it) (point-max)) (point-max)))
                         (found nil))
                     (goto-char (min (org-grid-item-pos it) (point-max)))
                     (while (and (< (point) end)
                                 (re-search-forward org-grid--link-re end t))
                       (let* ((target (match-string 1))
                              (found-id
                               (cond
                                ((string-prefix-p "id:" target)
                                 (let ((id (substring target 3)))
                                   (and (gethash id ids) id)))
                                ((string-prefix-p "file:" target)
                                 (gethash (ignore-errors
                                            (file-truename
                                             (expand-file-name (substring target 5) dir)))
                                          paths))
                                ((string-match-p "\\`[a-z]+:" target) nil) ; http:, https:, mailto:, etc.
                                (t
                                 (gethash (ignore-errors
                                            (file-truename (expand-file-name target dir)))
                                          paths)))))
                         (when (and found-id
                                    (not (equal found-id (org-grid-item-id it)))
                                    (not (member found-id found)))
                           (push found-id found)
                           (cl-pushnew found-id (gethash (org-grid-item-id it) links) :test #'equal)
                           (cl-pushnew (org-grid-item-id it) (gethash found-id links) :test #'equal)))))))))
         (error nil)))
     by-file)
    links))

(defun org-grid--clusters (items &optional links)
  (let ((links (or links (org-grid--links items)))
        (visited (make-hash-table :test 'equal))
        (components nil))
    (dolist (it items)
      (let ((id (org-grid-item-id it)))
        (unless (gethash id visited)
          (let ((queue (list id))
                (component nil))
            (while queue
              (let ((cur (pop queue)))
                (unless (gethash cur visited)
                  (puthash cur t visited)
                  (push cur component)
                  (dolist (nb (gethash cur links)) (push nb queue)))))
            (when component
              (push component components))))))
    (setq components (sort components (lambda (a b) (> (length a) (length b)))))
    (let ((cluster-of (make-hash-table :test 'equal))
          (n 0))
      (dolist (comp components)
        (setq n (1+ n))
        (dolist (id comp)
          (puthash id n cluster-of)))
      cluster-of)))

(defun org-grid--links-cached (items)
  "Link table for ITEMS, recomputed only when the set of items changes."
  (let ((key (mapcar (lambda (it) (org-grid-item-id it)) items)))
    (unless (equal key org-grid--links-cache-key)
      (setq org-grid--links-cache (org-grid--links items)
            org-grid--links-cache-key key))
    org-grid--links-cache))

(defun org-grid--clusters-cached (items)
  (let ((key (mapcar (lambda (it) (org-grid-item-id it)) items)))
    (unless (equal key org-grid--clusters-cache-key)
      (setq org-grid--clusters-cache
            (org-grid--clusters items (org-grid--links-cached items))
            org-grid--clusters-cache-key key))
    org-grid--clusters-cache))

(defun org-grid--build-link-counts ()
  (let ((h (make-hash-table :test 'equal)))
    (maphash (lambda (k v) (puthash k (length v) h))
             (org-grid--links-cached org-grid--items))
    h))

(defun org-grid--link-count (item)
  (if (hash-table-p org-grid--link-counts)
      (gethash (org-grid-item-id item) org-grid--link-counts 0)
    0))

;;; Folders (channels)

(defvar-local org-grid--roots nil
  "Slash-terminated, expanded top-level directories of this grid.")
(defvar-local org-grid--dirs nil
  "Directories being shown: all roots, or a single folder inside one.")

(defun org-grid--norm-dir (dir)
  (file-name-as-directory (expand-file-name dir)))

(defun org-grid--deep-p ()
  "Non-nil when the grid shows a whole subtree (flat) instead of one folder."
  (or (not (string-empty-p org-grid--filter))
      org-grid--cluster-p org-grid--orphan-p))

(defun org-grid--scope-items (deep)
  "Items under the current directories; only direct children unless DEEP."
  (cl-remove-if-not
   (lambda (it)
     (let* ((p (expand-file-name (org-grid-item-path it)))
            (d (cl-find-if (lambda (d) (string-prefix-p d p)) org-grid--dirs)))
       (and d (or deep (not (string-match-p "/" (substring p (length d))))))))
   org-grid--items))

(defun org-grid--channels ()
  "Folder cards for the sub-folders of the current directories."
  (let ((table (make-hash-table :test 'equal)) channels)
    (dolist (it org-grid--items)
      (let* ((p (expand-file-name (org-grid-item-path it)))
             (d (cl-find-if (lambda (d) (string-prefix-p d p)) org-grid--dirs))
             (slash (and d (string-match "/" (substring p (length d))))))
        (when slash
          (let* ((key (concat d (substring p (length d) (+ (length d) slash)) "/"))
                 (cur (gethash key table)))
            (puthash key (cons (1+ (or (car cur) 0))
                               (max (or (cdr cur) 0) (org-grid-item-mtime it)))
                     table)))))
    (maphash (lambda (key v)
               (push (make-org-grid-item
                      :id key :path key :type 'channel :tags nil
                      :title (file-name-nondirectory (directory-file-name key))
                      :count (car v) :mtime (cdr v) :snippet-fetched t :snippet "")
                     channels))
             table)
    (sort channels (lambda (a b) (string-collate-lessp (org-grid-item-title a)
                                                       (org-grid-item-title b)
                                                       nil t)))))

(defun org-grid--ago (time)
  (let ((d (max 0 (- (float-time) time))))
    (or (cl-loop for (secs . name) in '((31536000 . "year") (2592000 . "month")
                                         (86400 . "day") (3600 . "hour") (60 . "minute"))
                 when (>= d secs)
                 return (let ((n (floor d secs)))
                          (format "%d %s%s ago" n name (if (= n 1) "" "s"))))
        "just now")))

(defun org-grid--channel-svg (item)
  "A folder card: bordered square with its name, item count and age."
  (let* ((w org-grid-thumbnail-size) (h (org-grid--card-height))
         (bg (org-grid--bg)) (fg (org-grid--fg)) (muted (org-grid--muted))
         (border (org-grid--mix bg fg 0.45))
         (n (or (org-grid-item-count item) 0))
         (max-chars (floor (- w 40) 9.4))
         (lines (mapcar (lambda (l) (truncate-string-to-width l max-chars nil nil "…"))
                        (seq-take (org-grid--wrap-text (org-grid-item-title item) max-chars) 3)))
         (y (+ (- (/ h 2) 14) (* -11 (1- (length lines)))))
         (svg (svg-create w h :xmlns:xlink "http://www.w3.org/1999/xlink")))
    (svg-rectangle svg 0 0 w h :fill bg :rx org-grid-card-radius)
    (svg-rectangle svg 1 1 (- w 2) (- h 2) :fill "none" :stroke-color border
                   :stroke-width 2 :rx org-grid-card-radius)
    (dolist (line lines)
      (svg-text svg line :x (/ w 2) :y y :fill fg :font-size 17 :text-anchor "middle"
                :font-family org-grid-note-font-family)
      (setq y (+ y 22)))
    (svg-text svg (format "%d block%s" n (if (= n 1) "" "s")) :x (/ w 2) :y (+ y 8)
              :fill fg :font-size 11 :text-anchor "middle"
              :font-family org-grid-note-font-family)
    (svg-text svg (org-grid--ago (org-grid-item-mtime item)) :x (/ w 2) :y (+ y 25)
              :fill muted :font-size 11 :text-anchor "middle"
              :font-family org-grid-note-font-family)
    (svg-image svg :ascent 'center)))

;;; Card drawing

(defun org-grid--wrap-text (str width)
  "Wrap STR to WIDTH chars, keeping paragraph breaks as empty strings.
Lines of heading paragraphs carry the text property `org-grid-bold'."
  (let (lines)
    (dolist (raw (split-string str "\n"))
      (let* ((bold (string-prefix-p (string 1) raw))
             (para (if bold (substring raw 1) raw)))
        (if (string-blank-p para)
            (unless (or (null lines) (equal (car lines) ""))
              (push "" lines))
          (let ((cur "")
                (emit (lambda (l)
                        (push (if bold (propertize l 'org-grid-bold t) l) lines))))
            (dolist (w (split-string para))
              (cond ((string-empty-p cur) (setq cur w))
                    ((<= (+ (length cur) 1 (length w)) width)
                     (setq cur (concat cur " " w)))
                    (t (funcall emit cur) (setq cur w))))
            (unless (string-empty-p cur) (funcall emit cur))))))
    (nreverse lines)))

(defun org-grid--note-svg (item)
  "Text card: bordered box, body first, title as a caption."
  (let* ((w org-grid-thumbnail-size) (h (org-grid--card-height))
         (bg (org-grid--bg))
         (fg (org-grid--fg))
         (muted (org-grid--muted))
         (surface (org-grid--mix bg fg 0.05))
         (border (org-grid--mix bg fg 0.28))
         (font org-grid-note-font-family)
         (pad 14) (fs org-grid-note-font-size) (lh (round (* fs 1.42))) (footer 30)
         (chars (floor (- w (* 2 pad)) (* fs 0.56)))
         (body (org-grid--get-snippet item))
         (empty (string-empty-p (string-trim body)))
         (title (org-grid-item-title item))
         (nl (org-grid--link-count item))
         (badge (and (> nl 0) (format "%d link%s" nl (if (= nl 1) "" "s"))))
         (svg (svg-create w h :xmlns:xlink "http://www.w3.org/1999/xlink")))
    (svg-rectangle svg 0 0 w h :fill surface :rx org-grid-card-radius)
    (svg-rectangle svg 0.5 0.5 (1- w) (1- h) :fill "none" :stroke-color border
                   :stroke-width 1 :rx org-grid-card-radius)
    (if empty
        (let ((y (+ pad 8)))
          (dolist (line (seq-take (org-grid--wrap-text title (floor (- w (* 2 pad)) 8.4)) 4))
            (svg-text svg line :x pad :y (+ y 6) :fill fg :font-size 15
                      :font-weight "bold" :font-family font)
            (setq y (+ y 22))))
      (let* ((max-lines (floor (- h footer pad) lh))
             (all (org-grid--wrap-text body chars))
             (shown (seq-take all max-lines))
             (cut (> (length all) max-lines))
             (n (length shown))
             (y (+ pad fs -2))
             (i 0))
        (dolist (line shown)
          (if (string-empty-p line)
              (setq y (- y (/ lh 2)))
            (let ((bold (get-text-property 0 'org-grid-bold line))
                  (txt (if (and cut (= i (1- n)))
                           (concat (string-trim-right line) "…")
                         line))
                  (op (cond ((and cut (= i (1- n))) "0.3")
                            ((and cut (= i (- n 2))) "0.65")
                            (t "1"))))
              (apply #'svg-text svg txt :x pad :y y :fill fg :fill-opacity op
                     :font-size fs :font-family font
                     (and bold '(:font-weight "bold")))))
          (setq y (+ y lh) i (1+ i)))))
    (svg-rectangle svg 0 (- h footer) w 1 :fill border)
    (let ((base (+ (- h (/ footer 2)) 4)))
      (when badge
        (svg-text svg badge :x (- w pad) :y base :fill muted :font-size 10
                  :text-anchor "end" :font-family font))
      (svg-text svg (truncate-string-to-width
                     title
                     (floor (- w pad pad (if badge (+ 8 (* (length badge) 5.8)) 0))
                            (* 10.5 0.56))
                     nil nil "…")
                :x pad :y base :fill muted :font-size 10.5 :font-family font))
    (svg-image svg :ascent 'center)))

(defun org-grid--placeholder-svg (item label)
  (let* ((w org-grid-thumbnail-size) (h (org-grid--card-height))
         (bg (org-grid--bg))
         (fg (org-grid--fg))
         (accent (org-grid--muted))
         (svg (svg-create w h :xmlns:xlink "http://www.w3.org/1999/xlink")))
    (svg-rectangle svg 0 0 w h :fill bg :rx org-grid-card-radius)
    (svg-rectangle svg 0 0 w h :fill accent :fill-opacity "0.12" :rx org-grid-card-radius)
    (svg-text svg label :x (/ w 2) :y (/ h 2) :fill accent :font-size 22
              :font-weight "bold" :font-family "sans-serif" :text-anchor "middle")
    (svg-text svg (truncate-string-to-width (org-grid-item-title item) 26 nil nil "…")
              :x 14 :y (- h 14) :fill fg :font-size 11 :font-family "sans-serif")
    (svg-image svg :ascent 'center)))

(defun org-grid--cache-file (item ext)
  (expand-file-name (format "%s-%d.%s" (org-grid-item-id item)
                            (round (org-grid-item-mtime item)) ext)
                    org-grid--cache-dir))

(defun org-grid--mime-for (file)
  (pcase (downcase (or (file-name-extension file) ""))
    ((or "jpg" "jpeg") "image/jpeg")
    ("png" "image/png")
    ("gif" "image/gif")
    ("webp" "image/webp")
    ("svg" "image/svg+xml")
    ("bmp" "image/bmp")
    (_ "image/png")))

(defun org-grid--cached-probe (item ext fn)
  "Return the string cached for ITEM under EXT, computing it with FN once."
  (let ((f (org-grid--cache-file item ext)))
    (if (file-exists-p f)
        (let ((v (string-trim (with-temp-buffer (insert-file-contents f) (buffer-string)))))
          (and (not (string-empty-p v)) v))
      (let ((v (ignore-errors (funcall fn))))
        (ignore-errors (with-temp-file f (insert (or v ""))))
        v))))

(defun org-grid--format-duration (secs)
  (let* ((s (round secs)) (h (/ s 3600)) (m (/ (mod s 3600) 60)) (r (mod s 60)))
    (if (> h 0) (format "%d:%02d:%02d" h m r) (format "%d:%02d" m r))))

(defun org-grid--video-duration (item)
  (when (executable-find org-grid-ffprobe-executable)
    (org-grid--cached-probe
     item "dur"
     (lambda ()
       (with-temp-buffer
         (when (zerop (call-process org-grid-ffprobe-executable nil t nil
                                    "-v" "error" "-show_entries" "format=duration"
                                    "-of" "csv=p=0" (org-grid-item-path item)))
           (let ((d (string-to-number (buffer-string))))
             (and (> d 0) (org-grid--format-duration d)))))))))

(defun org-grid--pdf-pages (item)
  (when (executable-find org-grid-pdfinfo-executable)
    (org-grid--cached-probe
     item "pages"
     (lambda ()
       (with-temp-buffer
         (call-process org-grid-pdfinfo-executable nil t nil (org-grid-item-path item))
         (goto-char (point-min))
         (when (re-search-forward "^Pages:[ \t]+\\([0-9]+\\)" nil t)
           (format "%s p" (match-string 1))))))))

(defun org-grid--draw-badge (svg w text play)
  "Draw a small dark pill with TEXT (and a play triangle if PLAY) at the top right."
  (let* ((text (or text ""))
         (tw (+ 12 (* (length text) 6.4) (if play 10 0)))
         (x (- w tw 8)) (y 8))
    (svg-rectangle svg x y tw 18 :fill "#000000" :fill-opacity "0.65" :rx 3)
    (when play
      (svg-polygon svg (list (cons (+ x 7) (+ y 5)) (cons (+ x 7) (+ y 13))
                             (cons (+ x 14) (+ y 9)))
                   :fill "#ffffff"))
    (unless (string-empty-p text)
      (svg-text svg text :x (+ x (if play 18 6)) :y (+ y 13) :fill "#ffffff"
                :font-size 10 :font-family "sans-serif"))))

(defun org-grid--boxed-raster (item raw-file label &optional badge-text play)
  (if (null raw-file)
      (org-grid--placeholder-svg item label)
    (let* ((w org-grid-thumbnail-size) (h (org-grid--card-height))
           (dim (ignore-errors (image-size (create-image raw-file nil nil) t))))
      (if (not dim)
          (org-grid--placeholder-svg item label)
        (let* ((iw (car dim)) (ih (cdr dim))
               (cover (eq org-grid-image-fit 'cover))
               (pad (if cover 0 6))
               (scale (min (/ (float (- w (* 2 pad))) iw) (/ (float (- h (* 2 pad))) ih)))
               (dw (max 1 (round (* iw scale))))
               (dh (max 1 (round (* ih scale))))
               (x (round (/ (- w dw) 2.0)))
               (y (round (/ (- h dh) 2.0)))
               (svg (svg-create w h :xmlns:xlink "http://www.w3.org/1999/xlink"))
               (bg (org-grid--bg)))
          (svg-rectangle svg 0 0 w h :fill bg :rx org-grid-card-radius)
          (condition-case nil
              (progn
                (if cover
                    (svg-embed svg raw-file (org-grid--mime-for raw-file) nil
                               :x 0 :y 0 :width w :height h
                               :preserveAspectRatio "xMidYMid slice")
                  (svg-embed svg raw-file (org-grid--mime-for raw-file) nil
                             :x x :y y :width dw :height dh))
                (when (or badge-text play)
                  (org-grid--draw-badge svg w badge-text play))
                (svg-image svg :ascent 'center))
            (error (org-grid--placeholder-svg item label))))))))

(defvar org-grid--warned-missing-tools nil
  "Tool names we've already warned about missing this session, so the
warning doesn't repeat on every thumbnail.")

(defun org-grid--warn-missing-tool (tool)
  (unless (member tool org-grid--warned-missing-tools)
    (push tool org-grid--warned-missing-tools)
    (message "org-grid: %s not found via `executable-find' (exec-path/PATH issue?); showing placeholder thumbnails instead. Set the corresponding org-grid-*-executable to an absolute path, or sync PATH into Emacs (e.g. exec-path-from-shell)." tool)))

(defun org-grid--video-thumb (item)
  (let ((out nil))
    (if (executable-find org-grid-ffmpeg-executable)
        (progn
          (setq out (org-grid--cache-file item "jpg"))
          (unless (file-exists-p out)
            (call-process org-grid-ffmpeg-executable nil nil nil
                          "-y" "-ss" "1" "-i" (org-grid-item-path item)
                          "-frames:v" "1"
                          "-vf" (format "scale=%d:-1" (* org-grid-thumbnail-oversample
                                                        org-grid-thumbnail-size))
                          "-loglevel" "quiet" out))
          (unless (file-exists-p out) (setq out nil)))
      (org-grid--warn-missing-tool org-grid-ffmpeg-executable))
    (org-grid--boxed-raster item out "VIDEO"
                            (and out (org-grid--video-duration item)) t)))

(defun org-grid--pdf-thumb (item)
  (let ((out nil))
    (if (executable-find org-grid-pdftoppm-executable)
        (let* ((base (org-grid--cache-file item "pdfpage"))
               (png (concat base ".png")))
          (unless (file-exists-p png)
            (call-process org-grid-pdftoppm-executable nil nil nil
                          "-png" "-f" "1" "-singlefile"
                          "-scale-to" (number-to-string (* org-grid-thumbnail-oversample
                                                           org-grid-thumbnail-size))
                          (org-grid-item-path item) base))
          (when (file-exists-p png) (setq out png)))
      (org-grid--warn-missing-tool org-grid-pdftoppm-executable))
    (org-grid--boxed-raster item out "PDF"
                            (and out (org-grid--pdf-pages item)))))

(defun org-grid--image-thumb (item)
  (let ((out (org-grid-item-path item)))
    (if (executable-find org-grid-ffmpeg-executable)
        (let ((cached (org-grid--cache-file item "jpg")))
          (unless (file-exists-p cached)
            (call-process org-grid-ffmpeg-executable nil nil nil
                          "-y" "-i" (org-grid-item-path item)
                          "-vf" (format "scale=%d:-1" (* org-grid-thumbnail-oversample
                                                        org-grid-thumbnail-size))
                          "-loglevel" "quiet" cached))
          (when (file-exists-p cached)
            (setq out cached)))
      (org-grid--warn-missing-tool org-grid-ffmpeg-executable))
    (org-grid--boxed-raster item out "IMG")))

(defun org-grid--cache-key (item)
  (list (org-grid-item-id item) (org-grid-item-mtime item)
        (org-grid-item-type item) org-grid-thumbnail-size
        (org-grid--bg) (org-grid--fg)
        org-grid-card-aspect org-grid-card-radius org-grid-image-fit
        org-grid-note-font-size org-grid-note-font-family
        org-grid-note-snippet-length
        (org-grid-item-count item)
        (and (eq (org-grid-item-type item) 'text) (org-grid--link-count item))))

(defun org-grid--get-image (item)
  (unless org-grid--image-cache
    (setq org-grid--image-cache (make-hash-table :test 'equal)))
  (let* ((key (org-grid--cache-key item))
         (cached (gethash key org-grid--image-cache)))
    (or cached
        (puthash key
                 (pcase (org-grid-item-type item)
                   ('image (org-grid--image-thumb item))
                   ('video (org-grid--video-thumb item))
                   ('pdf (org-grid--pdf-thumb item))
                   ('text (org-grid--note-svg item))
                   ('channel (org-grid--channel-svg item))
                   (_ (let ((ext-label (upcase (or (file-name-extension (org-grid-item-path item)) "FILE"))))
                        (org-grid--placeholder-svg item ext-label))))
                 org-grid--image-cache))))

(defun org-grid--prune-image-cache (items)
  (when (hash-table-p org-grid--image-cache)
    (let ((live-ids (make-hash-table :test 'equal)))
      (dolist (it items) (puthash (org-grid-item-id it) t live-ids))
      (let (stale)
        (maphash (lambda (key _val)
                   (unless (gethash (car key) live-ids)
                     (push key stale)))
                 org-grid--image-cache)
        (dolist (key stale) (remhash key org-grid--image-cache))))))

(defun org-grid--raster-p (item)
  (memq (org-grid-item-type item) '(image video pdf)))

;;; Selection

(defun org-grid--selection-color ()
  (or (org-grid--hex (face-attribute 'org-grid-selection-face :foreground nil t))
      (org-grid--fg)))

(defun org-grid--select-decorate (img item)
  "Return a copy of IMG with a selection border (and a title caption for media)."
  (let ((data (and (eq (car-safe img) 'image) (plist-get (cdr img) :data))))
    (if (not (stringp data))
        img
      (let* ((w org-grid-thumbnail-size) (h (org-grid--card-height))
             (col (org-grid--selection-color))
             (caption
              (unless (memq (org-grid-item-type item) '(text channel))
                (format "<rect x=\"0\" y=\"%d\" width=\"%d\" height=\"26\" fill=\"%s\" fill-opacity=\"0.85\"/><text x=\"10\" y=\"%d\" font-family=\"sans-serif\" font-size=\"11\" fill=\"%s\">%s</text>"
                        (- h 26) w (org-grid--bg) (- h 9) (org-grid--fg)
                        (xml-escape-string
                         (truncate-string-to-width (org-grid-item-title item)
                                                   (floor (- w 20) 6.2) nil nil "…")))))
             (rect (format "<rect x=\"1.5\" y=\"1.5\" width=\"%d\" height=\"%d\" fill=\"none\" stroke=\"%s\" stroke-width=\"3\" rx=\"%d\"/>"
                           (- w 3) (- h 3) col org-grid-card-radius))
             (pos (string-match "</svg>[ \t\n]*\\'" data)))
        (if (not pos)
            img
          (let ((new (copy-sequence img)))
            (setcdr new (plist-put (copy-sequence (cdr img)) :data
                                   (concat (substring data 0 pos) (or caption "") rect
                                           (substring data pos))))
            new))))))

(defun org-grid--cached-image (item)
  (and (hash-table-p org-grid--image-cache)
       (gethash (org-grid--cache-key item) org-grid--image-cache)))

(defun org-grid--display-image (item)
  "Image to show for ITEM now: cached, a placeholder if deferred, or freshly made."
  (or (org-grid--cached-image item)
      (if (org-grid--raster-p item)
          (org-grid--placeholder-svg
           item (pcase (org-grid-item-type item)
                  ('image "IMG") ('video "VIDEO") ('pdf "PDF") (_ "FILE")))
        (org-grid--get-image item))))

(defun org-grid--refresh-card (start selected)
  "Redraw the card at START, with the selection border if SELECTED."
  (when-let ((item (get-text-property start 'org-grid-item)))
    (let* ((img (org-grid--display-image item))
           (img (if selected (org-grid--select-decorate img item) img)))
      ;; `insert-image' puts the title text under the image, so a card spans
      ;; several characters: recolor the whole run, not just the first one.
      (with-silent-modifications
        (put-text-property
         start (next-single-property-change start 'org-grid-item nil (point-max))
         'display img)))))

(defvar org-grid-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'org-grid-open-at-point)
    (define-key m [mouse-1] #'org-grid-open-at-point)
    (define-key m (kbd "^") #'org-grid-up-folder)
    (define-key m (kbd "u") #'org-grid-up-folder)
    (define-key m (kbd "t") #'org-grid-filter-tag)
    (define-key m (kbd "d") #'org-grid-jump-to-dired)
    (define-key m (kbd "/") #'org-grid-filter)
    (define-key m (kbd "s") #'org-grid-sort-cycle)
    (define-key m (kbd "r") #'org-grid-sort-reverse)
    (define-key m (kbd "c") #'org-grid-toggle-cluster)
    (define-key m (kbd "o") #'org-grid-toggle-orphan)
    (define-key m (kbd "g") #'org-grid-refresh)
    (define-key m (kbd "q") #'quit-window)
    (define-key m (kbd "<right>") #'org-grid-next-card)
    (define-key m (kbd "TAB") #'org-grid-next-card)
    (define-key m (kbd "n") #'org-grid-next-card)
    (define-key m (kbd "<left>") #'org-grid-prev-card)
    (define-key m (kbd "<backtab>") #'org-grid-prev-card)
    (define-key m (kbd "p") #'org-grid-prev-card)
    (define-key m (kbd "<down>") #'org-grid-down-card)
    (define-key m (kbd "<up>") #'org-grid-up-card)
    m))

(define-derived-mode org-grid-mode special-mode "Org-Grid"
  "Major mode for browsing org notes and media as an image-dired style grid."
  (setq truncate-lines t)
  (setq-local line-spacing org-grid-gutter)
  (setq org-grid--sort-key org-grid-default-sort-key)
  (setq org-grid--sort-desc org-grid-default-sort-desc)
  (setq header-line-format '(:eval (org-grid--header-line)))
  (add-hook 'post-command-hook #'org-grid--update-point-info nil t)
  (add-hook 'window-size-change-functions #'org-grid--on-window-size-change nil t)
  (add-hook 'text-scale-mode-hook #'org-grid--render nil t)
  (add-hook 'kill-buffer-hook #'org-grid--cleanup nil t))

(defun org-grid--cleanup ()
  (when (timerp org-grid--fill-timer)
    (cancel-timer org-grid--fill-timer))
  (when (hash-table-p org-grid--image-cache)
    (clrhash org-grid--image-cache))
  (setq org-grid--image-cache nil
        org-grid--clusters-cache nil
        org-grid--links-cache nil
        org-grid--file-cache nil
        org-grid--pending-fill nil
        org-grid--fill-timer nil))

;;; Header line

(defvar org-grid--header-map
  (let ((m (make-sparse-keymap)))
    (define-key m [header-line mouse-1] #'org-grid-header-click)
    m))

(defun org-grid--chip (label filter help)
  "Clickable header-line LABEL that sets the grid filter to FILTER."
  (propertize (replace-regexp-in-string "%" "%%" label)
              'face 'org-grid-chip-face
              'mouse-face 'highlight
              'help-echo help
              'org-grid-filter filter
              'local-map org-grid--header-map))

(defun org-grid-header-click (event)
  "Act on the header-line chip or breadcrumb that was clicked."
  (interactive "e")
  (let* ((start (event-start event))
         (ps (posn-string start))
         (filter (and ps (get-text-property (cdr ps) 'org-grid-filter (car ps))))
         (dir (and ps (get-text-property (cdr ps) 'org-grid-dir (car ps)))))
    (with-current-buffer (window-buffer (posn-window start))
      (cond (dir (org-grid--enter (list dir) (org-grid--child-toward dir)))
            (filter (setq org-grid--filter filter)
                    (org-grid--render))))))

(defun org-grid--crumbs ()
  "Breadcrumb for the current folder; ancestors are clickable."
  (let* ((cur (car org-grid--dirs))
         (root (and cur (null (cdr org-grid--dirs))
                    (cl-find-if (lambda (r) (string-prefix-p r cur)) org-grid--roots))))
    (if (not root)
        (propertize "notes" 'face 'org-grid-title-face)
      (let ((acc root)
            (crumbs (list (cons (file-name-nondirectory (directory-file-name root)) root))))
        (dolist (r (split-string (substring cur (length root)) "/" t))
          (setq acc (concat acc r "/"))
          (push (cons r acc) crumbs))
        (setq crumbs (nreverse crumbs))
        (let ((tail (car (last crumbs))))
          (mapconcat
           (lambda (c)
             (let ((label (replace-regexp-in-string "%" "%%" (car c))))
               (if (eq c tail)
                   (propertize label 'face 'org-grid-title-face)
                 (propertize label 'face 'org-grid-chip-face 'mouse-face 'highlight
                             'help-echo (format "Go to %s" (car c))
                             'org-grid-dir (cdr c)
                             'local-map org-grid--header-map))))
           crumbs (propertize " / " 'face 'shadow)))))))

(defun org-grid--child-toward (dir)
  "The folder directly below DIR on the way to the current one."
  (let ((cur (car org-grid--dirs)))
    (when (and cur (string-prefix-p dir cur) (not (equal dir cur)))
      (concat dir (car (split-string (substring cur (length dir)) "/" t)) "/"))))

(defun org-grid--header-line ()
  (let ((clear (if (string-empty-p org-grid--filter)
                   ""
                 (concat "  " (org-grid--chip "✕ clear filter" "" "Clear the filter"))))
        (it org-grid--current-item))
    (concat
     " " (org-grid--crumbs) "   "
     (cond
      ((and it (eq (org-grid-item-type it) 'channel))
       (propertize (format "%d blocks" (or (org-grid-item-count it) 0)) 'face 'shadow))
      (it
       (concat
        (propertize (replace-regexp-in-string "%" "%%" (org-grid-item-title it))
                    'face 'org-grid-title-face)
        "  "
        (mapconcat (lambda (tg)
                     (org-grid--chip (concat "#" tg) (concat "#" tg)
                                     (format "Show only #%s" tg)))
                   (org-grid-item-tags it) " ")))
      (t
       (format "%d items  sort:%s%s%s%s"
               (length org-grid--items) org-grid--sort-key
               (if org-grid--sort-desc "↓" "↑")
               (if org-grid--cluster-p "  [clustered]" "")
               (if org-grid--orphan-p "  [orphans]" ""))))
     clear)))

(defun org-grid--update-point-info ()
  (setq org-grid--current-item (get-text-property (point) 'org-grid-item))
  (when (and (derived-mode-p 'org-grid-mode)
             org-grid--card-starts
             (> (length org-grid--card-starts) 0))
    (let* ((idx (org-grid--card-index-at (point)))
           (start (and (>= idx 0) (aref org-grid--card-starts idx))))
      (unless (eql start org-grid--selected-start)
        (when org-grid--selected-start
          (org-grid--refresh-card org-grid--selected-start nil))
        (setq org-grid--selected-start start)
        (when start (org-grid--refresh-card start t)))))
  (force-mode-line-update))

(defun org-grid--cards-per-row ()
  "How many cards (plus gutters) fit on a line."
  (if-let* ((win (get-buffer-window (current-buffer)))
            (win-width (window-body-width win t)))
      (min org-grid-max-columns
           (max 1 (floor (- win-width (frame-char-width))
                         (+ org-grid-thumbnail-size org-grid-gutter))))
    1))

(defun org-grid--gap ()
  "A horizontal gutter of `org-grid-gutter' pixels."
  (propertize " " 'display `(space :width (,org-grid-gutter))))

(defun org-grid--on-window-size-change (win)
  (when (and (eq (window-buffer win) (current-buffer))
             (derived-mode-p 'org-grid-mode))
    (let ((w (window-pixel-width win)))
      (unless (equal w org-grid--last-win-width)
        (setq org-grid--last-win-width w)
        (org-grid--render)))))

(defun org-grid--matches-p (item filter)
  (if (string-empty-p filter)
      t
    (if (string-prefix-p "#" filter)
        (let ((wanted (split-string (downcase (substring filter 1)) "[, ]+" t))
              (tags (mapcar #'downcase (org-grid-item-tags item))))
          (and wanted (cl-every (lambda (tg) (member tg tags)) wanted)))
      (let ((hay (downcase (concat (org-grid-item-title item) " "
                                   (mapconcat #'identity (org-grid-item-tags item) " ")))))
        (or (string-match-p (regexp-quote (downcase filter)) hay)
            (and (eq (org-grid-item-type item) 'text) (org-grid-item-pos item)
                 (org-grid--file-contains-p (org-grid-item-path item) filter
                                                (org-grid-item-pos item)
                                                (org-grid-item-end item))))))))

(defun org-grid--id-timestamp (id)
  "Epoch seconds from a ts-style org ID, or nil if ID isn't timestamp-shaped.
Accepts 20260911T160239, 20260911-160239, and 20260911160239 alike.
Returns nil (rather than erroring) for UUIDs or any other ID shape,
so callers can fall back to file mtime."
  (when (string-match "\\`\\([0-9]\\{8\\}\\)[T-]?\\([0-9]\\{6\\}\\)" id)
    (ignore-errors
      (let ((d (match-string 1 id)) (tm (match-string 2 id)))
        (float-time
         (encode-time
          (string-to-number (substring tm 4 6))
          (string-to-number (substring tm 2 4))
          (string-to-number (substring tm 0 2))
          (string-to-number (substring d 6 8))
          (string-to-number (substring d 4 6))
          (string-to-number (substring d 0 4))))))))

(defun org-grid--sort-value (item key)
  (pcase key
    ('date (format "%020d-%010d"
                    (round (* 1000 (org-grid-item-mtime item)))
                    (or (org-grid-item-pos item) 0)))
    ('created (format "%020d-%010d"
                       (round (* 1000 (or (org-grid--id-timestamp (org-grid-item-id item))
                                           (org-grid-item-mtime item))))
                       (or (org-grid-item-pos item) 0)))
    ('title (org-grid-item-title item))
    ('tags (or (car (org-grid-item-tags item)) ""))
    ('type (symbol-name (org-grid-item-type item)))))

(defun org-grid--visible-items ()
  (let* ((filtered (cl-remove-if-not (lambda (it) (org-grid--matches-p it org-grid--filter))
                                     (org-grid--scope-items (org-grid--deep-p))))
         (sorted (sort (copy-sequence filtered)
                        (lambda (a b)
                          (let ((va (org-grid--sort-value a org-grid--sort-key))
                                (vb (org-grid--sort-value b org-grid--sort-key)))
                            (if org-grid--sort-desc (string> va vb) (string< va vb))))))
         (links (org-grid--links-cached org-grid--items)))
    (cond
     (org-grid--cluster-p
      (let* ((clusters (org-grid--clusters-cached org-grid--items))
             (connected (cl-remove-if-not
                         (lambda (it) (> (length (gethash (org-grid-item-id it) links)) 0))
                         sorted)))
        (sort (copy-sequence connected)
              (lambda (a b)
                (let ((ca (gethash (org-grid-item-id a) clusters))
                      (cb (gethash (org-grid-item-id b) clusters)))
                  (if (= ca cb)
                      (string> (org-grid-item-id a) (org-grid-item-id b))
                    (< ca cb)))))))

     (org-grid--orphan-p
      (cl-remove-if (lambda (it) (> (length (gethash (org-grid-item-id it) links)) 0))
                    sorted))

     (t sorted))))

(defun org-grid--render ()
  "Render the grid with hard line breaks and pixel gutters."
  (when (timerp org-grid--fill-timer)
    (cancel-timer org-grid--fill-timer)
    (setq org-grid--fill-timer nil))
  (setq org-grid--selected-start nil)
  (setq org-grid--link-counts (org-grid--build-link-counts))
  (let ((inhibit-read-only t)
        (pos (point))
        (clusters (and org-grid--cluster-p (org-grid--clusters-cached org-grid--items)))
        (starts nil)
        (rows nil)
        (cols-list nil)
        (cur-row 0)
        (pending nil)
        (cols (org-grid--cards-per-row))
        (count 0))
    (setq-local truncate-lines t)
    (erase-buffer)
    (let ((items (append (unless (org-grid--deep-p) (org-grid--channels))
                         (org-grid--visible-items)))
          (last-cluster nil))
      (if (null items)
          (insert (propertize "\n  (nothing here)\n" 'face 'shadow))
        (dolist (it items)
          (when (and org-grid--cluster-p clusters)
            (let ((c (gethash (org-grid-item-id it) clusters)))
              (unless (eq c last-cluster)
                (unless (null last-cluster)
                  (insert "\n")
                  (setq cur-row (1+ cur-row)))
                (insert (propertize (format "·· cluster %d ··\n" c) 'face 'shadow))
                (setq last-cluster c)
                (setq count 0))))
          (when (= (mod count cols) 0)
            (insert (org-grid--gap)))
          (let* ((deferred (and (org-grid--raster-p it)
                                (not (org-grid--cached-image it))))
                 (img (org-grid--display-image it))
                 (start (point)))
            (push start starts)
            (push cur-row rows)
            (push (mod count cols) cols-list)
            (insert-image img (org-grid-item-title it))
            (when deferred
              (push (list start (point) it) pending))
            (put-text-property start (point) 'org-grid-item it)
            (put-text-property start (point) 'help-echo
                               (format "%s\n%s"
                                       (org-grid-item-title it)
                                       (mapconcat (lambda (tg) (concat "#" tg)) (org-grid-item-tags it) " ")))
            (setq count (1+ count))
            (if (= (mod count cols) 0)
                (progn (insert "\n") (setq cur-row (1+ cur-row)))
              (insert (org-grid--gap)))))))
    (setq org-grid--card-starts (vconcat (nreverse starts)))
    (setq org-grid--card-rows (vconcat (nreverse rows)))
    (setq org-grid--card-cols (vconcat (nreverse cols-list)))
    (setq org-grid--pending-fill (nreverse pending))
    (goto-char (min pos (point-max))))
  (when org-grid--pending-fill
    (run-with-idle-timer 0 nil #'org-grid--idle-fill-tick (current-buffer))
    (org-grid--schedule-idle-fill)))

(defun org-grid--apply-thumbnail (card)
  (pcase-let ((`(,start ,end ,item) card))
    (when (<= end (point-max))
      (let ((inhibit-read-only t)
            (img (org-grid--get-image item)))
        (when (eql start org-grid--selected-start)
          (setq img (org-grid--select-decorate img item)))
        (put-text-property start end 'display img)))))

(defun org-grid--fill-visible ()
  (when org-grid--pending-fill
    (let (still-pending)
      (dolist (card org-grid--pending-fill)
        (let ((start (car card)) (visible nil))
          (dolist (win (get-buffer-window-list (current-buffer) nil t))
            (when (and (>= start (window-start win)) (<= start (window-end win)))
              (setq visible t)))
          (if visible
              (org-grid--apply-thumbnail card)
            (push card still-pending))))
      (setq org-grid--pending-fill (nreverse still-pending)))))

(defun org-grid--schedule-idle-fill ()
  (when (timerp org-grid--fill-timer)
    (cancel-timer org-grid--fill-timer))
  (when org-grid--pending-fill
    (setq org-grid--fill-timer
          (run-with-idle-timer 0.3 t #'org-grid--idle-fill-tick (current-buffer)))))

(defun org-grid--idle-fill-tick (buf)
  (if (not (buffer-live-p buf))
      (when (timerp org-grid--fill-timer) (cancel-timer org-grid--fill-timer))
    (with-current-buffer buf
      (org-grid--fill-visible)
      (let ((n 0))
        (while (and org-grid--pending-fill (< n org-grid-lazy-batch-size))
          (org-grid--apply-thumbnail (pop org-grid--pending-fill))
          (setq n (1+ n))))
      (unless org-grid--pending-fill
        (when (timerp org-grid--fill-timer)
          (cancel-timer org-grid--fill-timer)
          (setq org-grid--fill-timer nil))))))

(defun org-grid--card-index-at (pos)
  (let ((vec org-grid--card-starts)
        (low 0)
        (high (1- (length org-grid--card-starts)))
        (ans 0))
    (while (<= low high)
      (let ((mid (/ (+ low high) 2)))
        (if (<= (aref vec mid) pos)
            (progn
              (setq ans mid)
              (setq low (1+ mid)))
          (setq high (1- mid)))))
    ans))

(defun org-grid--snap-to-card (&optional direction)
  "Snap point to the nearest valid card starting position.
DIRECTION non-nil means move forward, nil means move backward."
  (let ((dir (or direction 1)))
    (while (and (not (get-text-property (point) 'org-grid-item))
                (not (if (> dir 0) (eobp) (bobp))))
      (forward-line dir))
    (when-let ((item (get-text-property (point) 'org-grid-item)))
      (let ((idx (org-grid--card-index-at (point))))
        (when (>= idx 0)
          (goto-char (aref org-grid--card-starts idx)))))))

;;; Interactive Commands

;;;###autoload
(defun org-grid--goto-path (path)
  "Move point to the card whose path is PATH, else to the first card."
  (let ((starts org-grid--card-starts))
    (when (> (length starts) 0)
      (goto-char
       (or (and path
                (cl-find-if (lambda (st)
                              (let ((it (get-text-property st 'org-grid-item)))
                                (and it (equal (org-grid-item-path it) path))))
                            starts))
           (aref starts 0))))))

(defun org-grid--enter (dirs &optional focus-path)
  "Show DIRS (a list of directories), leaving point on FOCUS-PATH if given."
  (setq org-grid--dirs dirs)
  (org-grid--render)
  (org-grid--goto-path focus-path))

(defun org-grid-up-folder ()
  "Go up to the parent folder."
  (interactive)
  (let ((cur (car org-grid--dirs)))
    (cond ((cdr org-grid--dirs) (user-error "Already at the top"))
          ((member cur org-grid--roots)
           (if (cdr org-grid--roots)
               (org-grid--enter org-grid--roots cur)
             (user-error "Already at the top")))
          (t (org-grid--enter
              (list (file-name-directory (directory-file-name cur))) cur)))))

(defun org-grid-filter-tag ()
  "Filter the grid by one or more org tags, chosen with completion.
Tags are the ones on your org headings and #+FILETAGS lines.  Empty
input clears the filter."
  (interactive)
  (require 'crm)
  (let* ((tags (sort (delete-dups
                      (apply #'append (mapcar #'org-grid-item-tags org-grid--items)))
                     #'string<))
         (picked (completing-read-multiple "Filter by tags (empty clears): " tags)))
    (setq org-grid--filter (if picked (concat "#" (string-join picked ",")) ""))
    (org-grid--render)))

(defun org-grid-open (&optional dir)
  "Open the grid view for DIR.
If DIR is nil, automatically fallback to `org-grid-directory` (or its
first entry if it's a list)."
  (interactive
   (list (when current-prefix-arg
           (let ((default-dir (if (listp org-grid-directory)
                                  (car org-grid-directory)
                                org-grid-directory)))
             (read-directory-name "Notes directory: " default-dir)))))
  (let* ((target (or dir org-grid-directory))
         (primary-dir (expand-file-name (if (listp target) (car target) target)))
         (buf-name (format "*org-grid: %s*" (file-name-nondirectory (directory-file-name primary-dir))))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (org-grid-mode)
      (setq-local org-grid--source-directory target)
      (setq-local org-grid--roots (mapcar #'org-grid--norm-dir
                                          (if (listp target) target (list target))))
      (setq-local org-grid--dirs org-grid--roots)
      (setq-local org-grid--cache-dir (org-grid--cache-dir-for primary-dir))
      (setq-local org-grid--items (org-grid--collect-items target))
      (org-grid--render))
    (switch-to-buffer buf)))

;;;###autoload
(defun org-grid-from-dired ()
  "Open org-grid view displaying files from the current Dired buffer."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (user-error "org-grid: not in a Dired buffer"))
  (let* ((dired-buf (current-buffer))
         (dir (expand-file-name default-directory))
         (buf-name (format "*org-grid dired: %s*" (buffer-name dired-buf)))
         (buf (get-buffer-create buf-name))
         (items (org-grid--collect-items-from-dired)))
    (unless items
      (user-error "org-grid: no valid notes/media found in this Dired buffer"))
    (with-current-buffer buf
      (org-grid-mode)
      (setq-local org-grid--source-directory dir)
      (setq-local org-grid--source-dired-buffer dired-buf)
      (setq-local org-grid--roots (list (org-grid--norm-dir dir)))
      (setq-local org-grid--dirs org-grid--roots)
      (setq-local org-grid--cache-dir (org-grid--cache-dir-for dir))
      (setq-local org-grid--items items)
      (org-grid--render))
    (switch-to-buffer buf)))

(defun org-grid-jump-to-dired ()
  "Jump to the file under point in a Dired buffer."
  (interactive)
  (if-let* ((it (get-text-property (point) 'org-grid-item))
            (file (expand-file-name (org-grid-item-path it))))
      (let ((dired-buf org-grid--source-dired-buffer))
        (if (and dired-buf (buffer-live-p dired-buf))
            (progn
              (pop-to-buffer dired-buf)
              (dired-goto-file file))
          (dired (file-name-directory file))
          (dired-goto-file file)))
    (user-error "No item at point")))

(defun org-grid-open-at-point ()
  "Open the card at point: enter a folder, or visit the file.
For a note, jump straight to its heading."
  (interactive)
  (if-let ((item (get-text-property (point) 'org-grid-item)))
      (if (eq (org-grid-item-type item) 'channel)
          (org-grid--enter (list (org-grid-item-path item)))
        (find-file (org-grid-item-path item))
        (when (org-grid-item-pos item)
          (goto-char (min (org-grid-item-pos item) (point-max)))
          (when (fboundp 'org-fold-show-context) (org-fold-show-context))
          (recenter 1)))
    (user-error "No item at point")))

(defun org-grid-toggle-orphan ()
  "Toggle orphan view mode in org-grid."
  (interactive)
  (setq org-grid--orphan-p (not org-grid--orphan-p))
  (when org-grid--orphan-p
    (setq org-grid--cluster-p nil))
  (org-grid--render))

(defun org-grid-toggle-cluster ()
  "Toggle cluster view mode in org-grid."
  (interactive)
  (setq org-grid--cluster-p (not org-grid--cluster-p))
  (when org-grid--cluster-p
    (setq org-grid--orphan-p nil))
  (org-grid--render))

(defun org-grid-filter (query)
  "Filter items in the grid buffer by QUERY."
  (interactive (list (read-string "Filter grid (text or #tag): " org-grid--filter)))
  (setq org-grid--filter query)
  (org-grid--render))

(defun org-grid-sort-cycle ()
  "Cycle through sorting keys (date -> created -> title -> tags -> type)."
  (interactive)
  (setq org-grid--sort-key
        (pcase org-grid--sort-key
          ('date 'created)
          ('created 'title)
          ('title 'tags)
          ('tags 'type)
          (_ 'date)))
  (org-grid--render))

(defun org-grid-sort-reverse ()
  "Toggle ascending/descending order for current sort."
  (interactive)
  (setq org-grid--sort-desc (not org-grid--sort-desc))
  (org-grid--render))

(defun org-grid-refresh ()
  "Refresh the grid buffer."
  (interactive)
  (when org-grid--source-directory
    (when (hash-table-p org-grid--file-cache) (clrhash org-grid--file-cache))
    (setq org-grid--links-cache nil
          org-grid--links-cache-key nil
          org-grid--clusters-cache nil
          org-grid--clusters-cache-key nil)
    (setq org-grid--items
          (if (and org-grid--source-dired-buffer
                   (buffer-live-p org-grid--source-dired-buffer))
              (with-current-buffer org-grid--source-dired-buffer
                (org-grid--collect-items-from-dired))
            (org-grid--collect-items org-grid--source-directory)))
    (org-grid--prune-image-cache org-grid--items)
    (org-grid--render)))

(defun org-grid-next-card (&optional count)
  "Move cursor forward by COUNT cards."
  (interactive "p")
  (let* ((cnt (or count 1))
         (idx (org-grid--card-index-at (point)))
         (max-idx (1- (length org-grid--card-starts)))
         (target (min max-idx (+ idx cnt))))
    (when (and (>= target 0) org-grid--card-starts)
      (goto-char (aref org-grid--card-starts target)))))

(defun org-grid-prev-card (&optional count)
  "Move cursor backward by COUNT cards."
  (interactive "p")
  (org-grid-next-card (- (or count 1))))

(defun org-grid--nearest-card-in-row (row col)
  "Return the index of the card in ROW whose column is closest to COL."
  (let (best best-diff)
    (dotimes (i (length org-grid--card-rows))
      (when (= (aref org-grid--card-rows i) row)
        (let ((diff (abs (- (aref org-grid--card-cols i) col))))
          (when (or (null best) (< diff best-diff))
            (setq best i best-diff diff)))))
    best))

(defun org-grid-down-card (&optional count)
  "Move cursor down by COUNT visual rows."
  (interactive "p")
  (if (or org-grid--cluster-p org-grid--orphan-p)
      (let ((idx (org-grid--card-index-at (point))))
        (when (and idx org-grid--card-rows (> (length org-grid--card-rows) 0))
          (let* ((cnt (or count 1))
                 (row (aref org-grid--card-rows idx))
                 (col (aref org-grid--card-cols idx))
                 (max-row (aref org-grid--card-rows (1- (length org-grid--card-rows))))
                 (target-row (max 0 (min max-row (+ row cnt))))
                 (target-idx (org-grid--nearest-card-in-row target-row col)))
            (when target-idx
              (goto-char (aref org-grid--card-starts target-idx))))))
    (let ((cols (org-grid--cards-per-row)))
      (org-grid-next-card (* (or count 1) cols)))))

(defun org-grid-up-card (&optional count)
  "Move cursor up by COUNT visual rows in the grid."
  (interactive "p")
  (org-grid-down-card (- (or count 1))))

(provide 'org-grid)
;;; org-grid.el ends here
