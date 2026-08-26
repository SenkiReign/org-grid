;;; org-grid.el --- Grid view for vanilla org notes -*- lexical-binding: t; -*-

;; Author:  Senki R.
;; Keywords: notes, multimedia, moodboard, emacs, org-mode
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.3.0

;;; Code:

(require 'cl-lib)
(require 'svg)
(require 'dired)
(require 'color)

(defgroup org-grid nil
  "An are.na-style local grid for org notes, images, pdfs, and videos."
  :group 'convenience
  :prefix "org-grid-")

(defcustom org-grid-directory
  (if (bound-and-true-p org-directory) org-directory "~/org")
  "Default directory (or list of directories) scanned for notes and media.
A \"note\" is any org heading with an :ID: property; every other file
in the tree (image/video/pdf) is a media item."
  :type '(choice directory (repeat directory))
  :group 'org-grid)

(defcustom org-grid-thumbnail-size 220
  "Max width/height in pixels for grid thumbnails."
  :type 'integer
  :group 'org-grid)

(defcustom org-grid-note-snippet-length 220
  "How many characters of a note's body to show on its card."
  :type 'integer
  :group 'org-grid)

(defcustom org-grid-thumbnail-oversample 1
  "Resolution multiplier for generated video/pdf thumbnail files."
  :type 'integer
  :group 'org-grid)

(defcustom org-grid-lazy-batch-size 6
  "How many offscreen thumbnails to generate per idle tick."
  :type 'integer
  :group 'org-grid)

(defcustom org-grid-ripgrep-executable "rg"
  "Path to ripgrep executable used for fast full-text search."
  :type '(choice (const :tag "Disable ripgrep" nil) string)
  :group 'org-grid)

(defcustom org-grid-ffmpeg-executable "ffmpeg"
  "ffmpeg executable used for video thumbnails. Optional."
  :type 'string
  :group 'org-grid)

(defcustom org-grid-pdftoppm-executable "pdftoppm"
  "pdftoppm executable used for PDF thumbnails. Optional."
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

(defface org-grid-title-face
  '((t :inherit bold))
  "Face for the current card's title in the header line.")

(defface org-grid-tag-face
  '((t :inherit shadow))
  "Face for tags in the header line.")

(defvar org-grid--cache-dir nil)

(defun org-grid--cache-dir-for (root)
  (let ((dir (expand-file-name ".org-grid-thumbs/" root)))
    (unless (file-directory-p dir) (make-directory dir t))
    dir))

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
  pos end)

(defun org-grid--file-type (ext)
  (cond
   ((member ext org-grid-image-extensions) 'image)
   ((member ext org-grid-video-extensions) 'video)
   ((member ext org-grid-pdf-extensions) 'pdf)
   ((equal ext "org") 'text)
   (t 'other)))

(defun org-grid--snippet (item)
  "Return the body text of ITEM's subtree (a note), or \"\" for media."
  (condition-case nil
      (let ((content (org-grid--file-content (org-grid-item-path item))))
        (with-temp-buffer
        (insert content)
        (let ((end (min (or (org-grid-item-end item) (point-max)) (point-max))))
          (goto-char (org-grid-item-pos item))
          (forward-line 1)
          (let (lines)
            (while (< (point) end)
              (let ((line (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position))))
                (unless (string-match-p
                         "\\`[ \t]*\\(#\\+\\|:PROPERTIES:\\|:END:\\|:[[:alnum:]_-]+:\\|---\\)"
                         line)
                  (push line lines)))
              (forward-line 1))
            (let ((body (string-trim (mapconcat #'identity (nreverse lines) "\n"))))
              (substring body 0 (min (length body) org-grid-note-snippet-length)))))))
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
  "Parse PATH and return a list of note items, one per heading with :ID:.
Reads PATH through the file-content cache, populating it as a side
effect so later link/snippet/search passes over PATH hit memory."
  (let (items (content (org-grid--file-content path)))
    (condition-case nil
        (with-temp-buffer
          (insert content)
          (let ((mtime (float-time (file-attribute-modification-time (file-attributes path)))))
            (goto-char (point-min))
            (while (re-search-forward org-grid--heading-re nil t)
              (let* ((level (length (match-string 1)))
                     (raw (string-trim (match-string 2)))
                     (hstart (line-beginning-position))
                     (tags nil) (title raw))
                (when (string-match org-grid--tags-re raw)
                  ;; Capture match data before `split-string' (which runs its
                  ;; own regexp matching internally and would clobber it).
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
                                         (point-max)))))
                      (ignore body-start)
                      (push (make-org-grid-item
                             :id id
                             :title (if (string-empty-p title) "(untitled)" title)
                             :tags tags :path path :type 'text :mtime mtime
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
    (dolist (dir dirs)
      (when (and dir (file-directory-p dir))
        (dolist (f (directory-files-recursively
                    dir ".*" nil
                    (lambda (d) (not (string-prefix-p "." (file-name-nondirectory d))))))
          (let ((base (file-name-nondirectory f)))
            (unless (string-prefix-p "." base)
              (if (equal (downcase (or (file-name-extension base) "")) "org")
                  (dolist (it (org-grid--parse-org-file f)) (push it items))
                (when-let ((item (org-grid--parse-media-file f)))
                  (push item items))))))))
    (nreverse items)))

(defun org-grid--dired-visible-files ()
  (unless (derived-mode-p 'dired-mode)
    (user-error "org-grid: not in a dired buffer"))
  (let (files)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((f (dired-get-filename nil t)))
          (when (and f (not (file-directory-p f)))
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

(defun org-grid--tag-counts (items)
  (let ((counts (make-hash-table :test 'equal)))
    (dolist (it items)
      (dolist (tag (org-grid-item-tags it))
        (puthash tag (1+ (gethash tag counts 0)) counts)))
    counts))

(defun org-grid--color-for (tags counts)
  (when-let ((tag (car-safe tags)))
    (when (>= (gethash tag counts 0) 2)
      (let* ((hash (secure-hash 'sha256 tag))
             (h1 (string-to-number (substring hash 0 4) 16))
             (h2 (string-to-number (substring hash 4 6) 16))
             (h3 (string-to-number (substring hash 6 8) 16))
             (hue (/ (mod (* h1 2654435761) 65536) 65536.0))
             (sat (+ 0.45 (* (/ h2 255.0) 0.50)))
             (lum (+ 0.40 (* (/ h3 255.0) 0.30)))
             (rgb (color-hsl-to-rgb hue sat lum)))
        (apply #'color-rgb-to-hex (append rgb '(2)))))))

(defun org-grid--wrap-text (str width)
  (let ((words (split-string str))
        (lines nil) (cur ""))
    (dolist (w words)
      (if (< (length cur) 1)
          (setq cur w)
        (if (<= (+ (length cur) 1 (length w)) width)
            (setq cur (concat cur " " w))
          (push cur lines) (setq cur w))))
    (when (> (length cur) 0) (push cur lines))
    (nreverse lines)))

(defun org-grid--note-svg (item counts)
  (let* ((w org-grid-thumbnail-size) (h (round (* w 0.72)))
         (bg (face-background 'default nil t))
         (fg (face-foreground 'default nil t))
         (muted (face-foreground 'shadow nil t))
         (color (org-grid--color-for (org-grid-item-tags item) counts))
         (svg (svg-create w h :xmlns:xlink "http://www.w3.org/1999/xlink")))
    (svg-rectangle svg 0 0 w h :fill bg :rx 10)
    (when color (svg-rectangle svg 0 0 6 h :fill color :rx 3))
    (svg-text svg (truncate-string-to-width (org-grid-item-title item) 24 nil nil "…")
              :x 16 :y 26 :fill fg :font-size 15 :font-weight "bold" :font-family "sans-serif")
    (let ((y 48))
      (dolist (line (org-grid--wrap-text (org-grid--get-snippet item) 32))
        (when (< y (- h 22))
          (svg-text svg line :x 16 :y y :fill muted :font-size 11 :font-family "sans-serif")
          (setq y (+ y 15)))))
    (when color
      (let ((tagstr (mapconcat (lambda (tg) (concat "#" tg)) (org-grid-item-tags item) "  ")))
        (svg-text svg (truncate-string-to-width tagstr 36 nil nil "…")
                  :x 16 :y (- h 12) :fill color :font-size 10 :font-family "sans-serif")))
    (svg-image svg :ascent 'center)))

(defun org-grid--placeholder-svg (item label counts)
  (let* ((w org-grid-thumbnail-size) (h (round (* w 0.72)))
         (bg (face-background 'default nil t))
         (fg (face-foreground 'default nil t))
         (color (org-grid--color-for (org-grid-item-tags item) counts))
         (accent (or color (face-foreground 'shadow nil t)))
         (svg (svg-create w h :xmlns:xlink "http://www.w3.org/1999/xlink")))
    (svg-rectangle svg 0 0 w h :fill bg :rx 10)
    (svg-rectangle svg 0 0 w h :fill accent :fill-opacity "0.12" :rx 10)
    (when color (svg-rectangle svg 0 0 6 h :fill color :rx 3))
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

(defun org-grid--boxed-raster (item raw-file label counts)
  (if (null raw-file)
      (org-grid--placeholder-svg item label counts)
    (let* ((w org-grid-thumbnail-size) (h (round (* w 0.72)))
           (dim (ignore-errors (image-size (create-image raw-file nil nil) t))))
      (if (not dim)
          (org-grid--placeholder-svg item label counts)
        (let* ((iw (car dim)) (ih (cdr dim))
               (pad 6)
               (scale (min (/ (float (- w (* 2 pad))) iw) (/ (float (- h (* 2 pad))) ih)))
               (dw (max 1 (round (* iw scale))))
               (dh (max 1 (round (* ih scale))))
               (x (round (/ (- w dw) 2.0)))
               (y (round (/ (- h dh) 2.0)))
               (color (org-grid--color-for (org-grid-item-tags item) counts))
               (svg (svg-create w h :xmlns:xlink "http://www.w3.org/1999/xlink"))
               (bg (face-background 'default nil t)))
          (svg-rectangle svg 0 0 w h :fill bg :rx 10)
          (condition-case nil
              (progn
                (svg-embed svg raw-file (org-grid--mime-for raw-file) nil
                           :x x :y y :width dw :height dh)
                (when color (svg-rectangle svg 0 0 6 h :fill color :rx 3))
                (svg-image svg :ascent 'center))
            (error (org-grid--placeholder-svg item label counts))))))))

(defvar-local org-grid--image-cache nil)

(defun org-grid--video-thumb (item counts)
  (let ((out nil))
    (when (executable-find org-grid-ffmpeg-executable)
      (setq out (org-grid--cache-file item "jpg"))
      (unless (file-exists-p out)
        (call-process org-grid-ffmpeg-executable nil nil nil
                      "-y" "-ss" "1" "-i" (org-grid-item-path item)
                      "-frames:v" "1"
                      "-vf" (format "scale=%d:-1" (* org-grid-thumbnail-oversample
                                                    org-grid-thumbnail-size))
                      "-loglevel" "quiet" out))
      (unless (file-exists-p out) (setq out nil)))
    (org-grid--boxed-raster item out "VIDEO" counts)))

(defun org-grid--pdf-thumb (item counts)
  (let ((out nil))
    (when (executable-find org-grid-pdftoppm-executable)
      (let* ((base (org-grid--cache-file item "pdfpage"))
             (png (concat base ".png")))
        (unless (file-exists-p png)
          (call-process org-grid-pdftoppm-executable nil nil nil
                        "-png" "-f" "1" "-singlefile"
                        "-scale-to" (number-to-string (* org-grid-thumbnail-oversample
                                                         org-grid-thumbnail-size))
                        (org-grid-item-path item) base))
        (when (file-exists-p png) (setq out png))))
    (org-grid--boxed-raster item out "PDF" counts)))

(defun org-grid--image-thumb (item counts)
  (org-grid--boxed-raster item (org-grid-item-path item) "IMG" counts))

(defun org-grid--get-image (item counts)
  (unless org-grid--image-cache
    (setq org-grid--image-cache (make-hash-table :test 'equal)))
  (let* ((key (org-grid--cache-key item counts))
         (cached (gethash key org-grid--image-cache)))
    (or cached
        (puthash key
                 (pcase (org-grid-item-type item)
                   ('image (org-grid--image-thumb item counts))
                   ('video (org-grid--video-thumb item counts))
                   ('pdf (org-grid--pdf-thumb item counts))
                   ('text (org-grid--note-svg item counts))
                   (_ (let ((ext-label (upcase (or (file-name-extension (org-grid-item-path item)) "FILE"))))
                        (org-grid--placeholder-svg item ext-label counts))))
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

(defvar-local org-grid--items nil)
(defvar-local org-grid--filter "")
(defvar-local org-grid--sort-key 'date)
(defvar-local org-grid--sort-desc t)
(defvar-local org-grid--cluster-p nil)
(defvar-local org-grid--orphan-p nil)
(defvar-local org-grid--current-item nil)
(defvar-local org-grid--card-starts nil)
(defvar-local org-grid--source-directory nil)
(defvar-local org-grid--source-dired-buffer nil)
(defvar-local org-grid--selection-overlay nil)
(defvar-local org-grid--last-win-width nil)
(defvar-local org-grid--clusters-cache nil)
(defvar-local org-grid--clusters-cache-key nil)
(defvar-local org-grid--links-cache nil)
(defvar-local org-grid--links-cache-key nil)
(defvar-local org-grid--pending-fill nil)
(defvar-local org-grid--fill-timer nil)

(defvar org-grid-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'org-grid-open-at-point)
    (define-key m [mouse-1] #'org-grid-open-at-point)
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

(defun org-grid--header-line ()
  (if org-grid--current-item
      (let ((it org-grid--current-item))
        (format " %s  %s"
                (propertize (org-grid-item-title it) 'face 'org-grid-title-face)
                (propertize (mapconcat (lambda (tg) (concat "#" tg)) (org-grid-item-tags it) " ")
                            'face 'org-grid-tag-face)))
    (format " %d items  sort:%s%s  filter:%s%s%s"
            (length org-grid--items) org-grid--sort-key
            (if org-grid--sort-desc "↓" "↑")
            (if (string-empty-p org-grid--filter) "(none)" org-grid--filter)
            (if org-grid--cluster-p "  [clustered]" "")
            (if org-grid--orphan-p "  [orphans]" ""))))

(defun org-grid--update-point-info ()
  (setq org-grid--current-item (get-text-property (point) 'org-grid-item))
  (when (and (derived-mode-p 'org-grid-mode) org-grid--card-starts)
    (unless (overlayp org-grid--selection-overlay)
      (setq org-grid--selection-overlay (make-overlay (point-min) (point-min)))
      (overlay-put org-grid--selection-overlay 'face '(:box (:line-width 2 :color "#5b6ee1"))))
    (let ((idx (org-grid--card-index-at (point))))
      (if (>= idx 0)
          (let ((start (aref org-grid--card-starts idx)))
            (move-overlay org-grid--selection-overlay start (1+ start)))
        (delete-overlay org-grid--selection-overlay))))
  (force-mode-line-update))

(defun org-grid--cards-per-row ()
  "Calculate how many cards fit on a line, capped at 5 max."
  (if-let* ((win (get-buffer-window (current-buffer)))
            (win-width (window-body-width win t))
            (space-width (frame-char-width))
            (card-width (+ org-grid-thumbnail-size (* space-width 2))))
      (min 5 (max 1 (floor win-width card-width)))
    1))

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

(defun org-grid--sort-value (item key)
  (pcase key
    ;; Zero-padded so lexicographic string< / string> (used by the sorter
    ;; below) sorts chronologically, since ids are no longer timestamps.
    ;; Position is a tiebreaker: headings in the same file share one mtime,
    ;; so a heading further down the file (created/edited later) ranks
    ;; higher than one above it.
    ('date (format "%020d-%010d"
                    (round (* 1000 (org-grid-item-mtime item)))
                    (or (org-grid-item-pos item) 0)))
    ('title (org-grid-item-title item))
    ('tags (or (car (org-grid-item-tags item)) ""))
    ('type (symbol-name (org-grid-item-type item)))))

(defun org-grid--links-cached (items)
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

(defun org-grid--visible-items ()
  (let* ((filtered (cl-remove-if-not (lambda (it) (org-grid--matches-p it org-grid--filter))
                                     org-grid--items))
         (sorted (sort (copy-sequence filtered)
                        (lambda (a b)
                          (let ((va (org-grid--sort-value a org-grid--sort-key))
                                (vb (org-grid--sort-value b org-grid--sort-key)))
                            (if org-grid--sort-desc (string> va vb) (string< va vb))))))
         (links (org-grid--links-cached sorted)))
    (cond
     (org-grid--cluster-p
      (let* ((clusters (org-grid--clusters-cached sorted))
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

(defun org-grid--raster-p (item)
  (memq (org-grid-item-type item) '(image video pdf)))

(defun org-grid--cache-key (item counts)
  (list (org-grid-item-id item) (org-grid-item-mtime item)
        (org-grid-item-type item) org-grid-thumbnail-size
        (face-background 'default nil t)
        (org-grid--color-for (org-grid-item-tags item) counts)))

(defun org-grid--render ()
  "Render grid using hard line breaks like `image-dired'."
  (when (timerp org-grid--fill-timer)
    (cancel-timer org-grid--fill-timer)
    (setq org-grid--fill-timer nil))
  (let ((inhibit-read-only t)
        (pos (point))
        (clusters (and org-grid--cluster-p (org-grid--clusters-cached org-grid--items)))
        (starts nil)
        (pending nil)
        (cols (org-grid--cards-per-row))
        (count 0))
    (setq-local truncate-lines t)
    (erase-buffer)
    (let* ((items (org-grid--visible-items))
           (counts (org-grid--tag-counts items))
           (last-cluster nil))
      (if (null items)
          (insert (propertize "\n  (no items match)\n" 'face 'shadow))
        (dolist (it items)
          (when (and org-grid--cluster-p clusters)
            (let ((c (gethash (org-grid-item-id it) clusters)))
              (unless (eq c last-cluster)
                (unless (null last-cluster) (insert "\n"))
                (insert (propertize (format "·· cluster %d ··\n" c) 'face 'shadow))
                (setq last-cluster c)
                (setq count 0))))
          (let* ((cached (and (hash-table-p org-grid--image-cache)
                              (gethash (org-grid--cache-key it counts) org-grid--image-cache)))
                 (deferred (and (not cached) (org-grid--raster-p it)))
                 (img (or cached
                          (if deferred
                              (org-grid--placeholder-svg
                               it (pcase (org-grid-item-type it)
                                    ('image "IMG") ('video "VIDEO") ('pdf "PDF") (_ "FILE"))
                               counts)
                            (org-grid--get-image it counts))))
                 (start (point)))
            (push start starts)
            (insert-image img (org-grid-item-title it))
            (when deferred
              (push (list start (point) it counts) pending))
            (put-text-property start (point) 'org-grid-item it)
            (put-text-property start (point) 'help-echo
                               (format "%s\n%s"
                                       (org-grid-item-title it)
                                       (mapconcat (lambda (tg) (concat "#" tg)) (org-grid-item-tags it) " ")))
            (setq count (1+ count))
            (if (= (mod count cols) 0)
                (insert "\n")
              (insert " "))))))
    (setq org-grid--card-starts (vconcat (nreverse starts)))
    (setq org-grid--pending-fill (nreverse pending))
    (goto-char (min pos (point-max))))
  (when org-grid--pending-fill
    (org-grid--fill-visible)
    (org-grid--schedule-idle-fill)))

(defun org-grid--apply-thumbnail (card)
  (pcase-let ((`(,start ,end ,item ,counts) card))
    (when (<= end (point-max))
      (let ((inhibit-read-only t)
            (img (org-grid--get-image item counts)))
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
  "Open the file corresponding to the card at point.
For a note, jump straight to its heading."
  (interactive)
  (if-let ((item (get-text-property (point) 'org-grid-item)))
      (progn
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
  "Cycle through sorting keys (date -> title -> tags -> type)."
  (interactive)
  (setq org-grid--sort-key
        (pcase org-grid--sort-key
          ('date 'title)
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
    ;; Stale content/links/clusters must not survive a refresh, since the
    ;; files on disk may have changed.
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

(defun org-grid-down-card (&optional count)
  "Move cursor down by COUNT rows in the grid."
  (interactive "p")
  (let ((cols (org-grid--cards-per-row)))
    (org-grid-next-card (* (or count 1) cols))))

(defun org-grid-up-card (&optional count)
  "Move cursor up by COUNT rows in the grid."
  (interactive "p")
  (org-grid-down-card (- (or count 1))))

(provide 'org-grid)
;;; org-grid.el ends here
