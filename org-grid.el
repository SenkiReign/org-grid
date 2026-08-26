;;; denote-grid.el --- Grid view for denote -*- lexical-binding: t; -*-

;; Author:  Senki R.
;; Keywords: denote, notes, multimedia, moodboard, emacs, org-mode
;; Package-Requires: ((emacs "27.1") (denote "1.0"))
;; Version: 0.2.3

;;; Code:

(require 'cl-lib)
(require 'svg)
(require 'dired)
(require 'color)
(require 'denote)

(defgroup denote-grid nil
  "An are.na-style local grid for denote notes, images, pdfs, and videos."
  :group 'convenience
  :prefix "denote-grid-")

(defcustom denote-grid-thumbnail-size 220
  "Max width/height in pixels for grid thumbnails."
  :type 'integer
  :group 'denote-grid)

(defcustom denote-grid-note-snippet-length 220
  "How many characters of a note's body to show on its card."
  :type 'integer
  :group 'denote-grid)

(defcustom denote-grid-thumbnail-oversample 1
  "Resolution multiplier for generated video/pdf thumbnail files."
  :type 'integer
  :group 'denote-grid)

(defcustom denote-grid-lazy-batch-size 6
  "How many offscreen thumbnails to generate per idle tick."
  :type 'integer
  :group 'denote-grid)

(defcustom denote-grid-ripgrep-executable "rg"
  "Path to ripgrep executable used for fast full-text search."
  :type '(choice (const :tag "Disable ripgrep" nil) string)
  :group 'denote-grid)

(defcustom denote-grid-ffmpeg-executable "ffmpeg"
  "ffmpeg executable used for video thumbnails. Optional."
  :type 'string
  :group 'denote-grid)

(defcustom denote-grid-pdftoppm-executable "pdftoppm"
  "pdftoppm executable used for PDF thumbnails. Optional."
  :type 'string
  :group 'denote-grid)

(defcustom denote-grid-image-extensions '("jpg" "jpeg" "png" "gif" "webp" "bmp" "svg")
  "File extensions treated as images."
  :type '(repeat string) :group 'denote-grid)

(defcustom denote-grid-video-extensions '("mp4" "webm" "mov" "mkv")
  "File extensions treated as videos."
  :type '(repeat string) :group 'denote-grid)

(defcustom denote-grid-pdf-extensions '("pdf")
  "File extensions treated as PDFs."
  :type '(repeat string) :group 'denote-grid)

(defcustom denote-grid-text-extensions '("md" "org" "txt")
  "File extensions treated as note text."
  :type '(repeat string) :group 'denote-grid)

(defface denote-grid-title-face
  '((t :inherit bold))
  "Face for the current card's title in the header line.")

(defface denote-grid-tag-face
  '((t :inherit shadow))
  "Face for tags in the header line.")

(defvar denote-grid--cache-dir nil)

(defun denote-grid--cache-dir-for (root)
  (let ((dir (expand-file-name ".denote-grid-thumbs/" root)))
    (unless (file-directory-p dir) (make-directory dir t))
    dir))

(defconst denote-grid--name-re
  "\\`\\([0-9]\\{8\\}T[0-9]\\{6\\}\\)--\\([^_]+\\)\\(?:__\\(.+\\)\\)?\\'")

(defconst denote-grid--id-re "[0-9]\\{8\\}T[0-9]\\{6\\}")

(cl-defstruct denote-grid-item
  id title tags path type mtime snippet-fetched snippet)

(defun denote-grid--file-type (ext)
  (cond
   ((member ext denote-grid-image-extensions) 'image)
   ((member ext denote-grid-video-extensions) 'video)
   ((member ext denote-grid-pdf-extensions) 'pdf)
   ((member ext denote-grid-text-extensions) 'text)
   (t 'other)))

(defun denote-grid--snippet (path)
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents path nil 0 4000)
        (goto-char (point-min))
        (let (lines)
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))))
              (unless (string-match-p "\\`\\(#\\+\\|---\\)" line)
                (push line lines)))
            (forward-line 1))
          (let ((body (string-trim (mapconcat #'identity (nreverse lines) "\n"))))
            (substring body 0 (min (length body) denote-grid-note-snippet-length)))))
    (error "")))

(defun denote-grid--get-snippet (item)
  (unless (denote-grid-item-snippet-fetched item)
    (setf (denote-grid-item-snippet item)
          (if (eq (denote-grid-item-type item) 'text)
              (denote-grid--snippet (denote-grid-item-path item))
            ""))
    (setf (denote-grid-item-snippet-fetched item) t))
  (denote-grid-item-snippet item))

(defun denote-grid--parse-file (path)
  (let* ((name (file-name-nondirectory path))
         (ext (downcase (or (file-name-extension name) "")))
         (stem (file-name-sans-extension name)))
    (when (string-match denote-grid--name-re stem)
      (let* ((id (match-string 1 stem))
             (title (replace-regexp-in-string "-" " " (match-string 2 stem)))
             (tags (and (match-string 3 stem) (split-string (match-string 3 stem) "_" t)))
             (type (denote-grid--file-type ext))
             (mtime (float-time (file-attribute-modification-time (file-attributes path)))))
        (make-denote-grid-item
         :id id :title title :tags tags :path path :type type :mtime mtime
         :snippet-fetched nil :snippet "")))))

(defun denote-grid--collect-items (root)
  "Collect all denote items under ROOT.
ROOT can be a directory path string or a list of directory path strings."
  (let ((dirs (if (listp root) root (list root)))
        items)
    (dolist (dir dirs)
      (when (and dir (file-directory-p dir))
        (dolist (f (directory-files-recursively
                    dir ".*" nil
                    (lambda (d) (not (string-prefix-p "." (file-name-nondirectory d))))))
          (unless (string-prefix-p "." (file-name-nondirectory f))
            (when-let ((item (denote-grid--parse-file f)))
              (push item items))))))
    (nreverse items)))

(defun denote-grid--dired-visible-files ()
  (unless (derived-mode-p 'dired-mode)
    (user-error "denote-grid: not in a dired buffer"))
  (let (files)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((f (dired-get-filename nil t)))
          (when (and f (not (file-directory-p f)))
            (push f files)))
        (forward-line 1)))
    (nreverse files)))

(defun denote-grid--collect-items-from-dired ()
  (delq nil (mapcar #'denote-grid--parse-file (denote-grid--dired-visible-files))))

(defun denote-grid--file-contains-p (path query)
  (let ((rg (and denote-grid-ripgrep-executable
                 (executable-find denote-grid-ripgrep-executable))))
    (if rg
        (zerop (call-process rg nil nil nil "-q" "-i" "-F" query path))
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents path)
            (goto-char (point-min))
            (let ((case-fold-search t))
              (search-forward query nil t)))
        (error nil)))))

(defun denote-grid--links (items)
  (let ((ids (make-hash-table :test 'equal))
        (links (make-hash-table :test 'equal)))
    (dolist (it items) (puthash (denote-grid-item-id it) t ids))
    (dolist (it items)
      (when (eq (denote-grid-item-type it) 'text)
        (condition-case nil
            (with-temp-buffer
              (insert-file-contents (denote-grid-item-path it))
              (goto-char (point-min))
              (let (found)
                (while (re-search-forward denote-grid--id-re nil t)
                  (let ((found-id (match-string 0)))
                    (unless (or (equal found-id (denote-grid-item-id it))
                                (member found-id found)
                                (not (gethash found-id ids)))
                      (push found-id found)
                      (cl-pushnew found-id (gethash (denote-grid-item-id it) links))
                      (cl-pushnew (denote-grid-item-id it) (gethash found-id links)))))))
          (error nil))))
    links))

(defun denote-grid--clusters (items)
  (let ((links (denote-grid--links items))
        (visited (make-hash-table :test 'equal))
        (components nil))
    (dolist (it items)
      (let ((id (denote-grid-item-id it)))
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

(defun denote-grid--tag-counts (items)
  (let ((counts (make-hash-table :test 'equal)))
    (dolist (it items)
      (dolist (tag (denote-grid-item-tags it))
        (puthash tag (1+ (gethash tag counts 0)) counts)))
    counts))

(defun denote-grid--color-for (tags counts)
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

(defun denote-grid--wrap-text (str width)
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

(defun denote-grid--note-svg (item counts)
  (let* ((w denote-grid-thumbnail-size) (h (round (* w 0.72)))
         (bg (face-background 'default nil t))
         (fg (face-foreground 'default nil t))
         (muted (face-foreground 'shadow nil t))
         (color (denote-grid--color-for (denote-grid-item-tags item) counts))
         (svg (svg-create w h :xmlns:xlink "http://www.w3.org/1999/xlink")))
    (svg-rectangle svg 0 0 w h :fill bg :rx 10)
    (when color (svg-rectangle svg 0 0 6 h :fill color :rx 3))
    (svg-text svg (truncate-string-to-width (denote-grid-item-title item) 24 nil nil "…")
              :x 16 :y 26 :fill fg :font-size 15 :font-weight "bold" :font-family "sans-serif")
    (let ((y 48))
      (dolist (line (denote-grid--wrap-text (denote-grid--get-snippet item) 32))
        (when (< y (- h 22))
          (svg-text svg line :x 16 :y y :fill muted :font-size 11 :font-family "sans-serif")
          (setq y (+ y 15)))))
    (when color
      (let ((tagstr (mapconcat (lambda (tg) (concat "#" tg)) (denote-grid-item-tags item) "  ")))
        (svg-text svg (truncate-string-to-width tagstr 36 nil nil "…")
                  :x 16 :y (- h 12) :fill color :font-size 10 :font-family "sans-serif")))
    (svg-image svg :ascent 'center)))

(defun denote-grid--placeholder-svg (item label counts)
  (let* ((w denote-grid-thumbnail-size) (h (round (* w 0.72)))
         (bg (face-background 'default nil t))
         (fg (face-foreground 'default nil t))
         (color (denote-grid--color-for (denote-grid-item-tags item) counts))
         (accent (or color (face-foreground 'shadow nil t)))
         (svg (svg-create w h :xmlns:xlink "http://www.w3.org/1999/xlink")))
    (svg-rectangle svg 0 0 w h :fill bg :rx 10)
    (svg-rectangle svg 0 0 w h :fill accent :fill-opacity "0.12" :rx 10)
    (when color (svg-rectangle svg 0 0 6 h :fill color :rx 3))
    (svg-text svg label :x (/ w 2) :y (/ h 2) :fill accent :font-size 22
              :font-weight "bold" :font-family "sans-serif" :text-anchor "middle")
    (svg-text svg (truncate-string-to-width (denote-grid-item-title item) 26 nil nil "…")
              :x 14 :y (- h 14) :fill fg :font-size 11 :font-family "sans-serif")
    (svg-image svg :ascent 'center)))

(defun denote-grid--cache-file (item ext)
  (expand-file-name (format "%s-%d.%s" (denote-grid-item-id item)
                            (round (denote-grid-item-mtime item)) ext)
                    denote-grid--cache-dir))

(defun denote-grid--mime-for (file)
  (pcase (downcase (or (file-name-extension file) ""))
    ((or "jpg" "jpeg") "image/jpeg")
    ("png" "image/png")
    ("gif" "image/gif")
    ("webp" "image/webp")
    ("svg" "image/svg+xml")
    ("bmp" "image/bmp")
    (_ "image/png")))

(defun denote-grid--boxed-raster (item raw-file label counts)
  (if (null raw-file)
      (denote-grid--placeholder-svg item label counts)
    (let* ((w denote-grid-thumbnail-size) (h (round (* w 0.72)))
           (dim (ignore-errors (image-size (create-image raw-file nil nil) t))))
      (if (not dim)
          (denote-grid--placeholder-svg item label counts)
        (let* ((iw (car dim)) (ih (cdr dim))
               (pad 6)
               (scale (min (/ (float (- w (* 2 pad))) iw) (/ (float (- h (* 2 pad))) ih)))
               (dw (max 1 (round (* iw scale))))
               (dh (max 1 (round (* ih scale))))
               (x (round (/ (- w dw) 2.0)))
               (y (round (/ (- h dh) 2.0)))
               (color (denote-grid--color-for (denote-grid-item-tags item) counts))
               (svg (svg-create w h :xmlns:xlink "http://www.w3.org/1999/xlink"))
               (bg (face-background 'default nil t)))
          (svg-rectangle svg 0 0 w h :fill bg :rx 10)
          (condition-case nil
              (progn
                (svg-embed svg raw-file (denote-grid--mime-for raw-file) nil
                           :x x :y y :width dw :height dh)
                (when color (svg-rectangle svg 0 0 6 h :fill color :rx 3))
                (svg-image svg :ascent 'center))
            (error (denote-grid--placeholder-svg item label counts))))))))

(defvar-local denote-grid--image-cache nil)

(defun denote-grid--video-thumb (item counts)
  (let ((out nil))
    (when (executable-find denote-grid-ffmpeg-executable)
      (setq out (denote-grid--cache-file item "jpg"))
      (unless (file-exists-p out)
        (call-process denote-grid-ffmpeg-executable nil nil nil
                      "-y" "-ss" "1" "-i" (denote-grid-item-path item)
                      "-frames:v" "1"
                      "-vf" (format "scale=%d:-1" (* denote-grid-thumbnail-oversample
                                                    denote-grid-thumbnail-size))
                      "-loglevel" "quiet" out))
      (unless (file-exists-p out) (setq out nil)))
    (denote-grid--boxed-raster item out "VIDEO" counts)))

(defun denote-grid--pdf-thumb (item counts)
  (let ((out nil))
    (when (executable-find denote-grid-pdftoppm-executable)
      (let* ((base (denote-grid--cache-file item "pdfpage"))
             (png (concat base ".png")))
        (unless (file-exists-p png)
          (call-process denote-grid-pdftoppm-executable nil nil nil
                        "-png" "-f" "1" "-singlefile"
                        "-scale-to" (number-to-string (* denote-grid-thumbnail-oversample
                                                         denote-grid-thumbnail-size))
                        (denote-grid-item-path item) base))
        (when (file-exists-p png) (setq out png))))
    (denote-grid--boxed-raster item out "PDF" counts)))

(defun denote-grid--image-thumb (item counts)
  (denote-grid--boxed-raster item (denote-grid-item-path item) "IMG" counts))

(defun denote-grid--get-image (item counts)
  (unless denote-grid--image-cache
    (setq denote-grid--image-cache (make-hash-table :test 'equal)))
  (let* ((key (denote-grid--cache-key item counts))
         (cached (gethash key denote-grid--image-cache)))
    (or cached
        (puthash key
                 (pcase (denote-grid-item-type item)
                   ('image (denote-grid--image-thumb item counts))
                   ('video (denote-grid--video-thumb item counts))
                   ('pdf (denote-grid--pdf-thumb item counts))
                   ('text (denote-grid--note-svg item counts))
                   (_ (let ((ext-label (upcase (or (file-name-extension (denote-grid-item-path item)) "FILE"))))
                        (denote-grid--placeholder-svg item ext-label counts))))
                 denote-grid--image-cache))))

(defun denote-grid--prune-image-cache (items)
  (when (hash-table-p denote-grid--image-cache)
    (let ((live-ids (make-hash-table :test 'equal)))
      (dolist (it items) (puthash (denote-grid-item-id it) t live-ids))
      (let (stale)
        (maphash (lambda (key _val)
                   (unless (gethash (car key) live-ids)
                     (push key stale)))
                 denote-grid--image-cache)
        (dolist (key stale) (remhash key denote-grid--image-cache))))))

(defvar-local denote-grid--items nil)
(defvar-local denote-grid--filter "")
(defvar-local denote-grid--sort-key 'date)
(defvar-local denote-grid--sort-desc t)
(defvar-local denote-grid--cluster-p nil)
(defvar-local denote-grid--orphan-p nil)
(defvar-local denote-grid--current-item nil)
(defvar-local denote-grid--card-starts nil)
(defvar-local denote-grid--source-directory nil)
(defvar-local denote-grid--source-dired-buffer nil)
(defvar-local denote-grid--selection-overlay nil)
(defvar-local denote-grid--last-win-width nil)
(defvar-local denote-grid--clusters-cache nil)
(defvar-local denote-grid--clusters-cache-key nil)
(defvar-local denote-grid--pending-fill nil)
(defvar-local denote-grid--fill-timer nil)

(defvar denote-grid-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'denote-grid-open-at-point)
    (define-key m [mouse-1] #'denote-grid-open-at-point)
    (define-key m (kbd "d") #'denote-grid-jump-to-dired)
    (define-key m (kbd "/") #'denote-grid-filter)
    (define-key m (kbd "s") #'denote-grid-sort-cycle)
    (define-key m (kbd "r") #'denote-grid-sort-reverse)
    (define-key m (kbd "c") #'denote-grid-toggle-cluster)
    (define-key m (kbd "o") #'denote-grid-toggle-orphan)
    (define-key m (kbd "g") #'denote-grid-refresh)
    (define-key m (kbd "q") #'quit-window)
    (define-key m (kbd "<right>") #'denote-grid-next-card)
    (define-key m (kbd "TAB") #'denote-grid-next-card)
    (define-key m (kbd "n") #'denote-grid-next-card)
    (define-key m (kbd "<left>") #'denote-grid-prev-card)
    (define-key m (kbd "<backtab>") #'denote-grid-prev-card)
    (define-key m (kbd "p") #'denote-grid-prev-card)
    (define-key m (kbd "<down>") #'denote-grid-down-card)
    (define-key m (kbd "<up>") #'denote-grid-up-card)
    m))

(define-derived-mode denote-grid-mode special-mode "Denote-Grid"
  "Major mode for browsing denote files as an image-dired style grid."
  (setq truncate-lines t)
  (setq header-line-format '(:eval (denote-grid--header-line)))
  (add-hook 'post-command-hook #'denote-grid--update-point-info nil t)
  (add-hook 'window-size-change-functions #'denote-grid--on-window-size-change nil t)
  (add-hook 'text-scale-mode-hook #'denote-grid--render nil t)
  (add-hook 'kill-buffer-hook #'denote-grid--cleanup nil t))

(defun denote-grid--cleanup ()
  (when (timerp denote-grid--fill-timer)
    (cancel-timer denote-grid--fill-timer))
  (when (hash-table-p denote-grid--image-cache)
    (clrhash denote-grid--image-cache))
  (setq denote-grid--image-cache nil
        denote-grid--clusters-cache nil
        denote-grid--pending-fill nil
        denote-grid--fill-timer nil))

(defun denote-grid--header-line ()
  (if denote-grid--current-item
      (let ((it denote-grid--current-item))
        (format " %s  %s  %s"
                (propertize (denote-grid-item-title it) 'face 'denote-grid-title-face)
                (propertize (substring (denote-grid-item-id it) 0 8) 'face 'shadow)
                (propertize (mapconcat (lambda (tg) (concat "#" tg)) (denote-grid-item-tags it) " ")
                            'face 'denote-grid-tag-face)))
    (format " %d items  sort:%s%s  filter:%s%s%s"
            (length denote-grid--items) denote-grid--sort-key
            (if denote-grid--sort-desc "↓" "↑")
            (if (string-empty-p denote-grid--filter) "(none)" denote-grid--filter)
            (if denote-grid--cluster-p "  [clustered]" "")
            (if denote-grid--orphan-p "  [orphans]" ""))))

(defun denote-grid--update-point-info ()
  (setq denote-grid--current-item (get-text-property (point) 'denote-grid-item))
  (when (and (derived-mode-p 'denote-grid-mode) denote-grid--card-starts)
    (unless (overlayp denote-grid--selection-overlay)
      (setq denote-grid--selection-overlay (make-overlay (point-min) (point-min)))
      (overlay-put denote-grid--selection-overlay 'face '(:box (:line-width 2 :color "#5b6ee1"))))
    (let ((idx (denote-grid--card-index-at (point))))
      (if (>= idx 0)
          (let ((start (aref denote-grid--card-starts idx)))
            (move-overlay denote-grid--selection-overlay start (1+ start)))
        (delete-overlay denote-grid--selection-overlay))))
  (force-mode-line-update))

(defun denote-grid--cards-per-row ()
  "Calculate how many cards fit on a line, capped at 5 max."
  (if-let* ((win (get-buffer-window (current-buffer)))
            (win-width (window-body-width win t))
            (space-width (frame-char-width))
            (card-width (+ denote-grid-thumbnail-size (* space-width 2))))
      (min 5 (max 1 (floor win-width card-width)))
    1))

(defun denote-grid--on-window-size-change (win)
  (when (and (eq (window-buffer win) (current-buffer))
             (derived-mode-p 'denote-grid-mode))
    (let ((w (window-pixel-width win)))
      (unless (equal w denote-grid--last-win-width)
        (setq denote-grid--last-win-width w)
        (denote-grid--render)))))

(defun denote-grid--matches-p (item filter)
  (if (string-empty-p filter)
      t
    (if (string-prefix-p "#" filter)
        (let ((wanted (split-string (downcase (substring filter 1)) "[, ]+" t))
              (tags (mapcar #'downcase (denote-grid-item-tags item))))
          (and wanted (cl-every (lambda (tg) (member tg tags)) wanted)))
      (let ((hay (downcase (concat (denote-grid-item-title item) " "
                                   (mapconcat #'identity (denote-grid-item-tags item) " ")))))
        (or (string-match-p (regexp-quote (downcase filter)) hay)
            (and (eq (denote-grid-item-type item) 'text)
                 (denote-grid--file-contains-p (denote-grid-item-path item) filter)))))))

(defun denote-grid--sort-value (item key)
  (pcase key
    ('date (denote-grid-item-id item))
    ('title (denote-grid-item-title item))
    ('tags (or (car (denote-grid-item-tags item)) ""))
    ('type (symbol-name (denote-grid-item-type item)))))

(defun denote-grid--clusters-cached (items)
  (let ((key (mapcar (lambda (it) (denote-grid-item-id it)) items)))
    (unless (equal key denote-grid--clusters-cache-key)
      (setq denote-grid--clusters-cache (denote-grid--clusters items)
            denote-grid--clusters-cache-key key))
    denote-grid--clusters-cache))

(defun denote-grid--visible-items ()
  (let* ((filtered (cl-remove-if-not (lambda (it) (denote-grid--matches-p it denote-grid--filter))
                                     denote-grid--items))
         (sorted (sort (copy-sequence filtered)
                        (lambda (a b)
                          (let ((va (denote-grid--sort-value a denote-grid--sort-key))
                                (vb (denote-grid--sort-value b denote-grid--sort-key)))
                            (if denote-grid--sort-desc (string> va vb) (string< va vb))))))
         (links (denote-grid--links sorted)))
    (cond
     (denote-grid--cluster-p
      (let* ((clusters (denote-grid--clusters-cached sorted))
             (connected (cl-remove-if-not
                         (lambda (it) (> (length (gethash (denote-grid-item-id it) links)) 0))
                         sorted)))
        (sort (copy-sequence connected)
              (lambda (a b)
                (let ((ca (gethash (denote-grid-item-id a) clusters))
                      (cb (gethash (denote-grid-item-id b) clusters)))
                  (if (= ca cb)
                      (string> (denote-grid-item-id a) (denote-grid-item-id b))
                    (< ca cb)))))))

     (denote-grid--orphan-p
      (cl-remove-if (lambda (it) (> (length (gethash (denote-grid-item-id it) links)) 0))
                    sorted))

     (t sorted))))

(defun denote-grid--raster-p (item)
  (memq (denote-grid-item-type item) '(image video pdf)))

(defun denote-grid--cache-key (item counts)
  (list (denote-grid-item-id item) (denote-grid-item-mtime item)
        (denote-grid-item-type item) denote-grid-thumbnail-size
        (face-background 'default nil t)
        (denote-grid--color-for (denote-grid-item-tags item) counts)))

(defun denote-grid--render ()
  "Render grid using hard line breaks like `image-dired'."
  (when (timerp denote-grid--fill-timer)
    (cancel-timer denote-grid--fill-timer)
    (setq denote-grid--fill-timer nil))
  (let ((inhibit-read-only t)
        (pos (point))
        (clusters (and denote-grid--cluster-p (denote-grid--clusters-cached denote-grid--items)))
        (starts nil)
        (pending nil)
        (cols (denote-grid--cards-per-row))
        (count 0))
    (setq-local truncate-lines t)
    (erase-buffer)
    (let* ((items (denote-grid--visible-items))
           (counts (denote-grid--tag-counts items))
           (last-cluster nil))
      (if (null items)
          (insert (propertize "\n  (no items match)\n" 'face 'shadow))
        (dolist (it items)
          (when (and denote-grid--cluster-p clusters)
            (let ((c (gethash (denote-grid-item-id it) clusters)))
              (unless (eq c last-cluster)
                (unless (null last-cluster) (insert "\n"))
                (insert (propertize (format "·· cluster %d ··\n" c) 'face 'shadow))
                (setq last-cluster c)
                (setq count 0))))
          (let* ((cached (and (hash-table-p denote-grid--image-cache)
                              (gethash (denote-grid--cache-key it counts) denote-grid--image-cache)))
                 (deferred (and (not cached) (denote-grid--raster-p it)))
                 (img (or cached
                          (if deferred
                              (denote-grid--placeholder-svg
                               it (pcase (denote-grid-item-type it)
                                    ('image "IMG") ('video "VIDEO") ('pdf "PDF") (_ "FILE"))
                               counts)
                            (denote-grid--get-image it counts))))
                 (start (point)))
            (push start starts)
            (insert-image img (denote-grid-item-title it))
            (when deferred
              (push (list start (point) it counts) pending))
            (put-text-property start (point) 'denote-grid-item it)
            (put-text-property start (point) 'help-echo
                               (format "%s\n%s\n%s"
                                       (denote-grid-item-title it)
                                       (denote-grid-item-id it)
                                       (mapconcat (lambda (tg) (concat "#" tg)) (denote-grid-item-tags it) " ")))
            (setq count (1+ count))
            (if (= (mod count cols) 0)
                (insert "\n")
              (insert " "))))))
    (setq denote-grid--card-starts (vconcat (nreverse starts)))
    (setq denote-grid--pending-fill (nreverse pending))
    (goto-char (min pos (point-max))))
  (when denote-grid--pending-fill
    (denote-grid--fill-visible)
    (denote-grid--schedule-idle-fill)))

(defun denote-grid--apply-thumbnail (card)
  (pcase-let ((`(,start ,end ,item ,counts) card))
    (when (<= end (point-max))
      (let ((inhibit-read-only t)
            (img (denote-grid--get-image item counts)))
        (put-text-property start end 'display img)))))

(defun denote-grid--fill-visible ()
  (when denote-grid--pending-fill
    (let (still-pending)
      (dolist (card denote-grid--pending-fill)
        (let ((start (car card)) (visible nil))
          (dolist (win (get-buffer-window-list (current-buffer) nil t))
            (when (and (>= start (window-start win)) (<= start (window-end win)))
              (setq visible t)))
          (if visible
              (denote-grid--apply-thumbnail card)
            (push card still-pending))))
      (setq denote-grid--pending-fill (nreverse still-pending)))))

(defun denote-grid--schedule-idle-fill ()
  (when (timerp denote-grid--fill-timer)
    (cancel-timer denote-grid--fill-timer))
  (when denote-grid--pending-fill
    (setq denote-grid--fill-timer
          (run-with-idle-timer 0.3 t #'denote-grid--idle-fill-tick (current-buffer)))))

(defun denote-grid--idle-fill-tick (buf)
  (if (not (buffer-live-p buf))
      (when (timerp denote-grid--fill-timer) (cancel-timer denote-grid--fill-timer))
    (with-current-buffer buf
      (denote-grid--fill-visible)
      (let ((n 0))
        (while (and denote-grid--pending-fill (< n denote-grid-lazy-batch-size))
          (denote-grid--apply-thumbnail (pop denote-grid--pending-fill))
          (setq n (1+ n))))
      (unless denote-grid--pending-fill
        (when (timerp denote-grid--fill-timer)
          (cancel-timer denote-grid--fill-timer)
          (setq denote-grid--fill-timer nil))))))

(defun denote-grid--card-index-at (pos)
  (let ((vec denote-grid--card-starts)
        (low 0)
        (high (1- (length denote-grid--card-starts)))
        (ans 0))
    (while (<= low high)
      (let ((mid (/ (+ low high) 2)))
        (if (<= (aref vec mid) pos)
            (progn
              (setq ans mid)
              (setq low (1+ mid)))
          (setq high (1- mid)))))
    ans))

(defun denote-grid--snap-to-card (&optional direction)
  "Snap point to the nearest valid card starting position.
DIRECTION non-nil means move forward, nil means move backward."
  (let ((dir (or direction 1)))
    (while (and (not (get-text-property (point) 'denote-grid-item))
                (not (if (> dir 0) (eobp) (bobp))))
      (forward-line dir))
    (when-let ((item (get-text-property (point) 'denote-grid-item)))
      (let ((idx (denote-grid--card-index-at (point))))
        (when (>= idx 0)
          (goto-char (aref denote-grid--card-starts idx)))))))

;;; Interactive Commands

;;;###autoload
(defun denote-grid-open (&optional dir)
  "Open the denote grid view for DIR.
If DIR is nil, automatically fallback to `denote-directory` (or its first
entry if `denote-directory` is a list)."
  (interactive
   (list (when current-prefix-arg
           (let ((default-dir (if (listp denote-directory)
                                  (car denote-directory)
                                denote-directory)))
             (read-directory-name "Denote directory: " default-dir)))))
  (let* ((target (or dir denote-directory))
         (primary-dir (expand-file-name (if (listp target) (car target) target)))
         (buf-name (format "*denote-grid: %s*" (file-name-nondirectory (directory-file-name primary-dir))))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (denote-grid-mode)
      (setq-local denote-grid--source-directory target)
      (setq-local denote-grid--cache-dir (denote-grid--cache-dir-for primary-dir))
      (setq-local denote-grid--items (denote-grid--collect-items target))
      (denote-grid--render))
    (switch-to-buffer buf)))

;;;###autoload
(defun denote-grid-from-dired ()
  "Open denote grid view displaying files from the current Dired buffer."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (user-error "denote-grid: not in a Dired buffer"))
  (let* ((dired-buf (current-buffer))
         (dir (expand-file-name default-directory))
         (buf-name (format "*denote-grid dired: %s*" (buffer-name dired-buf)))
         (buf (get-buffer-create buf-name))
         (items (denote-grid--collect-items-from-dired)))
    (unless items
      (user-error "denote-grid: no valid denote files found in this Dired buffer"))
    (with-current-buffer buf
      (denote-grid-mode)
      (setq-local denote-grid--source-directory dir)
      (setq-local denote-grid--source-dired-buffer dired-buf)
      (setq-local denote-grid--cache-dir (denote-grid--cache-dir-for dir))
      (setq-local denote-grid--items items)
      (denote-grid--render))
    (switch-to-buffer buf)))

(defun denote-grid-jump-to-dired ()
  "Jump to the file under point in a Dired buffer."
  (interactive)
  (if-let* ((it (get-text-property (point) 'denote-grid-item))
            (file (expand-file-name (denote-grid-item-path it))))
      (let ((dired-buf denote-grid--source-dired-buffer))
        (if (and dired-buf (buffer-live-p dired-buf))
            (progn
              (pop-to-buffer dired-buf)
              (dired-goto-file file))
          (dired (file-name-directory file))
          (dired-goto-file file)))
    (user-error "No denote item at point")))

(defun denote-grid-open-at-point ()
  "Open the file corresponding to the card at point."
  (interactive)
  (if-let ((item (get-text-property (point) 'denote-grid-item)))
      (find-file (denote-grid-item-path item))
    (user-error "No item at point")))

(defun denote-grid-toggle-orphan ()
  "Toggle orphan view mode in denote-grid."
  (interactive)
  (setq denote-grid--orphan-p (not denote-grid--orphan-p))
  (when denote-grid--orphan-p
    (setq denote-grid--cluster-p nil))
  (denote-grid--render))

(defun denote-grid-toggle-cluster ()
  "Toggle cluster view mode in denote-grid."
  (interactive)
  (setq denote-grid--cluster-p (not denote-grid--cluster-p))
  (when denote-grid--cluster-p
    (setq denote-grid--orphan-p nil))
  (denote-grid--render))

(defun denote-grid-filter (query)
  "Filter items in the grid buffer by QUERY."
  (interactive (list (read-string "Filter grid (text or #tag): " denote-grid--filter)))
  (setq denote-grid--filter query)
  (denote-grid--render))

(defun denote-grid-sort-cycle ()
  "Cycle through sorting keys (date -> title -> tags -> type)."
  (interactive)
  (setq denote-grid--sort-key
        (pcase denote-grid--sort-key
          ('date 'title)
          ('title 'tags)
          ('tags 'type)
          (_ 'date)))
  (denote-grid--render))

(defun denote-grid-sort-reverse ()
  "Toggle ascending/descending order for current sort."
  (interactive)
  (setq denote-grid--sort-desc (not denote-grid--sort-desc))
  (denote-grid--render))

(defun denote-grid-refresh ()
  "Refresh the grid buffer."
  (interactive)
  (when denote-grid--source-directory
    (setq denote-grid--items
          (if (and denote-grid--source-dired-buffer
                   (buffer-live-p denote-grid--source-dired-buffer))
              (with-current-buffer denote-grid--source-dired-buffer
                (denote-grid--collect-items-from-dired))
            (denote-grid--collect-items denote-grid--source-directory)))
    (denote-grid--prune-image-cache denote-grid--items)
    (denote-grid--render)))

(defun denote-grid-next-card (&optional count)
  "Move cursor forward by COUNT cards."
  (interactive "p")
  (let* ((cnt (or count 1))
         (idx (denote-grid--card-index-at (point)))
         (max-idx (1- (length denote-grid--card-starts)))
         (target (min max-idx (+ idx cnt))))
    (when (and (>= target 0) denote-grid--card-starts)
      (goto-char (aref denote-grid--card-starts target)))))

(defun denote-grid-prev-card (&optional count)
  "Move cursor backward by COUNT cards."
  (interactive "p")
  (denote-grid-next-card (- (or count 1))))

(defun denote-grid-down-card (&optional count)
  "Move cursor down by COUNT rows in the grid."
  (interactive "p")
  (let ((cols (denote-grid--cards-per-row)))
    (denote-grid-next-card (* (or count 1) cols))))

(defun denote-grid-up-card (&optional count)
  "Move cursor up by COUNT rows in the grid."
  (interactive "p")
  (denote-grid-down-card (- (or count 1))))

(provide 'denote-grid)
;;; denote-grid.el ends here
