;;; ox-leanpub-book.el --- Export an org-file to LeanPub's multifile setup  -*- lexical-binding: t; -*-

;;; Copyright (C) 2019  Zamboni Diego

;;; Author: Zamboni Diego <diego@zzamboni.org>
;;; Keywords: org, leanpub

;;; Commentary:

;;; (based originally on code by Lakshmi Narasimhan, published at
;;; https://medium.com/@lakshminp/publishing-a-book-using-org-mode-9e817a56d144,
;;; but heavily modified)
;;;
;;; New export backend called leanpub-book, which creates a new export
;;; section under the corresponding markua or makdown section with the
;;; following items:
;;;
;;; - "Book: Whole book", which exports the whole book using default
;;;   settings (see below);
;;;
;;; - "Book: Subset", which exports the subset files and the
;;;   Subset.txt file, according to the rules below.
;;;
;;; - "Book: Current chapter" to explicitly export only the current
;;;   chapter to its own file. This also updates Subset.txt, so it can be
;;;   used to preview the current chapter without having to set
;;;   `#+LEANPUB_WRITE_SUBSET: current'.
;;;
;;; The export splits chapters into files by top-level headline, and
;;; automatically populates the `Book.txt', `Sample.txt' and
;;; `Subset.txt' files used by LeanPub.
;;;
;;; The exported files are written to the `manuscript/' subdirectory
;;; by default, which is what LeanPub expects. This default allows
;;; you to keep the book's main `org' file in the top-level directory
;;; of a repository, and to automatically write the output files to
;;; `manuscript/' so that LeanPub can process them. The output
;;; directory can be changed using the `#+LEANPUB_OUTPUT_DIR' file
;;; property, for example if you want to export to the current
;;; directory, you can use:
;;;
;;;   #+LEANPUB_OUTPUT_DIR: .
;;;
;;; The book files are populated as follows:
;;;
;;; - `Book.txt' with all chapters, except those tagged with `noexport'.
;;; - `Sample.txt' with all chapters tagged with `sample'.
;;; - `Subset.txt' with chapters depending on the value of the
;;;   `#+LEANPUB_WRITE_SUBSET' file property (if set):
;;;   - `none' (default): not created.
;;;   - `tagged': use all chapters tagged `subset'.
;;;   - `all': use the same chapters as `Book.txt'.
;;;   - `sample': use same chapters as `Sample.txt'.
;;;   - `current': export the current chapter (where the cursor is at the
;;;     moment of the export) as the contents of `Subset.txt'.
;;;
;;; If a heading has the `frontmatter', `mainmatter' or `backmatter' tags,
;;; the corresponding markup is inserted in the output, before the
;;; headline. This way, you only need to tag the first chapter of the
;;; front, main, and backmatter, respectively.
;;;
;;; Each section's headline is exported as part of its corresponding
;;; output file.

;;; Code:

(eval-when-compile (require 'cl))
(require 'ob-core)

;;; Variable definitions

(defvar org-leanpub-manuscript-dir "manuscript"
  "Directory where to store exported manuscript files, for
  processing by Leanpub. Do not change this unless you know what
  you are doing, as this is the value expected by Leanpub.")
(defvar org-leanpub-resources-dir "resources"
  "Directory where Leanpub expects to find attachments (images,
  etc.) within `org-leanpub-manuscript-dir'. Do not change this
  unless you know what you are doing, as this is the value
  expected by Leanpub.")
(defvar org-leanpub-images-dir "images"
  "Directory in which you want to store images. This directory
  will be created under `org-leanpub-resources-dir', and
  symlinked from both the main directory (where your org file is
  stored) and from `org-leanpub-resources-dir', so that the
  images can be stored only once in your repository. You can
  change this value but need to be consistent in using it with
  the correct value in your org file.")

(defun org-leanpub-book-setup-menu-markdown ()
  "Set up the Book export menu entries within the Leanpub Markdown menu"
  (interactive)
  (require 'ox-leanpub-markdown)
  (org-export-define-derived-backend 'leanpub-book-markdown 'leanpub-markdown
    :menu-entry
    '(?L 1
         ((?b "Book: Whole book"      (lambda (a s v b) (org-leanpub-export-book #'org-leanpub-markdown-export-to-markdown ".md" 'leanpub-book-markdown a s v b)))
          (?s "Book: Subset"          (lambda (a s v b) (org-leanpub-export-book #'org-leanpub-markdown-export-to-markdown ".md" 'leanpub-book-markdown a s v b t)))
          (?c "Book: Current chapter" (lambda (a s v b) (org-leanpub-export-book #'org-leanpub-markdown-export-to-markdown ".md" 'leanpub-book-markdown a s v b t 'current)))))
    :options-alist
    '((:leanpub-book-output-dir          "LEANPUB_BOOK_OUTPUT_DIR"          nil org-leanpub-manuscript-dir t)
      (:leanpub-book-write-subset        "LEANPUB_BOOK_WRITE_SUBSET"        nil "none"       t)
      (:leanpub-book-recompute-filenames "LEANPUB_BOOK_RECOMPUTE_FILENAMES" nil nil       t))))

(defun org-leanpub-book-setup-menu-markua ()
  "Set up the Multifile export menu entries within the Leanpub Markua menu"
  (interactive)
  (require 'ox-leanpub-markua)
  (org-export-define-derived-backend 'leanpub-book-markua 'leanpub-markua
    :menu-entry
    '(?M 1
         ((?b "Book: Whole book"      (lambda (a s v b) (org-leanpub-export-book #'org-leanpub-markua-export-to-markua ".markua" 'leanpub-book-markua a s v b)))
          (?s "Book: Subset"          (lambda (a s v b) (org-leanpub-export-book #'org-leanpub-markua-export-to-markua ".markua" 'leanpub-book-markua a s v b t)))
          (?c "Book: Current chapter" (lambda (a s v b) (org-leanpub-export-book #'org-leanpub-markua-export-to-markua ".markua" 'leanpub-book-markua a s v b t 'current)))))
    :options-alist
    '((:leanpub-book-output-dir          "LEANPUB_BOOK_OUTPUT_DIR"          nil org-leanpub-manuscript-dir t)
      (:leanpub-book-write-subset        "LEANPUB_BOOK_WRITE_SUBSET"        nil "none"       t)
      (:leanpub-book-recompute-filenames "LEANPUB_BOOK_RECOMPUTE_FILENAMES" nil nil       t))))

;; Main export function
(defun org-leanpub-export-book (export-function export-extension export-backend-symbol &optional async subtreep visible-only body-only only-subset subset-type)
  "Export buffer to a Leanpub book, splitting by top-level headline and populating the corresponding book-specification files."
  (interactive)
  (let* ((info (org-combine-plists
                (org-export--get-export-attributes
                 export-backend-symbol subtreep visible-only)
                (org-export--get-buffer-attributes)
                (org-export-get-environment export-backend-symbol subtreep)))
         (outdir (plist-get info :leanpub-book-output-dir))
         (subset-mode (or subset-type (intern (plist-get info :leanpub-book-write-subset))))
         (ignore-stored-filenames (plist-get info :leanpub-book-recompute-filenames))
         (produce-subset (and subset-mode (not (eq subset-mode 'none))))
         (matter-tags '("frontmatter" "mainmatter" "backmatter"))
         (original-point (point)))

    ;; Internal auxiliary functions

    ;; Return the relative output pathname given the basename of a
    ;; file, using the correct output dir
    (defun outfile (f) (concat (file-name-as-directory outdir) f))

    ;; Main export function: processes an org element, and if it's a
    ;; top-level heading, exports it
    (defun process-chapter ()
      ;; This function gets called for all the elements in the org
      ;; document, but it only processes top-level headings
      (when (and (org-at-heading-p) (= (nth 1 (org-heading-components)) 1))
        (let* ((current-subtree (org-element-at-point))
               ;; Get all the information about the current subtree and heading
               (id (or (org-element-property :name      current-subtree)
                       (org-element-property :ID        current-subtree)
                       (org-element-property :CUSTOM_ID current-subtree)))
               (title (or (nth 4 (org-heading-components)) ""))
               (tags (org-get-tags))
               ;; Compute or get (from EXPORT_FILE_NAME) the output filename
               (basename (concat (replace-regexp-in-string "[^[:alnum:]]" "-" (downcase (or id title)))
                                 export-extension))
               (computed-filename (outfile basename))
               (stored-filename (org-entry-get (point) "EXPORT_FILE_NAME"))
               (final-filename (if ignore-stored-filenames computed-filename (or stored-filename computed-filename)))
               ;; Was the cursor in the current subtree when export started?
               (point-in-subtree (<= (org-element-property :begin current-subtree)
                                     original-point
                                     (1- (org-element-property :end current-subtree))))
               ;; Is this element part of the export subset (if any)?
               (is-subset (or (equal subset-mode 'all)
                              (and (equal subset-mode 'tagged) (member "subset" tags))
                              (and (equal subset-mode 'sample) (member "sample" tags))
                              (and (equal subset-mode 'current) point-in-subtree))))
          ;; Add a line to the correct output files, according to the current settings.
          (defun add-to-bookfiles (line &optional always)
            (let ((line-n (concat line "\n")))
              (unless only-subset
                (append-to-file line-n nil (outfile "Book.txt"))
                (when (or (member "sample" tags) always)
                  (append-to-file line-n nil (outfile "Sample.txt"))))
              (when (and produce-subset (or is-subset always))
                (append-to-file line-n nil (outfile "Subset.txt")))))
          ;; add appropriate tag for front/main/backmatter for tagged headlines
          (dolist (tag matter-tags)
            (when (member tag tags)
              (let* ((fname (concat tag ".txt")))
                (append-to-file (concat "{" tag "}\n") nil (outfile fname))
                (add-to-bookfiles fname t))))
          (when (or (not only-subset) is-subset)
            ;; set filename only if the property is missing or different from the correct filename
            (when (or (not stored-filename)
                      (and ignore-stored-filenames (not (string= stored-filename computed-filename))))
                (org-entry-put (point) "EXPORT_FILE_NAME" computed-filename))
            ;; add to the filename to the book files
            (add-to-bookfiles (file-name-nondirectory final-filename))
            ;; select the subtree so that its headline is also exported
            ;; (otherwise we get just the body)
            (org-mark-subtree)
            (message (format "Exporting %s (%s)" final-filename title))
            (funcall export-function nil t)))))

    ;; Create necessary directories and symlinks, if needed
    (let* ((img-dir-rel-to-outdir (concat (file-name-as-directory org-leanpub-resources-dir)
                                          org-leanpub-images-dir))
           (img-dir-rel-to-repo (concat (file-name-as-directory outdir)
                                        img-dir-rel-to-outdir))
           (img-symlink-in-outdir (concat (file-name-as-directory outdir) org-leanpub-images-dir)))
      (make-directory img-dir-rel-to-repo t)

      (make-symbolic-link img-dir-rel-to-repo org-leanpub-images-dir t)
      (make-symbolic-link img-dir-rel-to-outdir img-symlink-in-outdir t))

    ;; Initialization: delete all the book definition and *matter.txt
    ;; files, they get recreated as needed
    (dolist (fname (mapcar (lambda (s) (concat s ".txt"))
                           (append (if only-subset '("Subset") '("Book" "Sample" "Subset"))
                                   matter-tags)))
      (delete-file (outfile fname)))

    ;; Loop through all the elements in the document, exporting them
    ;; as needed, except for those tagged with "noexport"
    (save-mark-and-excursion
      (org-map-entries #'process-chapter "-noexport"))

    (message (format "LeanPub export to %s/ finished" outdir))))

(provide 'ox-leanpub-book)

;;; ox-leanpub-book.el ends here
