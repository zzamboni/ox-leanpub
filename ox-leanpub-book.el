;;; ox-leanpub-book.el --- Export an org-file to LeanPub's multifile setup  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Diego Zamboni

;;; Author: Diego Zamboni <diego@zzamboni.org>
;; URL: https://gitlab.com/zzamboni/ox-leanpub
;; Package-Version: 0.1
;; Keywords: files, org, wp, markdown, leanpub
;; Package-Requires: ((emacs "26.1"))

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

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
;;;   chapter to its own file.  This also updates Subset.txt, so it
;;;   can be used to preview the current chapter without having to set
;;;   `#+LEANPUB_WRITE_SUBSET: current'.
;;;
;;; The export splits chapters into files by top-level headline, and
;;; automatically populates the `Book.txt', `Sample.txt' and
;;; `Subset.txt' files used by LeanPub.
;;;
;;; The exported files are written to the `manuscript/' subdirectory
;;; by default, which is what LeanPub expects.  This default allows
;;; you to keep the book's main `org' file in the top-level directory
;;; of a repository, and to automatically write the output files to
;;; `manuscript/' so that LeanPub can process them.  The output
;;; directory can be changed using the `#+LEANPUB_OUTPUT_DIR' file
;;; property, for example if you want to export to the current
;;; directory, you can use:
;;;
;;;   #+LEANPUB_OUTPUT_DIR: .
;;;
;;; The book files are populated as follows:
;;;
;;; - `Book.txt' with all chapters, except those tagged with `noexport'.
;;; - `Sample.txt' with all chapters tagged with `sample', only for
;;;   Markdown exports.  In Markua exports, the conditional attributes
;;;   are used, as documented in https://leanpub.com/markua/read#conditional-inclusion.
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
;;; headline.  This way, you only need to tag the first chapter of the
;;; front, main, and backmatter, respectively.
;;;
;;; Each section's headline is exported as part of its corresponding
;;; output file.

;;; Code:

(require 'cl-lib)
(require 'ob-core)
(require 'ox)

;;; Variable definitions

(defvar org-leanpub-book-manuscript-dir "manuscript"
  "Directory where to store exported manuscript files for processing by Leanpub.
Do not change this unless you know what you are doing, as this is
the value expected by Leanpub.")
(defvar org-leanpub-book-resources-dir "resources"
  "Directory where Leanpub expects to find attachments (images, etc.).
Its value should be relative to `org-leanpub-book-manuscript-dir'.
Do not change this unless you know what you are doing, as this is
the value expected by Leanpub.")
(defvar org-leanpub-book-images-dir "images"
  "Directory in which you want to store images.
This directory will be created under `org-leanpub-book-resources-dir',
and symlinked from both the main directory (where your org file
is stored) and from `org-leanpub-book-resources-dir', so that the
images can be stored only once in your repository.  You can
change this value but need to be consistent in using it with the
correct value in your org file.")

(defun org-leanpub-book-setup-menu-markdown ()
  "Set up the Book export menu entries within the Leanpub Markdown menu."
  (interactive)
  (require 'ox-leanpub-markdown)
  (org-export-define-derived-backend 'leanpub-book-markdown 'leanpub-markdown
    :menu-entry
    '(?L 1
         ((?b "Book: Whole book"      (lambda (a s v b) (org-leanpub-book-export #'org-leanpub-markdown-export-to-markdown ".md" 'leanpub-book-markdown s v t)))
          (?s "Book: Subset"          (lambda (a s v b) (org-leanpub-book-export #'org-leanpub-markdown-export-to-markdown ".md" 'leanpub-book-markdown s v t t)))
          (?c "Book: Current chapter" (lambda (a s v b) (org-leanpub-book-export #'org-leanpub-markdown-export-to-markdown ".md" 'leanpub-book-markdown s v t t 'current)))))
    :options-alist
    '((:leanpub-book-output-dir          "LEANPUB_BOOK_OUTPUT_DIR"          nil org-leanpub-book-manuscript-dir t)
      (:leanpub-book-write-subset        "LEANPUB_BOOK_WRITE_SUBSET"        nil "none"       t)
      (:leanpub-book-recompute-filenames "LEANPUB_BOOK_RECOMPUTE_FILENAMES" nil nil       t))))

(defun org-leanpub-book-setup-menu-markua ()
  "Set up the Multifile export menu entries within the Leanpub Markua menu."
  (interactive)
  (require 'ox-leanpub-markua)
  (org-export-define-derived-backend 'leanpub-book-markua 'leanpub-markua
    :menu-entry
    '(?M 1
         ((?b "Book: Whole book"      (lambda (a s v b) (org-leanpub-book-export #'org-leanpub-markua-export-to-markua ".markua" 'leanpub-book-markua s v)))
          (?s "Book: Subset"          (lambda (a s v b) (org-leanpub-book-export #'org-leanpub-markua-export-to-markua ".markua" 'leanpub-book-markua s v nil t)))
          (?c "Book: Current chapter" (lambda (a s v b) (org-leanpub-book-export #'org-leanpub-markua-export-to-markua ".markua" 'leanpub-book-markua s v nil t 'current)))))
    :options-alist
    '((:leanpub-book-output-dir          "LEANPUB_BOOK_OUTPUT_DIR"          nil org-leanpub-book-manuscript-dir t)
      (:leanpub-book-write-subset        "LEANPUB_BOOK_WRITE_SUBSET"        nil "none"       t)
      (:leanpub-book-recompute-filenames "LEANPUB_BOOK_RECOMPUTE_FILENAMES" nil nil       t))))

;;; Utility functions

(defun org-leanpub-book--outfile (outdir f)
  "Return relative output pathname.
Concatenates `OUTDIR' with `F' using the correct separator, to
return a relative pathname."
  (concat (file-name-as-directory outdir) f))

(defun org-leanpub-book--add-to-bookfiles (line &optional always only-subset do-sample-file tags produce-subset is-subset outdir)
  "Add a `LINE' to the correct output files, according to the current settings."
  (let ((line-n (concat line "\n")))
    (unless only-subset
      (append-to-file line-n nil (org-leanpub-book--outfile outdir "Book.txt"))
      (when (and do-sample-file (or (member "sample" tags) always))
        (append-to-file line-n nil (org-leanpub-book--outfile outdir "Sample.txt"))))
    (when (and produce-subset (or is-subset always))
      (append-to-file line-n nil (org-leanpub-book--outfile outdir "Subset.txt")))))

(defun org-leanpub-book--process-chapter (outdir export-function export-extension ignore-stored-filenames
                                                 original-point subset-mode matter-tags only-subset
                                                 do-sample-file produce-subset)
  "Main Book chapter export function.
Processes an org element, and exports it if it's a top level
heading.  This function gets called for all the elements in the
org document, but it only processes top level headings"
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
           (computed-filename (org-leanpub-book--outfile outdir basename))
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
      ;; add appropriate tag for front/main/backmatter for tagged headlines
      (dolist (tag matter-tags)
        (when (member tag tags)
          (let* ((fname (concat tag ".txt")))
            (append-to-file (concat "{" tag "}\n") nil (org-leanpub-book--outfile outdir fname))
            (org-leanpub-book--add-to-bookfiles fname t only-subset do-sample-file tags produce-subset is-subset outdir))))
      (when (or (not only-subset) is-subset)
        ;; set filename only if the property is missing or different from the correct filename
        (when (or (not stored-filename)
                  (and ignore-stored-filenames (not (string= stored-filename computed-filename))))
          (org-entry-put (point) "EXPORT_FILE_NAME" computed-filename))
        ;; add to the filename to the book files
        (org-leanpub-book--add-to-bookfiles (file-name-nondirectory final-filename) nil only-subset do-sample-file tags produce-subset is-subset outdir)
        ;; select the subtree so that its headline is also exported (otherwise we get just the body)
        (org-mark-subtree)
        (message (format "Exporting %s (%s)" final-filename title))
        (funcall export-function nil t)))))

;; Main export function
(defun org-leanpub-book-export (export-function export-extension export-backend-symbol &optional subtreep visible-only do-sample-file only-subset subset-type)
  "Exports buffer to a Leanpub book.

The buffer is split by top level headlines, populating the
corresponding book-specification files.  This function is used
internally by `ox-leanpub-book' and should normally not be called
directly by the user."
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

    ;; Create necessary directories and symlinks, if needed
    (let* ((img-dir-rel-to-outdir (concat (file-name-as-directory org-leanpub-book-resources-dir)
                                          org-leanpub-book-images-dir))
           (img-dir-rel-to-repo (concat (file-name-as-directory outdir)
                                        img-dir-rel-to-outdir))
           (img-symlink-in-outdir (concat (file-name-as-directory outdir) org-leanpub-book-images-dir)))
      (make-directory img-dir-rel-to-repo t)

      (make-symbolic-link img-dir-rel-to-repo org-leanpub-book-images-dir t)
      (make-symbolic-link img-dir-rel-to-outdir img-symlink-in-outdir t))

    ;; Initialization: delete all the book definition and *matter.txt
    ;; files, they get recreated as needed
    (dolist (fname (mapcar (lambda (s) (concat s ".txt"))
                           (append (if only-subset '("Subset") '("Book" "Sample" "Subset"))
                                   matter-tags)))
      (delete-file (org-leanpub-book--outfile outdir fname)))

    ;; Loop through all the elements in the document, exporting them
    ;; as needed, except for those tagged with "noexport"
    (save-mark-and-excursion
      (org-map-entries (lambda ()
                         (org-leanpub-book--process-chapter
                          outdir export-function export-extension
                          ignore-stored-filenames original-point subset-mode
                          matter-tags only-subset do-sample-file produce-subset))
                       "-noexport"))

    (message (format "LeanPub export to %s/ finished" outdir))))

(provide 'ox-leanpub-book)

;;; ox-leanpub-book.el ends here
