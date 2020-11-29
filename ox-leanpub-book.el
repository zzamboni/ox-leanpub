;;; ox-leanpub-book.el --- Export an Org file to LeanPub multifile setup  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Diego Zamboni

;; Author: Diego Zamboni <diego@zzamboni.org>
;; URL: https://gitlab.com/zzamboni/ox-leanpub
;; Package-Version: 0.2
;; Keywords: files, org, wp, markdown, leanpub
;; Package-Requires: ((org "9.1") (emacs "26.1"))

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     https://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:

;; Export an Org file in multiple files and directories in the structure
;; required by Leanpub, including the necessary `manuscript/' directory and the
;; `Book.txt', `Sample.txt' and `Subset.txt' files. It can use either Markua or
;; LFM as the export backend through the `ox-leanpub-markua' and
;; `ox-leanpub-markdown' libraries.
;;
;; See full documentation in the README.org file or at
;; `https://github.com/zzamboni/ox-leanpub'.

;;; Code:

(require 'cl-lib)
(require 'ob-core)
(require 'ox)

;; Declare functions defined in libraries that get loaded on demand, to avoid
;; lint warnings
(declare-function org-leanpub-markdown-export-to-markdown "ox-leanpub-markdown")
(declare-function org-leanpub-markua-export-to-markua "ox-leanpub-markua")

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
(defvar org-leanpub-book-matter-tags '("frontmatter" "mainmatter" "backmatter")
  "Special front/main/backmatter tags recognized by Leanpub.")

(defun org-leanpub-book-setup-menu-markdown ()
  "Set up the Book export menu entries within the Leanpub Markdown menu."
  (interactive)
  (require 'ox-leanpub-markdown)
  (org-export-define-derived-backend 'leanpub-book-markdown 'leanpub-markdown
    :menu-entry
    '(?L 1
         ((?b "Book: Whole book"      (lambda (_a s v _b) (org-leanpub-book-export-markdown s v 'sample-file)))
          (?s "Book: Subset"          (lambda (_a s v _b) (org-leanpub-book-export-markdown s v 'sample-file 'only-subset)))))
    :options-alist
    '((:leanpub-book-output-dir          "LEANPUB_BOOK_OUTPUT_DIR"          nil org-leanpub-book-manuscript-dir t)
      (:leanpub-book-write-subset        "LEANPUB_BOOK_WRITE_SUBSET"        nil "none" t)
      (:leanpub-book-id-as-filename      "LEANPUB_BOOK_ID_AS_FILENAME"      nil nil    t)
      (:leanpub-book-recompute-filenames "LEANPUB_BOOK_RECOMPUTE_FILENAMES" nil nil    t))))

(defun org-leanpub-book-setup-menu-markua ()
  "Set up the Multifile export menu entries within the Leanpub Markua menu."
  (interactive)
  (require 'ox-leanpub-markua)
  (org-export-define-derived-backend 'leanpub-book-markua 'leanpub-markua
    :menu-entry
    '(?M 1
         ((?b "Book: Whole book"      (lambda (_a s v _b) (org-leanpub-book-export-markua s v)))
          (?s "Book: Subset"          (lambda (_a s v _b) (org-leanpub-book-export-markua s v nil 'only-subset)))))
    :options-alist
    '((:leanpub-book-output-dir          "LEANPUB_BOOK_OUTPUT_DIR"          nil org-leanpub-book-manuscript-dir t)
      (:leanpub-book-write-subset        "LEANPUB_BOOK_WRITE_SUBSET"        nil "none" t)
      (:leanpub-book-id-as-filename      "LEANPUB_BOOK_ID_AS_FILENAME"      nil nil    t)
      (:leanpub-book-recompute-filenames "LEANPUB_BOOK_RECOMPUTE_FILENAMES" nil nil    t))))

;;; Utility functions

(defun org-leanpub-book--outfile (outdir f)
  "Return relative output pathname.
Concatenates `OUTDIR' with `F' using the correct separator, to
return a relative pathname."
  (concat (file-name-as-directory outdir) f))

(defun org-leanpub-book--add-to-bookfiles (outdir line &optional always do-sample-file
                                                  do-subset only-subset is-subset tags)
  "Add a `LINE' to the correct book files, terminated with an EOL.
OUTDIR is the directory to which the files should be written.  If
ALWAYS is t, the line is inconditionally added regardless of
tags, but still subject to DO-SAMPLE-FILE and DO-SUBSET,
which govern whether those files are being written at all.
ONLY-SUBSET indicates whether only the Subset.txt file should be
updated.  IS-SUBSET indicates whether the current chapter should
be part of Subset.txt.  TAGS contains the tags of the current
chapter, which is used to check for the `sample' tag."
  (let ((line-n (concat line "\n")))
    (unless only-subset
      (append-to-file line-n nil (org-leanpub-book--outfile outdir "Book.txt"))
      (when (and do-sample-file (or (member "sample" tags) always))
        (append-to-file line-n nil (org-leanpub-book--outfile outdir "Sample.txt"))))
    (when (and do-subset (or is-subset always))
      (append-to-file line-n nil (org-leanpub-book--outfile outdir "Subset.txt")))))

(defun org-leanpub-book--process-chapter (info outdir original-point
                                               export-function export-extension
                                               do-sample-file only-subset subtreep)
  "Main Book chapter export function.
Processes an org element, and exports it if it's a top level
heading.  This function gets called for all the elements in the
org document, but it only processes top level headings.

INFO is used for context and document information. OUTDIR is the
directory where the output should be stored. ORIGINAL-POINT is
the cursor position before the export started (used for the
\"current chapter\" export). EXPORT-FUNCTION, EXPORT-EXTENSION,
DO-SAMPLE-FILE, ONLY-SUBSET and SUBTREEP are as passed to
`org-leanpub-book--export'"
  (let* ((current-subtree (org-element-at-point))
         (ignore-stored-filenames (plist-get info :leanpub-book-recompute-filenames))
         (subset-mode (or (and subtreep 'current) (intern (plist-get info :leanpub-book-write-subset))))
         (do-subset (and subset-mode (not (eq subset-mode 'none))))
         (use-id-for-fname (plist-get info :leanpub-book-id-as-filename))
         ;; Get all the information about the current subtree and heading
         (id-fname (when use-id-for-fname
                     (or (org-element-property :name      current-subtree)
                         (org-element-property :CUSTOM_ID current-subtree)
                         (org-element-property :ID        current-subtree))))
         (title (or (nth 4 (org-heading-components)) ""))
         (tags (org-get-tags))
         ;; Compute or get (from EXPORT_FILE_NAME) the output filename
         (basename (concat (replace-regexp-in-string "[^[:alnum:]]" "-" (downcase (or id-fname title)))
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
    (dolist (tag org-leanpub-book-matter-tags)
      (when (member tag tags)
        (let* ((fname (concat tag ".txt")))
          (append-to-file (concat "{" tag "}\n") nil (org-leanpub-book--outfile outdir fname))
          (org-leanpub-book--add-to-bookfiles outdir fname t do-sample-file do-subset only-subset is-subset tags))))
    (when (or (not only-subset) is-subset)
      ;; set filename only if the property is missing or different from the correct filename
      (when (or (not stored-filename)
                (and ignore-stored-filenames (not (string= stored-filename computed-filename))))
        (org-entry-put (point) "EXPORT_FILE_NAME" computed-filename))
      ;; add to the filename to the book files
      (org-leanpub-book--add-to-bookfiles outdir (file-name-nondirectory final-filename)
                                          nil do-sample-file do-subset only-subset is-subset tags)
      ;; select the subtree so that its headline is also exported (otherwise we get just the body)
      (org-mark-subtree)
      (message "Exporting %s (%s)" final-filename title)
      (funcall export-function nil t))))

;; Main export function
(defun org-leanpub-book--export (export-function export-extension export-backend-symbol
                                                 &optional subtreep visible-only do-sample-file only-subset)
  "Exports buffer to a Leanpub book.

The buffer is split by top level headlines, populating the
corresponding book-specification files.

EXPORT-FUNCTION is a regular Org exporter function, which must
receives three optional arguments ASYNC (which is always passed
as nil), SUBTREEP and VISIBLE-ONLY (which are passed unchanged
from the corresponding arguments received). In particular, the
SUBTREEP option must be obeyed for the current-chapter export to
work. Files will be created with the extension EXPORT-EXTENSION.
EXPORT-BACKEND-SYMBOL is the name (symbol) of the exporter to
use.

DO-SAMPLE-FILE specifies whether the `Sample.txt' file should be
generated (in Leanpub this is only needed for Markdown books, for
Markua it is handled through conditional directives in the text
itself). ONLY-SUBSET specified whether only the book subset
should be exported (without the entire book). If ONLY-SUBSET is
t, then the type of subset which should be produced is
determined as follows:

- If SUBTREEP is t, then only the current chapter is
  included.
- Otherwise, the value of the `#+LEANPUB_BOOK_WRITE_SUBSET'
  buffer option is used. The valid values are `none' (default,
  not created), `tagged' (use all chapters tagged `subset'),
  `all' (all the chapters), `sample' (chapters tagged `sample'),
  `current' (chapter where the cursor is at the moment of the
  export).

This function is used internally by `ox-leanpub-book' and should
normally not be called directly by the user."
  (interactive)
  (let* ((info (org-combine-plists
                (org-export--get-export-attributes
                 export-backend-symbol subtreep visible-only)
                (org-export--get-buffer-attributes)
                (org-export-get-environment export-backend-symbol subtreep)))
         (outdir (plist-get info :leanpub-book-output-dir))
         (original-point (point)))

    ;; Create necessary directories and symlinks, if needed
    (let* ((img-dir-rel-to-outdir (concat (file-name-as-directory org-leanpub-book-resources-dir)
                                          org-leanpub-book-images-dir))
           (img-dir-rel-to-repo (concat (file-name-as-directory outdir)
                                        img-dir-rel-to-outdir))
           (img-symlink-in-outdir (concat (file-name-as-directory outdir) org-leanpub-book-images-dir))
           (images-readme-text "manuscript/resources/README file

This file was created by ox-leanpub to force Git to store this
directory. There are symlinks to this directory both from the
top-level and from within manuscript/, to make it easier for you
to include figures in your book by referencing them as
`images/figure.png', and have that path be accessible from (a) your
original Org file, (b) from the exported files in `manuscript/', and
(c) to have the files stored within `manuscript/resources' as
required by Leanpub's Markua processor.

If you remove this file/directory from your git repo, please also
remember to remove the symlinks, otherwise book generation will
fail (Leanpub sees the broken symlink inside manuscript/ as an
error).")
           (images-readme-file (concat (file-name-as-directory img-dir-rel-to-repo) "README")))
      (make-directory img-dir-rel-to-repo t)
      (write-region images-readme-text nil images-readme-file)

      (make-symbolic-link img-dir-rel-to-repo org-leanpub-book-images-dir t)
      (make-symbolic-link img-dir-rel-to-outdir img-symlink-in-outdir t))

    ;; Initialization: delete all the book definition and *matter.txt
    ;; files, they get recreated as needed
    (dolist (fname (mapcar (lambda (s) (concat s ".txt"))
                           (append (if only-subset '("Subset") '("Book" "Sample" "Subset"))
                                   org-leanpub-book-matter-tags)))
      (delete-file (org-leanpub-book--outfile outdir fname)))

    ;; Loop through all the top-level headings in the document, exporting them
    ;; as needed, except for those tagged with "noexport"
    (save-mark-and-excursion
      (org-map-entries
       (lambda ()
         (when (and (org-at-heading-p) (= (nth 1 (org-heading-components)) 1))
           (org-leanpub-book--process-chapter info outdir original-point
                                              export-function export-extension
                                              do-sample-file only-subset subtreep)))
       "-noexport"))

    (message "LeanPub export to %s/ finished" outdir)))

;; Shortcuts to export the whole book in the two supported formats
(defun org-leanpub-book-export-markdown (&optional subtreep visible-only do-sample-file only-subset)
  "Export the book in LeanPub Markdown format.
Frontend to `org-leanpub-book--export' with the appropriate
parameters SUBTREEP, VISIBLE-ONLY, DO-SAMPLE-FILE and
ONLY-SUBSET."
  (org-leanpub-book--export #'org-leanpub-markdown-export-to-markdown ".md" 'leanpub-book-markdown
                            subtreep visible-only do-sample-file only-subset))

(defun org-leanpub-book-export-markua (&optional subtreep visible-only do-sample-file only-subset)
  "Export the book in LeanPub Markua format.
Frontend to `org-leanpub-book--export' with the appropriate
parameters SUBTREEP, VISIBLE-ONLY, DO-SAMPLE-FILE and
ONLY-SUBSET."
  (org-leanpub-book--export #'org-leanpub-markua-export-to-markua ".markua" 'leanpub-book-markua
                            subtreep visible-only do-sample-file only-subset))

(provide 'ox-leanpub-book)

;;; ox-leanpub-book.el ends here
