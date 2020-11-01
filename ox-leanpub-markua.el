;;; ox-leanpub-markua.el --- Markua Back-End for Org Export Engine  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Diego Zamboni

;; Author: Diego Zamboni <diego@zzamboni.org>
;; URL: https://gitlab.com/zzamboni/ox-leanpub
;; Package-Version: 0.2
;; Keywords: files, org, wp, markdown, leanpub, markua
;; Package-Requires: ((org "9.1") (ox-gfm "1.0") (emacs "26.1"))

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

;; Export Org files in Leanpub’s Markua format (see
;; `https://leanpub.com/markua/read'), the default and recommended format for
;; Leanpub books and courses.
;;
;; See full documentation in the README.org file or at
;; `https://github.com/zzamboni/ox-leanpub'.

;;; Code:

(require 'cl-lib)
(require 'ox-md)
(require 'ob-core)
(require 'subr-x)
(require 'ox-gfm)

;;; Define Back-End

(org-export-define-derived-backend 'leanpub-markua 'md
  :menu-entry
  '(?M "Export to Leanpub Markua"
       ((?M "To temporary buffer"
            (lambda (a s v b) (org-leanpub-markua-export-as-markua a s v)))
        (?m "To file" (lambda (a s v b) (org-leanpub-markua-export-to-markua a s v)))
        (?o "To file and open"
            (lambda (a s v b)
              (if a (org-leanpub-markua-export-to-markua t s v)
                (org-open-file (org-leanpub-markua-export-to-markua nil s v)))))))
  :translate-alist '((fixed-width        . org-leanpub-markua-fixed-width-block)
                     (example-block      . org-leanpub-markua-example-block)
                     (special-block      . org-leanpub-markua-special-block)
                     (src-block          . org-leanpub-markua-src-block)
                     (plain-text         . org-leanpub-markua-plain-text)
                     (inner-template     . org-leanpub-markua-inner-template)
                     (footnote-reference . org-leanpub-markua-footnote-reference)
                     (headline           . org-leanpub-markua-headline)
                     (item               . org-leanpub-markua-item)
                     (keyword            . org-leanpub-markua-keyword)
                     (link               . org-leanpub-markua-link)
                     (latex-fragment     . org-leanpub-markua-latex-fragment)
                     (latex-environment  . org-leanpub-markua-latex-environment)
                     (line-break         . org-leanpub-markua-line-break)
                     (paragraph          . org-leanpub-markua-paragraph)
                     (table-cell         . org-gfm-table-cell)
                     (table-row          . org-gfm-table-row)
                     (table              . org-leanpub-markua-table)
                     (export-block       . org-leanpub-markua-ignore)
                     (export-snippet     . org-leanpub-markua-ignore)
                     (superscript        . org-leanpub-markua-superscript)
                     (subscript          . org-leanpub-markua-subscript))
  :options-alist
  '((:markua-noweb-ref-caption        "MARKUA_NOWEB_REF_CAPTION"        nil nil              t)
    (:markua-tangle-caption           "MARKUA_TANGLE_CAPTION"           nil nil              t)
    (:markua-tangle-caption-fmt       "MARKUA_TANGLE_CAPTION_FMT"       nil "[%s]"           t)
    (:markua-noweb-ref-caption-fmt    "MARKUA_NOWEB_REF_CAPTION_FMT"    nil "«%s»≡"          t)
    (:markua-tangle-noweb-caption-fmt "MARKUA_TANGLE_NOWEB_CAPTION_FMT" nil "[%1$s] «%2$s»≡" t)
    (:markua-export-type              "MARKUA_EXPORT_TYPE"              nil "book"           t)))

;;; Variable definitions

;;; Mapping from org blocks to Markua blocks.
(defvar org-leanpub-markua--block-mapping
  '(("aside"       "aside" nil)
    ("blurb"       "blurb" nil)
    ("center"      "blurb" "center")
    ("discussion"  "blurb" "discussion")
    ("error"       "blurb" "error")
    ("exercise"    "blurb" "exercise")
    ("information" "blurb" "information")
    ("note"        "blurb" "information")
    ("question"    "blurb" "question")
    ("tip"         "blurb" "tip")
    ("warning"     "blurb" "warning"))
  "Mapping from org block types to Markua aside and blurb blocks.
The default value corresponds to the block types as documentated
at https://leanpub.com/markua/read#leanpub-auto-asides-a-or-aside

Structure of each element is (org-block markua-block &optional markua-class)

    Example:            Gets exported as:

    #+begin_tip         {blurb, class: tip}
    This is a tip       This is a tip
    #+end_tip           {/blurb}

    #+begin_aside       {aside}
    This is an aside    This is an aside
    #+end_aside         {/aside}

Note that `exercise' blocks get handled differently depending on
whether you are exporting a book or a course, see the
documentation for `org-leanpub-markua-special-block' for
details.")

(defvar org-leanpub-markua--exclude-attributes
  '(:export-type)
  "List of ATTR_LEANPUB attributes that are omitted in the Markua output.

You should normally not need to modify this variable.

These are attributes which are used internally by
`ox-leanpub-markua', but which have to be omitted in the output
Markua attribute lines.")

;;; Utility functions

(defun org-leanpub-markua--attr_leanpub-attrs (elem)
  "Return an alist containing ELEM's parsed #+ATTR_LEANPUB line, or nil if not specified."
  (let ((attr-leanpub-str (car (org-element-property :attr_leanpub elem))))
    (when (string-prefix-p "{" attr-leanpub-str)
      (lwarn '(ox-leanpub-markua) :warning "Old-style ATTR_LEANPUB format '%s' no longer supported. Please use format ':attr val ...'" attr-leanpub-str))
    (org-babel-parse-header-arguments attr-leanpub-str)))

(defun org-leanpub-markua--attribute-line (elem info &optional other-attrs nonewline exclude-attrs env-name)
  "Generate a Leanpub attribute or environment line.
Collect #+NAME, #+CAPTION, and any attributes specified as :key
value in the #+ATTR_LEANPUB line for ELEM, and put them all
together in a Leanpub-style attribute line of the form {key:
value,...}. If an attribute is present in both places (e.g. if
both #+CAPTION and :title are specified), then the values from
#+ATTR_LEANPUB take precedence.

INFO is a plist holding contextual information. OTHER-ATTRS, if
given, is an alist holding additional attributes to include.
NONEWLINE, supresses a trailing newline in the produced attribute
line. EXCLUDE-ATTRS can be used to specify a list of attributes
to exclude in the output, its default value is
`org-leanpub-markua--exclude-attributes'. ENV-NAME can be
specified to format the line as an environment name followed by
the attributes, e.g. for a quiz or exercise environment in
Markua."
  (let* (
         ;; Populate initial list with :id and :caption, if given
         (init (list (cons :id (or (org-element-property :name elem)
                                   (org-element-property :ID elem)
                                   (org-element-property :CUSTOM_ID elem)))
                     (cons :caption (org-export-data (org-export-get-caption elem) info))))
         ;; Parse the attributes from #+ATTR_LEANPUB and concatenate with any
         ;; other arguments given, and with the initial list constructed above.
         ;; Earlier elements of the list override later ones.
         (lpattr (delq nil (append (org-leanpub-markua--attr_leanpub-attrs elem) other-attrs init)))
         ;;; Use the default value for exclude-attrs if not specified
         (exclude-attrs (or exclude-attrs org-leanpub-markua--exclude-attributes))
         ;; Build the attribute line to print
         (attribute-line
          (mapconcat #'identity
                     (delq nil
                           (append
                            (list env-name)
                            (mapcar (lambda (elem)
                                      (cl-destructuring-bind (key . val) elem
                                        (when (and (> (length val) 0) (not (member key exclude-attrs)))
                                          (format "%s: \"%s\""
                                                  (substring (symbol-name key) 1)
                                                  val))))
                                    (cl-remove-duplicates lpattr :key #'car :from-end t))))
                     ", "))
         ;; Compute the final output string
         (output (when (> (length attribute-line) 0)
                   (format "{%s}" attribute-line))))
    (when (> (length output) 0)
      (concat
       output
       (unless nonewline "\n")))))

(defun org-leanpub-markua--chomp-end (str)
  "Chomp trailing whitespace from STR."
  (replace-regexp-in-string (rx (* (any " \t\n")) eos)
                            ""
                            str))

;;; Tables

(defun org-leanpub-markua-table (table contents info)
  "Use ox-gfm to transcode TABLE element into Github Flavored Markdown table.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information.  We prepend the Leanpub attribute line if needed."
  (concat (org-leanpub-markua--attribute-line table info)
          (org-gfm-table table contents info)))

;;; LaTeX fragments and environments

(defun org-leanpub-markua-latex-fragment (latex-fragment _contents _info)
  "Transcode a LATEX-FRAGMENT (math) object from Org to Markua."
  (concat
   (format "`%s`$"
           ;; Removes the \[, \] and $ that mark latex fragments
           (replace-regexp-in-string
            (rx bos (or "\\[" "\\(" "$")) ""
            (replace-regexp-in-string
             (rx (or "\\]" "\\)" "$") eos) ""
             (org-element-property :value latex-fragment))))))

(defun org-leanpub-markua-latex-environment (latex-environment _contents _info)
  "Transcode a LATEX-ENVIRONMENT (math env) object from Org to Markua."
  (let ((latex-frag (org-remove-indentation
                     (org-leanpub-markua--chomp-end
                      (org-element-property :value latex-environment)))))
    (format "```$\n%s\n```\n" latex-frag)))

;;; Other elements

(defun org-leanpub-markua-headline-without-anchor (headline contents info)
  "Transcode HEADLINE element into Markua format.
CONTENTS is the headline contents.  INFO is a plist used as
a communication channel.  This is the same function as
`org-md-headline' but without inserting the <a> anchors."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((level (org-export-get-relative-level headline info))
           (title (org-export-data (org-element-property :title headline) info))
           (todo (and (plist-get info :with-todo-keywords)
                      (let ((todo (org-element-property :todo-keyword
                                                        headline)))
                        (and todo (concat (org-export-data todo info) " ")))))
           (tag-list (org-export-get-tags headline info))
           (tags (and (plist-get info :with-tags)
                      (and tag-list
                           (concat "     " (format ":%s:" (mapconcat #'identity tag-list ":"))))))
           (is-part (and (member "part" tag-list) (= level 1)))
           (priority
            (and (plist-get info :with-priority)
                 (let ((char (org-element-property :priority headline)))
                   (and char (format "[#%c] " char)))))
           ;; Headline text without tags.
           (heading (concat todo priority title (when is-part " #")))
           (style (plist-get info :md-headline-style)))
      (cond
       ;; Cannot create a headline.  Fall-back to a list.
       ((or (org-export-low-level-p headline info)
            (not (memq style '(atx setext)))
            (and (eq style 'atx) (> level 6))
            (and (eq style 'setext) (> level 2)))
        (let ((bullet
               (if (not (org-export-numbered-headline-p headline info)) "-"
                 (concat (number-to-string
                          (car (last (org-export-get-headline-number
                                      headline info))))
                         "."))))
          (concat bullet " " heading tags "\n\n"
                  (and contents (replace-regexp-in-string "^" (make-string (1+ (length bullet)) ?\s) contents)))))
       (t
        (concat (org-md--headline-title style level heading nil tags)
                contents))))))

(defun org-leanpub-markua-headline (headline contents info)
  "Add Leanpub attribute line before HEADLINE.
This function also processes the `sample' and `nobook' tags and
produces the appropriate Leanpub attributes.  CONTENTS is the
item contents.  INFO is a plist used as a communication channel."
  (let* ((tags (org-export-get-tags headline info))
         (other-attrs (cl-remove-if 'null
                                    (mapcar (lambda (elem)
                                              (if (string= elem "sample")
                                                  '(:sample . "true")
                                                (when (string= elem "nobook")
                                                  '(:book . "false")))) tags))))
    (concat (org-leanpub-markua--attribute-line headline info other-attrs)
            (string-trim-left (org-leanpub-markua-headline-without-anchor headline contents info)))))

(defun org-leanpub-markua-item (item contents info)
  "Transcode ITEM element into Markua format.
CONTENTS is the item contents.  INFO is a plist used as
a communication channel."
  (let* ((type (org-element-property :type (org-export-get-parent item)))
         (struct (org-element-property :structure item))
         (bullet (if (not (eq type 'ordered)) "*"
                   (concat (number-to-string
                            (car (last (org-list-get-item-number
                                        (org-element-property :begin item)
                                        struct
                                        (org-list-prevs-alist struct)
                                        (org-list-parents-alist struct)))))
                           ".")))
         (tag (org-element-property :tag item)))
    (concat (if tag
                (concat (org-export-data tag info) "\n")
              (concat bullet " "))
            (and contents
                 (concat (and tag ": ")
                         (org-trim (replace-regexp-in-string "^" (make-string (1+ (length bullet)) ?\s) contents)))))))

(defun org-leanpub-markua-keyword (keyword _contents _info)
  "Transcode a KEYWORD element from Org to Markua."
  (let ((key (org-element-property :key keyword))
        (value (org-element-property :value keyword)))
    (cond
     ((string= key "MARKUA") value)
     (t ""))))

(defun org-leanpub-markua-inner-template (contents info)
  "Return complete document string after Markua conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options.  Required in order to add footnote
definitions at the end."
  (concat
   contents
   "\n\n"
   (let ((definitions (org-export-collect-footnote-definitions
                       info
                       (plist-get info :parse-tree))))
     ;; Looks like leanpub do not like : in labels.
     (mapconcat (lambda (ref)
                  (let ((id (format "[^%s]: " (replace-regexp-in-string
                                               ":" "_"
                                               (let ((label (cadr ref)))
                                                 (if label
                                                     label
                                                   (car ref)))))))
                    (let ((def (nth 2 ref)))
                      (concat id (org-export-data def info)))))
                definitions "\n\n"))))

(defun org-leanpub-markua-footnote-reference (footnote _contents info)
  "Export a `FOOTNOTE'.
INFO is a plist holding contextual information."
  ;; Leanpub does not like : in labels, so we replace them with underscores
  (format "[^%s]"
          (replace-regexp-in-string
           ":" "_"
           (let ((label (org-element-property :label footnote)))
             (if label
                 label
               (org-export-get-footnote-number footnote info))))))

(defun org-leanpub-markua-ignore (_src-block _contents _info)
  "Return an empty string for `SRC-BLOCK' elements which are ignored.
CONTENTS and INFO are also ignored."
  "")

(defun org-leanpub-markua-superscript (_superscript contents _info)
  "Transcode a SUPERSCRIPT object from Org to MARKUA.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format "^%s^" contents))

(defun org-leanpub-markua-subscript (_subscript contents _info)
  "Transcode a SUBSCRIPT object from Org to MARKUA.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format "~%s~" contents))

(defun org-leanpub-markua-plain-text (plain-text _info)
  "Return `PLAIN-TEXT' elements as-is."
  plain-text)

;;; EOLs are removed from paragraphs in Markua

(defun org-leanpub-markua-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to Markua.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  (concat (org-leanpub-markua--attribute-line paragraph info)
          (replace-regexp-in-string "{{markua:linebreak}}" "\n"
                                    (replace-regexp-in-string "\n" " " contents)
                                    nil 'literal)))

;;; Internal functions to get header arguments from a src block

(defun org-leanpub-markua--header-alist (src-block)
  "Return an alist with all the header arguments of SRC-BLOCK."
  (org-babel-parse-header-arguments (org-element-property :parameters src-block)))

(defun org-leanpub-markua--get-header-arg (arg src-block)
  "Get and return a single header ARG from a SRC-BLOCK."
  (alist-get arg (org-leanpub-markua--header-alist src-block)))

;;; {lang="python"}
;;; ~~~~~~~~
;;; def longitude_circle(diameter):
;;;     return math.pi * diameter
;;; longitude(10)
;;; ~~~~~~~~
(defun org-leanpub-markua-src-block (src-block _contents info)
  "Transcode SRC-BLOCK element into Markua format.
INFO is a plist used as a communication channel."
  (let* ((header-args (org-leanpub-markua--header-alist src-block))
         (do-export (not (member (alist-get :exports header-args) '("results" "none")))))
    (when do-export
      (let* (;; If needed, build caption from :noweb-ref
             (use-noweb-ref    (plist-get info :markua-noweb-ref-caption))
             (noweb-ref        (alist-get :noweb-ref header-args))
             (noweb-ref-fmt    (plist-get info :markua-noweb-ref-caption-fmt))
             (noweb-caption    (when (and use-noweb-ref noweb-ref)
                                 (format noweb-ref-fmt noweb-ref)))
             ;; If needed, build caption from :tangle
             (use-tangle       (plist-get info :markua-tangle-caption))
             (tangle-target    (alist-get :tangle header-args))
             (tangle-file      (unless (member tangle-target '("yes" "no"))
                                 tangle-target))
             (tangle-file-fmt  (plist-get info :markua-tangle-caption-fmt))
             (tangle-caption   (when (and use-tangle tangle-file)
                                 (format tangle-file-fmt tangle-file)))
             ;; Compute the final caption
             (given-caption    (org-element-property :caption src-block))
             (both-caption-fmt (plist-get info :markua-tangle-noweb-caption-fmt))
             (built-caption
              (cond
               ;; A specified :caption overrides any generated caption
               (given-caption nil)
               ;; If both :noweb-ref and :tangle are given, use the corresponding format
               ((and noweb-caption tangle-caption) (format both-caption-fmt tangle-file noweb-ref))
               ;; Otherwise use the corresponding individual format
               (tangle-caption tangle-caption)
               (noweb-caption noweb-caption)
               (t nil)))
             ;; Store relevant attributes in the structure used to construct the
             ;; Markua attribute line
             (attrs (list (cons :format
                                (org-element-property :language src-block))
                          (cons :line-numbers
                                (when (org-element-property :number-lines src-block) "true"))
                          ;; Include a custom :caption only if we have one, to
                          ;; allow any manually-specified #+caption to be used
                          (when (org-string-nw-p built-caption)
                            (cons :caption built-caption))))
             (block-value (org-element-property :value src-block)))
        (concat
         (org-leanpub-markua--attribute-line src-block info attrs)
         "```\n"
         (org-remove-indentation block-value)
         (unless (string-suffix-p "\n" block-value) "\n")
         "```")))))

;;; > ~~~~~~~~
;;; > 123.0
;;; > ~~~~~~~~
(defun org-leanpub-markua-example-block (src-block contents info)
  "Transcode SRC-BLOCK element into Markua format.
CONTENTS holds the contents of the block. INFO is a plist used as
a communication channel."
  (org-leanpub-markua-src-block src-block contents info))

;;; > ~~~~~~~~
;;; > 123.0
;;; > ~~~~~~~~
(defun org-leanpub-markua-fixed-width-block (src-block contents info)
  "Transcode SRC-BLOCK element into Markua format.
CONTENTS holds the contents of the block. INFO is a plist used as
a communication channel."
  (org-leanpub-markua-src-block src-block contents info))

(defun org-leanpub-markua-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to Markua.
CONTENTS is the contents of the block. INFO is a plist used as a
communication channel.

Special blocks are mapped to corresponding Markua aside and blurb
types according to the documentation at
https://leanpub.com/markua/read#leanpub-auto-asides-a-or-aside

The supported block types and their conversions are defined in
`org-leanpub-markua--block-mapping'.

    Example:            Gets exported as:

    #+begin_tip         {blurb, class: tip}
    This is a tip       This is a tip
    #+end_tip           {/blurb}

    #+begin_aside       {aside}
    This is an aside    This is an aside
    #+end_aside         {/aside}

Blocks of type QUIZ are exported as {quiz} environments according
to the documentation at
https://leanpub.com/markua/read#leanpub-auto-quizzes-and-exercises.
These blocks require an ID attribute in Markua, which needs to be
provided using a `#+name' attribute before the start of the
block.

The content of quiz blocks is transcribed as-is into the output,
to prevent Org's conversions from interfering with the special
formatting used by Markua quizzes and exercises. If you need any
special formatting inside the block, you need to specify it
directly in Markua format.

Blocks of type EXAMPLE are handled differently depending on the
`#+MARKUA_EXPORT_TYPE' option specified for the current
buffer, or the `:export-type' option specified in
`#+ATTR_LEANPUB' for the current block. With its default
value (`book'), example blocks are exported using the blurb
notation `X>'. If set to `course', then example blocks are
exported as {example} environments, and otherwise handled the
same as {quiz} environments."
  (let* ((type (org-element-property :type special-block))
         (caption (org-export-data (org-export-get-caption special-block) info))
         (lp-attrs (org-leanpub-markua--attr_leanpub-attrs special-block))
         (export-type (or (alist-get :export-type lp-attrs)
                          (plist-get info :markua-export-type))))
    (if (or (string= type "quiz")
            (and (string= type "exercise")
                 (string= export-type "course")))
        (let ((block-value (buffer-substring (org-element-property :contents-begin special-block)
                                             (org-element-property :contents-end special-block))))
          (concat
           (org-leanpub-markua--attribute-line special-block info nil nil nil type)
           (when (> (length caption) 0) (format "### %s\n" caption))
           (org-leanpub-markua--chomp-end block-value)
           (format "\n{/%s}\n" type)))
      (cl-destructuring-bind (markua-block &optional markua-class)
          (alist-get type org-leanpub-markua--block-mapping nil nil #'equal)
        (concat
         (org-leanpub-markua--attribute-line special-block info (list (cons :class markua-class)) nil nil markua-block)
         (when (> (length caption) 0) (format "### %s\n" caption))
         (org-leanpub-markua--chomp-end (org-remove-indentation contents))
         (format "\n{/%s}\n" markua-block))))))

(defun org-leanpub-markua-link (link contents info)
  "Transcode a LINK object into Markua format.
CONTENTS is the link description. INFO is a plist used as a
communication channel."
  (let ((type (org-element-property :type link)))
    (cond ((member type '("custom-id" "id"))
           (let ((id (org-element-property :path link)))
             (format "[%s](#%s)" contents id)))
          ((org-export-inline-image-p link org-html-inline-image-rules)
           (let ((path (let ((raw-path (org-element-property :path link)))
                         (if (not (file-name-absolute-p raw-path)) raw-path
                           (expand-file-name raw-path)))))
             (format "![%s](%s)"
                     (let ((caption (org-export-get-caption
                                     (org-export-get-parent-element link))))
                       (if caption
                           (org-export-data caption info)
                         ""))
                     path)))
          (t (let* ((raw-path (org-element-property :path link))
                    (path (if (member type '("http" "https" "ftp"))
                              (concat type ":" raw-path)
                            nil)))
               (if path
                   (if (not contents) (format "<%s>" path)
                     (format "[%s](%s)" contents path))
                 contents))))))

;;;; Line Break

(defun org-leanpub-markua-line-break (_line-break _contents _info)
  "Transcode a LINE-BREAK object from Org to Markua.

Arguments are ignored. Linebreaks are temporarily converted to
the string `{{markua:linebreak}}', which is later removed in
`org-leanpub-markua-paragraph', since the Markua spec requires
paragraphs to be in a single line without linebreaks."
  "{{markua:linebreak}}")

;;; Interactive function

;;;###autoload
(defun org-leanpub-markua-export-as-markua (&optional async subtreep visible-only)
  "Export current buffer to a Markua buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org MD Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (org-export-to-buffer 'leanpub-markua "*Org MARKUA Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))

;;;###autoload
(defun org-leanpub-markua-export-to-markua (&optional async subtreep visible-only)
  "Export current buffer to a Leanpub compatible Markua file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".markua" subtreep)))
    (org-export-to-file 'leanpub-markua outfile async subtreep visible-only)))

(provide 'ox-leanpub-markua)

;;; ox-leanpub-markua.el ends here
