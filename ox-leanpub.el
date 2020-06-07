;;; ox-leanpub.el --- Export Org documents to Leanpub book format  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Diego Zamboni

;; Author: Diego Zamboni <diego@zzamboni.org>
;; Keywords: files, org, leanpub
;; URL: https://gitlab.com/zzamboni/ox-leanpub
;; Package-Version: 0.1
;; Package-Requires: ((ox-gfm "20170628.2102") (emacs "26.1"))

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

;; This package contains three Emacs libraries:

;; - `ox-leanpub-markua.el' exports Org files in Leanpub's Markua
;;   format, which is the default and recommended format for Leanpub
;;   books.

;; - `ox-leanpub-markdown.el' exports Org files in Leanpub Flavored
;;   Markdown (LFM), the original markup format for Leanpub
;;   books.

;; - `ox-leanpub-book.el' exports an Org file in multiple files in
;;   the structure required by Leanpub, including the necessary
;;   `Book.txt', `Sample.txt' and `Subset.txt' files.  It can use
;;   either Markua or LFM as the export backend.

;; This package allows you to write your book entirely in Org mode,
;; and completely manages the production of the necessary files for
;; Leanpub to be able to render your book.

;; Note: I highly recommend you use the Markua exporter, as it's more
;; mature and complete.  Some Org constructs might not be exported
;; correctly to Markdown.  When you load `ox-leanpub', the Markua
;; exporter is set up by default.  To enable the Markdown exporter,
;; include the following lines in your configuration after loading
;; `ox-leanpub':

;;     (require 'ox-leanpub)
;;     (require 'ox-leanpub-markdown)
;;     (org-leanpub-book-setup-menu-markdown)

;;; Code:

(require 'ox-leanpub-markua)
(require 'ox-leanpub-book)

(org-leanpub-book-setup-menu-markua)

(provide 'ox-leanpub)
;;; ox-leanpub.el ends here
