;;; ox-leanpub.el --- Export Org documents to Leanpub book format  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Zamboni Diego

;; Author: Zamboni Diego <taazadi1@UM01723>
;; Keywords: org, leanpub

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
;;   `Book.txt', `Sample.txt' and `Subset.txt' files. It can use
;;   either Markua or LFM as the export backend.

;; This package allows you to write your book entirely in Org mode,
;; and completely manages the production of the necessary files for
;; Leanpub to be able to render your book.

;; Note: I highly recommend you use the Markua exporter, as it's more
;; mature and complete. Some Org constructs might not be exported
;; correctly to Markdown. When you load `ox-leanpub', the Markua
;; exporter is set up by default. To enable the Markdown exporter,
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
