;;; ox-leanpub.el --- Export Org documents to Leanpub book format  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Diego Zamboni

;; Author: Diego Zamboni <diego@zzamboni.org>
;; Keywords: files, org, leanpub
;; URL: https://gitlab.com/zzamboni/ox-leanpub
;; Package-Version: 0.2
;; Package-Requires: ((org "9.1") (ox-gfm "1.0") (emacs "26.1") (s "1.12.0"))

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

;; Org-mode export backends to produce books and courses in the correct
;; structure and format for publication with Leanpub (https://leanpub.com).
;; `ox-leanpub' allows you to write your material entirely in Org mode, and
;; completely manages the production of the files and directories needed for
;; Leanpub to render your book.

;; This package contains three libraries:

;; - `ox-leanpub-markua.el' exports Org files in Leanpub’s Markua format (see
;;   `https://leanpub.com/markua/read'), the default and recommended format for
;;   Leanpub books and courses.
;;
;; - `ox-leanpub-markdown.el' exports Org files in Leanpub Flavored Markdown
;;   (LFM) (see `https://leanpub.com/lfm/read'), the original markup format for
;;   Leanpub books.
;;
;; - `ox-leanpub-book.el' exports an Org file in multiple files and directories
;;   in the structure required by Leanpub, including the necessary `manuscript/'
;;   directory and the `Book.txt', `Sample.txt' and `Subset.txt' files. It can
;;   use either Markua or LFM as the export backend.

;; *Note:* it is highly recommended to use the Markua exporter, as it’s more
;;  mature and complete. Some Org constructs might not be exported correctly to
;;  Markdown.

;; If you have any feedback or bug reports, please open an issue at
;; `https://gitlab.com/zzamboni/ox-leanpub/-/issues'.

;; See full documentation in the README.org file or at
;; `https://github.com/zzamboni/ox-leanpub'.

;;; Code:

(require 'ox-leanpub-markua)
(require 'ox-leanpub-book)

(org-leanpub-book-setup-menu-markua)

(provide 'ox-leanpub)

;;; ox-leanpub.el ends here
