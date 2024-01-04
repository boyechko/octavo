;;; octavo-org-link.el --- Org-link integration for Octavo -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 Richard Boyechko

;; Created: June 28, 2023
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Homepage: https://github.com/boyechko/octavo
;; Package-Requires: ((emacs "26.1") (octavo "0.1"))

;; Created by Richard Boyechko based on zk.el by Grant Rosson
;; Original Author: Grant Rosson <https://github.com/localauthor>
;; Forked from: https://github.com/localauthor/zk
;; Original Creation Date: January 4, 2022
;; Copyright (C) 2022-2023 Grant Rosson

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

;; This package provides for the use of org-links with octavo, through the
;; creation of a `octavo' org-link type. To use, add the following to your
;; init.el:

;; (with-eval-after-load 'org
;;   (with-eval-after-load 'octavo
;;     (require 'octavo-org-link)))

;; Thanks to @jgru for getting this started, and @protesilaos for the
;; push to finish it.

;;; Code:

(require 'octavo)

(declare-function org-link-set-parameters "ol.el")
(declare-function org-link-store-props "ol.el")

(org-link-set-parameters "octavo"
			 :follow #'octavo-org-link--follow
                         :export #'octavo-org-link--export
			 :store #'octavo-org-link--store
                         :complete #'octavo-org-link--complete
                         :help-echo #'octavo-org-link--help-echo)

;; Set up org-style link format by setting variables
(setq octavo-link-format "[[octavo:%i]]")
(setq octavo-link-and-title-format "[[octavo:%i][%t]]")
(setq octavo-enable-link-buttons nil)

(defun octavo-org-link--follow (id)
  "Follow an octavo ID."
  (let ((file (octavo--parse-id 'file-path id)))
    (if file
        (octavo-find-file file)
      (user-error "Could not find octavo-note with ID %s" id))))

(defun octavo-org-link--export (link description format)
  "Export a `octavo:' link from Org files.
The LINK, DESCRIPTION, and FORMAT are handled by the export
backend."
  (let* ((id link)
         (path (octavo--parse-id 'file-path id))
         (p (file-name-sans-extension path))
	 (desc (or description (concat "octavo:" id))))
    (cond
     ((eq format 'html) (format "<a target=\"_blank\" href=\"%s.html\">%s</a>" p desc))
     ((eq format 'latex) (format "\\href{%s}{%s}" (replace-regexp-in-string "[\\{}$%&_#~^]" "\\\\\\&" path) desc))
     ((eq format 'texinfo) (format "@uref{%s,%s}" path desc))
     ((eq format 'ascii) (format "[%s] <octavo:%s>" desc path))
     ((eq format 'md) (format "[%s](%s.md)" desc p))
     (t path))))

(defun octavo-org-link--store ()
  "Store a link to a octavo-note."
  (when (octavo-file-p)
    (let ((id (octavo--id-at-point)))
      (org-link-store-props
       :type "octavo"
       :link (concat "octavo:" id)
       :description (octavo--parse-id 'title id)))))

(defun octavo-org-link--complete ()
  "Like `octavo-insert-link' but for Org integration.
This lets the user complete a link through the `org-insert-link'
interface by first selecting the `octavo:' hyperlink type."
  (concat
   "octavo:"
   (car (octavo--parse-file (octavo-select-file)))))

(defun octavo-org-link--help-echo (_win _obj pos)
  "Generate help-echo tooltip for `octavo:' Org links.
Takes WIN, OBJ, and POS arguments."
  (save-excursion
    (goto-char pos)
    (re-search-backward octavo-id-regexp)
    (format
     "%s"
     (octavo--parse-id
      'title
      (match-string 0)))))

(provide 'octavo-org-link)

;;; octavo-org-link.el ends here
