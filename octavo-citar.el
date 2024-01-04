;;; octavo-citar.el --- Citar integration for Octavo -*- lexical-binding: t; -*-

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

;; This package provides integration between Octavo and Citar
;; <https://github.com/emacs-citar/citar>, allowing Octavo notes with
;; bibliographic citekeys in their titles to be registered as notes in the
;; Citar interface.

;; To use, arrange for the package to be loaded:

;; (with-eval-after-load 'citar
;;   (with-eval-after-load 'octavo
;;     (require 'octavo-citar)))

;; Then set the following variable:

;; (setq citar-notes-source 'octavo)

;; And finally set `octavo-citar-citekey-regexp' to a regular expression that will match
;; the citekeys found in the title of your notes.

;; You can also customize the template for titling new notes, with the
;; variable `octavo-citar-title-template'. See the variable `citar-templates' for
;; examples.

;;; Code:

(require 'octavo)
(require 'citar)


;;;; Custom variables

(defgroup octavo-citar nil
  "Citar integration for Octavo."
  :group 'text
  :group 'files
  :prefix "octavo-citar")

(defcustom octavo-citar-citekey-regexp nil
  "Regular expression to match citekeys in note file-names."
  :type 'string)

(defcustom octavo-citar-title-template "${=key=} - ${author} - ${title} (${year})"
  "Template for formatting new note titles.
Must include \"${=key=}\"."
  :type 'string)


;;;; items

(defun octavo-citar--get-notes (&optional keys)
  "Return hash-table with KEYS with file notes."
  (let* ((files (make-hash-table :test 'equal))
         (key-string (string-join keys "\\|"))
         (filematch (or key-string octavo-citar-citekey-regexp)))
    (prog1 files
      (dolist (file (octavo--directory-files t filematch))
        (let ((key (or (car keys)
                       (and (string-match octavo-citar-citekey-regexp file)
                            (match-string 0 file)))))
          (push file (gethash key files)))))))


;;;; hasitems

(defun octavo-citar--has-notes (&optional _entries)
  "Return predicate testing whether cite key has associated notes."
  (let ((files (octavo-citar--get-notes)))
    (lambda (key)
      (gethash key files))))


;;;; create

(defun octavo-citar--create-note (_key entry)
  "Create a note file from KEY and ENTRY."
  (when (y-or-n-p "No note associated - create one?")
    (let* ((title
            (subst-char-in-string ?: ?-
                                  (citar-format--entry
                                   octavo-citar-title-template
                                   entry))))
      (octavo-new-note title))))


;;;; Register citar-note-source

(citar-register-notes-source 'octavo '(:name "octavo"
                                       :category octavo-file
                                       :items octavo-citar--get-notes
                                       :hasitems octavo-citar--has-notes
                                       :open octavo-find-file
                                       :create octavo-citar--create-note
                                       :transform file-name-nondirectory))

(provide 'octavo-citar)
;;; octavo-citar.el ends here
