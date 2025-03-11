;;; octavo-consult.el --- Consult integration for Octavo -*- lexical-binding: t; -*-

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

;; This package offers several integrations of Consult with Octavo:

;; 1) Two functions as alternatives to the default `octavo-grep' functions:
;;    `octavo-consult-grep' and `octavo-consult-grep-tag-search'. Instead of
;;    displaying search results in a `grep' buffer, these functions display
;;    search results using Consult.

;;    To use these alternative functions, set one or both of the following
;;    variables:
;;    (setq octavo-grep-function 'octavo-consult-grep)
;;    (setq octavo-tag-grep-function 'octavo-consult-grep-tag-search)

;; 2) Two ways of accessing a list of currently open notes via `consult-buffer':
;;    first through `consult-buffer' itself, accessible via narrowing with the
;;    'z' key; second, as alternative to the command `octavo-current-notes',
;;    such that it brings up the Consult buffer source directly.

;;    To add the Octavo Consult buffer source to `consult-buffer-sources',
;;    evaluate:
;;    (add-to-list 'consult-buffer-sources 'octavo-consult-source 'append)

;;    To set the alternative `octavo-current-note' function, evaluate:
;;    (setq octavo-current-notes-function 'octavo-consult-current-notes)

;; 3) Note previews when selecting a Octavo file in the minibuffer.

;;    To implement note previews, evaluate:
;;    (setq octavo-select-file-function 'octavo-consult-select-file)

;;    NOTE: The list of functions for which previews will be shown can be
;;    customized by amending the functions listed in the variable
;;    `octavo-consult-preview-functions'.

;; To load this package, put `octavo-consult.el' into your load path, load
;; Consult, and evaluate the following:

;; (with-eval-after-load 'consult
;;   (with-eval-after-load 'octavo
;;     (require 'octavo-consult)))

;;; Code:

(require 'octavo)
(require 'consult)

;;; Customizations

(defcustom octavo-consult-preview-functions
  '(octavo-find-file
    octavo-find-file-by-full-text-search
    octavo-current-notes
    octavo-links-in-note
    octavo-insert-link
    octavo-embark-save-reference
    octavo-backlinks
    octavo-unlinked-notes)
  "List of functions for which previews should be rendered."
  :group 'octavo
  :type '(repeat function))

;;; Consult-Grep Functions

(defun octavo-consult-grep (&optional initial)
  "Search `octavo-directory' with `consult-grep'.
With option for INITIAL input when called non-interactively."
  (interactive)
  (let ((consult--grep-history octavo-search-history))
    (if initial
        (consult-grep octavo-directory (format "%s" initial))
      (consult-grep octavo-directory))))

(defun octavo-consult-grep-tag-search (tag)
  "Search for TAG in `octavo-directory' using `consult-grep'.
Select TAG, with completion, from list of all tags in octavo notes."
  (interactive (list (completing-read "Find tag: " (octavo--grep-tag-list))))
  (consult-grep octavo-directory tag))

;;; Current Notes Consult Source

(defvar octavo-consult-source
  `(:name "octavo"
          :narrow (?z . "octavo - current notes")
          :hidden n
          :category buffer
          :history 'octavo-file-history
          :state 'consult--buffer-state
          :items ,(lambda ()
                    (remq nil
                        (mapcar
                         (lambda (x)
                           (when
                               (and (buffer-file-name x)
                                    (octavo-file-p (buffer-file-name x)))
                             (buffer-name x)))
                         (buffer-list))))))

(defun octavo-consult-current-notes ()
  "Select a currently open note using `consult-buffer'.
To use, set the variable `octavo-current-notes-function' to the
name of this function."
  (minibuffer-with-setup-hook
      '(lambda ()
         (setq unread-command-events
               (append unread-command-events (list ?z 32))))
    (consult-buffer)))

;;; Consult Select File with Preview

(defun octavo-consult-select-file (&optional prompt files group sort)
  "Select an Octavo from FILES, offering the PROMPT.
If FILES is not specified, get candidates from
`octavo--directory-files'. GROUP is passed to
`consult-read'. If SORT is non-nil, assume that FILES is a
sorted list."
  (let* ((files (or files (octavo--directory-files 'full)))
         (prompt (or prompt "Select Octavo: ")))
    (consult--read
     files
     :prompt prompt
     :sort (null sort)
     :require-match t
     :group (or group 'octavo-group-function)
     :category 'octavo-file
     :state (consult--file-preview)
     :preview-key (octavo-consult--preview-functions)
     :history 'octavo-file-history)))

(defun octavo-consult--preview-functions ()
  "Set `consult-preview-key' for specified functions."
  (when (member this-command octavo-consult-preview-functions)
    consult-preview-key))

(provide 'octavo-consult)

;;; octavo-consult.el ends here
