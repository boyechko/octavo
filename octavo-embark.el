;;; octavo-embark.el --- Embark integration for Octavo -*- lexical-binding: t -*-

;; Copyright (C) 2024 Richard Boyechko

;; Author: Richard Boyechko <code@diachronic.net>
;; Created: 2024-01-01
;; Version: 0.1
;; Package-Requires: ((emacs 28.2) (octavo 0.1) (embark))
;; Keywords: none
;; URL: https://github.com/boyechko/

;; Created by Richard Boyechko based on zk.el by Grant Rosson
;; Original Author: Grant Rosson <https://github.com/localauthor>
;; Forked from: https://github.com/localauthor/zk
;; Original Creation Date: January 4, 2022
;; Copyright (C) 2022-2023 Grant Rosson

;; This file is not part of Emacs

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

;; Add (octavo-setup-embark) to your init config.

;;; Code:

(require 'octavo)
(require 'embark)

(defvar octavo-id-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'octavo-follow-link-at-point)
    (define-key map (kbd "k") #'octavo-embark-save-reference)
    (define-key map (kbd "s") #'octavo-search)
    map)
  "Keymap for Embark octavo-id at-point actions.")

(defvar octavo-file-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") #'octavo-insert-link)
    (define-key map (kbd "f") #'octavo-find-file)
    (define-key map (kbd "k") #'octavo-embark-save-reference)
    map)
  "Keymap for Embark octavo-file minibuffer actions.")

;;;###autoload
(defun octavo-embark-save-reference (file-or-id)
  "Save link and title for FILE-OR-ID."
  (interactive (list (octavo-select-file "Copy link: ")))
  (let ((links (octavo--formatted-string file-or-id octavo-link-and-title-format)))
    (kill-new links)
    (message "Copied: %s" links)))

;;;###autoload
(defun octavo-embark-target-octavo-id-at-point ()
  "Target octavo-id at point."
  (when (thing-at-point-looking-at octavo-id-regexp)
    (let ((octavo-id (match-string-no-properties 0)))
      `(octavo-id ,octavo-id . ,(bounds-of-thing-at-point 'symbol)))))

;;;###autoload
(defun octavo-setup-embark ()
  "Setup Embark integration for Octavo.
Adds octavo-id as an Embark target, and adds `octavo-id-map' and
`octavo-file-map' to `embark-keymap-alist'."
  (with-eval-after-load 'embark
    (add-to-list 'embark-multitarget-actions 'octavo-embark-save-reference)
    (add-to-list 'embark-multitarget-actions 'octavo-insert-link)
    (add-to-list 'embark-target-finders 'octavo-embark-target-octavo-id-at-point)
    (add-to-list 'embark-keymap-alist '(octavo-id . octavo-id-map))
    (add-to-list 'embark-keymap-alist '(octavo-file . octavo-file-map))
    (set-keymap-parent octavo-id-map embark-general-map)
    (set-keymap-parent octavo-file-map embark-file-map)))

(provide 'octavo-embark)
;;; octavo-embark.el ends here
