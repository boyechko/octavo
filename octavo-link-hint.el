;;; octavo-link-hint.el --- Link-Hint integration for Octavo  -*- lexical-binding: t; -*-

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

;; This package provides integration between link-hint.el and octavo. To use,
;; arrange for it to be loaded once both of those are loaded:

;; (with-eval-after-load 'link-hint
;;   (with-eval-after-load 'octavo
;;     (require 'octavo-link-hint)))

;;; Code:

(require 'octavo)
(require 'link-hint)

(defun octavo-link-hint--octavo-link-at-point-p ()
  "Return the ID for the octavo-link at the point or nil."
  (and (octavo--id-at-point)
       (thing-at-point-looking-at (octavo-link-regexp))))

(defun octavo-link-hint--next-octavo-link (bound)
  "Find the next octavo-link.
Only search the range between just after the point and BOUND."
  (link-hint--next-regexp octavo-id-regexp bound))

(link-hint-define-type 'octavo-link
  :next #'octavo-link-hint--next-octavo-link
  :at-point-p #'octavo-link-hint--octavo-link-at-point-p
  :open #'octavo-follow-link-at-point
  :copy #'kill-new)

(push 'link-hint-octavo-link link-hint-types)


;;; link-hint-aw-select support

(with-eval-after-load 'link-hint-aw-select

  (link-hint-define-type 'octavo-link
    :aw-select #'link-hint--aw-select-octavo-link)

  (defun link-hint--aw-select-octavo-link (id)
    (with-demoted-errors "%s"
      (if (> (length (aw-window-list)) 1)
          (let ((window (aw-select nil))
                (buffer (current-buffer))
                (new-buffer))
            (octavo-follow-link-at-point id)
            (setq new-buffer
                  (current-buffer))
            (switch-to-buffer buffer)
            (aw-switch-to-window window)
            (switch-to-buffer new-buffer))
        (link-hint-open-link-at-point))))

  ;; add exception for octavo-index buttons
  (defun link-hint--aw-select-button (_link)
    (with-demoted-errors "%s"
      (if (> (length (aw-window-list)) 1)
          (let ((window (aw-select nil))
                (buffer (current-buffer))
                (new-buffer))
            (if (re-search-forward octavo-id-regexp (line-end-position))
                (octavo-follow-link-at-point (match-string-no-properties 0))
              (push-button))
            (setq new-buffer
                  (current-buffer))
            (switch-to-buffer buffer)
            (aw-switch-to-window window)
            (switch-to-buffer new-buffer))
        (link-hint-open-link-at-point)))))

;;; link-hint-preview support

(with-eval-after-load 'link-hint-preview

  (link-hint-define-type 'octavo-link
    :preview #'link-hint-preview-octavo-link)

  (defun link-hint-preview-octavo-link (&optional id)
    "Pop up a frame containing octavo-file for ID at point.
Set pop-up frame parameters in 'link-hint-preview-frame-parameters'."
    (interactive)
    (let* ((id (or (octavo--id-at-point)
                   (octavo-index--button-at-point-p)))
           (file (octavo--parse-id 'file-path id))
           (buffer (get-file-buffer file))
           (frame (selected-frame)))
      (if (get-file-buffer file)
          (setq link-hint-preview--kill-last nil)
        (setq buffer (octavo-find-file file 'noselect))
        (setq link-hint-preview--kill-last t))
      (display-buffer-pop-up-frame
       buffer
       `((pop-up-frame-parameters . ,(link-hint-preview--params 'delete-before frame))
         (dedicated . t)))
      (with-current-buffer buffer
        (setq-local link-hint-preview--origin-frame frame)
        (link-hint-preview-mode))))

  (defalias 'octavo-preview 'link-hint-preview-octavo-link)

  (link-hint-define-type 'button
    :preview #'link-hint-preview-button)

  ;; add exception for octavo-index buttons
  (defun link-hint-preview-button ()
    (interactive)
    (let ((buffer (current-buffer))
          (frame (selected-frame))
          (new-buffer))
      (if-let (id (octavo-index--button-at-point-p))
          (progn
            (if (get-file-buffer (octavo--parse-id 'file-path id))
                (setq link-hint-preview--kill-last nil)
              (setq link-hint-preview--kill-last t))
            (octavo-follow-link-at-point id))
        (push-button))
      (setq new-buffer
            (current-buffer))
      (switch-to-buffer buffer)
      (display-buffer-pop-up-frame
       new-buffer
       `((pop-up-frame-parameters . ,(link-hint-preview--params 'delete-before frame))
         (dedicated . t)))
      (with-current-buffer new-buffer
        (setq-local link-hint-preview--origin-frame frame)
        (link-hint-preview-mode))))
  )

(provide 'octavo-link-hint)

;;; octavo-link-hint.el ends here
