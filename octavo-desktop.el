;;; octavo-desktop.el --- Desktop environment for Octavo -*- lexical-binding: t; -*-

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

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Octavo-Desktop: A place (or places) for collecting, grouping, arranging, and
;; saving curated selections of notes (also in the form of clickable links).

;; To enable integration with Embark, include '(octavo-desktop-setup-embark)' in
;; your init config.

;;; Code:

(require 'octavo)
(require 'octavo-index)

;;; Custom Variables

(defgroup octavo-desktop nil
  "Desktop interface for Octavo."
  :group 'text
  :group 'files
  :prefix "octavo-desktop")

(defcustom octavo-desktop-directory nil
  "Directory for saved Octavo-Desktops."
  :type 'directory)

(defcustom octavo-desktop-basename "*Octavo-Desktop:"
  "Basename for Octavo-Desktops.
The names of all Octavo-Desktops should begin with this string."
  :type 'string)

(defcustom octavo-desktop-entry-prefix ""
  "String to prepend to entries in Octavo-Desktop."
  :type 'string)

(defcustom octavo-desktop-entry-suffix ""
  "String to append to entries in Octavo-Desktop."
  :type 'string)

(defcustom octavo-desktop-entry-format "%t %i"
  "Format string for entries in Octavo-Desktop.
This is the part of each line in Octavo-Desktop buffer that
become buttons (see `octavo-desktop-make-buttons'); use
`octavo-desktop-entry-prefix' and `octavo-desktop-entry-suffix' to
add arbitary text around the entry, and which would not be
part of the buttons themselves.

See `octavo-format-function' and `octavo-format-id-and-title' for
valid control strings."
  :type 'string)

(defcustom octavo-desktop-make-buttons t
  "If non-nil, Octavo-Desktop will make buttons.
Possible values are t (make normal buttons), 'invisible
\(make buttons with invisible IDs), or nil (don't make any
buttons)."
  :type '(choice (const :tag "Yes" t)
                 (const :tag "Yes, with invisible IDs" invisible)
                 (const :tag "No" nil)))

(make-obsolete-variable 'octavo-desktop-invisible-ids 'octavo-desktop-make-buttons "0.6")

(defcustom octavo-desktop-mark-missing "<- ID NOT FOUND"
  "If non-nil, Octavo-Desktop will mark missing IDs.
Possible values are a string for the text of an overlay to
add at the end of lines with missing IDs, non-nil to merely
display their buttons with `octavo-desktop-missing-button' face,
or nil to eschew checking for missing IDs at all."
  :type '(choice (string :tag "Add overlay text" "<- ID NOT FOUND")
                 (const :tag "Propertize missing" t)
                 (const :tag "Do not mark" nil)))

(defun octavo-desktop-line-regexp ()
  "Return the regexp for the relevant Octavo-Desktop lines.
The value is computed from `octavo-desktop-entry-prefix',
`octavo-desktop-entry-suffix', `octavo-desktop-entry-format', and
`octavo-id-regexp'.

Group 1 is the note octavo-ID.
Group 2 is the note title.
Group 3 is the entire entry."
  (octavo--format (concat (regexp-quote octavo-desktop-entry-prefix)
                      "\\(?3:"
                      (regexp-quote octavo-desktop-entry-format)
                      "\\)"
                      (regexp-quote octavo-desktop-entry-suffix))
              (concat "\\(?1:" octavo-id-regexp "\\)")
              (concat "\\(?2:" ".*" "\\)"))) ; FIXME: `octavo-title-regexp' (PR #68)

(defcustom octavo-desktop-major-mode nil
  "Name of major-mode for Octavo-Desktop buffers.
The value should be a symbol that is a major mode command.
If nil, buffers will be in `fundamental-mode'."
  :type 'function)

(defcustom octavo-desktop-button-display-function 'octavo-desktop-button-display-action
  "Function called when buttons pressed in Octavo-Desktop.
The function is called by `octavo-desktop-button-action'. A custom
function must take two arguments, FILE and BUFFER respectively.
See the default function `octavo-desktop-button-display-action' for an
example."
  :type 'function)

(defcustom octavo-desktop-help-echo-function 'octavo-desktop-help-echo
  "Default help-echo function for Octavo-Index buttons.
Set to nil to inhibit help-echo."
  :type 'function)

(defcustom octavo-desktop-add-pos 'append
  "Behavior for placement of notes in Octavo-Desktop via `octavo-desktop-send-to-desktop'.

Options:
1. `append - Place notes at end of current Octavo-Desktop
2. `prepend - Place notes at beginning of current Octavo-Desktop
3. `at-point - Place notes at current point of current Octavo-Desktop

To quickly change this setting, call `octavo-desktop-add-toggle'."
  :type '(choice (const :tag "Append" append)
                 (const :tag "Prepend" prepend)
                 (const :tag "At point" at-point)))

(defface octavo-desktop-button
  '((t :inherit default))
  "Face used for buttons in `octavo-desktop-mode'.")

(defface octavo-desktop-missing-button
  '((t :inherit error))
  "Face used for buttons in `octavo-desktop-mode' with missing IDs.")

;;; Declarations

(defvar octavo-desktop-current nil
  "Buffer object of the current Octavo-Desktop.")

;;; Embark Integration

(defvar embark-multitarget-actions)
(defvar embark-target-finders)
(defvar embark-exporters-alist)

(defun octavo-desktop-setup-embark ()
  "Setup Embark integration for `octavo-desktop'."
  (with-eval-after-load 'embark
    (add-to-list 'embark-multitarget-actions 'octavo-desktop-send-to-desktop)
    (define-key octavo-file-map (kbd "d") #'octavo-desktop-send-to-desktop)
    (define-key octavo-id-map (kbd "d") #'octavo-desktop-send-to-desktop)))

;;; Octavo-Desktop Minor Mode Settings

(defvar octavo-desktop-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<up>") #'octavo-desktop-move-line-up)
    (define-key map (kbd "C-<down>") #'octavo-desktop-move-line-down)
    (define-key map [remap delete-char] #'octavo-desktop-delete-char)
    (define-key map [remap delete-backward-char] #'octavo-desktop-delete-backward-char)
    (define-key map [remap kill-region] #'octavo-desktop-kill-region)
    (define-key map [remap yank] #'octavo-desktop-yank)
    map)
  "Keymap for Octavo-Desktop buffers.")

(define-minor-mode octavo-desktop-mode
  "Minor mode for `octavo-desktop'."
  :lighter " Octavo-Desktop"
  :init-value nil
  :keymap octavo-desktop-map
  (cond (octavo-desktop-mode                ; enabled
         (when octavo-desktop-make-buttons
           (octavo-desktop-make-buttons))
         (when-let ((major-mode octavo-desktop-major-mode))
           (funcall major-mode))
         (setq octavo-desktop-mode t))
        (t                              ; disabled
         (octavo-desktop--clear))))

(eval-and-compile
  (defvar octavo-desktop-button-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-<up>") #'octavo-desktop-move-line-up)
      (define-key map (kbd "C-<down>") #'octavo-desktop-move-line-down)
      (define-key map [remap kill-line] #'octavo-desktop-kill-line)
      (define-key map [remap delete-char] #'octavo-desktop-delete-char)
      (define-key map [remap kill-region] #'octavo-desktop-kill-region)
      (define-key map (kbd "v") #'octavo-index-view-note)
      (define-key map (kbd "n") #'octavo-index-next-line)
      (define-key map (kbd "p") #'octavo-index-previous-line)
      (define-key map [remap self-insert-command] 'ignore)
      (set-keymap-parent map button-map)
      map)
    "Keymap for Octavo-Desktop buttons."))

(define-key octavo-index-view-mode-map (kbd "d") #'octavo-desktop-send-to-desktop)

;;; Octavo-Desktop

;;;###autoload
(defun octavo-desktop ()
  "Open Octavo-Desktop."
  (interactive)
  (let ((buffer (if (and octavo-desktop-current
                         (buffer-live-p (get-buffer octavo-desktop-current)))
                    octavo-desktop-current
                  (octavo-desktop-select)))
        (choice (unless (eq (current-buffer) octavo-desktop-current)
                  (read-char "Choice: \[s\]witch or \[p\]op-up?"))))
    (pcase choice
      ('?s (switch-to-buffer buffer))
      ('?p (pop-to-buffer buffer
                          '(display-buffer-at-bottom)))
      (_ nil))))


;;;###autoload
(defun octavo-desktop-select ()
  "Select a Octavo-Desktop to work with.
Return the buffer object visiting the selected or created
desktop."
  (interactive)
  (unless octavo-desktop-directory
    (error "Please set `octavo-desktop-directory' first"))
  (let* ((last-command last-command)
         (desktop
          (completing-read "Select or Create Octavo-Desktop: "
                           (directory-files octavo-desktop-directory
                                            nil
                                            (concat
                                             octavo-desktop-basename
                                             ".*"))
                           nil nil octavo-desktop-basename nil))
         (file (concat octavo-desktop-directory "/" desktop)))
    (setq octavo-desktop-current
      (if (file-exists-p (expand-file-name file))
          (octavo-find-file file 'noselect)
        (generate-new-buffer desktop)))
    (with-current-buffer octavo-desktop-current
      (setq require-final-newline 'visit-save)
      (unless (bound-and-true-p truncate-lines)
        (toggle-truncate-lines))
      (set-visited-file-name file t t)
      (octavo-desktop-mode)
      (save-buffer))
    (if (and (not (eq last-command 'octavo-desktop))
             (y-or-n-p (format "Visit %s? " octavo-desktop-current)))
        (switch-to-buffer octavo-desktop-current)
      (message "Desktop set to: %s" octavo-desktop-current)))
  octavo-desktop-current)

(eval-and-compile
  (define-button-type 'octavo-desktop
    'supertype 'octavo-index
    'read-only t
    'front-sticky t
    'rear-sticky t
    'button-data nil                    ; filled by `octavo-desktop--make-button'
    'keymap octavo-desktop-button-map
    'action 'octavo-desktop-button-action
    'face 'octavo-desktop-button
    'cursor-face 'highlight))

(defun octavo-desktop--make-button ()
  "Try to make a Octavo-Desktop button after point.
Return nil if there are no more buttons to be made in the
buffer. Otherwise, move point after the button created and
return a tuple of button boundaries."
  (save-match-data
    (when-let* ((beg        (point))
                (_          (re-search-forward (octavo-desktop-line-regexp) nil t))
                (id         (match-string-no-properties 1))
                (id-beg     (match-beginning 1))
                (id-end     (match-end 1))
                (title      (match-string-no-properties 2))
                (button-beg (match-beginning 3))
                (button-end (match-end 3)))
      (replace-match (save-match-data
                       (octavo--format octavo-desktop-entry-format id title))
                     nil t nil 3)
      (if (not (eq 'invisible octavo-desktop-make-buttons))
          ;; I.e. can add text in front of the button?
          (add-text-properties button-beg (1+ button-beg) '(front-sticky nil))
        ;; Make entire link invisible, not just the ID
        (goto-char beg)
        (when (re-search-forward (octavo-link-regexp) (line-end-position) t)
          (setq id-beg (match-beginning 0)
                id-end (match-end 0)))
        ;; I.e. can add text in the rear of invisible IDs, but not in the front?
        (add-text-properties id-beg id-end '(invisible t rear-nonsticky t))
        ;; Org-mode requires more drastic measures
        (when (eq octavo-desktop-major-mode 'org-mode)
          (let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
            (overlay-put overlay 'invisible t)
            (overlay-put overlay 'type 'octavo-desktop))))
      (make-text-button button-beg button-end
                        'type 'octavo-desktop
                        'button-data (list id title nil) ; matches `octavo--alist'
                        'help-echo octavo-desktop-help-echo-function)
      (goto-char button-end)
      (cons button-beg button-end))))

(defun octavo-desktop--clear ()
  "Clear special text properties added by `octavo-desktop-make-buttons'.
This removes buttons, overlays, and text properties from the
entire buffer."
  (save-excursion
    (let ((inhibit-read-only t))
      (remove-overlays (point-min) (point-max) 'type 'octavo-desktop)
      (set-text-properties (point-min) (point-max) '()))))

;;;###autoload
(defun octavo-desktop-make-buttons ()
  "Re-make buttons in Octavo-Desktop.
If `octavo-desktop-make-buttons' is nil, just clear any existing
buttons and overlays."
  (interactive)
  (unless (and (string-match-p octavo-desktop-basename (buffer-name))
               (file-in-directory-p default-directory octavo-desktop-directory))
    (user-error "Can only make buttons in Octavo desktop file; %s isn't"
                (buffer-name)))
  (let* ((inhibit-read-only t)
         (ids (if octavo-desktop-mark-missing
                  (octavo--id-list nil (octavo--alist))
                nil))
         button-bounds)
    (octavo-desktop--clear)
    (when octavo-desktop-make-buttons
      (save-excursion
        (goto-char (point-min))
        (while (setq button-bounds (octavo-desktop--make-button))
          (let* ((button-data (get-text-property (car button-bounds) 'button-data))
                 (button-id (car button-data)))
            (cond ((and (stringp octavo-desktop-mark-missing)
                        (not (member button-id ids)))
                   (let ((overlay (make-overlay (line-end-position) (line-end-position))))
                     (overlay-put overlay 'type 'octavo-desktop)
                     (overlay-put overlay 'before-string
                                  (propertize octavo-desktop-mark-missing
                                              'font-lock-face 'octavo-desktop-missing-button))))
                  ((and octavo-desktop-mark-missing
                        (not (member button-id ids)))
                   (add-text-properties (car button-bounds) (cdr button-bounds)
                                        '(face octavo-desktop-missing-button)))
                  (t
                   ;; do nothing
                   ))))))))

;;; Utilities

;; TODO Why both args?
(defun octavo-desktop-button-display-action (file buffer)
  "Function to display FILE or BUFFER on button press in Octavo-Desktop."
  (if (one-window-p)
      (pop-to-buffer buffer
                     (display-buffer-in-direction
                      buffer
                      '((direction . bottom)
                        (window-height . 0.5))))
    (octavo-find-file file 'other-window)))

(defun octavo-desktop-button-action (_)
  "Action taken when `octavo-desktop' button is pressed."
  (let* ((id (octavo-index--button-at-point-p))
         (file (octavo--parse-id 'file-path id))
         (buffer (octavo-find-file file 'noselect))) ; TODO Why?!
    (funcall octavo-desktop-button-display-function file buffer)))

(defun octavo-desktop-help-echo (win _obj pos)
  "Generate help-echo for `octavo-desktop' button in WIN at POS."
  (save-excursion
    (with-selected-window win
      (goto-char pos)
      (let* ((beg (+ (line-beginning-position)
                     (length octavo-desktop-entry-prefix)))
             (end (- (line-end-position)
                     (length octavo-desktop-entry-suffix)))
             (title (buffer-substring-no-properties beg end)))
        (format "%s" title)))))

;;; Commands

(defun octavo-desktop--gather-items (arg)
  "Normalize ARG into a list of files."
  (cond ((stringp arg)
         (octavo--formatter arg octavo-desktop-entry-format))
        ((eq major-mode 'octavo-index-mode)
         (let ((ids (if (use-region-p)
                        (octavo-index--current-id-list (current-buffer)
                                                   (region-beginning)
                                                   (region-end))
                      (octavo-index--current-id-list (current-buffer)
                                                 (line-beginning-position)
                                                 (line-end-position)))))
           (octavo--formatter ids octavo-desktop-entry-format)))
        ((octavo-file-p)
         (octavo--formatter buffer-file-name octavo-desktop-entry-format))
        (t (user-error "No item to send to desktop"))))

;;;###autoload
(defun octavo-desktop-send-to-desktop (&optional items prefix suffix)
  "Add ITEMS to the current Octavo-Desktop.
New entries are inserted according to `octavo-desktop-add-pos'.
In Octavo-Index, works on note at point or notes in active
region. Also works on files or group of files in minibuffer,
passed as ITEMS, and on Octavo-ID at point. With non-nil PREFIX
and/or SUFFIX, use those rather than values defined in
`octavo-desktop-entry-prefix' and `octavo-desktop-entry-suffix',
respectively.

See `octavo-desktop-entry-format' for the format of each line."
  (interactive)
  (unless octavo-desktop-directory
    (error "Please set `octavo-desktop-directory' first"))
  (let ((inhibit-read-only t)
        (items (octavo-desktop--gather-items items))
        (buffer (if (buffer-live-p octavo-desktop-current)
                    octavo-desktop-current
                  (octavo-desktop-select))))
    (with-current-buffer buffer
      (setq require-final-newline 'visit-save)
      (pcase octavo-desktop-add-pos
        ('append (goto-char (point-max))
                 (beginning-of-line)
                 (when (looking-at-p ".")
                   (end-of-line)
                   (newline)))
        ('prepend (goto-char (point-min)))
        ('at-point (goto-char (point))))
      (mapc (lambda (item)
              (insert (concat (or prefix octavo-desktop-entry-prefix)
                              item
                              (or suffix octavo-desktop-entry-suffix)
                              "\n")))
            items)
      (beginning-of-line)
      (unless (bound-and-true-p truncate-lines)
        (toggle-truncate-lines))
      (when octavo-desktop-make-buttons
        (octavo-desktop--make-button)))
    (if (eq major-mode 'octavo-index-mode)
        (message "Sent to %s - press D to switch" buffer)
      (message "Sent to %s" buffer))))

(defun octavo-desktop-add-toggle ()
  "Set `octavo-desktop-add-pos' interactively."
  (interactive)
  (let ((choice (read-char "Choice: \[a\]ppend; \[p\]repend; at-\[P\]oint")))
    (pcase choice
      ('?a (setq octavo-desktop-add-pos 'append))
      ('?p (setq octavo-desktop-add-pos 'prepend))
      ('?P (setq octavo-desktop-add-pos 'at-point)))))

;;;###autoload
(defun octavo-desktop-switch-to-desktop ()
  "Switch to Octavo-Desktop.
With prefix-argument, raise Octavo-Desktop in other frame."
  (interactive)
  (unless (and octavo-desktop-current
               (buffer-live-p (get-buffer octavo-desktop-current)))
    (octavo-desktop-select))
  (let ((buffer octavo-desktop-current))
    (if current-prefix-arg
        (if (get-buffer-window buffer 'visible)
            (display-buffer-pop-up-frame
             buffer
             ;; not general
             '((pop-up-frame-parameters . ((top . 80)
                                           (left . 850)
                                           (width . 80)
                                           (height . 35)))))
          (switch-to-buffer-other-frame buffer))
      (switch-to-buffer buffer))))


;;; Octavo-Desktop Keymap Commands

(defun octavo-desktop-move-line-down ()
  "Move line at point down in Octavo-Desktop buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)
    (when octavo-desktop-make-buttons
      (octavo-desktop-make-buttons))))

(defun octavo-desktop-move-line-up ()
  "Move line at point up in Octavo-Desktop buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (transpose-lines 1)
    (forward-line -2)
    (when octavo-desktop-make-buttons
      (octavo-desktop-make-buttons))))

(defun octavo-desktop-delete-region-maybe ()
  "Maybe delete region in `octavo-desktop-mode'."
  (cond ((and (not (use-region-p))
              (octavo-index--button-at-point-p))
         (delete-region (line-beginning-position)
                        (line-end-position)))
        ((and (use-region-p)
              (octavo-index--button-at-point-p (region-beginning))
              (not (octavo-index--button-at-point-p (region-end))))
         (delete-region (save-excursion
                          (goto-char (region-beginning))
                          (line-beginning-position))
                        (region-end))
         t)
        ((and (use-region-p)
              (not (octavo-index--button-at-point-p (region-beginning)))
              (octavo-index--button-at-point-p (region-end)))
         (delete-region (region-beginning)
                        (save-excursion
                          (goto-char (region-end))
                          (line-end-position)))
         t)
        ((and (use-region-p)
              (octavo-index--button-at-point-p (region-beginning))
              (octavo-index--button-at-point-p (region-end)))
         (delete-region
          (save-excursion
            (goto-char (region-beginning))
            (line-beginning-position))
          (save-excursion
            (goto-char (region-end))
            (line-end-position)))
         t)
        ((use-region-p)
         (delete-region (region-beginning)
                        (region-end))
         t)))

(defun octavo-desktop-delete-char ()
  "Wrapper around `delete-char' for `octavo-desktop-mode'."
  (interactive)
  (unless (and (and (looking-back octavo-id-regexp
                                  (line-beginning-position))
                    (looking-at "$"))
               (save-excursion
                 (beginning-of-line)
                 (octavo-index--button-at-point-p)))
    (let ((inhibit-read-only t))
      (unless (octavo-desktop-delete-region-maybe)
        (funcall #'delete-char (or current-prefix-arg 1))))))

(defun octavo-desktop-delete-backward-char ()
  "Wrapper around `delete-backward-char' for `octavo-desktop-mode'."
  (interactive)
  (unless (and (looking-back octavo-id-regexp
                             (line-beginning-position))
               (save-excursion
                 (beginning-of-line)
                 (octavo-index--button-at-point-p)))
    (let ((inhibit-read-only t))
      (unless (octavo-desktop-delete-region-maybe)
        (funcall #'delete-char (or current-prefix-arg -1))))))

(defun octavo-desktop-kill-line ()
  "Kill line in `octavo-desktop-mode'."
  (interactive)
  (let ((inhibit-read-only t))
    (if (not (octavo-index--button-at-point-p))
        (kill-line)
      (kill-region (line-beginning-position)
                   (line-end-position)))))

(defun octavo-desktop-kill-region ()
  "Wrapper around `kill-region' for `octavo-desktop-mode'."
  (interactive)
  (let ((inhibit-read-only t))
    (cond ((and (use-region-p)
                (octavo-index--button-at-point-p (region-beginning))
                (not (octavo-index--button-at-point-p (region-end))))
           (kill-region (save-excursion
                          (goto-char (region-beginning))
                          (line-beginning-position))
                        (region-end)))
          ((and (use-region-p)
                (not (octavo-index--button-at-point-p (region-beginning)))
                (octavo-index--button-at-point-p (region-end)))
           (kill-region (region-beginning)
                        (save-excursion
                          (goto-char (region-end))
                          (line-end-position))))
          ((and (use-region-p)
                (octavo-index--button-at-point-p (region-beginning))
                (octavo-index--button-at-point-p (region-end)))
           (kill-region
            (save-excursion
              (goto-char (region-beginning))
              (line-beginning-position))
            (save-excursion
              (goto-char (region-end))
              (line-end-position))))
          ((use-region-p)
           (kill-region (region-beginning)
                        (region-end))))))

(defun octavo-desktop-yank ()
  "Wrapper around `yank' for `octavo-desktop-mode'."
  (interactive)
  (let ((inhibit-read-only t))
    (yank)
    (when octavo-desktop-make-buttons
      (octavo-desktop-make-buttons))))

(provide 'octavo-desktop)

;;; octavo-desktop.el ends here
