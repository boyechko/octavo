;;; zk-index.el --- Index for zk   -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Grant Rosson

;; Author: Grant Rosson <https://github.com/localauthor>
;; Created: January 25, 2022
;; License: GPL-3.0-or-later
;; Version: 0.9
;; Homepage: https://github.com/localauthor/zk
;; Package-Requires: ((emacs "27.1")(zk "0.3"))

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

;; ZK-Index: A sortable, searchable, narrowable, semi-persistent selection of
;; notes, in the form of clickable links.

;; To enable integration with Embark, include '(zk-index-setup-embark)' in
;; your init config.

;;; Code:

(require 'zk)
(require 'hl-line)

;;; Custom Variables

(defgroup zk-index nil
  "Index interface for zk."
  :group 'text
  :group 'files
  :prefix "zk-index")

(defcustom zk-index-buffer-name "*ZK-Index*"
  "Name for ZK-Index buffer."
  :type 'string)

(defcustom zk-index-format-function 'zk-index--format-candidates
  "Default formatting function for ZK-Index candidates.
See `zk-index--format-candidates' for details."
  :type 'function)

(defcustom zk-index-invisible-ids t
  "If non-nil, IDs will not be visible in the index."
  :type 'boolean)

(defcustom zk-index-format "%t %i"
  "Default format for candidates in the index."
  :type 'string)

(defcustom zk-index-prefix "-> "
  "String to prepend to note names in ZK-Index."
  :type 'string)

(defcustom zk-index-auto-scroll t
  "Enable automatically showing note at point in ZK-Index."
  :type 'boolean)

(defcustom zk-index-view-hide-cursor t
  "Hide cursor in `zk-index-view-mode'."
  :type 'boolean)

(defcustom zk-index-display-buffer-function 'zk-index-display-buffer
  "Function that shows the buffer of selected Zk-Index candidate.
It should take one argument, the BUFFER associated with a
zk-file. See `zk-index-display-buffer' for an example."
  :type 'function)
(make-obsolete 'zk-index-button-display-function
               'zk-index-display-buffer-function "0.9")

(defcustom zk-index-help-echo zk-index-format
  "Whether or how to display help-echo for the Zk-Index buttons.
It can be NIL (inhibit help echo), a function taking one
argument, the FILE associated with the button, or a format
string appropriate for `zk--format'."
  :type '(choice (string :tag "Format string")
                 (function :tag "Format function")
                 (const :tag "None" nil)))
(make-obsolete 'zk-index-help-echo-function
               'zk-index-help-echo "0.9")

;;; ZK-Index Major Mode Settings

(defvar zk-index-mode-line-orig nil
  "Value of `mode-line-misc-info' at the start of mode.")

(defvar zk-index-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'zk-index-next-line)
    (define-key map (kbd "p") #'zk-index-previous-line)
    (define-key map (kbd "v") #'zk-index-view-note)
    (define-key map (kbd "o") #'other-window)
    (define-key map (kbd "f") #'zk-index-focus)
    (define-key map (kbd "s") #'zk-index-search)
    (define-key map (kbd "g") #'zk-index-query-refresh)
    (define-key map (kbd "c") #'zk-index-current-notes)
    (define-key map (kbd "i") #'zk-index-refresh)
    (define-key map (kbd "S") #'zk-index-sort-size)
    (define-key map (kbd "M") #'zk-index-sort-modified)
    (define-key map (kbd "C") #'zk-index-sort-created)
    (define-key map (kbd "RET") #'zk-index-open-note)
    (define-key map (kbd "q") #'delete-window)
    (make-composed-keymap map tabulated-list-mode-map))
  "Keymap for ZK-Index buffer.")

(define-derived-mode zk-index-mode nil "ZK-Index"
  "Mode for `zk-index'.
\\{zk-index-mode-map}"
  (setq zk-index-mode-line-orig mode-line-misc-info)
  (read-only-mode)
  (hl-line-mode)
  (make-local-variable 'show-paren-mode)
  (setq-local show-paren-mode nil)
  (setq cursor-type nil))


;;; Declarations

(defvar zk-index-last-sort-function nil)
(defvar zk-index-last-format-function nil)
(defvar zk-index-query-mode-line nil)
(defvar zk-index-query-terms nil)
(defvar zk-search-history)

(declare-function zk-file-p zk)
(declare-function zk--grep-id-list zk)


;;; Embark Integration

(defvar embark-multitarget-actions)
(defvar embark-target-finders)
(defvar embark-exporters-alist)

(defun zk-index-setup-embark ()
  "Setup Embark integration for `zk-index'."
  (with-eval-after-load 'embark
    (add-to-list 'embark-multitarget-actions 'zk-index)
    (add-to-list 'embark-multitarget-actions 'zk-copy-link-and-title)
    (add-to-list 'embark-multitarget-actions 'zk-follow-link-at-point)
    (add-to-list 'embark-multitarget-actions 'zk-index-insert-link)
    (add-to-list 'embark-multitarget-actions 'zk-index-narrow)
    (add-to-list 'embark-target-finders 'zk-index-embark-target)
    (add-to-list 'embark-exporters-alist '(zk-file . zk-index-narrow))
    (add-to-list 'embark-exporters-alist '(zk-id . zk-index-narrow))
    (define-key zk-file-map (kbd "n")  #'zk-index-narrow)
    (define-key zk-id-map (kbd "n") #'zk-index-narrow)
    (define-key zk-id-map (kbd "i") #'zk-index-insert-link)))

(defun zk-index-embark-target ()
  "Target zk-id of button at point in ZK-Index."
  (when (zk-index--button-at-point-p)
    (save-excursion
      (beginning-of-line)
      (re-search-forward zk-id-regexp (line-end-position)))
    (let ((zk-id (match-string-no-properties 0)))
      `(zk-id ,zk-id . ,(cons (line-beginning-position) (line-end-position))))))

(defun zk-index-narrow (arg)
  "Produce a ZK-Index narrowed to notes listed in ARG.
For details of ARG see `zk--processor'. When called on items
selected by `embark-select', narrows index to selected
candidates. Alternatively, `embark-export' exports candidates to
a new index."
  (let ((files (zk--processor arg)))
    (zk-index-refresh files)
    (zk-index--reset-mode-line)))

;;; Formatting

(defun zk-index--format-candidates (files &optional format)
  "Return a list of Zk-Index candidates based on FILES and FORMAT.
Each candidate is an alist with keys 'FILE and 'LABEL, the
latter being the return value of `zk--format' called with
FORMAT (defaults to `zk-index-format'). FILES must be a list
of filepaths. This is the default `zk-index-format-function'."
  (let* ((format (or format zk-index-format))
         output)
    (dolist (file files output)
      (when-let* ((id-title (zk--parse-file (or file "")))
                  (id (if zk-index-invisible-ids
                          (propertize (car id-title) 'invisible t)
                        (car id-title)))
                  (title (cdr id-title)))
        (push `((file . ,file)
                (label . ,(zk--format format id title)))
              output)))))

;;; Main Stack

;;;###autoload
(defun zk-index (&optional buf-name)
  "Open ZK-Index.
If BUF-NAME is specified, either switch or initialize a
buffer with that name."
  (interactive)
  (let* ((buf-name (or buf-name zk-index-buffer-name))
         ;; TODO Make ALL-FRAMES argument to `get-buffer-window' customizable?
         (win (get-buffer-window buf-name nil))) ; == check only current frame
    (unless (get-buffer zk-index-buffer-name)
      (zk-index-refresh nil nil nil buf-name))
    (if win
        (select-window win)
      (pop-to-buffer buf-name))))

(defun zk-index-refresh (&optional files format-fn sort-fn buf-name)
  "Refresh the Zk-Index.
Optionally refresh with FILES, using FORMAT-FN, SORT-FN, BUF-NAME."
  (interactive)
  (let ((inhibit-read-only t)
        (files (or files (zk--directory-files 'full nil 'refresh)))
        (buf-name (or buf-name zk-index-buffer-name)))
    (setq zk-index-last-format-function format-fn
          zk-index-last-sort-function sort-fn
          default-directory (expand-file-name zk-directory))
    (unless (get-buffer buf-name)
      (when zk-default-backlink
        (unless (zk-file-p)
          (zk-find-file-by-id zk-default-backlink)))
      (generate-new-buffer buf-name))
    (with-current-buffer buf-name
      (let ((line (line-number-at-pos)))
        (setq-local truncate-lines t)
        (erase-buffer)
        (zk-index-mode)
        (zk-index--reset-mode-name)
        (zk-index--populate-index files format-fn sort-fn)
        (goto-char (point-min))
        (unless (zk-index-narrowed-p buf-name)
          (zk-index--reset-mode-line)
          (goto-line line))))))

(eval-and-compile
  (define-button-type 'zk-index
    'follow-link t
    'button-data nil
    'action 'zk-index--button-action
    'face 'default))

(defun zk-index--populate-index (files &optional format-fn sort-fn)
  "Populate the current buffer with Zk-Index candidates.
FILES are sorted with SORT-FN (or `zk-index--sort-modified')
and formatted with FORMAT-FN (or `zk-index-format-function')."
  (let* ((sort-fn (or sort-fn 'zk-index--sort-modified))
         (format-fn (or format-fn zk-index-format-function))
         (candidates (nreverse (funcall format-fn (funcall sort-fn files))))
         (count 0))
    (dolist (alist candidates)
      (let-alist alist
        (unless (zerop count) (insert "\n"))
        (insert-text-button .label
                            'type 'zk-index
                            'button-data .file
                            'help-echo (when zk-index-help-echo
                                         (zk-index--help-echo .file .label)))
        (setq count (1+ count))))
    (zk-index--set-mode-name (format " [%d]" count))))

(defun zk-index--help-echo (file label)
  "Generate help-echo when FILE's button (with LABEL) is at point.
The return depends on the value of `zk-index-help-echo'."
  (cond ((functionp zk-index-help-echo)
         (funcall zk-index-help-echo file))
        ((equal zk-index-help-echo zk-index-format)
         label)
        ((stringp zk-index-help-echo)
         (let ((id-title (zk--parse-file file)))
           (zk--format zk-index-help-echo (car id-title) (cdr id-title))))
        (t
         nil)))

(defun zk-index-display-buffer (buffer)
  "Display BUFFER corresponding to the pressed Zk-Index button.
This is the default value of `zk-index-display-buffer-function'."
  (pop-to-buffer
   buffer
   (if (one-window-p)
       (display-buffer-in-direction
        buffer '((direction . top) (window-height . 0.6)))
     (display-buffer-use-some-window
      buffer '((inhibit-same-window . t))))))

(defun zk-index--button-action (file)
  "Action taken when `zk-index' button is pressed.
FILE is the content of button's 'BUTTON-DATA property, which
for `zk-index' button should be the filepath of the note
represented by the button."
  ;; This function merely passes the buck to `zk-index-display-buffer-function',
  ;; which should do the actual displaying. Maintaining this extra layer of
  ;; indirection allows adding other behavior in the future (e.g. keeping a
  ;; tally of all buttons pressed in a session).
  (funcall zk-index-display-buffer-function (find-file-noselect file)))

(defun zk-index-narrowed-p (buf-name)
  "Return t when index is narrowed in buffer BUF-NAME."
  (with-current-buffer (or buf-name
                           zk-index-buffer-name)
    (if (< (count-lines (point-min) (point-max))
           (length (zk--directory-files)))
        t nil)))

;;; Index Search and Focus Functions

;;;; Index Search
;; narrow index based on search of notes' full text

(defun zk-index-search (regexp)
  "Narrow index based on REGEXP search of note contents."
  (interactive
   (list (read-string "Search: " nil 'zk-search-history)))
  (if (eq major-mode 'zk-index-mode)
      (zk-index-refresh (zk-index-query-files regexp 'search)
                        zk-index-last-format-function
                        zk-index-last-sort-function
                        (buffer-name))
    (user-error "Not in a ZK-Index")))

;;;; Index Focus
;; narrow index based on search of note titles (case sensitive)
;; an alternative to `consult-focus-lines'

(defun zk-index-focus (regexp)
  "Narrow index based on REGEXP search of note titles."
  (interactive
   (list (read-string "Focus: " nil 'zk-search-history)))
  (if (eq major-mode 'zk-index-mode)
      (zk-index-refresh (zk-index-query-files regexp 'focus)
                        zk-index-last-format-function
                        zk-index-last-sort-function
                        (buffer-name))
    (user-error "Not in a ZK-Index")))

;;;; Low-level Query Functions

(defvar zk-index-query-terms nil
  "Ordered list of current query terms.
Takes form of (QUERY-TYPE . REGEXP), where QUERY-TYPE is
`FOCUS or `SEARCH, and REGEXP is the query string. Recent
items listed first.")

(defun zk-index-query-files (regexp query-type)
  "Return narrowed list of Zk-Files matching REGEXP.
QUERY-TYPE can be either `FOCUS (filename only) or
`SEARCH (full text)."
  (let* ((scope (when (zk-index-narrowed-p (buffer-name))
                  (zk-index--current-id-list (buffer-name))))
         (matches (pcase query-type
                    ('focus (zk--id-list regexp))
                    ('search (zk--grep-id-list regexp))
                    (_ (error "Unknown query type: `%s'" query-type))))
         (matches (if scope
                      (cl-intersection scope matches :test #'string=)
                    matches))
         (files (mapcar (lambda (id) (zk--parse-id 'file-path id)) matches)))
    (add-to-history 'zk-search-history regexp)
    (when files
      (let ((mode-line (zk-index-query-mode-line query-type regexp)))
        (setq zk-index-query-mode-line mode-line)
        (zk-index--set-mode-line mode-line)
        (zk-index--reset-mode-name)))
    (when (stringp files)
      (setq files (list files)))
    (or files
        (user-error "No matches for \"%s\"" regexp))))

(defun zk-index-query-refresh ()
  "Refresh narrowed index, based on last focus or search query."
  (interactive)
  (let ((mode mode-name)
        (files (zk-index--current-file-list)))
    (unless (stringp files)
      (zk-index-refresh files
                        nil
                        zk-index-last-sort-function)
      (setq mode-name mode))))

(defun zk-index-query-mode-line (query-type regexp)
  "Generate new mode line after search query.
QUERY-TYPE is either 'focus or 'search, with query term REGEXP."
  (push (cons query-type regexp) zk-index-query-terms)
  ;; Sort the different terms into two lists
  (let* ((focused (cl-remove 'search zk-index-query-terms :key 'car))
         (searched (cl-remove 'focus zk-index-query-terms :key 'car))
         (formatted (mapcar (lambda (term-list)
                              (when term-list
                                ;; (CMD . REGEXP)
                                (cons (caar term-list)
                                      (mapconcat #'cdr term-list "\" + \""))))
                            ;;      CAR     CDR
                            (list focused searched))))
    ;; Format each list and update appropriate list
    (concat "["
            (mapconcat (lambda (query)
                         (format "%s: \"%s"
                                 (capitalize (symbol-name (car query)))
                                 (cdr query)))
                       ;; Put the last query type at the end
                       (sort (remq nil formatted)
                             (lambda (a _b)
                               (not (equal (car a) query-type))))
                       "\" | ")
            "\"]")))

(defun zk-index--set-mode-line (string)
  "Add STRING to mode-line in `zk-index-mode'."
  (when (eq major-mode 'zk-index-mode)
    (setq-local mode-line-misc-info string)))

(defun zk-index--reset-mode-line ()
  "Reset mode-line in `zk-index-mode'."
  (setq-local mode-line-misc-info zk-index-mode-line-orig)
  (setq zk-index-query-mode-line nil
        zk-index-query-terms nil))

(defun zk-index--current-id-list (buf-name &optional beg end)
  "Return list of IDs for index in BUF-NAME.
If BEG and END are given, only return the IDs in the lines
between those positions, inclusive."
  (with-current-buffer buf-name
    (let ((beg (if (not beg)
                   (point-min)
                 (goto-char beg)
                 (line-beginning-position)))
          (end (if (not end)
                   (point-max)
                 (goto-char end)
                 (line-end-position)))
          ids)
      (save-excursion
        (goto-char beg)
        (save-match-data
          (while (re-search-forward zk-id-regexp end t)
            (push (match-string-no-properties 0) ids))))
      (nreverse ids))))

;;; Index Sort Functions

(defun zk-index-sort-modified ()
  "Sort index by last modified."
  (interactive)
  (if (eq major-mode 'zk-index-mode)
      (progn
        (zk-index-refresh (zk-index--current-file-list)
                          zk-index-last-format-function
                          #'zk-index--sort-modified
                          (buffer-name))
        (zk-index--set-mode-name " by modified"))
    (user-error "Not in a ZK-Index")))

(defun zk-index-sort-created ()
  "Sort index by date created."
  (interactive)
  (if (eq major-mode 'zk-index-mode)
      (progn
        (zk-index-refresh (zk-index--current-file-list)
                          zk-index-last-format-function
                          #'zk-index--sort-created
                          (buffer-name))
        (zk-index--set-mode-name " by created"))
    (user-error "Not in a ZK-Index")))

(defun zk-index-sort-size ()
  "Sort index by size."
  (interactive)
  (if (eq major-mode 'zk-index-mode)
      (progn
        (zk-index-refresh (zk-index--current-file-list)
                          zk-index-last-format-function
                          #'zk-index--sort-size
                          (buffer-name))
        (zk-index--set-mode-name " by size"))
    (user-error "Not in a ZK-Index")))

(defun zk-index--set-mode-name (string)
  "Add STRING to `mode-name' in `zk-index-mode'."
  (when (eq major-mode 'zk-index-mode)
    (setq mode-name (concat mode-name string))))

(defun zk-index--reset-mode-name ()
  "Reset `mode-name' in `zk-index-mode'."
  (setq mode-name "ZK-Index"))

(defun zk-index--current-file-list ()
  "Return list files in current index."
  (let* ((ids (zk-index--current-id-list (buffer-name)))
         (files (mapcar (lambda (id) (zk--parse-id 'file-path id)) ids)))
    (when files
      files)))

(defun zk-index--sort-created (files)
  "Sort FILES in ascending alphabetical order by ID."
  (let ((ht (make-hash-table :test #'equal :size 5000)))
    (mapc (lambda (x)
            (puthash x (car (zk--parse-file x)) ht))
          files)
    (sort files
          (lambda (a b)
            (let ((one (gethash a ht))
                  (two (gethash b ht)))
              (string< two one))))))

(defun zk-index--sort-modified (list)
  "Sort LIST for latest modification."
  (let ((ht (make-hash-table :test #'equal :size 5000)))
    (dolist (x list)
      (puthash x (file-attribute-modification-time (file-attributes x)) ht))
    (sort list
          (lambda (a b)
            (let ((one
                   (gethash a ht))
                  (two
                   (gethash b ht)))
              (time-less-p two one))))))

(defun zk-index--sort-size (list)
  "Sort LIST for latest modification."
  (sort list
        (lambda (a b)
          ;; `>' signals an error if it's passed a nil, but `file-attributes'
          ;; can return nil the file does not exist (or when passed a nil).
          (> (or (file-attribute-size (file-attributes a)) -1)
             (or (file-attribute-size (file-attributes b)) -1)))))

;;; ZK-Index Keymap Commands

(defun zk-index-open-note ()
  "Open note."
  (interactive)
  (beginning-of-line)
  (push-button nil t))

(defvar-local zk-index-view--kill nil)

(defun zk-index-view-note ()
  "View note in `zk-index-view-mode'."
  (interactive)
  (beginning-of-line)
  (let* ((id (zk-index--button-at-point-p))
         (file (zk--parse-id 'file-path id))
         (kill (unless (get-file-buffer file)
                 t))
         (buffer (find-file-noselect file)))
    (funcall zk-index-display-buffer-function buffer)
    (setq-local zk-index-view--kill kill)
    (zk-index-view-mode)))

(defun zk-index-current-notes ()
  "Open ZK-Index listing currently open notes."
  (interactive)
  (zk-index
   (zk--current-notes-list)
   zk-index-last-format-function
   zk-index-last-sort-function))

(defun zk-index--button-at-point-p (&optional pos)
  "Return zk-ID when `zk-index' button is at point.
Takes an option POS position argument."
  (when-let* ((button (button-at (or pos (point))))
              (_ (button-has-type-p button 'zk-index))
              (file (button-get button 'button-data)))
    (ezeka-zk-file-id file)))

(defun zk-index-insert-link (&optional id)
  "Insert zk-link in `other-window' for button ID at point."
  (interactive (list (or (zk--id-at-point)
                         (zk-index--button-at-point-p))))
  (cond ((derived-mode-p 'zk-index-mode)
         (with-selected-window (other-window-for-scrolling)
           (zk-insert-link id)))
        ((zk--id-at-point)
         (user-error "Move point off zk-id before inserting"))
        (t
         (zk-insert-link id))))

(defvar-local zk-index-view--cursor nil)

(define-minor-mode zk-index-view-mode
  "Minor mode for `zk-index-auto-scroll'."
  :init-value nil
  :global nil
  :keymap '(((kbd "n") . zk-index-next-line)
            ((kbd "p") . zk-index-previous-line)
            ([remap read-only-mode] . zk-index-view-mode)
            ((kbd "q") . quit-window))
  (if zk-index-view-mode
      (progn
        (read-only-mode)
        (use-local-map zk-index-mode-map)
        (when zk-index-view-hide-cursor
          (progn
            (scroll-lock-mode 1)
            (setq-local zk-index-view--cursor
                        cursor-type)
            (setq-local cursor-type nil))))
    (read-only-mode -1)
    (use-local-map nil)
    (when zk-index-view-hide-cursor
      (scroll-lock-mode -1)
      (setq-local cursor-type (or zk-index-view--cursor
                                  t)))))

(defun zk-index-next-line ()
  "Move to next line.
If `zk-index-auto-scroll' is non-nil, show note in other window."
  (interactive)
  (let ((split-width-threshold nil))
    (if zk-index-auto-scroll
        (progn
          (cond ((not (zk-file-p)))
                (zk-index-view--kill
                 (kill-buffer)
                 (other-window -1))
                ((not zk-index-view--kill)
                 (zk-index-view-mode)
                 (other-window -1)))
          (forward-button 1)
          (hl-line-highlight)
          (unless (looking-at-p "[[:space:]]*$")
            (zk-index-view-note)))
      (forward-button 1))))

(defun zk-index-previous-line ()
  "Move to previous line.
If `zk-index-auto-scroll' is non-nil, show note in other window."
  (interactive)
  (let ((split-width-threshold nil))
    (if zk-index-auto-scroll
        (progn
          (cond ((not (zk-file-p)))
                (zk-index-view--kill
                 (kill-buffer)
                 (other-window -1))
                ((not zk-index-view--kill)
                 (zk-index-view-mode)
                 (other-window -1)))
          (forward-button -1)
          (hl-line-highlight)
          (unless (looking-at-p "[[:space:]]*$")
            (zk-index-view-note)))
      (forward-button -1))))

;;;###autoload
(defun zk-index-switch-to-index ()
  "Switch to ZK-Index buffer."
  (interactive)
  (let ((buffer zk-index-buffer-name))
    (unless (get-buffer buffer)
      (progn
        (generate-new-buffer buffer)
        (zk-index-refresh)))
    (switch-to-buffer buffer)))


(provide 'zk-index)

;;; zk-index.el ends here
