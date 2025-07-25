;;; octavo-index.el --- Index for Octavo   -*- lexical-binding: t; -*-

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

;; Octavo-Index: A sortable, searchable, narrowable, semi-persistent selection of
;; notes, in the form of clickable links.

;; To enable integration with Embark, include '(octavo-index-setup-embark)' in
;; your init config.

;;; Code:

(require 'octavo)
(require 'hl-line)

;;; Custom Variables

(defgroup octavo-index nil
  "Index interface for Octavo."
  :group 'text
  :group 'files
  :prefix "octavo-index")

(defcustom octavo-index-buffer-name "*Octavo-Index*"
  "Name for Octavo-Index buffer."
  :type 'string)

(defcustom octavo-index-format-function 'octavo-index--format-candidates
  "Default formatting function for Octavo-Index candidates.
See `octavo-index--format-candidates' for details."
  :type 'function)

(defcustom octavo-index-invisible-ids t
  "If non-nil, IDs will not be visible in the index."
  :type 'boolean)

(defcustom octavo-index-format "%t %i"
  "Default format for candidates in the index."
  :type 'string)

(defcustom octavo-index-prefix "-> "
  "String to prepend to note names in Octavo-Index."
  :type 'string)

(defcustom octavo-index-auto-scroll t
  "Enable automatically showing note at point in Octavo-Index."
  :type 'boolean)

(defcustom octavo-index-view-hide-cursor t
  "Hide cursor in `octavo-index-view-mode'."
  :type 'boolean)

(defcustom octavo-index-display-buffer-function 'octavo-index-display-buffer
  "Function that shows the buffer of selected Octavo-Index candidate.
It should take one argument, the BUFFER associated with a
octavo-file. See `octavo-index-display-buffer' for an example."
  :type 'function)
(make-obsolete 'octavo-index-button-display-function
               'octavo-index-display-buffer-function "0.9")

(defcustom octavo-index-help-echo octavo-index-format
  "Whether or how to display help-echo for the Octavo-Index buttons.
It can be NIL (inhibit help echo), a function taking one
argument, the FILE associated with the button, or a format
string appropriate for `octavo--format'."
  :type '(choice (string :tag "Format string")
                 (function :tag "Format function")
                 (const :tag "None" nil)))
(make-obsolete 'octavo-index-help-echo-function
               'octavo-index-help-echo "0.9")

;;; Octavo-Index Major Mode Settings

(defvar octavo-index--mode-line-orig nil
  "Value of `mode-line-misc-info' at the start of mode.")

(defvar octavo-index-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'octavo-index-next-line)
    (define-key map (kbd "p") #'octavo-index-previous-line)
    (define-key map (kbd "v") #'octavo-index-view-note)
    (define-key map (kbd "k") #'octavo-index-kill-buffer)
    (define-key map (kbd "o") #'other-window)
    (define-key map (kbd "f") #'octavo-index-focus)
    (define-key map (kbd "s") #'octavo-index-search)
    (define-key map (kbd "g") #'octavo-index-query-refresh)
    (define-key map (kbd "c") #'octavo-index-current-notes)
    (define-key map (kbd "i") #'octavo-index-refresh)
    (define-key map (kbd "S") #'octavo-index-sort-size)
    (define-key map (kbd "M") #'octavo-index-sort-modified)
    (define-key map (kbd "C") #'octavo-index-sort-created)
    (define-key map (kbd "RET") #'octavo-index-open-note)
    (define-key map (kbd "q") #'delete-window)
    (make-composed-keymap map tabulated-list-mode-map))
  "Keymap for Octavo-Index buffer.")

(define-derived-mode octavo-index-mode nil "Octavo-Index"
  "Mode for `octavo-index'.
\\{octavo-index-mode-map}"
  (setq octavo-index--mode-line-orig mode-line-misc-info)
  (octavo-index--set-mode-name)
  (read-only-mode)
  (hl-line-mode)
  (make-local-variable 'show-paren-mode)
  (setq-local show-paren-mode nil)
  (setq cursor-type nil))

;;; Declarations

(defvar octavo-index-query-mode-line nil)
(defvar octavo-index-query-terms nil)
(defvar octavo-search-history)

;;; Embark Integration

(defvar embark-multitarget-actions)
(defvar embark-target-finders)
(defvar embark-exporters-alist)
(defvar octavo-file-map)
(defvar octavo-id-map)

(defun octavo-index-setup-embark ()
  "Setup Embark integration for `octavo-index'."
  (with-eval-after-load 'embark
    (add-to-list 'embark-multitarget-actions 'octavo-index)
    (add-to-list 'embark-multitarget-actions 'octavo-embark-save-reference)
    (add-to-list 'embark-multitarget-actions 'octavo-follow-link-at-point)
    (add-to-list 'embark-multitarget-actions 'octavo-index-insert-link)
    (add-to-list 'embark-multitarget-actions 'octavo-index-narrow)
    (add-to-list 'embark-target-finders 'octavo-index-embark-target)
    (add-to-list 'embark-exporters-alist '(octavo-file . octavo-index-narrow))
    (add-to-list 'embark-exporters-alist '(octavo-id . octavo-index-narrow))
    (define-key octavo-file-map (kbd "n")  #'octavo-index-narrow)
    (define-key octavo-id-map (kbd "n") #'octavo-index-narrow)
    (define-key octavo-id-map (kbd "i") #'octavo-index-insert-link)))

(defun octavo-index-embark-target ()
  "Target octavo-id of button at point in Octavo-Index."
  (when (octavo-index--button-at-point-p)
    (save-excursion
      (beginning-of-line)
      (re-search-forward octavo-id-regexp (line-end-position)))
    (let ((octavo-id (match-string-no-properties 0)))
      `(octavo-id ,octavo-id . ,(cons (line-beginning-position) (line-end-position))))))

(defun octavo-index-narrow (arg)
  "Produce a Octavo-Index narrowed to notes listed in ARG.
For details of ARG see `octavo--processor'. When called on items
selected by `embark-select', narrows index to selected
candidates. Alternatively, `embark-export' exports candidates to
a new index."
  (let ((files (octavo--processor arg)))
    (octavo-index-refresh files)
    (octavo-index--query-reset)))

;;; Formatting

(defun octavo-index--format-candidates (files &optional format)
  "Return a list of Octavo-Index candidates based on FILES and FORMAT.
Each candidate is an alist with keys 'FILE and 'LABEL, the
latter being the return value of `octavo--format' called with
FORMAT (defaults to `octavo-index-format'). FILES must be a list
of filepaths. This is the default `octavo-index-format-function'."
  (let* ((format (or format octavo-index-format))
         output)
    (dolist (file files output)
      (when-let* ((id-title (octavo--parse-file (or file "")))
                  (id (if octavo-index-invisible-ids
                          (propertize (car id-title) 'invisible t)
                        (car id-title)))
                  (title (cdr id-title)))
        (push `((file . ,file)
                (label . ,(octavo--format format id title)))
              output)))))

;;; Low-Level Functions

(defun octavo-index--set-mode-name (&optional string)
  "Set mode name in Octavo Index buffer.
If STRING is given, add it to the `mode-name', otherwise
reset to \"Octavo-Index\"."
  (when (eq major-mode 'octavo-index-mode)
    (setq-local mode-name
                (if (stringp string)
                    (concat mode-name string)
                  "Octavo-Index"))
    (force-mode-line-update)))

(defun octavo-index--set-mode-line (string)
  "Set `mode-line-misc-info' to STRING in Octavo Index buffer."
  (when (eq major-mode 'octavo-index-mode)
    (setq-local mode-line-misc-info string)
    (force-mode-line-update)))

;;; Main Stack

;;;###autoload
(defun octavo-index (&optional buf-name)
  "Open Octavo-Index.
If BUF-NAME is specified, either switch or initialize a
buffer with that name."
  (interactive)
  (let* ((buf-name (or buf-name octavo-index-buffer-name))
         ;; TODO Make ALL-FRAMES argument to `get-buffer-window' customizable?
         (win (get-buffer-window buf-name nil))) ; == check only current frame
    (if win
        (select-window win)
      (octavo-index-refresh nil nil nil buf-name)
      (pop-to-buffer buf-name))))

(defvar octavo-index-last-sort-method nil
  "The last-used sort method (e.g. `octavo-index--sort-modified').")
(defvar octavo-index-last-sort-descending nil
  "Whether the last sorting operation was in descending order.")
(defvar octavo-index-last-format-function nil
  "The last-used format function.")

(defun octavo-index-refresh (&optional files format-fn sort-method buf-name)
  "Refresh the Octavo-Index.
Optionally refresh with FILES, using FORMAT-FN, SORT-METHOD, BUF-NAME."
  (interactive)
  (let ((inhibit-read-only t)
        (files (or files (octavo--directory-files 'full nil 'refresh)))
        (buf-name (or buf-name octavo-index-buffer-name)))
    (setq-local default-directory (expand-file-name octavo-directory))
    (unless (get-buffer buf-name)
      (when octavo-default-backlink
        (unless (octavo-file-p)
          (octavo-find-file-by-id octavo-default-backlink)))
      (generate-new-buffer buf-name))
    (with-current-buffer buf-name
      (let ((line (line-number-at-pos)))
        (setq-local truncate-lines t)
        (erase-buffer)
        (octavo-index--populate-index files format-fn sort-method)
        (goto-char (point-min))
        (unless (octavo-index-narrowed-p buf-name)
          (octavo-index--query-reset)
          (goto-line line))))))

(eval-and-compile
  (define-button-type 'octavo-index
    'follow-link t
    'id nil
    'file nil
    'title nil
    'button-data nil
    'action 'octavo-index--button-action
    'face 'default))

(defun octavo-index--populate-index (files &optional format-fn sort-method)
  "Populate the current buffer with Octavo-Index candidates.
FILES are sorted with SORT-METHOD (or `octavo-index--sort-modified')
and formatted with FORMAT-FN (or `octavo-index-format-function')."
  (let* ((sort-method (or sort-method
                          octavo-index-last-sort-method
                          'octavo-index--sort-modified))
         (sort-fn (funcall sort-method octavo-index-last-sort-descending))
         (format-fn (or format-fn octavo-index-format-function))
         (candidates (nreverse (funcall format-fn (funcall sort-fn files))))
         (count 0))
    (setq octavo-index-last-format-function format-fn
          octavo-index-last-sort-method sort-method)
    (dolist (cand candidates)
      (let-alist cand
        (let ((id-title (octavo--parse-file \.file)))
          (unless (zerop count) (insert "\n"))
          (insert-text-button \.label
                              'type 'octavo-index
                              'id (car id-title)
                              'file \.file
                              'title (cdr id-title)
                              'button-data \.file
                              'help-echo (when octavo-index-help-echo
                                           (octavo-index--help-echo \.file (cdr id-title))))
          (setq count (1+ count)))))
    (octavo-index-mode)
    (octavo-index--set-mode-name (format " [%d]" count))))

(defun octavo-index--help-echo (file label)
  "Generate help-echo when FILE's button (with LABEL) is at point.
The return depends on the value of `octavo-index-help-echo'."
  (cond ((functionp octavo-index-help-echo)
         (funcall octavo-index-help-echo file))
        ((equal octavo-index-help-echo octavo-index-format)
         label)
        ((stringp octavo-index-help-echo)
         (let ((id-title (octavo--parse-file file)))
           (octavo--format octavo-index-help-echo (car id-title) (cdr id-title))))
        (t
         nil)))

(defun octavo-index-display-buffer (buffer)
  "Display BUFFER corresponding to the pressed Octavo-Index button.
This is the default value of `octavo-index-display-buffer-function'."
  (pop-to-buffer
   buffer
   (if (one-window-p)
       (display-buffer-in-direction
        buffer '((direction . top) (window-height . 0.6)))
     (display-buffer-use-some-window
      buffer '((inhibit-same-window . t))))))

(defun octavo-index--button-action (file)
  "Action taken when `octavo-index' button is pressed.
FILE is the content of button's 'BUTTON-DATA property, which
for `octavo-index' button should be the filepath of the note
represented by the button."
  ;; This function merely passes the buck to `octavo-index-display-buffer-function',
  ;; which should do the actual displaying. Maintaining this extra layer of
  ;; indirection allows adding other behavior in the future (e.g. keeping a
  ;; tally of all buttons pressed in a session).
  (funcall octavo-index-display-buffer-function
           (octavo-find-file file 'noselect)))

(defun octavo-index-narrowed-p (buf-name)
  "Return t when index is narrowed in buffer BUF-NAME."
  (with-current-buffer (or buf-name
                           octavo-index-buffer-name)
    (if (< (count-lines (point-min) (point-max))
           (length (octavo--directory-files)))
        t nil)))

;;; Index Search and Focus Functions

;;;; Index Search
;; narrow index based on search of notes' full text

(defun octavo-index-search (regexp)
  "Narrow index based on REGEXP search of note contents."
  (interactive
   (list (read-string "Search: " nil 'octavo-search-history)))
  (if (eq major-mode 'octavo-index-mode)
      (octavo-index-refresh (octavo-index-query-files regexp 'search)
                            octavo-index-last-format-function
                            octavo-index-last-sort-method
                            (buffer-name))
    (user-error "Not in an Octavo-Index buffer")))

;;;; Index Focus
;; narrow index based on search of note titles (case sensitive)
;; an alternative to `consult-focus-lines'

(defun octavo-index-focus (regexp)
  "Narrow index based on REGEXP search of note titles."
  (interactive
   (list (read-string "Focus: " nil 'octavo-search-history)))
  (if (eq major-mode 'octavo-index-mode)
      (octavo-index-refresh (octavo-index-query-files regexp 'focus)
                        octavo-index-last-format-function
                        octavo-index-last-sort-method
                        (buffer-name))
    (user-error "Not in a Octavo-Index")))

;;;; Low-level Query Functions

(defvar octavo-index-query-terms nil
  "Ordered list of current query terms.
Takes form of (QUERY-TYPE . REGEXP), where QUERY-TYPE is
`FOCUS or `SEARCH, and REGEXP is the query string. Recent
items listed first.")

(defun octavo-index-query-files (regexp query-type)
  "Return narrowed list of Octavo-Files matching REGEXP.
QUERY-TYPE can be either `FOCUS (filename only) or
`SEARCH (full text)."
  (let* ((scope (when (octavo-index-narrowed-p (buffer-name))
                  (octavo-index--current-file-list)))
         (files (pcase query-type
                  ('focus (octavo--directory-files 'full regexp))
                  ('search (octavo--grep-file-list regexp))
                  (_ (error "Unknown query type: `%s'" query-type))))
         (files (if (null scope)
                    files
                  (cl-intersection scope files
                                   :test #'string=
                                   :key #'octavo--file-id))))
    (add-to-history 'octavo-search-history regexp)
    (when files
      (let ((mode-line (octavo-index-query-mode-line query-type regexp)))
        (setq octavo-index-query-mode-line mode-line)
        (octavo-index--set-mode-line mode-line)
        (octavo-index--set-mode-name)))
    (or files
        (user-error "No matches for \"%s\"" regexp))))

(defun octavo-index-query-refresh ()
  "Refresh narrowed index, based on last focus or search query."
  (interactive)
  (let ((mode mode-name)
        (files (octavo-index--current-file-list)))
    (unless (stringp files)
      (octavo-index-refresh files
                            nil
                            octavo-index-last-sort-method)
      (setq mode-name mode))))

(defun octavo-index-query-mode-line (query-type regexp)
  "Generate new mode line after search query.
QUERY-TYPE is either 'focus or 'search, with query term REGEXP."
  (push (cons query-type regexp) octavo-index-query-terms)
  ;; Sort the different terms into two lists
  (let* ((focused (cl-remove 'search octavo-index-query-terms :key 'car))
         (searched (cl-remove 'focus octavo-index-query-terms :key 'car))
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

(defun octavo-index--query-reset ()
  "Reset query parameters in the current Octavo Index buffer."
  (setq-local mode-line-misc-info octavo-index--mode-line-orig)
  (setq octavo-index-query-mode-line nil
        octavo-index-query-terms nil))

(defun octavo-index--current-id-list (buf-name &optional beg end)
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
          (while (re-search-forward octavo-id-regexp end t)
            (push (match-string-no-properties 0) ids))))
      (nreverse ids))))

;;; Index Sort Functions

(defun octavo-index--sort (sort-fn sort-name)
  "Sort the index using SORT-FN and update mode name with SORT-NAME.
SORT-FN is a function that takes a list of files and returns a sorted list.
SORT-NAME is a string describing the sort type."
  (unless (eq major-mode 'octavo-index-mode)
    (user-error "Not in an Octavo-Index buffer"))
  ;; TODO: A way to toggle descending sort only if the sort method is the same?
  (setq octavo-index-last-sort-descending
    (not octavo-index-last-sort-descending))
  (setq octavo-index-last-sort-name
    (format " by %s%s"
            sort-name
            (if octavo-index-last-sort-descending
                " (desc)"
              "")))
  (octavo-index-refresh (octavo-index--current-file-list)
                        octavo-index-last-format-function
                        sort-fn
                        (buffer-name))
  (octavo-index--set-mode-name octavo-index-last-sort-name))

(defun octavo-index-sort-modified ()
  "Sort index by last modified time."
  (interactive)
  (octavo-index--sort #'octavo-index--sort-modified "modified"))

(defun octavo-index-sort-created ()
  "Sort index by date created."
  (interactive)
  (octavo-index--sort #'octavo-index--sort-created "created"))

(defun octavo-index-sort-size ()
  "Sort index by size."
  (interactive)
  (octavo-index--sort #'octavo-index--sort-size "size"))

(defun octavo-index--current-file-list ()
  "Return a list of files in the current Octavo-Index."
  (save-excursion
    (let (files
          button)
      (goto-char (point-min))
      (while (and (setq button (forward-button 0 nil nil 'noerror))
                  (zerop (forward-line)))
        (when (eq (button-type button) 'octavo-index)
          (push (button-get button 'button-data) files)))
      files)))

(defun octavo-index--sort-function (key-fn comparator-fn)
  "Return a sort function accepting a list of files.
KEY-FN is a function that computes the sorting key for each file.
COMPARATOR-FN is a function that compares two keys."
  (lambda (files)
    (let ((ht (make-hash-table :test #'equal :size 5000)))
      (dolist (file files)
        (puthash file (funcall key-fn file) ht))
      (sort files
            (lambda (a b)
              (let ((ka (gethash a ht))
                    (kb (gethash b ht)))
                (funcall comparator-fn ka kb)))))))

(defun octavo--complement (func)
  "Return a function that is a complement of FUNC."
  (lambda (&rest args)
    (format "Complement of `%s'." (symbol-name func))
    (not (apply func args))))

;; Define sort methods

(defun octavo-index--sort-created (&optional descending)
  "Return a function to sort alphabetically by filename.
If DESCENDING is non-nil, sort in descending order."
  (octavo-index--sort-function (lambda (file) (car (octavo--parse-file file)))
                               (if descending #'string> #'string<)))

(defun octavo-index--sort-modified (&optional descending)
  "Return a function to sort by modification time.
If DESCENDING is non-nil, sort in descending order."
  (octavo-index--sort-function (lambda (file)
                                 (file-attribute-modification-time (file-attributes file)))
                               (if descending
                                   (octavo--complement #'time-less-p)
                                 #'time-less-p)))

(defun octavo-index--sort-size (&optional descending)
  "Return a function to sort by file size.
If DESCENDING is non-nil, sort in descending order."
  (octavo-index--sort-function (lambda (file)
                                 (file-attribute-size (file-attributes file)))
                               (if descending #'< #'>)))

;;; Octavo-Index Keymap Commands

(defun octavo-index-open-note ()
  "Open note."
  (interactive)
  (beginning-of-line)
  (push-button nil t))

(defvar-local octavo-index-view--kill nil)

(defun octavo-index-view-note ()
  "View note in `octavo-index-view-mode'."
  (interactive)
  (beginning-of-line)
  (let* ((button (octavo-index--button-at-point))
         (file (button-get button 'file))
         (kill (unless (get-file-buffer file)
                 t))
         (buffer (octavo-find-file file 'noselect)))
    (funcall octavo-index-display-buffer-function buffer)
    (setq-local octavo-index-view--kill kill)
    (octavo-index-view-mode)))

(defun octavo-index-kill-buffer ()
  "Kill any currently opened buffers for the note at point."
  (interactive)
  (when-let ((button (octavo-index--button-at-point))
             (line (line-number-at-pos)))
    (kill-buffer (get-file-buffer (button-get button 'file)))
    (octavo-index-current-notes)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun octavo-index-current-notes ()
  "Open Octavo-Index listing currently open notes."
  (interactive)
  (let ((buf-name "*Octavo-Index: Current*")
        (notes (octavo--current-notes-list)))
    (if (not notes)
        (user-error "There are no current notes")
      (octavo-index-refresh notes
                            octavo-index-last-format-function
                            octavo-index-last-sort-method
                            buf-name)
      (switch-to-buffer buf-name))))

(defun octavo-index-unlinked-notes ()
  "Open Octavo-Index listing unlinked notes."
  (interactive)
  (let ((buf-name "*Octavo-Index: Unlinked*")\
        (notes (octavo--unlinked-notes-list)))
    (if (not notes)
        (user-error "There are no unlinked notes")
      (octavo-index-refresh notes
                            octavo-index-last-format-function
                            octavo-index-last-sort-method
                            buf-name)
      (switch-to-buffer buf-name))))

(defun octavo-index--button-at-point (&optional pos)
  "Return the `octavo-index' button at point.
Takes an option POS position argument."
  (when-let* ((button (button-at (or pos (point))))
              (_ (button-has-type-p button 'octavo-index)))
    button))

(defun octavo-index--id-at-point (&optional pos)
  "Return octavo-ID when `octavo-index' button is at point.
Takes an option POS position argument."
  (when-let* ((button (button-at (or pos (point))))
              (_ (button-has-type-p button 'octavo-index)))
    (button-get button 'id)))

(defun octavo-index-insert-link (&optional id)
  "Insert octavo-link in `other-window' for button ID at point."
  (interactive (list (or (octavo-index--id-at-point)
                         (octavo--id-at-point))))
  (cond ((derived-mode-p 'octavo-index-mode)
         (with-selected-window (other-window-for-scrolling)
           (octavo-insert-link id)))
        ((octavo--id-at-point)
         (user-error "Move point off octavo-id before inserting"))
        (t
         (octavo-insert-link id))))

(defvar-local octavo-index-view--cursor nil)

(define-minor-mode octavo-index-view-mode
  "Minor mode for `octavo-index-auto-scroll'."
  :init-value nil
  :global nil
  :keymap '(((kbd "n") . octavo-index-next-line)
            ((kbd "p") . octavo-index-previous-line)
            ([remap read-only-mode] . octavo-index-view-mode)
            ((kbd "q") . quit-window))
  (if octavo-index-view-mode
      (progn
        (read-only-mode)
        (use-local-map octavo-index-mode-map)
        (when octavo-index-view-hide-cursor
          (progn
            (scroll-lock-mode 1)
            (setq-local octavo-index-view--cursor
                        cursor-type)
            (setq-local cursor-type nil))))
    (read-only-mode -1)
    (use-local-map nil)
    (when octavo-index-view-hide-cursor
      (scroll-lock-mode -1)
      (setq-local cursor-type (or octavo-index-view--cursor
                                  t)))))

(defun octavo-index-next-line ()
  "Move to next line.
If `octavo-index-auto-scroll' is non-nil, show note in other window."
  (interactive)
  (let ((split-width-threshold nil))
    (if octavo-index-auto-scroll
        (progn
          (cond ((not (octavo-file-p)))
                (octavo-index-view--kill
                 (kill-buffer)
                 (other-window -1))
                ((not octavo-index-view--kill)
                 (octavo-index-view-mode)
                 (other-window -1)))
          (forward-button 1)
          (hl-line-highlight)
          (unless (looking-at-p "[[:space:]]*$")
            (octavo-index-view-note)))
      (forward-button 1))))

(defun octavo-index-previous-line ()
  "Move to previous line.
If `octavo-index-auto-scroll' is non-nil, show note in other window."
  (interactive)
  (let ((split-width-threshold nil))
    (if octavo-index-auto-scroll
        (progn
          (cond ((not (octavo-file-p)))
                (octavo-index-view--kill
                 (kill-buffer)
                 (other-window -1))
                ((not octavo-index-view--kill)
                 (octavo-index-view-mode)
                 (other-window -1)))
          (forward-button -1)
          (hl-line-highlight)
          (unless (looking-at-p "[[:space:]]*$")
            (octavo-index-view-note)))
      (forward-button -1))))

;;;###autoload
(defun octavo-index-switch-to-index ()
  "Switch to Octavo-Index buffer."
  (interactive)
  (let ((buffer octavo-index-buffer-name))
    (unless (get-buffer buffer)
      (progn
        (generate-new-buffer buffer)
        (octavo-index-refresh)))
    (switch-to-buffer buffer)))


(provide 'octavo-index)

;;; octavo-index.el ends here
