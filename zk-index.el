;;; zk-index.el --- Index and Desktop for zk   -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Grant Rosson

;; Author: Grant Rosson <https://github.com/localauthor>
;; Created: January 25, 2022
;; License: GPL-3.0-or-later
;; Version: 0.7
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

;; Two interfaces for zk:

;; ZK-Index: A sortable, searchable, narrowable, semi-persistent selection of
;; notes, in the form of clickable links.

;; ZK-Desktop: A place (or places) for collecting, grouping, arranging, and
;; saving curated selections of notes (also in the form of clickable links).

;; To enable integration with Embark, include '(zk-index-setup-embark)' in
;; your init config.

;;; Code:

(require 'zk)
(require 'hl-line)

;;; Custom Variables

(defgroup zk-index nil
  "Index and Desktop interfaces for zk."
  :group 'text
  :group 'files
  :prefix "zk-index")

(defcustom zk-index-buffer-name "*ZK-Index*"
  "Name for ZK-Index buffer."
  :type 'string)

(defcustom zk-index-format-function 'zk-index--format-line
  "Default formatting function for listing notes in ZK-Index."
  :type 'function)

(defcustom zk-index-invisible-ids t
  "If non-nil, IDs will not be visible in the index."
  :type 'boolean)

(defcustom zk-index-format "%t [[%i]]"
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

(defcustom zk-index-button-display-function 'zk-index-button-display-action
  "Function called when buttons pressed in ZK-Index and ZK-Desktop.
The function is called by `zk-index-button-action'. A custom
function must take two arguments, FILE and BUFFER respectively.
See the default function `zk-index-button-display-action' for an
example."
  :type 'function)

(defcustom zk-index-desktop-directory nil
  "Directory for saved ZK-Desktops."
  :type 'directory)

(defcustom zk-index-desktop-basename "*ZK-Desktop:"
  "Basename for ZK-Desktops.
The names of all ZK-Desktops should begin with this string."
  :type 'string)

(defcustom zk-index-desktop-prefix ""
  "String to prepend to note names in ZK-Desktop."
  :type 'string)

(defcustom zk-index-desktop-major-mode nil
  "Name of major-mode for ZK-Desktop buffers.
The value should be a symbol that is a major mode command.
If nil, buffers will be in `fundamental-mode'."
  :type 'function)

(defcustom zk-index-desktop-add-pos 'append
  "Behavior for placement of notes in ZK-Desktop via `zk-index-send-to-desktop'.

Options:
1. `append - Place notes at end of current ZK-Desktop
2. `prepend - Place notes at beginning of current ZK-Desktop
3. `at-point - Place notes at current point of current ZK-Desktop

To quickly change this setting, call `zk-index-desktop-add-toggle'."
  :type '(choice (const :tag "Append" append)
                 (const :tag "Prepend" prepend)
                 (const :tag "At point" at-point)))

(defface zk-index-desktop-button
  '((t :inherit default))
  "Face used for buttons in `zk-index-desktop-mode'.")

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
    (define-key map (kbd "d") #'zk-index-send-to-desktop)
    (define-key map (kbd "D") #'zk-index-switch-to-desktop)
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
  (setq mode-name "ZK-Index")
  (setq zk-index-mode-line-orig mode-line-misc-info)
  (read-only-mode)
  (hl-line-mode)
  (make-local-variable 'show-paren-mode)
  (setq-local show-paren-mode nil)
  (setq cursor-type nil))

;;; ZK-Desktop Minor Mode Settings

(defvar zk-index-desktop-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<up>") #'zk-index-move-line-up)
    (define-key map (kbd "C-<down>") #'zk-index-move-line-down)
    (define-key map [remap delete-char] #'zk-index-desktop-delete-char)
    (define-key map [remap delete-backward-char] #'zk-index-desktop-delete-backward-char)
    (define-key map [remap kill-region] #'zk-index-desktop-kill-region)
    (define-key map [remap yank] #'zk-index-desktop-yank)
    map)
  "Keymap for ZK-Desktop buffers.")

(define-minor-mode zk-index-desktop-mode
  "Minor mode for `zk-index-desktop'."
  :init-value nil
  :keymap zk-index-desktop-map
  (zk-index-desktop-make-buttons)
  (when-let ((mode zk-index-desktop-major-mode))
    (funcall mode))
  ;;(setq truncate-lines t)
  (setq-local zk-index-desktop-mode t))

(defvar zk-index-desktop-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<up>") #'zk-index-move-line-up)
    (define-key map (kbd "C-<down>") #'zk-index-move-line-down)
    (define-key map [remap kill-line] #'zk-index-desktop-kill-line)
    (define-key map [remap delete-char] #'zk-index-desktop-delete-char)
    (define-key map [remap kill-region] #'zk-index-desktop-kill-region)
    (define-key map (kbd "v") #'zk-index-view-note)
    (define-key map (kbd "n") #'zk-index-next-line)
    (define-key map (kbd "p") #'zk-index-previous-line)
    (define-key map [remap self-insert-command] 'ignore)
    (set-keymap-parent map button-map)
    map)
  "Keymap for ZK-Desktop buttons.")

;;; Declarations

(defvar zk-index-last-sort-function nil)
(defvar zk-index-last-format-function nil)
(defvar zk-index-query-mode-line nil)
(defvar zk-index-query-terms nil)
(defvar zk-index-desktop-current nil)
(defvar zk-search-history)

(declare-function zk-file-p zk)
(declare-function zk--grep-id-list zk)


;;; Embark Integration

(defvar embark-multitarget-actions)
(defvar embark-target-finders)
(defvar embark-exporters-alist)

(defun zk-index-setup-embark ()
  "Setup Embark integration for zk.
Adds zk-id as an Embark target, and adds `zk-id-map' and
`zk-file-map' to `embark-keymap-alist'."
  (with-eval-after-load 'embark
    (add-to-list 'embark-multitarget-actions 'zk-index)
    (add-to-list 'embark-multitarget-actions 'zk-index-send-to-desktop)
    (add-to-list 'embark-multitarget-actions 'zk-copy-link-and-title)
    (add-to-list 'embark-multitarget-actions 'zk-follow-link-at-point)
    (add-to-list 'embark-target-finders 'zk-index-embark-target)
    (add-to-list 'embark-exporters-alist '(zk-file . zk-index))
    (define-key zk-file-map (kbd "d") #'zk-index-send-to-desktop)
    (define-key zk-id-map (kbd "d") #'zk-index-send-to-desktop)
    (define-key zk-id-map (kbd "i") #'zk-index-insert-link)))

(defun zk-index-embark-target ()
  "Target zk-id of button at point in ZK-Index and ZK-Desktop."
  (when (zk-index--button-at-point)
    (save-excursion
      (beginning-of-line)
      (re-search-forward zk-id-regexp (line-end-position)))
    (let ((zk-id (match-string-no-properties 1)))
      `(zk-id ,zk-id . ,(cons (line-beginning-position) (line-end-position))))))

;;; Formatting

(defun zk-index--format-line (id title &optional format)
  "Return a formatted string, following FORMAT, for the given ID and TITLE.

FORMAT must be a `format-spec' template, wherein `%i' is replaced by the ID
and `%t' by the title. It can be a string, such as \"%t [[%i]]\", or a
variable whose value is a string. If nil, `zk-completion-at-point-format'
will be used by default."
  (let ((format (or format (if zk-index-invisible-ids
                                "%t %i"
                              zk-index-format)))
        (id (if zk-index-invisible-ids
                (propertize id 'invisible t)
              id)))
    (format-spec format `((?i . ,id) (?t . ,title)))))

;;; Main Stack

;;;###autoload
(defun zk-index (&optional files format-fn sort-fn buf-name)
  "Open ZK-Index, with optional FILES, FORMAT-FN, SORT-FN, BUF-NAME."
  (interactive)
  (setq zk-index-last-format-function format-fn)
  (setq zk-index-last-sort-function sort-fn)
  (let ((inhibit-message nil)
        (inhibit-read-only t)
        (buf-name (or buf-name zk-index-buffer-name))
        (files (or files (zk--directory-files t))))
    (unless (get-buffer buf-name)
      (when zk-default-backlink
        (unless (zk-file-p)
          (zk-find-file-by-id zk-default-backlink)))
      (generate-new-buffer buf-name))
    (with-current-buffer buf-name
      (setq default-directory (expand-file-name zk-directory)
            truncate-lines t)
      (zk-index-mode)
      (when files
        (zk-index-refresh files format-fn sort-fn buf-name))
      (goto-char (point-min)))
    (pop-to-buffer buf-name
                   '(display-buffer-at-bottom))))


(defun zk-index-refresh (&optional files format-fn sort-fn buf-name)
  "Refresh the index.
Optionally refresh with FILES, using FORMAT-FN, SORT-FN, BUF-NAME."
  (interactive)
  (let ((inhibit-message nil)
        (inhibit-read-only t)
        (files (zk-index--sort (or files (zk--directory-files)) sort-fn)))
    (with-current-buffer (or buf-name zk-index-buffer-name)
      (let ((current-line (line-number-at-pos))
            (n 0))
        (unless sort-fn
          (setq zk-index-last-sort-function nil))
        (erase-buffer)
        (dolist (file files)
          (zk-index--insert-button file format-fn)
          (setq n (1+ n))
          (insert "\n"))
        ;; Delete the extraneous newline at the end of the buffer.
        (backward-delete-char 1)
        (zk-index--reset-mode-name)
        (zk-index--set-mode-name (format " [%s]" n))
        (goto-char (point-min))
        (setq truncate-lines t)
        (unless (zk-index-narrowed-p buf-name)
          (zk-index--reset-mode-line)
          (forward-line current-line))))))

(defun zk-index--sort (files &optional sort-fn)
  "Sort FILES (destructively) with SORT-FN.
If no SORT-FN is given, use `zk-index--sort-modified'."
  (if (eq 1 (length files))
      files
    (nreverse (funcall (or sort-fn
                           'zk-index--sort-modified)
                       files))))

(eval-and-compile
  (define-button-type 'zk-index
    'follow-link t
    'face 'default))

(defmacro zk-index--make-button (begin end file id title)
  "A macro that standardizes a call to `make-text-button'."
  `(make-text-button ,begin ,end
                     'type 'zk-index
                     'zk-triplet (zk--triplet ,file ,id ,title)
                     'action 'zk-index-button-action
                     'help-echo 'zk-index-help-echo))

(defun zk-index--insert-button (file &optional format-fn)
  "Makes and inserts a button for the given FILE.
If given, FORMAT-FN is used to format the line, otherwise
`zk-index-format-function' is used. Returns a triplet of file, id, and
title (like those used `zk--alist')."
  (when file
    (let ((beg (point))
          (id (zk--parse-file 'id file))
          (title (zk--parse-file 'title file)))
      (insert zk-index-prefix
              (funcall (or format-fn zk-index-format-function) id title))
      (zk-index--make-button beg (point-at-eol) file id title))))

;;;###autoload
(defun zk-index-make-buttons ()
  "Make buttons in ZK-Index."
  (interactive)
  (let ((inhibit-read-only t)
        (alist (zk--alist (zk--directory-files))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward zk-id-regexp nil t)
        (let* ((beg (line-beginning-position))
               (end (line-end-position))
               (id (match-string-no-properties 1))
               (triplet (assoc-string id alist #'equal)))
          (when triplet
            (beginning-of-line)
            (zk-index--make-button beg end
                                   (zk--triplet-file triplet)
                                   id
                                   (zk--triplet-title triplet))
            (when zk-index-invisible-ids
              (beginning-of-line)
              (when (re-search-forward id)
                (replace-match (propertize id 'invisible t))
                (goto-char (match-end 0))))))))))

;;;; Utilities

(defun zk-index-button-display-action (file buffer)
  "Function to display FILE or BUFFER on button press in Index and Desktop."
  (if (and zk-index-desktop-directory
	       (file-in-directory-p zk-index-desktop-directory
				                default-directory))
      ;; display action for ZK-Desktop
      (progn
        (if (one-window-p)
            (pop-to-buffer buffer
                           (display-buffer-in-direction
                            buffer
                            '((direction . bottom)
                              (window-height . 0.5))))
          (find-file-other-window file)))
    ;; display action for ZK-Index
    (if (one-window-p)
        (pop-to-buffer buffer
                       (display-buffer-in-direction
                        buffer
                        '((direction . top)
                          (window-height . 0.6))))
      (find-file-other-window file))))

(defun zk-index-button-action (button)
  "Action taken when `zk-index' button is pressed."
  (let ((file (zk--triplet-file (button-get button 'zk-triplet))))
    (if file
        (funcall zk-index-button-display-function
                 file
                 (find-file-noselect file))
      (error "Don't know how to open this zk note"))))

(defun zk-index-help-echo (win _obj pos)
  "Generate help-echo zk-index button in WIN at POS."
  (with-selected-window win
    (let ((id (save-excursion
                (goto-char pos)
                (when (re-search-forward zk-id-regexp (line-end-position) t)
                  (match-string-no-properties 0)))))
      (format "%s" (zk--parse-id 'title id)))))

(defun zk-index-narrowed-p (buf-name)
  "Return t when index is narrowed in buffer BUF-NAME."
  (with-current-buffer (or buf-name
                           zk-index-buffer-name)
    (if (< (count-lines (point-min) (point-max))
           (length (zk--id-list (zk--directory-files))))
        t nil)))

;;; Index Search and Focus Functions

;;;; Index Search
;; narrow index based on search of notes' full text

(defun zk-index-search (regexp)
  "Narrow index based on REGEXP search of note contents."
  (interactive (list (read-string "Search: " nil 'zk-search-history)))
  (if (eq major-mode 'zk-index-mode)
      (zk-index-refresh (zk-index-query-files 'search regexp)
                        zk-index-last-format-function
                        zk-index-last-sort-function
                        (buffer-name))
    (user-error "Not in a ZK-Index")))

;;;; Index Focus
;; narrow index based on search of note titles (case sensitive)
;; an alternative to consult-focus-lines

(defun zk-index-focus (regexp)
  "Narrow index based on REGEXP search of note titles."
  (interactive (list (read-string "Focus: " nil 'zk-search-history)))
  (if (eq major-mode 'zk-index-mode)
      (zk-index-refresh (zk-index-query-files 'focus regexp)
                        zk-index-last-format-function
                        zk-index-last-sort-function
                        (buffer-name))
    (user-error "Not in a ZK-Index")))

;;;; Low-level Query Functions

(defvar zk-index-query-terms nil
  "Ordered list of current query terms.
Takes form of (COMMAND . TERM), where COMMAND is 'ZK-INDEX-FOCUS
or 'ZK-INDEX-SEARCH, and TERM is the query string. Recent
items listed first.")

;; See https://www.gnu.org/software/gnulib/manual/html_node/Regular-expression-syntaxes.html
(defun zk-index--egrepify-emacs-regexp (regexp)
  "Make some basic conversions between Emacs regexp syntax and egrep syntax."
  (dolist (tuple
           '(("\\\\\\([(){}]\\)" . "\\1") ; unescape () and {}
             ("\\\\" . "")           ; strip other double backslashes
             ("(\\?[0-9]:" . "(")    ; strip explicit grouping numbers
             ))
    (setq regexp
      (replace-regexp-in-string (car tuple) (cdr tuple) regexp)))
  regexp)

(defun zk-index--construct-title-line-regexp (string &optional verbatim)
  "Incorporate STRING into `zk-header-title-line-regexp' by replacing spaces
with '.*', then convert to (extended) grep style. If VERBATIM is non-nil,
insert the STRING into the query directly."
  ;; Replace everything from beginning of group #2, i.e. the title, to the end
  ;; of the line with the user-provided string.
  (zk-index--egrepify-emacs-regexp
   (concat
    (replace-regexp-in-string "\\\\(\\?2:.*$"
                              (concat ".*"
                                      (if verbatim
                                          string
                                        (replace-regexp-in-string " " ".*"
                                                                  string))
                                      ".*")
                              zk-header-title-line-regexp))))

(defun zk-index-query-files (type regexp)
  "Return narrowed list of notes after the given TYPE (either 'focus
or 'search) of query matching matching REGEXP."
  (let* ((query (pcase type
                 ('focus
                  (zk--grep-file-list
                   (zk-index--construct-title-line-regexp regexp) t))
                 ('search
                  (zk--grep-file-list regexp))
                 (_
                  (error "Unknown query type %s" type))))
         (scope (zk-index--current-id-list (buffer-name)))
         (files (cl-remove-if-not
                 (lambda (file)
                   (cl-find (zk--parse-file 'id file)
                            scope
                            :test #'string=))
                 query)))
    (add-to-history 'zk-search-history regexp)
    (when files
      (let ((mode-line (zk-index-query-mode-line type regexp)))
        (setq zk-index-query-mode-line mode-line)
        (zk-index--set-mode-line mode-line)
        (zk-index--reset-mode-name)))
    (unless files
      (message "No matches for \"%s\"" regexp))
    files))

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

(defun zk-index-query-mode-line (query-type string)
  "Generate new mode line after query.
QUERY-TYPE is either 'focus or 'search, with query term STRING."
  (push (cons query-type string) zk-index-query-terms)
  ;; Sort the different terms into two lists
  (let (focused
        searched)
    (dolist (term zk-index-query-terms)
      (if (equal (car term) 'focus)
          (push term focused)
        (push term searched)))
    ;; Format each list and update appropriate list
    (let* ((formatted
            (mapcar (lambda (term-list)
                      (when term-list
                        ;; (CMD . STRING)
                        (cons (caar term-list)
                              (mapconcat #'cdr term-list "\" + \""))))
                    ;;      CAR     CDR
                    (list focused searched))))
      (concat "["
              (mapconcat (lambda (query)
                           (when query
                             (concat
                              (capitalize (symbol-name (car query)))
                              ": \""
                              (cdr query))))
                         ;; Put the last query type at the end
                         (sort (remq nil formatted)
                               (lambda (a _b)
                                 (not (equal (car a) query-type))))
                         "\" | ")
              "\"]"))))

(defun zk-index--set-mode-line (string)
  "Add STRING to mode-line in `zk-index-mode'."
  (when (eq major-mode 'zk-index-mode)
    (setq-local mode-line-misc-info string)))

(defun zk-index--reset-mode-line ()
  "Reset mode-line in `zk-index-mode'."
  (setq-local mode-line-misc-info zk-index-mode-line-orig)
  (setq zk-index-query-mode-line nil
        zk-index-query-terms nil))

(defun zk-index--current-id-list (buf-name)
  "Return list of IDs for index in BUF-NAME, as filepaths."
  (let (ids)
    (save-excursion
      (with-current-buffer (or buf-name
                               zk-index-buffer-name)
        (goto-char (point-min))
        (save-match-data
          (while (re-search-forward (concat "^" zk-id-regexp) nil t)
            (push (match-string-no-properties 0) ids)))
        ids))))

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
  (let ((ids (zk-index--current-id-list (buffer-name))))
    (zk--parse-ids 'file-path ids)))

(defun zk-index--sort-created (list &optional predicate)
  "Sort LIST alphabetically based on the ID. If not given, PREDICATE defaults
to `string<' (i.e. ascending order)."
  (let ((ht (make-hash-table :test #'equal :size 5000))
        (predicate (or predicate #'string>))) ; FIXME: personal preference
    (dolist (x list)
      (puthash x (zk--parse-file 'id x) ht))
    (sort list
          (lambda (a b)
            (let ((id-a (gethash a ht))
                  (id-b (gethash b ht)))
              (funcall predicate id-b id-a))))))

(defun zk-index--sort-modified (list &optional predicate)
  "Sort LIST based on file modification attribute. If not given,
PREDICATE defaults to `time-less-p' (i.e., most recently modified
first)."
  (let ((ht (make-hash-table :test #'equal :size 5000))
        (predicate (or predicate #'time-less-p)))
    (dolist (x list)
      (puthash x (file-attribute-modification-time (file-attributes x)) ht))
    (sort list
          (lambda (a b)
            (let ((time-a (gethash a ht))
                  (time-b (gethash b ht)))
              (funcall predicate time-b time-a))))))

(defun zk-index--sort-size (list &optional predicate)
  "Sort LIST by file size. If not given, PREDICATE defaults to `>'
(i.e. largest first)."
  (let ((predicate (or predicate #'>)))
    (sort list
          (lambda (a b)
            (funcall predicate
                     (file-attribute-size (file-attributes a))
                     (file-attribute-size (file-attributes b)))))))

;;; ZK-Index Keymap Commands

(defun zk-index-open-note ()
  "Open note."
  (interactive)
  (beginning-of-line)
  (push-button nil t))

;; TODO: What is this variable for?
(defvar-local zk-index-view--kill nil)

(defun zk-index-view-note ()
  "View note in `zk-index-view-mode'."
  (interactive)
  (beginning-of-line)
  (let ((file (zk--triplet-file
               (button-get (button-at (point)) 'zk-triplet))))
    (if (not file)
        (error "Cannot view this note")
      (with-current-buffer (get-file-buffer file)
        (push-button nil t)
        (setq-local zk-index-view--kill t)
        (zk-index-view-mode)))))

(defun zk-index-current-notes ()
  "Open ZK-Index listing currently open notes."
  (interactive)
  (zk-index (zk--current-notes-list)
            zk-index-last-format-function
            zk-index-last-sort-function))

(defun zk-index--button-at-point (&optional pos)
  "Return zk-id when `zk-index' button is at point.
Takes an option POS position argument."
  (let ((button (or pos
                    (button-at (point)))))
    (when (and button
               (or (eq (button-type button) 'zk-index)
                   (eq (button-type button) 'zk-index-desktop)))
      (save-excursion
        (re-search-forward zk-id-regexp)
        (match-string-no-properties 1)))))

(defun zk-index-insert-link (&optional id)
  "Insert zk-link in `other-window' for button ID at point."
  (interactive)
  (let ((id (or id
                (zk-index--button-at-point))))
    (with-selected-window (other-window-for-scrolling)
      (zk-insert-link id)
      (newline))))

(defvar-local zk-index-view--cursor nil)

(define-minor-mode zk-index-view-mode
  "Minor mode for `zk-index-auto-scroll'."
  :init-value nil
  :global nil
  :keymap '(((kbd "n") . zk-index-next-line)
            ((kbd "p") . zk-index-previous-line)
            ((kbd "d") . zk-index-send-to-desktop)
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


;;; ZK-Desktop
;; index's more flexible, savable cousin; a place to collect and order notes
;; in the form of links

;;;###autoload
(defun zk-index-desktop ()
  "Open ZK-Desktop."
  (interactive)
  (let ((buffer (if (and zk-index-desktop-current
                         (buffer-live-p (get-buffer zk-index-desktop-current)))
                    zk-index-desktop-current
                  (zk-index-desktop-select)))
        (choice (unless (eq (current-buffer) zk-index-desktop-current)
                  (read-char "Choice: \[s\]witch or \[p\]op-up?"))))
    (pcase choice
      ('?s (switch-to-buffer buffer))
      ('?p (pop-to-buffer buffer
                          '(display-buffer-at-bottom)))
      (_ nil))))


;;;###autoload
(defun zk-index-desktop-select ()
  "Select a ZK-Desktop to work with."
  (interactive)
  (unless zk-index-desktop-directory
    (error "Please set `zk-index-desktop-directory' first"))
  (let* ((last-command last-command)
         (desktop
          (completing-read "Select or Create ZK-Desktop: "
                           (directory-files
                            zk-index-desktop-directory
                            nil
                            (concat
                             zk-index-desktop-basename
                             ".*"))
                           nil nil nil nil
                           (concat zk-index-desktop-basename " ")))
         (file (concat zk-index-desktop-directory "/" desktop)))
    (if (file-exists-p (expand-file-name file))
        (setq zk-index-desktop-current
              (find-file-noselect file))
      (progn
        (generate-new-buffer desktop)
        (setq zk-index-desktop-current desktop)))
    (with-current-buffer zk-index-desktop-current
      (setq require-final-newline 'visit-save)
      (unless (bound-and-true-p truncate-lines)
        (toggle-truncate-lines))
      (set-visited-file-name file t t)
      (zk-index-desktop-mode)
      (save-buffer))
    (if (and (not (eq last-command 'zk-index-desktop))
             (y-or-n-p (format "Visit %s? " zk-index-desktop-current)))
        (switch-to-buffer zk-index-desktop-current)
      (message "Desktop set to: %s" zk-index-desktop-current)))
  zk-index-desktop-current)

(eval-and-compile
  (define-button-type 'zk-index-desktop
    'read-only t
    'front-sticky t
    'rear-sticky t
    'keymap zk-index-desktop-button-map
    'action 'zk-index-button-action
    'help-echo 'zk-index-help-echo
    'face 'zk-index-desktop-button
    'cursor-face 'highlight))

;;;###autoload
(defun zk-index-desktop-make-buttons ()
  "Re-make buttons in ZK-Desktop."
  (interactive)
  (when (string-match-p zk-index-desktop-basename (buffer-name))
    (let ((inhibit-read-only t))
      (save-excursion
        ;; replace titles
        (goto-char (point-min))
        (let* ((files (zk--directory-files))
               (ids (zk--id-list files))
               (alist (zk--alist files)))
          (while (re-search-forward zk-id-regexp nil t)
            (let* ((beg (line-beginning-position))
                   (end (line-end-position))
                   (id  (match-string-no-properties 1))
                   (title (buffer-substring-no-properties beg (match-beginning 0)))
                   (new-title (when (member id ids)
                                (concat zk-index-desktop-prefix
                                        (zk--parse-id 'title id alist) " ")))
                   ;; (save-rest (prog1 (buffer-substring-no-properties
                   ;;                    (match-end 0) end)
                   ;;              (kill-region (match-end 0) end)))
                   )
              (beginning-of-line)
              (when (and (search-forward title end)
                         (stringp new-title)
                         (not (string= title new-title)))
                (replace-match new-title))
              (end-of-line)))
          ;; make buttons
          (goto-char (point-min))
          (while (re-search-forward zk-id-regexp nil t)
            (let* ((beg (line-beginning-position))
                   (end (line-end-position))
                   (id (match-string-no-properties 1)))
              (if (not (member id ids))
                  (progn
                    (end-of-line)
                    (unless (overlays-at (point))
                      (overlay-put (make-overlay (point) (point))
                                 'before-string
                                 (propertize" <- ID NOT FOUND" 'font-lock-face 'error))))
                (make-text-button beg end 'type 'zk-index-desktop)
                (when zk-index-invisible-ids
                  (beginning-of-line)
                  ;; find zk-links and plain zk-ids
                  (if (re-search-forward (zk-link-regexp) (line-end-position) t)
                      (replace-match
                       (propertize (match-string 0) 'invisible t) nil t)
                    (progn
                      (re-search-forward id)
                      (replace-match
                       (propertize id
                                   'read-only t
                                   'front-sticky t
                                   'rear-nonsticky t))
                      ;; enable invisibility in org-mode
                      (overlay-put
                       (make-overlay (match-beginning 0) (match-end 0))
                       'invisible t)
                      )))
                (add-text-properties beg (+ beg 1)
                                     '(front-sticky nil))))
            (end-of-line)))))))

;;;###autoload
(defun zk-index-send-to-desktop (desktop &optional files)
  "Send notes from ZK-Index to ZK-Desktop.
In ZK-Index, works on note at point or notes in active region.
Also works on FILES or group of files in minibuffer, and on zk-id
at point. With prefix argument, asks user to select the desktop
even if `zk-index-desktop-current' is set."
  (interactive (list (if (or current-prefix-arg
                             (null zk-index-desktop-current))
                         (zk-index-desktop-select)
                       zk-index-desktop-current)))
  (unless zk-index-desktop-directory
    (error "Please set `zk-index-desktop-directory' first"))
  (let ((inhibit-read-only t)
        items)
    (cond ((eq 1 (length files))
           (unless
               (ignore-errors
                 (setq items (car (funcall zk-index-format-function files))))
             (setq items
                   (car
                    (funcall
                     zk-index-format-function
                     (list (zk--parse-ids 'file-path files)))))))
          ((and files
                (< 1 (length files)))
           (setq items
                 (mapconcat
                  #'identity
                  (funcall zk-index-format-function files) "\n")))
          ((eq major-mode 'zk-index-mode)
           (setq items (if (use-region-p)
                           (buffer-substring
                            (save-excursion
                              (goto-char (region-beginning))
                              (line-beginning-position))
                            (save-excursion
                              (goto-char (region-end))
                              (line-end-position)))
                         (buffer-substring
                          (line-beginning-position)
                          (line-end-position)))))
          ((zk-file-p)
           (setq items
                 (car
                  (funcall
                   zk-index-format-function
                   (list (zk--parse-id 'file-path (zk--current-id))))))))
    (unless (get-buffer desktop)
      (generate-new-buffer desktop))
    (with-current-buffer desktop
      (setq require-final-newline 'visit-save)
      (pcase zk-index-desktop-add-pos
        ('append (progn
                   (goto-char (point-max))
                   (beginning-of-line)
                   (when (looking-at-p ".")
                     (end-of-line)
                     (newline))))
        ('prepend (progn
                    (goto-char (point-min))))
        ('at-point (goto-char (point))))
      (insert items "\n")
      (beginning-of-line)
      (unless (bound-and-true-p truncate-lines)
        (toggle-truncate-lines))
      (zk-index-desktop-mode))
    (if (eq major-mode 'zk-index-mode)
        (message "Sent to %s - press D to switch" desktop)
      (message "Sent to %s" desktop))))

(defun zk-index-desktop-add-toggle ()
  "Set `zk-index-desktop-add-pos' interactively."
  (interactive)
   (let ((choice (read-char "Choice: \[a\]ppend; \[p\]repend; at-\[P\]oint")))
     (pcase choice
      ('?a (setq zk-index-desktop-add-pos 'append))
      ('?p (setq zk-index-desktop-add-pos 'prepend))
      ('?P (setq zk-index-desktop-add-pos 'at-point)))))

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

;;;###autoload
(defun zk-index-switch-to-desktop ()
  "Switch to ZK-Desktop.
With prefix-argument, raise ZK-Desktop in other frame."
  (interactive)
  (unless (and zk-index-desktop-current
               (buffer-live-p (get-buffer zk-index-desktop-current)))
    (zk-index-desktop-select))
  (let ((buffer zk-index-desktop-current))
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

;;; ZK-Desktop Keymap Commands

(defun zk-index-move-line-down ()
  "Move line at point down in ZK-Desktop buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)))

(defun zk-index-move-line-up ()
  "Move line at point up in ZK-Desktop buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (transpose-lines 1)
    (forward-line -2)))

(defun zk-index-desktop-delete-region-maybe ()
  "Maybe delete region in `zk-index-desktop-mode'."
  (cond ((and (not (use-region-p))
              (zk-index--button-at-point))
         (delete-region (line-beginning-position)
                        (line-end-position)))
        ((and (use-region-p)
              (zk-index--button-at-point (region-beginning))
              (not (zk-index--button-at-point (region-end))))
         (delete-region (save-excursion
                          (goto-char (region-beginning))
                          (line-beginning-position))
                        (region-end))
         t)
        ((and (use-region-p)
              (not (zk-index--button-at-point (region-beginning)))
              (zk-index--button-at-point (region-end)))
         (delete-region (region-beginning)
                        (save-excursion
                          (goto-char (region-end))
                          (line-end-position)))
         t)
        ((and (use-region-p)
              (zk-index--button-at-point (region-beginning))
              (zk-index--button-at-point (region-end)))
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

(defun zk-index-desktop-delete-char ()
  "Wrapper around `delete-char' for `zk-index-desktop-mode'."
  (interactive)
  (unless (and (and (looking-back zk-id-regexp
                                  (line-beginning-position))
                    (looking-at "$"))
               (save-excursion
                 (beginning-of-line)
                 (zk-index--button-at-point)))
    (let ((inhibit-read-only t))
      (unless (zk-index-desktop-delete-region-maybe)
        (funcall #'delete-char (or current-prefix-arg 1))))))

(defun zk-index-desktop-delete-backward-char ()
  "Wrapper around `delete-backward-char' for `zk-index-desktop-mode'."
  (interactive)
  (unless (and (looking-back zk-id-regexp
                             (line-beginning-position))
               (save-excursion
                 (beginning-of-line)
                 (zk-index--button-at-point)))
    (let ((inhibit-read-only t))
      (unless (zk-index-desktop-delete-region-maybe)
        (funcall #'delete-char (or current-prefix-arg -1))))))

(defun zk-index-desktop-kill-line ()
  "Kill line in `zk-index-desktop-mode'."
  (interactive)
  (let ((inhibit-read-only t))
    (if (not (zk-index--button-at-point))
        (kill-line)
      (kill-region (line-beginning-position)
                   (line-end-position)))))

(defun zk-index-desktop-kill-region ()
  "Wrapper around `kill-region' for `zk-index-desktop-mode'."
  (interactive)
  (let ((inhibit-read-only t))
    (cond ((and (use-region-p)
                (zk-index--button-at-point (region-beginning))
                (not (zk-index--button-at-point (region-end))))
           (kill-region (save-excursion
                            (goto-char (region-beginning))
                            (line-beginning-position))
                          (region-end)))
          ((and (use-region-p)
                (not (zk-index--button-at-point (region-beginning)))
                (zk-index--button-at-point (region-end)))
           (kill-region (region-beginning)
                          (save-excursion
                            (goto-char (region-end))
                            (line-end-position))))
          ((and (use-region-p)
                (zk-index--button-at-point (region-beginning))
                (zk-index--button-at-point (region-end)))
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

(defun zk-index-desktop-yank ()
  "Wrapper around `yank' for `zk-index-desktop-mode'."
  (interactive)
  (let ((inhibit-read-only t))
    (yank)
    (zk-index-desktop-make-buttons)))

(provide 'zk-index)

;;; zk-index.el ends here
