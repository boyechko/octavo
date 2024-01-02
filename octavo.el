;;; octavo.el --- Functions for working with Zettelkasten-style linked notes -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 Richard Boyechko

;; Created: June 28, 2023
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Homepage: https://github.com/boyechko/octavo
;; Package-Requires: ((emacs "26.1"))

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

;; This set of functions aims to implement many (but not all) of the features
;; of the package 'Zetteldeft', while circumventing and eliminating any
;; dependency on 'Deft', or any other external packages for that matter. It
;; does not use any backend cache or database, but instead queries a
;; directory of notes directly, treating and utilizing that directory as a
;; sufficient database unto itself.

;; To that end, these functions rely, at the lowest level, on simple calls to
;; 'grep', which returns lists of files, links, and tags to
;; 'completing-read', from which files can be opened and links and tags can
;; be inserted into an open buffer.

;; The primary connector between notes is the simple link, which takes the
;; form of an ID number enclosed in double-brackets, eg, [[202012091130]]. A
;; note's ID number, by default, is a twelve-digit string corresponding to
;; the date and time the note was originally created. For example, a note
;; created on December 9th, 2020 at 11:30 will have the Octavo ID "202012091130".
;; Linking to such a note involves nothing more than placing the string
;; [[202012091130]] into another note in the directory.

;; A note's filename is constructed as follows: the Octavo ID number followed by
;; the title of the note followed by the file extension, e.g. "202012091130
;; On the origin of species.txt". A key consequence of this ID/linking scheme
;; is that a note's title can change without any existing links to the note
;; being broken, wherever they might be in the directory.

;; The directory is a single folder containing all notes.

;; The structural simplicity of this set of functions is---one hopes, at
;; least---in line with the structural simplicity of the so-called
;; "Zettelkasten method," of which much can be read in many places, including
;; at https://www.zettelkasten.de.

;;; Code:

(require 'thingatpt)
(require 'format-spec)
(require 'seq)

;;; Variable Declarations

(defvar embark-keymap-alist)
(defvar embark-target-finders)
(defvar embark-multitarget-actions)
(defvar embark-general-map)
(defvar embark-file-map)

;;; Variables

(defgroup octavo nil
  "A Zettelkasten implementation for Emacs."
  :group 'text
  :group 'files
  :prefix "octavo-")

;; Fundamental variables

(defcustom octavo-directory nil
  "Main Octavo directory."
  :type 'string)

;; Borrowed from Deft by Jason R. Blevins <jblevins@xbeta.org>
(defcustom octavo-directory-recursive nil
  "Recursively search for files in subdirectories of `octavo-directory'.
If you set this, also consider setting `octavo-subdirectory-function'."
  :type 'boolean)

(defcustom octavo-directory-recursive-ignore-dir-regexp
  "\\(?:\\.\\|\\.\\.\\)$"
  "Regexp for subdirs to be ignored when ‘octavo-directory-recursive’ is non-nil."
  :type 'string)

(defcustom octavo-subdirectory-function nil
  "Function that returns a subdirectory of `octavo-directory'.
Used when `octavo-directory-recursive' is non-nil to create new notes
in the desired subdirectory. When nil, new notes are created in
`octavo-directory'."
  :type 'function)

(defcustom octavo-file-extension nil
  "The extension for Octavo files."
  :type 'string)

(defcustom octavo-file-name-separator " "
  "Character(s), as a string, to separate elements of filename.

Useful for keeping spaces out of file-names. When set to \"-\",
for example, the file-name will be in the form
\"202012341234-Title-of-note.ext\". In notes, the title will be
rendered with spaces."
  :type 'string)

(defcustom octavo-id-time-string-format "%Y%m%d%H%M"
  "Format for new Octavo IDs.
For supported options,  consult `format-time-string'.

Note: The regexp to find Octavo IDs is set separately. If you change
this value, set `octavo-id-regexp' so that the Octavo IDs can be found."
  :type 'string)

(defcustom octavo-id-regexp "[0-9]\\{12\\}"
  "The regular expression used to search for Octavo IDs.
Set it so that it matches strings generated with
`octavo-id-time-string-format'. The expression should not
capture any explicitly numbered groups.

See `octavo-file-name-regexp' and `octavo-link-regexp' functions for
how this regexp is used."
  :type 'regexp)

(defcustom octavo-title-regexp ".*?"
  "The regular expression used to match the Octavo note's title.
This is only relevant if `octavo-link-format' includes the title."
  :type 'regexp)

(defcustom octavo-tag-regexp "\\s#[a-zA-Z0-9]\\+"
  "The regular expression used to search for tags."
  :type 'regexp)

;; Function variables

(defcustom octavo-new-note-header-function #'octavo-new-note-header
  "Function called by `octavo-new-note' to insert header in a new note.
A user-defined function should use `insert' to insert a string or
strings. The arguments NEW-ID, TITLE, and ORIG-ID can be used to
those corresponding values from `octavo-new-note' available for
insertion. See `octavo-new-note-header' for an example."
  :type 'function)

(defcustom octavo-select-file-function #'octavo--select-file
  "Function `octavo-select-file' uses for selecting an Octavo file.
Must take an optional prompt and a list of files. See also
`octavo--select-file' for the default implementation."
  :type 'function)

(defcustom octavo-tag-insert-function nil
  "Function for inserting tag.
Function must take a single argument TAG, as a string.
If nil, tag will be inserted at point."
  :type 'function)

(defcustom octavo-search-function #'octavo-grep
  "Function used by `octavo-search'.
Must take a single STRING argument."
  :type 'function)

(make-obsolete-variable 'octavo-grep-function "The use of the
 'octavo-grep-function' variable is deprecated.
 'octavo-search-function' should be used instead"
                        "0.5")

(defcustom octavo-tag-search-function #'octavo-grep
  "Function used by `octavo-tag-search'.
Must take a single STRING argument."
  :type 'function)

(make-obsolete-variable 'octavo-tag-grep-function "The use of the
  'octavo-tag-grep-function' variable is deprecated.
 'octavo-tag-search-function' should be used instead"
                        "0.5")

(defcustom octavo-current-notes-function nil
  "User-defined function for listing currently open notes.
See `octavo-current-notes' for details."
  :type 'function)

(defcustom octavo-format-function #'octavo-format-id-and-title
  "Function for formatting Octavo file information.
It should accept three variables: FORMAT-SPEC, ID, and TITLE.
See `octavo-format-id-and-title' for an example."
  :type 'function)

;; Format variables

(defcustom octavo-link-format "[[%i]]"
  "Format for inserted links.

See `octavo-format-id-and-title' for what the default control
sequences mean."
  :type 'string)

(defcustom octavo-link-and-title-format "%t [[%i]]"
  "Format for link and title when inserted to together.

See `octavo-format-id-and-title' for what the default control
sequences mean."
  :type 'string)

(defcustom octavo-completion-at-point-format "[[%i]] %t"
  "Format for completion table used by `octavo-completion-at-point'.

See `octavo-format-id-and-title' for what the default control
sequences mean."
  :type 'string)

;; Link variables

(defcustom octavo-new-note-link-insert 'ask
  "Should `octavo-new-note' insert link to new note at point?

Options:
1. t - Always insert a link
2. `octavo - Insert link only inside an existing note
3. `ask - Ask user, yes or no
4. nil - Never insert a link

Calling `octavo-new-note' with a prefix-argument inserts a link
regardless of how `octavo-new-note-link-insert' is set."
  :type '(choice (const :tag "Always" t)
                 (const :tag "Ask" ask)
                 (const :tag "Only in Octavo notes" octavo)
                 (const :tag "Never" nil)))

(defcustom octavo-link-and-title t
  "Should `octavo-insert-link' insert both link and title?

Options:
1. t - Always inserts link and title; with `prefix-arg', only link
2. `ask - Ask user, yes or no; with `prefix-arg', only link
3. nil - Only insert link, not title; with `prefix-arg', include title

The format in which link and title are inserted can be configured
by setting the variable `octavo-link-and-title-format'."
  :type '(choice (const :tag "Always" t)
                 (const :tag "Ask" ask)
                 (const :tag "Never" nil)))

(defcustom octavo-enable-link-buttons t
  "When non-nil, valid octavo-id links will be clickable buttons.
Allows `octavo-make-link-buttons' to be added to `find-file-hook', so
buttons will be automatically created when a note is opened."
  :type 'boolean)

(defcustom octavo-default-backlink nil
  "When non-nil, should be a single Octavo ID.
See `octavo-new-note' for details."
  :type 'string)

(defvar octavo-file-history nil)
(defvar octavo-search-history nil)

;;; Embark Integration

(defvar octavo-id-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'octavo-follow-link-at-point)
    (define-key map (kbd "k") #'octavo-copy-link-and-title)
    (define-key map (kbd "s") #'octavo-search)
    map)
  "Keymap for Embark octavo-id at-point actions.")

(defvar octavo-file-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") #'octavo-insert-link)
    (define-key map (kbd "f") #'octavo-find-file)
    (define-key map (kbd "k") #'octavo-copy-link-and-title)
    map)
  "Keymap for Embark octavo-file minibuffer actions.")

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
    (add-to-list 'embark-multitarget-actions 'octavo-copy-link-and-title)
    (add-to-list 'embark-multitarget-actions 'octavo-insert-link)
    (add-to-list 'embark-target-finders 'octavo-embark-target-octavo-id-at-point)
    (add-to-list 'embark-keymap-alist '(octavo-id . octavo-id-map))
    (add-to-list 'embark-keymap-alist '(octavo-file . octavo-file-map))
    (set-keymap-parent octavo-id-map embark-general-map)
    (set-keymap-parent octavo-file-map embark-file-map)))

;;; Low-Level Functions

(defun octavo--singleton-p (list)
  "Return non-NIL if LIST is not null, is a list, and has a single element."
  (and list
       (listp list)
       (null (cdr list))))

(defun octavo-file-name-regexp ()
  "Return the correct regexp matching Octavo file names.
The regexp captures these groups:

Group 1 is the Octavo ID.
Group 2 is the title."
  (concat "\\(?1:" octavo-id-regexp "\\)"
          "."
          "\\(?2:" octavo-title-regexp "\\)"
          "\\."
          octavo-file-extension
          ".*"))

(defun octavo-link-regexp (&optional id title)
  "Return the correct regexp matching Octavo links.
If ID and/or TITLE are given, use those, generating a regexp
that specifically matches them. Othewrise use `octavo-id-regexp'
and `octavo-title-regexp', respectively.
The regexp captures these groups:

Group 1 is the Octavo ID.
Group 2 is the title."
  (octavo--format (regexp-quote octavo-link-format)
              (concat "\\(?1:" (or id octavo-id-regexp) "\\)")
              (concat "\\(?2:" (or title octavo-title-regexp) "\\)")))

(defun octavo--parse-file (file)
  "If FILE is an Octavo file, return (ID . TITLE); otherwise, nil."
  (when-let* ((_ (stringp file))
              (filename (file-name-nondirectory file))
              (_ (string-match (octavo-file-name-regexp) filename)))
    (cons (match-string 1 filename)
          (string-trim
           (replace-regexp-in-string octavo-file-name-separator " "
                                     (match-string 2 filename))))))

(defun octavo--file-id (file)
  "Return the ID of the given Octavo FILE."
  (car (octavo--parse-file file)))

(defun octavo--file-title (file)
  "Return the title of the given Octavo FILE."
  (cdr (octavo--parse-file file)))

(defun octavo--id-file (id)
  "Return the full file path for the existing Octavo note with ID.
Use wildcards to match files against the ID, signalling an error if
there are multiple matches (so ID is not unique). If there are no
matches, return nil."
  (let* ((wild-base-name (format "%s*.%s" id octavo-file-extension))
         (matches (file-expand-wildcards
                   (concat (file-name-as-directory octavo-directory)
                           (when (functionp octavo-subdirectory-function)
                             (file-name-as-directory
                              (funcall octavo-subdirectory-function id)))
                           wild-base-name))))
    (cond ((octavo--singleton-p matches)
           (expand-file-name (car matches)))
          ((null matches)
           nil)
          (t
           (error "There are multiple (%d) files with ID %s"
                  (length matches)
                  id)))))

(defalias 'octavo-id-p 'octavo--id-file)
(make-obsolete 'octavo-id-p 'octavo--id-file "0.6")

(defun octavo-file-p (&optional file strict)
  "Return t if FILE is an Octavo file.
If FILE is not given, get it from variable `buffer-file-name'.
If STRICT is non-nil, make sure the file is in `octavo-directory',
otherwise just match against `octavo-file-name-regexp'."
  (when-let ((file (cond ((stringp file) file)
                         ((null file) buffer-file-name)
                         (t
                          (signal 'wrong-type-argument '(file))))))
    (and (octavo--file-id file)
         (or (not strict)
             (save-match-data
               (file-in-directory-p file octavo-directory))))))

(defun octavo--generate-id ()
  "Generate and return an Octavo ID.
The ID is created using `octavo-id-time-string-format'."
  (let ((id (format-time-string octavo-id-time-string-format)))
    (while (octavo--id-file id)
      (setq id (number-to-string (1+ (string-to-number id)))))
    id))

(defun octavo--id-list (&optional regexp octavo-alist)
  "Return a list of Octavo IDs for notes in `octavo-directory'.
If REGEXP is non-nil, only include notes whose IDs or titles
match; ignore case. If OCTAVO-ALIST is non-nil, use it."
  (if (or regexp octavo-alist)
      (let ((octavo-alist (or octavo-alist (octavo--alist)))
            (case-fold-search t)
            (ids))
        (dolist (item octavo-alist)
          (if regexp
              (when (or (string-match regexp (car item))
                        (string-match regexp (cadr item)))
                (push (car item) ids))
            (push (car item) ids)))
        ids)
    (mapcar 'octavo--file-id (octavo--directory-files 'full))))

(defun octavo--current-id ()
  "Return the ID of Octavo in current buffer."
  (or (octavo--file-id buffer-file-name)
      (user-error "Not an Octavo file")))
(make-obsolete 'octavo--current-id 'octavo--file-id "0.5")

(defvar octavo--directory-files-cache nil
  "Store the result of `octavo--directory-files' to prevent re-scanning.
This is an alist with key [DIRECTORY FULL REGEXP] and list
of FILES as value.")

(defun octavo--directory-files-cache-key-equal (key1 key2)
  "Return non-nil if KEY1 and KEY2 are the same."
  (and (string= (elt key1 0) (elt key2 0))
       (eq (elt key1 1) (elt key2 1))
       (string= (elt key1 2) (elt key2 2))))

(defmacro octavo--directory-files-cached (&optional directory full regexp)
  "Return the cached file list for DIRECTORY, FULL, and REGEXP.
DIRECTORY defaults to `octavo-directory'."
  `(alist-get (vector (or ,directory octavo-directory) ,full ,regexp)
              octavo--directory-files-cache
              nil nil #'octavo--directory-files-cache-key-equal))

(defun octavo--directory-files (&optional full regexp refresh)
  "Return list of octavo-files in `octavo-directory'.
Excludes lockfiles, autosave files, and backup files. When
FULL is non-nil, return full file-paths. If REGEXP is non-nil,
it must be a regexp to replace the default, `octavo-id-regexp'.
With REFRESH, rescan the file system and update the cache.

When `octavo-directory-recursive' is non-nil, searches recursively in
subdirectories of `octavo-directory' (except those matching
`octavo-directory-recursive-ignore-dir-regexp') and returns full
file-paths."
  (let* ((regexp (or regexp octavo-id-regexp)))
    (garbage-collect)                   ; prevents eventual slowdown
    (or (and (not refresh)
             (octavo--directory-files-cached octavo-directory full regexp))
        (setf (octavo--directory-files-cached octavo-directory full regexp)
              (seq-filter #'octavo-file-p
                          (if (not octavo-directory-recursive)
                              (directory-files octavo-directory full regexp)
                            (directory-files-recursively
                             octavo-directory regexp nil
                             (lambda (dir)
                               (not (string-match
                                     octavo-directory-recursive-ignore-dir-regexp
                                     dir))))))))))

(defun octavo--current-notes-list ()
  "Return list of files for currently open notes."
  (remq nil
        (mapcar
         (lambda (x)
           (when (and (buffer-file-name x)
                      (octavo-file-p (buffer-file-name x)))
             (buffer-file-name x)))
         (buffer-list))))

(defun octavo--posix-regexp (regexp &optional basic)
  "Convert Elisp-style REGEXP to extended POSIX 1003.2 regexp.
If BASIC is non-nil, convert as much as possible to basic
regexp instead. See manual page `re_format(7)' for details."
  (let (result)
    ;; 1. For basic REs, warn the user about lack of \| (or) operator
    (when (and basic (string-match "\\\\|" regexp))
      ;; FIXME: Basic REs don't have or (\|) operator, as in \(one\|two\); one
      ;; would need to pass multiple -e command line args to grep. So, just
      ;; treat the operator as normal text, but let the user know.
      (warn "Operator \\| (or) cannot be used with basic regexps: %s" regexp)
      (setq result regexp))
    ;; 2. Strip numbered groups for extended REs, numbered and shy groups for basic
    (setq result
      (if basic
          (replace-regexp-in-string "\\\\(\\?[0-9]?:" "\\(" regexp nil 'literal)
        (replace-regexp-in-string "\\\\(\\?[0-9]:" "\\(" regexp nil 'literal)))
    ;; 3. Un-escape special characters (){}|+ for extended REs
    (unless basic
      (setq result
        (replace-regexp-in-string "\\\\\\([(){}+|]\\)" "\\1" result)))
    result))

(defun octavo--grep-command (regexp &rest other-options)
  "Return a list of files containing REGEXP.
Any remaining OTHER-OPTIONS should be strings that will be
passed directly to `grep' command."
  (split-string
   (shell-command-to-string
    (mapconcat #'identity
      (append (list "egrep"
                    "--recursive"
                    "--ignore-case"
                    (concat "--include=\\*." octavo-file-extension)
                    (concat "--regexp="
                            (shell-quote-argument (octavo--posix-regexp regexp)))
                      octavo-directory
                      "2>/dev/null")
                other-options)
        " "))
     "\n" 'omit-nulls "\s"))

(defun octavo--grep-file-list (regexp &optional invert)
  "Return a list of files containing REGEXP.
If INVERT is non-nil, return list of files *not* matching."
  (octavo--grep-command regexp
                    (if invert
                        "--files-without-match"
                      "--files-with-matches")))

(defun octavo--grep-id-list (regexp &optional invert)
  "Return a list of IDs for files containing REGEXP.
If INVERT is non-nil, return list of files *not* matching."
  (let ((ids (mapcar #'octavo--file-id (octavo--grep-file-list regexp invert))))
    (if (stringp ids)
        (list ids)
      ids)))

(defun octavo--grep-match-list (regexp &optional unique)
  "Return list of matches for REGEXP from notes in `octavo-directory'.
If UNIQUE is non-nil, remove duplicate matches."
  (let ((result (octavo--grep-command regexp
                                  "--only-matching"
                                  "--no-filename")))
    (if unique
        (delete-dups result)
      result)))

(defun octavo--grep-tag-list ()
  "Return list of tags from all notes in Octavo directory.
What counts as a tag depends on `octavo-tag-regexp'."
  (octavo--grep-match-list octavo-tag-regexp 'unique))

(defun octavo-select-file (&optional prompt files &rest args)
  "Call `octavo-select-file-function', passing PROMPT, FILES, and ARGS to it."
  (apply octavo-select-file-function prompt files `,@args))

(defun octavo--select-file (&optional prompt files group sort initial-input)
  "Select an Octavo file with `completing-read' showing PROMPT.
Offers candidates from list of FILES, if supplied, or from
`octavo--directory-files'. INITIAL-INPUT, GROUP and SORT are
passed to `completion-read'."
  (let* ((files (or files (octavo--directory-files 'full)))
         (group (or group 'octavo--group-function))
         (sort (or sort nil)))
    (completing-read (or prompt "Select Zettel: ")
                     (lambda (string predicate action)
                       (if (eq action 'metadata)
                           `(metadata
                             (group-function . ,group)
                             (display-sort-function . ,sort)
                             (category . octavo-file))
                         (complete-with-action action files string predicate)))
                     nil t initial-input 'octavo-file-history)))

(defun octavo--group-function (file transform)
  "TRANSFORM completion candidate FILE to note title."
  (if transform
      (octavo--file-title file)
    "octavo"))

(defun octavo--id-at-point ()
  "Return ID at point."
  (cond ((thing-at-point-looking-at octavo-id-regexp)
         (match-string-no-properties 0))
        ((thing-at-point-looking-at (octavo-link-regexp))
         (match-string-no-properties 1))))

(defun octavo--alist ()
  "Return an alist ID, title, and file-path triples."
  (mapcar (lambda (file)
            (let ((id-title (octavo--parse-file file)))
              (list (car id-title) (cdr id-title) file)))
          (octavo--directory-files 'full)))

(defun octavo--parse-id (target id &optional octavo-alist)
  "Return TARGET, either `file-path or `title, from file with ID.
Takes a single ID, as a string. Takes an optional OCTAVO-ALIST, for
backward compatibility, but ignores it in favor of checking against
the file system directly via `octavo--id-file'."
  (let ((file (octavo--id-file id)))
    (cond ((eq target 'file-path)
           file)
          ((eq target 'title)
           (octavo--file-title file))
          (t (error "Invalid target: %s" target)))))

;;; Formatting

(defun octavo--processor (arg)
  "Process ARG into a list of octavo-files.
ARG can be a string (octavo-file or octavo-id) or a list of such strings."
  (let* ((octavo-alist (octavo--alist))
         (process-single-arg
          (lambda (single-arg)
            (if (octavo-file-p single-arg)
                single-arg
              (octavo--parse-id 'file-path single-arg octavo-alist)))))
    (cond ((stringp arg)                ; Single octavo-file or octavo-id as string
           (list (funcall process-single-arg arg)))
          ((listp arg)                  ; List of octavo-files or octavo-ids
           (mapcar process-single-arg arg))
          (t
           (signal 'wrong-type-argument (list 'list-or-string-p arg))))))

(defun octavo--formatter (arg format &optional no-proc)
  "Return formatted list from FILES, according to FORMAT.
ARG can be octavo-file or octavo-id as string or list, or single or multiple.
When NO-PROC is non-nil, bypass `octavo--processor'."
  (mapcar (lambda (file)
            (when-let ((id-title (octavo--parse-file file)))
              (octavo--format format (car id-title) (cdr id-title))))
          (if no-proc
              arg
            (octavo--processor arg))))

(defun octavo--formatted-string (arg format)
  "Format a multi-line string from items in ARG, following FORMAT."
  (let ((items (octavo--formatter arg format)))
    (mapconcat #'identity items "\n\n")))

(defun octavo-format-id-and-title (format id title)
  "Format ID and TITLE based on the `format-spec' FORMAT.
The sequence `%t' in FORMAT is replaced with the TITLE
and `%i' with the ID. This is the default function
that `octavo-format-function' is set to."
  (format-spec format `((?i . ,id) (?t . ,title))))

(defun octavo--format (format id title)
  "Format ID and TITLE based on the `format-spec' FORMAT.
This is a wrapper around `octavo-format-function', which see."
  (funcall octavo-format-function format id title))

;;; Buttons

(defun octavo-setup-auto-link-buttons ()
  "Enable automatic link creation when octavo-file is opened.
Adds `octavo-make-link-buttons' to `find-file-hook.'"
  (setq octavo-enable-link-buttons t)
  (add-hook 'find-file-hook #'octavo-make-link-buttons))

(defun octavo-button-help-echo (_win obj pos)
  "Return a string of help-echo for `octavo-link' button.
_WIN is the current window; OBJ is the button itself; POS is
the starting position of the button."
  (octavo--parse-id 'title (button-label (or obj (button-at pos)))))

(eval-and-compile
  (define-button-type 'octavo-link
    'action 'octavo-follow-link-at-point
    'follow-link t
    'face 'octavo-desktop-button
    'help-echo 'octavo-button-help-echo))

(defun octavo-make-link-buttons ()
  "Make Octavo links in current buffer into `octavo-link' buttons."
  (interactive)
  (when (and (octavo-file-p) octavo-enable-link-buttons)
    (remove-overlays (point-min) (point-max) 'type 'octavo-link)
    (let* ((octavo-alist (octavo--alist))
           (ids (octavo--id-list nil octavo-alist)))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward (octavo-link-regexp) nil t)
          (let ((beg (match-beginning 1))
                (end (match-end 1))
                (id (match-string-no-properties 1)))
            (when (member id ids)
              ;; Since we have octavo-alist handy, might as well set the buttons'
              ;; help-echo to a static string rather than having `octavo-button-
              ;; help-echo' have to parse again.
              (make-button beg end
                           'type 'octavo-link
                            'help-echo
                           (octavo--parse-id 'title id octavo-alist)))))))))

(defun octavo-make-button-before-point ()
  "Find `octavo-link-regexp' before point and make it an octavo-link button."
  (interactive)
  (save-excursion
    (re-search-backward (octavo-link-regexp) (line-beginning-position))
    (make-button (match-beginning 1) (match-end 1)
                 'type 'octavo-link)))

;;; Note Functions

(defun octavo--note-file-path (id title)
  "Generate full file-path for note with given ID and TITLE."
  (let ((base-name (format "%s%s%s.%s"
                           id
                           octavo-file-name-separator
                           title
                           octavo-file-extension)))
    (concat (file-name-as-directory octavo-directory)
            (when (functionp octavo-subdirectory-function)
              (file-name-as-directory (funcall octavo-subdirectory-function id)))
            (replace-regexp-in-string " "
                                      octavo-file-name-separator
                                      base-name))))

;;;###autoload
(defun octavo-new-note (&optional title)
  "Create a new note, insert link at point of creation.
Optional TITLE argument."
  (interactive)
  (let* ((pref-arg current-prefix-arg)
         (new-id (octavo--generate-id))
         (orig-id (ignore-errors (octavo--file-id buffer-file-name)))
         (text (when (use-region-p)
                 (buffer-substring
                  (region-beginning)
                  (region-end))))
         (title (cond (title title)
                      ((use-region-p)
                       (with-temp-buffer
                         (insert text)
                         (goto-char (point-min))
                         (buffer-substring
                          (point)
                          (line-end-position))))
                      (t (read-string "Note title: "))))
         (body (when (use-region-p)
                 (with-temp-buffer
                   (insert text)
                   (goto-char (point-min))
                   (forward-line 2)
                   (buffer-substring
                    (point)
                    (point-max)))))
         (file-name (octavo--note-file-path new-id title)))
    (unless orig-id
      (setq orig-id octavo-default-backlink))
    (when (use-region-p)
      (kill-region (region-beginning) (region-end)))
    (when (or pref-arg
              (eq octavo-new-note-link-insert 't)
              (and (eq octavo-new-note-link-insert 'octavo)
                   (octavo-file-p))
              (and (eq octavo-new-note-link-insert 'ask)
                   (y-or-n-p "Insert link at point? ")))
      (unless buffer-read-only
        (octavo-insert-link new-id title)))
    (when buffer-file-name
      (save-buffer))
    (find-file file-name)
    (funcall octavo-new-note-header-function title new-id orig-id)
    (when body (insert body))
    (when octavo-enable-link-buttons (octavo-make-link-buttons))
    (save-buffer)))

(defun octavo-new-note-header (title new-id &optional orig-id)
  "Insert header in new notes with args TITLE and NEW-ID.
Optionally use ORIG-ID for backlink."
  (insert (format "# %s %s\n===\ntags: \n" new-id title))
  (when (ignore-errors (octavo--parse-id 'title orig-id)) ;; check for file
    (progn
      (insert "===\n<- ")
      (octavo--insert-link orig-id (octavo--parse-id 'title orig-id))
      (newline)))
  (insert "===\n\n"))

;;;###autoload
(defun octavo-rename-note ()
  "Rename current note and replace title in header.
When header title does not match file title, ask to accept header
title as new title. If no, prompt for new title and replace
header title in buffer. If yes, file name changed to header
title."
  (interactive)
  (read-only-mode -1)
  (let* ((id (octavo--file-id buffer-file-name))
         (file-title (octavo--parse-id 'title id))
         (header-title (progn
                         (save-excursion
                           (goto-char (point-min))
                           (re-search-forward (concat id "."))
                           (buffer-substring-no-properties
                            (point)
                            (line-end-position)))))
         (new-title))
    (unless id
      (user-error "Not an Octavo file"))
    (if (not (string= file-title header-title))
        (if (y-or-n-p (format "Change from \"%s\" to \"%s\"? " file-title header-title))
            (setq new-title header-title)
          (setq new-title (read-string "New title: " file-title)))
      (setq new-title (read-string "New title: " file-title)))
    (when (string-match "\n" new-title)
      (setq new-title (replace-regexp-in-string "\n" "" new-title)))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward id)
      (re-search-forward " ")
      (delete-region (point) (line-end-position))
      (insert new-title))
    (let ((new-file (octavo--note-file-path id new-title)))
      (rename-file buffer-file-name new-file t)
      (set-visited-file-name new-file t t)
      (save-buffer))))

;;; Find File

;;;###autoload
(defun octavo-find-file (&optional other-window)
  "Find file in `octavo-directory'.
If OTHER-WINDOW is non-nil (or command is executed with
\\[universal-argument]), find file in other window."
  (interactive "p")
  (if other-window
      (find-file-other-window
       (octavo-select-file "Find file in other window: "))
    (find-file
       (octavo-select-file "Find file: "))))

;;;###autoload
(defun octavo-find-file-by-id (id)
  "Find file associated with ID."
  (find-file (octavo--parse-id 'file-path id)))

;;;###autoload
(defun octavo-find-file-by-full-text-search (regexp)
  "Find files containing REGEXP."
  (interactive
   (list (read-string "Search string: "
                      nil 'octavo-search-history)))
  (let ((files (octavo--grep-file-list regexp)))
    (if files
        (find-file (octavo-select-file
                    (format "Files containing \"%s\": " regexp) files))
      (user-error "No results for \"%s\"" regexp))))

;;;###autoload
(defun octavo-current-notes ()
  "Select from list of currently open notes.
Optionally call a custom function by setting the variable
`octavo-current-notes-function' to a function name."
  (interactive)
  (if octavo-current-notes-function
      (funcall octavo-current-notes-function)
    (find-file
     (octavo-select-file "Current Notes:" (octavo--current-notes-list)))))

;;; Follow Links

;;;###autoload
(defun octavo-follow-link-at-point (&optional id)
  "Open note that corresponds with the Octavo ID at point."
  (interactive)
  (let ((id (or (octavo--id-at-point)
                id)))
    (if id
        (find-file (octavo--parse-id 'file-path id))
      (error "No octavo-link at point"))))

(defun octavo--links-in-note-list ()
  "Return list of Octavo files that are linked from the current buffer."
  (let* ((octavo-alist (octavo--alist))
         (octavo-ids (octavo--id-list nil octavo-alist))
         id-list)
    (save-buffer)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (octavo-link-regexp) nil t)
        (when (member (match-string-no-properties 1) octavo-ids)
          (push (match-string-no-properties 1) id-list))))
    (if id-list
        (mapcar (lambda (id)
                  (octavo--parse-id 'file-path id octavo-alist))
                (delete-dups id-list))
      (error "No octavo-links in note"))))

;;;###autoload
(defun octavo-links-in-note ()
  "Select from list of notes linked to in the current note."
  (interactive)
  (let* ((files (ignore-errors (octavo--links-in-note-list))))
    (if files
        (find-file (octavo-select-file "Links: " files))
      (user-error "No links found"))))

;;; Insert Link

;;;###autoload
(defun octavo-insert-link (arg &optional title)
  "Insert link to note, from ARG.
By default, only a link is inserted. With prefix-argument, both
link and title are inserted. See variable `octavo-link-and-title'
for additional configurations. Optional TITLE."
  (interactive
   (list (list (octavo-select-file "Insert link: "))))
  (if (octavo--id-at-point)
      (user-error "Move point off octavo-id before inserting")
    (let* ((pref current-prefix-arg))
      (cond
       ((or (and (not pref) (eq 't octavo-link-and-title))
            (and pref (not octavo-link-and-title)))
        (octavo--insert-link arg title))
       ((and (not pref) (eq 'ask octavo-link-and-title))
        (if (y-or-n-p "Include title? ")
            (octavo--insert-link arg title)
          (octavo--insert-link arg)))
       ((or t
            (and pref (eq 't octavo-link-and-title)))
        (octavo--insert-link arg))))))

(defun octavo--insert-link (id &optional title)
  "Insert link to note with ID and TITLE.
If TITLE is non-nil, use `octavo-link-and-title-format',
otherwise `octavo-link-format'."
  (insert (octavo--format (if title
                          octavo-link-and-title-format
                        octavo-link-format)
                      id title))
  (when octavo-enable-link-buttons
    (octavo-make-link-buttons)))

;;; Completion at Point

(defun octavo--format-candidates (&optional files format)
  "Return a list of FILES as formatted candidates, following FORMAT.

See `octavo--format' for details about FORMAT. If nil,
`octavo-completion-at-point-format' will be used by default.

FILES must be a list of filepaths. If nil, all files in `octavo-directory'
will be returned as formatted candidates."
  (let* ((format (or format
                     octavo-completion-at-point-format)))
    (if files
        (octavo--formatter files format)
      (octavo--formatter (octavo--directory-files) format t))))

(defun octavo-completion-at-point ()
  "Completion-at-point function for octavo-links.
When added to `completion-at-point-functions', typing two
brackets \"[[\" initiates completion."
  (let ((case-fold-search t)
        (origin (point)))
    (save-excursion
      (when (and (re-search-backward "\\[\\["
                                     (line-beginning-position)
                                     t)
                 (save-excursion
                   (not (search-forward "]]" origin t))))
        (list (match-end 0)
              origin
              (completion-table-dynamic
               (lambda (_)
                 (octavo--format-candidates)))
              :exit-function
              (lambda (str _status)
                (delete-char (- -2 (length str)))
                (insert str)
                (when octavo-enable-link-buttons
                  (octavo-make-button-before-point))))))))

;;; Copy Link and Title

;;;###autoload
(defun octavo-copy-link-and-title (arg)
  "Copy link and title for id or file ARG."
  (interactive (list (octavo-select-file "Copy link: ")))
  (let ((links (octavo--formatted-string arg octavo-link-and-title-format)))
    (kill-new links)
    (message "Copied: %s" links)))

;;; List Backlinks

(defun octavo--backlinks-list (id)
  "Return list of notes that link to note with ID."
  (octavo--grep-file-list (octavo-link-regexp id)))

;;;###autoload
(defun octavo-backlinks ()
  "Select from list of all notes that link to the current note."
  (interactive)
  (let* ((id (octavo--file-id buffer-file-name))
         (files (octavo--backlinks-list id)))
    (if files
        (find-file (octavo-select-file "Backlinks: " files))
      (user-error "No backlinks found"))))

;;; Search

;;;###autoload
(defun octavo-search (string)
  "Search for STRING using function set in `octavo-search-function'.
Defaults to `octavo-grep.'"
  (interactive
   (list (read-string "Search: "
                      nil 'octavo-search-history)))
  (funcall octavo-search-function string))

(defun octavo-grep (regexp)
  "Wrapper around `rgrep' to search for REGEXP in all notes.
Opens search results in a grep buffer."
  (interactive
   (list (read-string "octavo-grep: "
                      nil 'octavo-search-history)))
  (grep-compute-defaults)
  (rgrep regexp (concat "*." octavo-file-extension) octavo-directory nil))

;;; Tag Functions

;;;###autoload
(defun octavo-tag-search (tag)
  "Open grep buffer containing results of search for TAG.
Select TAG, with completion, from list of all tags in Octavo notes.
Defaults to `octavo-grep'."
  (interactive (list (completing-read "Find tag: " (octavo--grep-tag-list))))
  (funcall octavo-tag-search-function tag))

;;;###autoload
(defun octavo-tag-insert (tag)
  "Insert TAG at point.
Select TAG, with completion, from list of all tags in Octavo notes."
  (interactive (list (completing-read "Insert tag: " (octavo--grep-tag-list))))
  (if (eq octavo-tag-insert-function nil)
      (insert tag)
    (save-excursion
      (funcall octavo-tag-insert-function tag))))

;;; Find Dead Links and Unlinked Notes
(defun octavo--grep-link-id-list ()
  "Return list of all ids that appear as links in `octavo-directory' files."
  (mapcar (lambda (link)
            (when (string-match octavo-id-regexp link)
              (match-string 0 link)))
          (octavo--grep-match-list (octavo-link-regexp) 'unique)))

(defun octavo--dead-link-id-list ()
  "Return list of all links with no corresponding note."
  (let* ((all-link-ids (octavo--grep-link-id-list))
         (all-ids (octavo--id-list)))
    (delete-dups (remq nil (mapcar
                            (lambda (x)
                              (string-match octavo-id-regexp x)
                              (when (not (member (match-string-no-properties 0 x) all-ids))
                                x))
                            all-link-ids)))))

;;;###autoload
(defun octavo-grep-dead-links ()
  "Search for dead links using `octavo-search-function'."
  (interactive)
  (let* ((dead-link-ids (octavo--dead-link-id-list)))
    (if dead-link-ids
        (funcall octavo-search-function (mapconcat
                                     #'identity
                                     dead-link-ids
                                     "\\|"))
      (user-error "No dead links found"))))

(defun octavo--unlinked-notes-list ()
  "Return list of IDs for notes that no notes link to."
  (let* ((all-link-ids (octavo--grep-link-id-list))
         (all-ids (octavo--id-list)))
    (remq nil (mapcar
               (lambda (x)
                 (when (not (member x all-link-ids))
                   x))
               all-ids))))

;;;###autoload
(defun octavo-unlinked-notes ()
  "Find unlinked notes."
  (interactive)
  (let* ((ids (octavo--unlinked-notes-list))
         (notes (mapcar (lambda (id) (octavo--parse-id 'file-path id)) ids)))
    (if notes
        (find-file (octavo-select-file "Unlinked notes: " notes))
      (user-error "No unlinked notes found"))))

;;; octavo-network - Backlinks and Forward Links Together

(defun octavo-network ()
  "Find `octavo-backlinks' and `octavo-links-in-note' for current or selected note.
Backlinks and Links-in-Note are grouped separately."
  (interactive)
  (unless (octavo-file-p)
    (user-error "Not an Octavo file"))
  (let* ((id (octavo--file-id buffer-file-name))
         (backlinks (ignore-errors (octavo--backlinks-list id)))
         (links-in-note (ignore-errors (octavo--links-in-note-list)))
         (resources))
    (if (or backlinks links-in-note)
        (progn
          (dolist (file links-in-note)
            ;; abbreviate-file-name allows a file to be in both groups
            (push (propertize (abbreviate-file-name file) 'type 'link) resources))
          (dolist (file backlinks)
            (push (propertize file 'type 'backlink) resources))
          (find-file (octavo-select-file "Links: "
                                     resources
                                     'octavo--network-group-function
                                     'identity)))
      (user-error "No links found"))))

(defun octavo--network-group-function (file transform)
  "Group FILE by type and TRANSFORM."
  (cond (transform (octavo--file-title file))
        ((eq 'backlink (get-text-property 0 'type file)) "Backlinks")
        ((eq 'link (get-text-property 0 'type file)) "Links-in-Note")
        (t
         (error "Unexpected condition"))))

;; (defun octavo--network-sort-function (list)
;;   "Sort LIST of links so Backlinks group is first."
;;   (sort list
;;         (lambda (a _b)
;;           (when (eq 'backlink (get-text-property 0 'type a))
;;               t))))

(provide 'octavo)

;;; octavo.el ends here
