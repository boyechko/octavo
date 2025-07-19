;;; octavo-tests.el --- Unit tests for octavo.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Richard Boyechko

;; Author: Richard Boyechko <https://github.com/boyechko>
;; Created: January 4, 2022
;; License: GPL-3.0-or-later
;; Version: 0.5
;; Homepage: https://github.com/boyechko/octavo
;; Package-Requires: ((emacs "25.1"))

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

;; To run the tests: 1) evaluate this buffer; 2) press C-c C-e (or `ert')
;;
;; Tags:
;; - disabled :: Broken or obsolete; will most likely fail.
;; - benchmark :: The test will take a few seconds or minutesj to run.

;;; Code:

(require 'ert)
(require 'elp)

(custom-set-variables
 '(octavo-file-extension "txt")
 '(octavo-index-invisible-ids nil)
 '(octavo-index-format "%t [[%i]]")
 '(octavo-directory-recursive t))

(defmacro defun-in-dir (name directory)
  "Define function NAME that will return a pathname relative to DIRECTORY."
  `(defun ,name (&optional path)
     ,(format "Return absolute pathname of relative PATH in %s."
              (eval directory))
     (if path
         (expand-file-name path ,directory)
       (file-name-as-directory ,directory))))

(defun-in-dir in-crispy-memory (getenv "ZETTEL_DIR"))

(defvar octavo-tests-environments
  '((:many-in-subdirs
     (octavo-directory "~/.emacs.d/straight/repos/octavo/tests/sandbox/many-in-subdirs")
     (octavo-id-regexp "\\([0-9]\\{12\\}\\)")
     (octavo-subdirectory-function nil))
    (:standard
     (octavo-directory "~/.emacs.d/straight/repos/octavo/tests/sandbox/standard")
     (octavo-id-regexp "\\([0-9]\\{12\\}\\)")
     (octavo-directory-recursive nil)
     (octavo-subdirectory-function nil))
    (:wikilinks
     (octavo-directory "~/.emacs.d/straight/repos/octavo/tests/sandbox/wikilinks")
     (octavo-id-regexp "\\([[:alnum:]]+\\)")
     (octavo-index-format "%t [[%i]]")
     (octavo-directory-recursive nil)
     (octavo-subdirectory-function nil))
    (:numerus
     (octavo-directory (in-crispy-memory "numerus/"))
     (octavo-subdirectory-function (ezeka-kasten-subdir-func (ezeka-kasten "numerus")))
     (octavo-id-regexp "\\([a-z0-9T~-]\\{6,13\\}\\)"))
    (:tempus
     (octavo-directory (in-crispy-memory "tempus/"))
     (octavo-subdirectory-function (ezeka-kasten-subdir-func (ezeka-kasten "tempus")))
     (octavo-id-regexp "\\([a-z0-9T~-]\\{6,13\\}\\)"))
    (:tempus2013
     (octavo-directory (in-crispy-memory "tempus/2013/"))
     (octavo-subdirectory-function (ezeka-kasten-subdir-func (ezeka-kasten "tempus")))
     (octavo-id-regexp "\\([a-z0-9T~-]\\{6,13\\}\\)"))
    (:scriptum
     (octavo-directory (in-crispy-memory "scriptum/"))
     (octavo-subdirectory-function (ezeka-kasten-subdir-func (ezeka-kasten "scriptum")))
     (octavo-id-regexp "\\([a-z0-9T~-]\\{6,13\\}\\)")))
  "Description of my testing environments.
Each item has the form of (:name varlist)")

(defvar octavo-tests-sample-files
  '((:standard
     "202206052002 {Event} WaterBear Film Screening Series at Minim.txt")
    (:tempus
     "20220812T2046 Testing Octavo with T filename.txt")
    (:numerus
     "a-0000 {κ} Central Index.txt")
    (:scriptum
     "v-1268~73 {Review} _A Prehistory of the Cloud_ @Hu2015.txt"))
  "An alist of sample files relative to `octavo-directory'.
The form is (:ENVIRONMENT SAMPLE1 [... SAMPLEN]).")

;;;=============================================================================
;;; Helper Functions
;;;=============================================================================

(defun octavo-tests-sample-files-for (env &optional full-path)
  "Return a sample file for the given ENV'ironment.
If FULL-PATH is non-nil, return full paths."
  (let ((dir (alist-get 'octavo-directory
                        (alist-get
                         env
                         octavo-tests-environments))))
    (mapcar (if full-path
                (lambda (f)
                  (expand-file-name f dir))
              #'identity)
            (alist-get env octavo-tests-sample-files))))

(defmacro with-octavo-tests-environment (env varlist &rest body)
  "Evaluate BODY inside a `let` form binding ENV's varlist.
Additional variables can be defined in VARLIST"
  (declare (indent 2))
  `(let (,@(alist-get env octavo-tests-environments)
         ,@varlist)
     ,@body))

(defun octavo-tests-set-environment (env)
  "Set the variables needed to work wiht environment ENV."
  (interactive
   (list (completing-read "Which environment? " octavo-tests-environments)))
  (apply #'custom-set-variables (alist-get (intern-soft env) octavo-tests-environments)))

(ert-deftest with-octavo-tests-environment ()
  :tags '(:disabled)
  (with-octavo-tests-environment :many-in-subdirs
    ((octavo-subdirectory-function nil))
    (should-not octavo-subdirectory-function)
    (should (string= octavo-directory
                     (cadr
                      (cl-find 'octavo-directory (alist-get :many-in-subdirs octavo-tests-environments)
                               :key #'car :test #'eq))))
    (should (string=
             (car (octavo-tests-sample-files-for :standard))
             "202206052002 {Event} WaterBear Film Screening Series at Minim.txt"))))

(ert-deftest octavo-tests-reload-octavo ()
  (load-file (straight--repos-file "octavo" "octavo.el"))
  (load-file (straight--repos-file "octavo" "octavo-index.el")))

;;;=============================================================================
;;; Actual Tests
;;;=============================================================================

(ert-deftest octavo--file-id ()
  (with-octavo-tests-environment :tempus ()
    (with-current-buffer "octavo-tests.el"
      (should-not (octavo--file-id buffer-file-name)))
    (let* ((file (car (octavo-tests-sample-files-for :tempus))))
      (should (string= (octavo--file-id file) "20220812T2046")))))

;; FIXME: Update? Fix?
(ert-deftest octavo--format ()
  (should
   (string= "a-1234 This is a title"
            (octavo--format "%i %t" "a-1234" "This is a title")))
  (should
   (string= "This is a title <<a-1234>>"
            (octavo--format "%t <<%i>>" "a-1234" "This is a title")))
  (let ((octavo-format-function
         (lambda (fmt id title)
           (format fmt id title))))
    (should (string= "a-1234 --- This is a title"
             (octavo--format "%s --- %s" "a-1234" "This is a title")))))

(ert-deftest octavo-link-regexp ()
  (should (string= (octavo-link-regexp) "\\[\\[\\([0-9]\\{12\\}\\)]]")))

(ert-deftest octavo--grep-file-list ()
  :tags '(:disabled)                    ; after PR #41
  (with-octavo-tests-environment :standard ()
    (should (eq 144 (length (octavo--grep-file-list " single "))))
    (should (eq 42 (length (octavo--grep-file-list " double "))))
    (should (eq 180 (length (octavo--grep-file-list " \\(single\\|double\\) " nil))))
    (should (eq 0 (length (octavo--grep-file-list " (single|double) " nil))))
    (should (eq 180 (length (octavo--grep-file-list " (single|double) " t))))
    (should (eq 2063 (length (octavo--grep-file-list " (single|double) " t t))))))

(ert-deftest octavo--directory-files ()
  (ezeka-octavo-hacks-mode -1)
  (with-octavo-tests-environment :standard ()
    (should (= 2243 (length (octavo--directory-files)))))
  (with-octavo-tests-environment :many-in-subdirs ()
    (should (= 2242 (length (octavo--directory-files)))))
  (ezeka-octavo-hacks-mode 1)
  (with-octavo-tests-environment :numerus ()
    (should-not (= 0 (length (octavo--directory-files)))))
  (with-octavo-tests-environment :tempus ()
    ;; requires changing (octavo-file-name-regexp)
    (should (= 0 (length (octavo--directory-files))))))

(ert-deftest octavo--directory-files-w/-tempus-currens ()
  :tags '(:disabled)                    ; needs implementing
  (with-octavo-tests-environment :tempus ()
    (should-not (= 0 (length (octavo--directory-files))))))

(ert-deftest octavo--wildcard-file-path ()
  :tags '(:disabled)                    ; pull request
  (with-octavo-tests-environment :numerus ()
    (should (string= "/Users/richard/Zettelkasten/numerus/a/a-0000 {κ} Central Index."
                     (octavo--wildcard-file-path "a-0000"))))
  (with-octavo-tests-environment :tempus ()
    (should (string= "/Users/richard/Zettelkasten/tempus/2022/20221025T2032.txt"
                     (octavo--wildcard-file-path "20221025T2032")))))

(ert-deftest octavo--note-file-path ()
  :tags '(:disabled)
  (with-octavo-tests-environment :many-in-subdirs
    ((octavo-subdirectory-function nil))
    (should (string=
             (car (octavo-tests-sample-files-for :standard t))
             (octavo--note-file-path "202111102331"
                                 "{Event} WaterBear Film Screening Series at Minim")))))

(ert-deftest octavo-subdirectory-function ()
  (with-octavo-tests-environment :many-in-subdirs
    ((octavo-subdirectory-function (lambda (id) (cl-subseq id 0 4)))
     (file (car (octavo-tests-sample-files-for :many-in-subdirs))))
    ;; FIXME: Rewrite using FILE
    (should (string=
             "~/Octavo/many-in-subdirs/2020/20200101T0101 Title of note.txt"
             (octavo--note-file-path "20200101T0101" "Title of note")))))

(ert-deftest octavo--parse-file ()
  (with-octavo-tests-environment :standard
    ((file (car (octavo-tests-sample-files-for :standard))))
    (should (string= (octavo--parse-file 'id file) "202206052002"))
    (should (string= (octavo--parse-file 'title file)
                     "{Event} WaterBear Film Screening Series at Minim"))
    (should-error (octavo--parse-file 'foobar file))))

(ert-deftest octavo--parse-file ()
  "Test `octavo--parse-file' after `octavo--file-name-(id|title)' are added."
  :tags '(:disabled)
  (let ((orig-a (symbol-function 'octavo--file-name-id))
        (orig-b (symbol-function 'octavo--file-name-title)))
    (unwind-protect
        (with-octavo-tests-environment :standard
          ((file (car (octavo-tests-sample-files-for :standard))))
          (defalias 'octavo--file-name-id
            #'(lambda (_) "202206052002"))
          (defalias 'octavo--file-name-title
            #'(lambda (_) "{Event} WaterBear Film Screening Series at Minim"))
          (should (string= (octavo--parse-file 'id file) "202206052002"))
          (should (string= (octavo--parse-file 'title file)
                           "{Event} WaterBear Film Screening Series at Minim")))
      (defalias 'octavo--file-name-id orig-a)
      (defalias 'octavo--file-name-id orig-b))))

(ert-deftest octavo--parse-file ()
  "Test that `octavo--parse-file' behaves correctly when `octavo-id-regexp' changes."
  :tags '(:disabled)
  (let ((file1 (octavo-tests-sample-files-for :standard))
        (file2 (octavo-tests-sample-files-for :tempus))
        (octavo-id-regexp "\\([0-9]\\{12\\}\\)"))
    (should (string= (octavo--parse-file 'id file1) "202206052002"))
    (should (string= (octavo--parse-file 'id file2) nil))
    (let ((octavo-id-regexp "\\([0-9T]\\{13\\}\\)"))
      (should (string= (octavo--parse-file 'id file1) nil))
      (should (string= (octavo--parse-file 'id file2) "20220812T2046")))))

(ert-deftest octavo-file-p ()
  "Test that `octavo-file-p' catches non-octavo files."
  :tags '(:disabled)
  (let ((file1 (car (octavo-tests-sample-files-for :standard)))
        (file2 (car (octavo-tests-sample-files-for :tempus)))
        (octavo-id-regexp "\\([0-9]\\{12\\}\\)"))
    (should (octavo-file-p file1))
    (should (string= (match-string 1 file1) "202111102331"))
    (should-not (octavo-file-p file2))))

(ert-deftest octavo-file-name-regexp ()
  "Check that `octavo-file-name-regexp' matches the right files."
  :tags '(:disabled)
  (let ((file1 (elt tests-octavo--files 0))
        (octavo-id-regexp "\\([0-9]\\{12\\}\\)"))
    (message octavo-id-regexp)
    (should (string-match (octavo-file-name-regexp) file1))
    (should (string= (octavo--file-name-id file1) "202111102331"))
    (should (string= (octavo--file-name-title file1) "{Talk} Animal History in the Anthropocene"))))

(ert-deftest octavo--singleton-p ()
  :tags '(:benchmark :disabled)
  (with-octavo-tests-environment :standard
    ((files (octavo--directory-files)))
    (should (octavo--singleton-p '(1)))
    (should-not (octavo--singleton-p '()))
    (should-not (octavo--singleton-p '(1 2 3)))
    (garbage-collect)
    (should (equal
             (benchmark-run 1000 (eq 1 (length files)))
             (benchmark-run 1000 (octavo--singleton-p files))))))

;;;=============================================================================
;;; Unlinked Notes (2025-07-19)
;;;=============================================================================

(ert-deftest octavo--ids-hash-table ()
  (let ((table (octavo--ids-hash-table '("a-1234" "b-2345" "c-3456"))))
    (should (eq 'hash-table (type-of table)))
    (should (gethash "b-2345" table))
    (should-not (gethash "d-4567" table))))

(ert-deftest octavo-unlinked-notes ()
  (let ((unlinked (octavo-unlinked-notes)))
    (should (= (length unlinked) 1))))

;;;=============================================================================
;;; octavo--processor
;;;=============================================================================

(defun octavo--processor/gr (arg)
  "Return list of files.
ARG can be octavo-file or octavo-id as string or list, single or multiple."
  (let* ((octavo-alist (octavo--alist))
         (files (cond
                 ((stringp arg)
                  (if (octavo-file-p arg)
                      (list arg)
                    (list (octavo--parse-id 'file-path arg octavo-alist))))
                 ((octavo--singleton-p arg)
                  (if (octavo-file-p (car arg))
                      arg
                    (list (octavo--parse-id 'file-path (car arg) octavo-alist))))
                 (t
                  (if (octavo-file-p (car arg))
                      arg
                    (octavo--parse-id 'file-path arg octavo-alist))))))
    files))

(ert-deftest octavo--processor ()
  (with-octavo-tests-environment :numerus ()
    (should (equal (octavo--processor/gr "a-0000")
                   (octavo--processor "a-0000")))
    (should (equal (octavo--processor/gr '("a-0000" "c-4317"))
                   (octavo--processor '("a-0000" "c-4317"))))
    (should-error (octavo--processor 4))
    ))

;;;=============================================================================
;;; Octavo-note
;;;=============================================================================

(ert-deftest octavo--alist ()
  "Make sure `octavo--alist' generates the correct structure."
  :tags '(:disabled)                    ; octavo-note
  (with-octavo-tests-environment :standard ()
    (let* ((alist (octavo--alist (octavo--directory-files)))
           (note (cdr (assoc "201004292342" alist))))
      (should (octavo--note-p note))
      (should (string= "201004292342" (octavo--note-id note)))
      (should (string= "{Talk} Lauren Berlant, 'On the Desire for the Political' @ PSU"
                       (octavo--note-title note)))
      (should (string= "201004292342 {Talk} Lauren Berlant, 'On the Desire for the Political' @ PSU.txt"
                       (octavo--note-file note))))))

(ert-deftest bm/octavo--alist ()
  :tags '(:benchmark)
  (ert-run-tests-batch "octavo-tests-reload-octavo")
  (garbage-collect)
  (with-octavo-tests-environment :standard ()
    (should (benchmark-run 100 (octavo--alist)))
    (let ((files (octavo--directory-files)))
      (should (null (benchmark-run 100 (octavo--alist files)))))))

;;; 100 (octavo--alist) on :standard (6.449468 12 1.1682259999999998)
;;; 100 (octavo--alist files) on :standard (4.3255930000000005 11 1.0541220000000004)

;;;=============================================================================
;;; Benchmarks
;;;=============================================================================

(ert-deftest bm/octavo--generate-id ()
  :tags '(:benchmark)
  (ert-run-tests-batch "octavo-tests-reload-octavo")
  (with-octavo-tests-environment :standard ()
    (should (null
             (benchmark-run 100
               (octavo--generate-id))))))

(ert-deftest bm/octavo--parse-id ()
  :tags '(:benchmark)
  ;;; 100 on :numerus with 3829 files (14.105008 24 2.713031)
  (ert-run-tests-batch "octavo-tests-reload-octavo")
  (garbage-collect)
  (with-octavo-tests-environment :numerus ()
    (should (null
             (benchmark-run 100
               (octavo--parse-id 'file-path "l-0614"))))))

(ert-deftest bm/octavo--wildcard-file-path ()
  :tags '(:benchmark)
  ;; 100 on :numerus with 3829 files (0.092291 0 0.0)
  (ert-run-tests-batch "octavo-tests-reload-octavo")
  (garbage-collect)
  (with-octavo-tests-environment :numerus ()
    (should (string= (octavo--wildcard-file-path "g-9172")
                     "/Users/richard/Zettelkasten/numerus/g/g-9172 {λ} size of a thought @Kuehn.txt"))
    (should (null
             (benchmark-run 100
               (octavo--wildcard-file-path "g-9172"))))))

(ert-deftest bm/octavo-insert-link ()
  :tags '(:benchmark)
  ;;; 100 on :numerus with 4502 files
  ;;(ert-run-tests-batch "octavo-tests-reload-octavo")
  (garbage-collect)
  (with-octavo-tests-environment :numerus ()
    (with-temp-buffer
      (should (null
               (benchmark-run 100
                 (octavo-insert-link "l-0614")
                 (insert "\n")))))))

(ert-deftest bm/octavo-completion-at-point ()
  :tags '(:benchmark)
  (elp-reset-all)
  (elp-instrument-package "octavo")
  (fset 'five-completions
        (kmacro-lambda-form  0 "%d"))
  (with-octavo-tests-environment :standard
    ((file (car (octavo-tests-sample-files-for :standard)))
     (five-completions [?\[ ?\[ ?m ?e ?m ?o tab tab return
                            ?\[ ?\[ ?t ?r ?p ?g tab return
                            ?\[ ?\[ ?w ?o ?m ?e ?n tab tab return
                            ?\[ ?\[ ?m ?e ?n tab tab return
                            ?\[ ?\[ ?a ?n ?o ?t ?h ?e ?r tab tab return]))
    (switch-to-buffer-other-window (generate-new-buffer "*Test*"))
    (org-mode)
    (add-to-list 'completion-at-point-functions #'octavo-completion-at-point)
    (dotimes (n 10)
      (execute-kbd-macro five-completions)))
  (let ((commit (read-string "Commit: "))
        (time (format-time-string "%F %R"))
        (files (length (octavo--directory-files))))
    (elp-results)
    (with-current-buffer (get-buffer "*ELP Profiling Results*")
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert
         (format ";;; %s on %s with %d files\n" time commit files)))
      (write-file (format "%s on %s.txt" time commit)))
    (elp-restore-all)))

(ert-deftest bm/octavo-index ()
  :tags '(:benchmark)
  (elp-instrument-package "octavo")
  (with-octavo-tests-environment
      :standard
      ((reps 10)
       (commit (or (ignore-errors
                     (string-trim
                      (shell-command-to-string "git rev-parse --short HEAD")))
                   (read-string "Commit: ")))
       (file (car (octavo-tests-sample-files-for :standard)))
       (time (format-time-string "%F %R"))
       (lines)
     (results))
    (setq results
      (benchmark-run reps
        (should (string= (octavo--parse-file 'id file) "202206052002"))
        (should (string= (octavo--parse-file 'title file)
                         "{Event} WaterBear Film Screening Series at Minim"))
        (octavo-index)
        (with-current-buffer octavo-index-buffer-name
          (setq lines (count-lines (point-min) (point-max))))
        (kill-buffer octavo-index-buffer-name)))
    (elp-results)
    (with-current-buffer (get-buffer "*ELP Profiling Results*")
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert
         (format ";;; %s on %s\n;;; %d rep(s), %d files: %s\n"
                 time commit
                 reps lines results)))
      (write-file (format "%s on %s.txt" time commit)))))

;;;=============================================================================
;;; octavo--id-file (2023-06-30)
;;;=============================================================================

(ert-deftest octavo--id-file ()
  (with-octavo-tests-environment :standard
    ((file (car (octavo-tests-sample-files-for :standard))))
    (should (string= (file-name-base (octavo--id-file "202206052002"))
                     (file-name-base file)))))

;;;=============================================================================
;;; octavo--parse-id (2023-06-30)
;;;=============================================================================

(ert-deftest octavo--parse-id ()
  (with-octavo-tests-environment :standard
    ((file (car (octavo-tests-sample-files-for :standard))))
    (should (string= (file-name-base (octavo--parse-id 'file-path "202206052002"))
                     (file-name-base file)))
    (should (string= (octavo--parse-id 'title "202206052002")
                     "{Event} WaterBear Film Screening Series at Minim"))
    (should-error (octavo--parse-id 'foobar "202206052002"))))

(defun rb/octavo--parse-id (target id &optional octavo-alist)
  "Return TARGET, either `file-path or `title, from file with ID.
Takes a single ID, as a string. Takes an optional OCTAVO-ALIST, for
efficiency if `octavo--parse-id' is called in an internal loop."
  (let ((file (octavo--id-file id)))
    (cond ((eq target 'file-path)
           file)
          ((eq target 'title)
           (if (string-match (octavo-file-name-regexp) (file-name-nondirectory file))
               (match-string 2 (file-name-nondirectory file))
             (error "Cannot figure out title for file with ID %s: %s"
                    id (file-name-nondirectory file))))
          (t (error "Invalid target: %s" target)))))

(defun gr/octavo--parse-id (target ids &optional octavo-alist)
  "Return TARGET, either `file-path or `title, from files with IDS.
Takes a single ID, as a string, or a list of IDs. Takes an
optional OCTAVO-ALIST, for efficiency if `octavo--parse-id' is called
in an internal loop."
  (cond
   ((and (eq target 'file-path)
         (stringp ids))
    (car (octavo--directory-files t ids)))
   ((and (eq target 'file-path)
         (octavo--singleton-p ids))
    (car (octavo--directory-files t (car ids))))
   (t
    (let* ((octavo-alist (or octavo-alist
                         (octavo--alist)))
           (octavo-id-list (octavo--id-list))
           (return
            (cond ((eq target 'file-path)
                   (cond ((stringp ids)
                          (if (member ids octavo-id-list)
                              (cddr (assoc ids octavo-alist))
                            (user-error "No file associated with %s" ids)))
                         ((listp ids)
                          (mapcar
                           (lambda (x)
                             (caddr (assoc x octavo-alist)))
                           ids))))
                  ((eq target 'title)
                   (cond ((stringp ids)
                          (if (member ids octavo-id-list)
                              (cadr (assoc ids octavo-alist))
                            (user-error "No file associated with %s" ids)))
                         ((listp ids)
                          (mapcar
                           (lambda (x)
                             (cadr (assoc x octavo-alist)))
                           ids)))))))
      (if (octavo--singleton-p return)
          (car return)
        return)))))

(ert-deftest rb/octavo--parse-id ()
  (with-octavo-tests-environment :standard
    ((file (car (octavo-tests-sample-files-for :standard))))
    (should (string= (file-name-base (rb/octavo--parse-id 'file-path "202206052002"))
                     (file-name-base file)))
    (should (string= (rb/octavo--parse-id 'title "202206052002")
                     "{Event} WaterBear Film Screening Series at Minim"))
    (should-error (rb/octavo--parse-id 'foobar "202206052002"))))

(ert-deftest benchmark/octavo--parse-id ()
  :tags '(:benchmark)
  (ert-run-tests-batch "octavo-tests-reload-octavo")
  (garbage-collect)
  (with-octavo-tests-environment :standard ()
    (should (benchmark-run 100 (gr/octavo--parse-id 'file-path "202206052002")))
    (should (benchmark-run 100 (octavo--parse-id 'file-path "202206052002")))))

;;;=============================================================================
;;; Octavo-index-query-files (2023-07-10)
;;;=============================================================================

(ert-deftest octavo-index-query-files ()
  (with-octavo-tests-environment :numerus ()
    (switch-to-buffer octavo-index-buffer-name)
    (octavo-index-refresh)
    (should (= 14 (length (octavo-index-query-files "humor" 'octavo-index-focus))))
    (octavo-index-refresh)
    (should (= 83 (length (octavo-index-query-files "humor" 'octavo-index-search))))))

(ert-deftest benchmark/octavo-index-query-files ()
  :tags '(:benchmark)
  (ert-run-tests-batch "octavo-tests-reload-octavo")
  (with-octavo-tests-environment :numerus ()
    (switch-to-buffer octavo-index-buffer-name)
    (should (benchmark-run 10           ; (26.718512 80 9.696325000000002)
              (octavo-index-refresh)
              (garbage-collect)
              (octavo-index-query-files "humor" 'octavo-index-focus)))
    (should (benchmark-run 10           ; (34.558523 70 11.692167000000005)
              (octavo-index-refresh)
              (garbage-collect)
              (octavo-index-query-files "humor" 'octavo-index-search)))))

(ert-deftest benchmark/rb/octavo-index-query-files ()
  :tags '(:benchmark)
  (ert-run-tests-batch "octavo-tests-reload-octavo")
  (with-octavo-tests-environment :numerus ()
    (switch-to-buffer octavo-index-buffer-name)
    (should (benchmark-run 10           ; (21.83563 70 9.479023999999995)
              (octavo-index-refresh)
              (garbage-collect)
              (octavo-index-query-files/rb "humor" 'octavo-index-focus)))
    (should (benchmark-run 10           ; (26.18411 70 9.764894999999996)
              (octavo-index-refresh)
              (garbage-collect)
              (octavo-index-query-files/rb "humor" 'octavo-index-search)))))

;;;=============================================================================
;;; Regexps (pull request #63; 2023-07-14)
;;;=============================================================================

(ert-deftest octavo--posix-regexp ()
  (let ((numerus "\\(?:[a-z]-[0-9]\\{4\\}\\)")
        (tempus "\\([0-9]\\{8\\}T[0-9]\\{4\\}\\)")
        (all "\\([a-z]-[0-9]\\{4\\}~[0-9][0-9]\\|[0-9]\\{8\\}T[0-9]\\{4\\}\\|[a-z]-[0-9]\\{4\\}\\)"))
    (should (string= (octavo--posix-regexp "\\(.*\\)") "(.*)"))
    (should (string= (octavo--posix-regexp "\\(.*\\)" 'basic) "\\(.*\\)"))
    (should (string= (octavo--posix-regexp numerus)
                     "(?:[a-z]-[0-9]{4})"))
    (should (string= (octavo--posix-regexp numerus 'basic)
                     "\\([a-z]-[0-9]\\{4\\}\\)"))
    (should (string= (octavo--posix-regexp tempus)
                     "([0-9]{8}T[0-9]{4})"))
    (should
     (string= (octavo--posix-regexp all)
              "([a-z]-[0-9]{4}~[0-9][0-9]|[0-9]{8}T[0-9]{4}|[a-z]-[0-9]{4})"))
    (should
     (string= (octavo--posix-regexp all 'basic)
              "\\([a-z]-[0-9]\\{4\\}~[0-9][0-9]\\|[0-9]\\{8\\}T[0-9]\\{4\\}\\|[a-z]-[0-9]\\{4\\}\\)"))
    (should (benchmark 10000 (octavo--posix-regexp all)))))

(ert-deftest octavo--grep-commands ()
  (with-octavo-tests-environment :scriptum ()
    (should (= 18 (length (octavo--grep-file-list "Taiwan"))))
    ;; extended regexp
    (should (= 32 (length (octavo--grep-file-list "\\(garbage\\|waste\\)"))))
    ;; tags
    (should (= 15 (length (octavo--grep-tag-list)))))
  (with-octavo-tests-environment :numerus ()
    (should (= (length (octavo--backlinks-list "t-7019")) 19)) ; Yomi Braester [[t-7019]]
    (should (= (length (octavo--backlinks-list "y-7690")) 1)))) ; Lee Yu-lin [[y-7690]]

;;;=============================================================================
;;; octavo--id-list (2023-07-16)
;;;=============================================================================

(defun octavo--id-list/orig (&optional str octavo-alist) ; 2023-07-16
  "Return a list of octavo IDs for notes in `octavo-directory'.
Optional search for STR in note title, case-insenstive. Takes an
optional OCTAVO-ALIST, for efficiency if `octavo--id-list' is called in
an internal loop."
  (if str
      (let ((octavo-alist (or octavo-alist (octavo--alist)))
            (case-fold-search t)
            (ids))
        (dolist (item octavo-alist)
          (if str
              (when (string-match str (cadr item))
                (push (car item) ids))
            (push (car item) ids)))
        ids)
    (octavo--parse-file 'id (octavo--directory-files t))))

(ert-deftest octavo--id-list ()
  :tags '()
  (with-octavo-tests-environment :tempus ()
    (should (= (length (octavo--id-list)) (length (octavo--id-list/orig)))) ; no args
    (should (= (length (octavo--id-list "yomi")) (length (octavo--id-list/orig "yomi")))) ; str
    (let ((octavo-alist (octavo--alist)))
      (should (= (length (octavo--id-list nil octavo-alist))
                 (length (octavo--id-list/orig nil octavo-alist)))) ; octavo-alist
      (should (= (length (octavo--id-list "yomi" octavo-alist))
                 (length (octavo--id-list/orig "yomi" octavo-alist)))) ; octavo-alist + str
      (should (= (length (octavo--id-list "^2014" octavo-alist))
                 (length (octavo--id-list/orig "^2014" octavo-alist))))
      )))

(defmacro octavo-tests-benchmark-run (n description &rest forms)
  "Call `benchmark-run' on FORMS with N reptitions, and formatted string.
The return consists of DESCRIPTION and return of
`benchmark-run'; if DESCRIPTION is nil, use FORMS."
  (declare (indent 2))
  `(progn
     (garbage-collect)
     (format "%45s in %s"
             (format "%-40S => %5d results"
                     (or ,description (quote ,forms))
                     (length ,@forms))
             (cl-destructuring-bind (time gcs gc-time)
                 (benchmark-run n ,@forms)
               (format "%0.2f sec (inc. %0.2f sec for %d GCs)"
                       time gc-time time)))))

(ert-deftest bm/octavo--id-list+search-ids ()
  :tags '(:benchmark)
  (ert-run-tests-batch "octavo-tests-reload-octavo")
  (garbage-collect)
  (let ((n 10)
        results)
    (unwind-protect
        (with-octavo-tests-environment :tempus ()
          (push (octavo-tests-benchmark-run n nil (octavo--id-list/orig)) results)
          (push (octavo-tests-benchmark-run n nil (octavo--id-list)) results)
          (let ((octavo-alist (octavo--alist)))
            (push (octavo-tests-benchmark-run n nil (octavo--id-list/orig nil octavo-alist)) results)
            (push (octavo-tests-benchmark-run n nil (octavo--id-list nil octavo-alist)) results)
            (push (octavo-tests-benchmark-run n nil (octavo--id-list/orig "yomi" octavo-alist)) results)
            (push (octavo-tests-benchmark-run n nil (octavo--id-list "yomi" octavo-alist)) results)))
      (with-current-buffer (get-buffer-create "*ERT Results*")
        (goto-char (point-max))
        (insert (format "\n\n=== %s (%d reps) at %s ===\n"
                        "bm/octavo--id-list+search-ids" n
                        (format-time-string "%F %R")))
        (insert (mapconcat #'identity (nreverse results) "\n"))
        (goto-char (point-max)))
      (switch-to-buffer-other-window "*ERT Results*"))))

(ert-deftest bm/octavo--id-list+use-octavo-alist ()
  :tags '(:benchmark)
  (ert-run-tests-batch "octavo-tests-reload-octavo")
  (garbage-collect)
  (let ((n 10)
        results)
    (unwind-protect
        (with-octavo-tests-environment :tempus ()
          (push (octavo-tests-benchmark-run n nil (octavo--id-list/orig)) results)
          (push (octavo-tests-benchmark-run n nil (octavo--id-list)) results)
          (push (octavo-tests-benchmark-run n nil (octavo--id-list/orig "yomi")) results)
          (push (octavo-tests-benchmark-run n nil (octavo--id-list "yomi")) results)
          (push (octavo-tests-benchmark-run n nil (octavo--id-list/orig "^2014")) results)
          (push (octavo-tests-benchmark-run n nil (octavo--id-list "^2014")) results)
          (let ((octavo-alist (octavo--alist)))
            (push (octavo-tests-benchmark-run n nil (octavo--id-list/orig nil octavo-alist)) results)
            (push (octavo-tests-benchmark-run n nil (octavo--id-list nil octavo-alist)) results)
            (push (octavo-tests-benchmark-run n nil (octavo--id-list/orig "yomi" octavo-alist)) results)
            (push (octavo-tests-benchmark-run n nil (octavo--id-list "yomi" octavo-alist)) results)))
      (with-current-buffer (get-buffer-create "*ERT Results*")
        (goto-char (point-max))
        (insert (format "\n\n=== %s (%d reps) at %s ===\n"
                        "bm/octavo--id-list+use-octavo-alist" n
                        (format-time-string "%F %R")))
        (insert (mapconcat #'identity (nreverse results) "\n"))
        (goto-char (point-max)))
      (switch-to-buffer-other-window "*ERT Results*"))))

;;;=============================================================================
;;; Ripgrep (2025-07-19)
;;;=============================================================================

(defun octavo--benchmark-grep-performance ()
  "Benchmark egrep vs ripgrep performance."
  (interactive)
  (when (octavo--ripgrep-available-p)
    (let ((test-regexp (octavo-link-regexp))
          (test-options '("--only-matching" "--no-filename")))

      ;; Test egrep
      (message "Testing egrep...")
      (let ((egrep-time (benchmark-run 1
                          (let ((octavo-use-ripgrep nil))
                            (octavo--grep-command test-regexp octavo-directory test-options)))))

        ;; Test ripgrep
        (message "Testing ripgrep...")
        (let ((ripgrep-time (benchmark-run 1
                              (let ((octavo-use-ripgrep t))
                                (octavo--grep-command test-regexp octavo-directory test-options)))))

          (message "egrep time: %.2f seconds" (car egrep-time))
          (message "ripgrep time: %.2f seconds" (car ripgrep-time))
          (message "ripgrep is %.1fx faster" (/ (car egrep-time) (car ripgrep-time))))))))

;;;=============================================================================
;;; octavo-index-octavo-index--current-id-list
;;;=============================================================================

(ert-deftest octavo-index--current-id-list ()
  (with-octavo-tests-environment :numerus ()
    (should (= 6 (length (octavo-index--current-id-list (get-buffer "*Octavo-Index: Numerus*") 0 275))))
    (should (= 6 (length (octavo-index--current-id-list (get-buffer "*Octavo-Index: Numerus*") 0 310))))
    (should (= 4592 (length (octavo-index--current-id-list (get-buffer "*Octavo-Index: Numerus*")))))))

(provide 'octavo-tests)
;;; octavo-tests.el ends here
