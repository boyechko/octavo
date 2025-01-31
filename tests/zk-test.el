;;; zk-test.el --- Unit tests for zk.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Richard Boyechko

;; Author: Richard Boyechko <https://github.com/boyechko>
;; Created: January 4, 2022
;; License: GPL-3.0-or-later
;; Version: 0.5
;; Homepage: https://github.com/localauthor/zk
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
(define-key emacs-lisp-mode-map (kbd "C-c C-e") 'ert)

(custom-set-variables
 '(zk-file-extension "txt")
 '(zk-index-invisible-ids nil)
 '(zk-index-format "%t [[%i]]")
 '(zk-directory-recursive t))

(defvar zk-test-environments
  '((:many-in-subdirs
     (zk-directory "~/.emacs.d/straight/repos/zk/tests/sandbox/many-in-subdirs")
     (zk-id-regexp "\\([0-9]\\{12\\}\\)")
     (zk-subdirectory-function nil))
    (:standard
     (zk-directory "~/.emacs.d/straight/repos/zk/tests/sandbox/standard")
     (zk-id-regexp "\\([0-9]\\{12\\}\\)")
     (zk-directory-recursive nil)
     (zk-subdirectory-function nil))
    (:wikilinks
     (zk-directory "~/.emacs.d/straight/repos/zk/tests/sandbox/wikilinks")
     (zk-id-regexp "\\([[:alnum:]]+\\)")
     (zk-index-format "%t [[%i]]")
     (zk-directory-recursive nil)
     (zk-subdirectory-function nil))
    (:numerus
     (zk-directory (file-name-concat (getenv "ZETTEL_DIR")
                                     (file-name-as-directory "numerus")))
     (zk-subdirectory-function #'ezeka-subdirectory)
     (zk-id-regexp "\\([a-z]-[0-9]\\{4\\}\\)"))
    (:tempus
     (zk-directory (file-name-concat (getenv "ZETTEL_DIR")
                                     (file-name-as-directory "tempus")))
     (zk-subdirectory-function #'ezeka-subdirectory)
     (zk-id-regexp "\\([0-9T]\\{13\\}\\)"))
    (:scriptum
     (zk-directory (file-name-concat (getenv "ZETTEL_DIR")
                                     (file-name-as-directory "scriptum")))
     (zk-subdirectory-function #'ezeka-subdirectory)
     (zk-id-regexp "\\([a-z]-[0-9]\\{4\\}~[0-9][0-9]\\)")))
  "Description of my testing environments.
Each item has the form of (:name varlist)")

(defvar zk-test-sample-files
  '((:standard
     "202206052002 {Event} WaterBear Film Screening Series at Minim.txt")
    (:tempus
     "20220812T2046 Testing Zk with T filename.txt")
    (:numerus
     "a-0000 {κ} Central Index.txt")
    (:scriptum
     "v-1268~73 {Review} _A Prehistory of the Cloud_ @Hu2015.txt"))
  "An alist of sample files relative to `zk-directory'.
The form is (:ENVIRONMENT SAMPLE1 [... SAMPLEN]).")

;;;=============================================================================
;;; Helper Functions
;;;=============================================================================

(defun zk-test-sample-files-for (env &optional full-path)
  "Return a sample file for the given ENV'ironment.
If FULL-PATH is non-nil, return full paths."
  (let ((dir (alist-get 'zk-directory
                        (alist-get
                         env
                         zk-test-environments))))
    (mapcar (if full-path
                (lambda (f)
                  (expand-file-name f dir))
              #'identity)
            (alist-get env zk-test-sample-files))))

(defmacro with-zk-test-environment (env varlist &rest body)
  "Evaluate BODY inside a `let` form binding ENV's varlist.
Additional variables can be defined in VARLIST"
  (declare (indent 2))
  `(let (,@(alist-get env zk-test-environments)
         ,@varlist)
     ,@body))

(defun zk-test-set-environment (env)
  "Set the variables needed to work wiht environment ENV."
  (interactive
   (list (completing-read "Which environment? " zk-test-environments)))
  (apply #'custom-set-variables (alist-get (intern-soft env) zk-test-environments)))

(ert-deftest with-zk-test-environment ()
  :tags '(:disabled)
  (with-zk-test-environment :many-in-subdirs
    ((zk-subdirectory-function nil))
    (should-not zk-subdirectory-function)
    (should (string= zk-directory
                     (cadr
                      (cl-find 'zk-directory (alist-get :many-in-subdirs zk-test-environments)
                               :key #'car :test #'eq))))
    (should (string=
             (car (zk-test-sample-files-for :standard))
             "202206052002 {Event} WaterBear Film Screening Series at Minim.txt"))))

(ert-deftest zk-test-reload-zk ()
  (load-file (straight--repos-file "zk" "zk.el"))
  (load-file (straight--repos-file "zk" "zk-index.el")))

;;;=============================================================================
;;; Actual Tests
;;;=============================================================================

(ert-deftest zk--file-id ()
  (with-zk-test-environment :tempus ()
    (with-current-buffer "tests-zk.el"
      (should-not (zk--file-id buffer-file-name)))
    (let* ((file (car (zk-test-sample-files-for :tempus))))
      (should (string= (zk--file-id file) "20220812T2046")))))

;; FIXME: Update? Fix?
(ert-deftest zk--format ()
  (should
   (string= "a-1234 This is a title"
            (zk--format "%i %t" "a-1234" "This is a title")))
  (should
   (string= "This is a title <<a-1234>>"
            (zk--format "%t <<%i>>" "a-1234" "This is a title")))
  (let ((zk-format-function
         (lambda (fmt id title)
           (format fmt id title))))
    (should (string= "a-1234 --- This is a title"
             (zk--format "%s --- %s" "a-1234" "This is a title")))))

(ert-deftest zk-link-regexp ()
  (should (string= (zk-link-regexp) "\\[\\[\\([0-9]\\{12\\}\\)]]")))

(ert-deftest zk--grep-file-list ()
  :tags '(:disabled)                    ; after PR #41
  (with-zk-test-environment :standard ()
    (should (eq 144 (length (zk--grep-file-list " single "))))
    (should (eq 42 (length (zk--grep-file-list " double "))))
    (should (eq 180 (length (zk--grep-file-list " \\(single\\|double\\) " nil))))
    (should (eq 0 (length (zk--grep-file-list " (single|double) " nil))))
    (should (eq 180 (length (zk--grep-file-list " (single|double) " t))))
    (should (eq 2063 (length (zk--grep-file-list " (single|double) " t t))))))

(ert-deftest zk--directory-files ()
  (ezeka-zk-hacks-mode -1)
  (with-zk-test-environment :standard ()
    (should (= 2243 (length (zk--directory-files)))))
  (with-zk-test-environment :many-in-subdirs ()
    (should (= 2242 (length (zk--directory-files)))))
  (ezeka-zk-hacks-mode 1)
  (with-zk-test-environment :numerus ()
    (should-not (= 0 (length (zk--directory-files)))))
  (with-zk-test-environment :tempus ()
    ;; requires changing (zk-file-name-regexp)
    (should (= 0 (length (zk--directory-files))))))

(ert-deftest zk--directory-files-w/-tempus-currens ()
  :tags '(:disabled)                    ; needs implementing
  (with-zk-test-environment :tempus ()
    (should-not (= 0 (length (zk--directory-files))))))

(ert-deftest zk--wildcard-file-path ()
  :tags '(:disabled)                    ; pull request
  (with-zk-test-environment :numerus ()
    (should (string= "/Users/richard/Zettelkasten/numerus/a/a-0000 {κ} Central Index."
                     (zk--wildcard-file-path "a-0000"))))
  (with-zk-test-environment :tempus ()
    (should (string= "/Users/richard/Zettelkasten/tempus/2022/20221025T2032.txt"
                     (zk--wildcard-file-path "20221025T2032")))))

(ert-deftest zk--note-file-path ()
  :tags '(:disabled)
  (with-zk-test-environment :many-in-subdirs
    ((zk-subdirectory-function nil))
    (should (string=
             (car (zk-test-sample-files-for :standard t))
             (zk--note-file-path "202111102331"
                                 "{Event} WaterBear Film Screening Series at Minim")))))

(ert-deftest zk-subdirectory-function ()
  (with-zk-test-environment :many-in-subdirs
    ((zk-subdirectory-function (lambda (id) (cl-subseq id 0 4)))
     (file (car (zk-test-sample-files-for :many-in-subdirs))))
    ;; FIXME: Rewrite using FILE
    (should (string=
             "~/Zk/many-in-subdirs/2020/20200101T0101 Title of note.txt"
             (zk--note-file-path "20200101T0101" "Title of note")))))

(ert-deftest zk--parse-file ()
  (with-zk-test-environment :standard
    ((file (car (zk-test-sample-files-for :standard))))
    (should (string= (zk--parse-file 'id file) "202206052002"))
    (should (string= (zk--parse-file 'title file)
                     "{Event} WaterBear Film Screening Series at Minim"))
    (should-error (zk--parse-file 'foobar file))))

(ert-deftest zk--parse-file ()
  "Test `zk--parse-file' after `zk--file-name-(id|title)' are added."
  :tags '(:disabled)
  (let ((orig-a (symbol-function 'zk--file-name-id))
        (orig-b (symbol-function 'zk--file-name-title)))
    (unwind-protect
        (with-zk-test-environment :standard
          ((file (car (zk-test-sample-files-for :standard))))
          (defalias 'zk--file-name-id
            #'(lambda (_) "202206052002"))
          (defalias 'zk--file-name-title
            #'(lambda (_) "{Event} WaterBear Film Screening Series at Minim"))
          (should (string= (zk--parse-file 'id file) "202206052002"))
          (should (string= (zk--parse-file 'title file)
                           "{Event} WaterBear Film Screening Series at Minim")))
      (defalias 'zk--file-name-id orig-a)
      (defalias 'zk--file-name-id orig-b))))

(ert-deftest zk--parse-file ()
  "Test that `zk--parse-file' behaves correctly when `zk-id-regexp' changes."
  :tags '(:disabled)
  (let ((file1 (zk-test-sample-files-for :standard))
        (file2 (zk-test-sample-files-for :tempus))
        (zk-id-regexp "\\([0-9]\\{12\\}\\)"))
    (should (string= (zk--parse-file 'id file1) "202206052002"))
    (should (string= (zk--parse-file 'id file2) nil))
    (let ((zk-id-regexp "\\([0-9T]\\{13\\}\\)"))
      (should (string= (zk--parse-file 'id file1) nil))
      (should (string= (zk--parse-file 'id file2) "20220812T2046")))))

(ert-deftest zk-file-p ()
  "Test that `zk-file-p' catches non-zk files."
  :tags '(:disabled)
  (let ((file1 (car (zk-test-sample-files-for :standard)))
        (file2 (car (zk-test-sample-files-for :tempus)))
        (zk-id-regexp "\\([0-9]\\{12\\}\\)"))
    (should (zk-file-p file1))
    (should (string= (match-string 1 file1) "202111102331"))
    (should-not (zk-file-p file2))))

(ert-deftest zk-file-name-regexp ()
  "Check that `zk-file-name-regexp' matches the right files."
  :tags '(:disabled)
  (let ((file1 (elt tests-zk--files 0))
        (zk-id-regexp "\\([0-9]\\{12\\}\\)"))
    (message zk-id-regexp)
    (should (string-match (zk-file-name-regexp) file1))
    (should (string= (zk--file-name-id file1) "202111102331"))
    (should (string= (zk--file-name-title file1) "{Talk} Animal History in the Anthropocene"))))

(ert-deftest zk--singleton-p ()
  :tags '(:benchmark :disabled)
  (with-zk-test-environment :standard
    ((files (zk--directory-files)))
    (should (zk--singleton-p '(1)))
    (should-not (zk--singleton-p '()))
    (should-not (zk--singleton-p '(1 2 3)))
    (garbage-collect)
    (should (equal
             (benchmark-run 1000 (eq 1 (length files)))
             (benchmark-run 1000 (zk--singleton-p files))))))

;;;=============================================================================
;;; zk--processor
;;;=============================================================================

(defun zk--processor/gr (arg)
  "Return list of files.
ARG can be zk-file or zk-id as string or list, single or multiple."
  (let* ((zk-alist (zk--alist))
         (files (cond
                 ((stringp arg)
                  (if (zk-file-p arg)
                      (list arg)
                    (list (zk--parse-id 'file-path arg zk-alist))))
                 ((zk--singleton-p arg)
                  (if (zk-file-p (car arg))
                      arg
                    (list (zk--parse-id 'file-path (car arg) zk-alist))))
                 (t
                  (if (zk-file-p (car arg))
                      arg
                    (zk--parse-id 'file-path arg zk-alist))))))
    files))

(ert-deftest zk--processor ()
  (with-zk-test-environment :numerus ()
    (should (equal (zk--processor/gr "a-0000")
                   (zk--processor "a-0000")))
    (should (equal (zk--processor/gr '("a-0000" "c-4317"))
                   (zk--processor '("a-0000" "c-4317"))))
    (should-error (zk--processor 4))
    ))

;;;=============================================================================
;;; Zk-note
;;;=============================================================================

(ert-deftest zk--alist ()
  "Make sure `zk--alist' generates the correct structure."
  :tags '(:disabled)                    ; zk-note
  (with-zk-test-environment :standard ()
    (let* ((alist (zk--alist (zk--directory-files)))
           (note (cdr (assoc "201004292342" alist))))
      (should (zk--note-p note))
      (should (string= "201004292342" (zk--note-id note)))
      (should (string= "{Talk} Lauren Berlant, 'On the Desire for the Political' @ PSU"
                       (zk--note-title note)))
      (should (string= "201004292342 {Talk} Lauren Berlant, 'On the Desire for the Political' @ PSU.txt"
                       (zk--note-file note))))))

(ert-deftest bm/zk--alist ()
  :tags '(:benchmark)
  (ert-run-tests-batch "zk-test-reload-zk")
  (garbage-collect)
  (with-zk-test-environment :standard ()
    (should (benchmark-run 100 (zk--alist)))
    (let ((files (zk--directory-files)))
      (should (null (benchmark-run 100 (zk--alist files)))))))

;;; 100 (zk--alist) on :standard (6.449468 12 1.1682259999999998)
;;; 100 (zk--alist files) on :standard (4.3255930000000005 11 1.0541220000000004)

;;;=============================================================================
;;; Benchmarks
;;;=============================================================================

(ert-deftest bm/zk--generate-id ()
  :tags '(:benchmark)
  (ert-run-tests-batch "zk-test-reload-zk")
  (with-zk-test-environment :standard ()
    (should (null
             (benchmark-run 100
               (zk--generate-id))))))

(ert-deftest bm/zk--parse-id ()
  :tags '(:benchmark)
  ;;; 100 on :numerus with 3829 files (14.105008 24 2.713031)
  (ert-run-tests-batch "zk-test-reload-zk")
  (garbage-collect)
  (with-zk-test-environment :numerus ()
    (should (null
             (benchmark-run 100
               (zk--parse-id 'file-path "l-0614"))))))

(ert-deftest bm/zk--wildcard-file-path ()
  :tags '(:benchmark)
  ;; 100 on :numerus with 3829 files (0.092291 0 0.0)
  (ert-run-tests-batch "zk-test-reload-zk")
  (garbage-collect)
  (with-zk-test-environment :numerus ()
    (should (string= (zk--wildcard-file-path "g-9172")
                     "/Users/richard/Zettelkasten/numerus/g/g-9172 {λ} size of a thought @Kuehn.txt"))
    (should (null
             (benchmark-run 100
               (zk--wildcard-file-path "g-9172"))))))

(ert-deftest bm/zk-insert-link ()
  :tags '(:benchmark)
  ;;; 100 on :numerus with 4502 files
  ;;(ert-run-tests-batch "zk-test-reload-zk")
  (garbage-collect)
  (with-zk-test-environment :numerus ()
    (with-temp-buffer
      (should (null
               (benchmark-run 100
                 (zk-insert-link "l-0614")
                 (insert "\n")))))))

(ert-deftest bm/zk-completion-at-point ()
  :tags '(:benchmark)
  (elp-reset-all)
  (elp-instrument-package "zk")
  (fset 'five-completions
        (kmacro-lambda-form  0 "%d"))
  (with-zk-test-environment :standard
    ((file (car (zk-test-sample-files-for :standard)))
     (five-completions [?\[ ?\[ ?m ?e ?m ?o tab tab return
                            ?\[ ?\[ ?t ?r ?p ?g tab return
                            ?\[ ?\[ ?w ?o ?m ?e ?n tab tab return
                            ?\[ ?\[ ?m ?e ?n tab tab return
                            ?\[ ?\[ ?a ?n ?o ?t ?h ?e ?r tab tab return]))
    (switch-to-buffer-other-window (generate-new-buffer "*Test*"))
    (org-mode)
    (add-to-list 'completion-at-point-functions #'zk-completion-at-point)
    (dotimes (n 10)
      (execute-kbd-macro five-completions)))
  (let ((commit (read-string "Commit: "))
        (time (format-time-string "%F %R"))
        (files (length (zk--directory-files))))
    (elp-results)
    (with-current-buffer (get-buffer "*ELP Profiling Results*")
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert
         (format ";;; %s on %s with %d files\n" time commit files)))
      (write-file (format "%s on %s.txt" time commit)))
    (elp-restore-all)))

(ert-deftest bm/zk-index ()
  :tags '(:benchmark)
  (elp-instrument-package "zk")
  (with-zk-test-environment
      :standard
      ((reps 10)
       (commit (or (ignore-errors
                     (string-trim
                      (shell-command-to-string "git rev-parse --short HEAD")))
                   (read-string "Commit: ")))
       (file (car (zk-test-sample-files-for :standard)))
       (time (format-time-string "%F %R"))
       (lines)
     (results))
    (setq results
      (benchmark-run reps
        (should (string= (zk--parse-file 'id file) "202206052002"))
        (should (string= (zk--parse-file 'title file)
                         "{Event} WaterBear Film Screening Series at Minim"))
        (zk-index)
        (with-current-buffer zk-index-buffer-name
          (setq lines (count-lines (point-min) (point-max))))
        (kill-buffer zk-index-buffer-name)))
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
;;; zk--id-file (2023-06-30)
;;;=============================================================================

(ert-deftest zk--id-file ()
  (with-zk-test-environment :standard
    ((file (car (zk-test-sample-files-for :standard))))
    (should (string= (file-name-base (zk--id-file "202206052002"))
                     (file-name-base file)))))

;;;=============================================================================
;;; zk--parse-id (2023-06-30)
;;;=============================================================================

(ert-deftest zk--parse-id ()
  (with-zk-test-environment :standard
    ((file (car (zk-test-sample-files-for :standard))))
    (should (string= (file-name-base (zk--parse-id 'file-path "202206052002"))
                     (file-name-base file)))
    (should (string= (zk--parse-id 'title "202206052002")
                     "{Event} WaterBear Film Screening Series at Minim"))
    (should-error (zk--parse-id 'foobar "202206052002"))))

(defun rb/zk--parse-id (target id &optional zk-alist)
  "Return TARGET, either `file-path or `title, from file with ID.
Takes a single ID, as a string. Takes an optional ZK-ALIST, for
efficiency if `zk--parse-id' is called in an internal loop."
  (let ((file (zk--id-file id)))
    (cond ((eq target 'file-path)
           file)
          ((eq target 'title)
           (if (string-match (zk-file-name-regexp) (file-name-nondirectory file))
               (match-string 2 (file-name-nondirectory file))
             (error "Cannot figure out title for file with ID %s: %s"
                    id (file-name-nondirectory file))))
          (t (error "Invalid target: %s" target)))))

(defun gr/zk--parse-id (target ids &optional zk-alist)
  "Return TARGET, either `file-path or `title, from files with IDS.
Takes a single ID, as a string, or a list of IDs. Takes an
optional ZK-ALIST, for efficiency if `zk--parse-id' is called
in an internal loop."
  (cond
   ((and (eq target 'file-path)
         (stringp ids))
    (car (zk--directory-files t ids)))
   ((and (eq target 'file-path)
         (zk--singleton-p ids))
    (car (zk--directory-files t (car ids))))
   (t
    (let* ((zk-alist (or zk-alist
                         (zk--alist)))
           (zk-id-list (zk--id-list))
           (return
            (cond ((eq target 'file-path)
                   (cond ((stringp ids)
                          (if (member ids zk-id-list)
                              (cddr (assoc ids zk-alist))
                            (user-error "No file associated with %s" ids)))
                         ((listp ids)
                          (mapcar
                           (lambda (x)
                             (caddr (assoc x zk-alist)))
                           ids))))
                  ((eq target 'title)
                   (cond ((stringp ids)
                          (if (member ids zk-id-list)
                              (cadr (assoc ids zk-alist))
                            (user-error "No file associated with %s" ids)))
                         ((listp ids)
                          (mapcar
                           (lambda (x)
                             (cadr (assoc x zk-alist)))
                           ids)))))))
      (if (zk--singleton-p return)
          (car return)
        return)))))

(ert-deftest rb/zk--parse-id ()
  (with-zk-test-environment :standard
    ((file (car (zk-test-sample-files-for :standard))))
    (should (string= (file-name-base (rb/zk--parse-id 'file-path "202206052002"))
                     (file-name-base file)))
    (should (string= (rb/zk--parse-id 'title "202206052002")
                     "{Event} WaterBear Film Screening Series at Minim"))
    (should-error (rb/zk--parse-id 'foobar "202206052002"))))

(ert-deftest benchmark/zk--parse-id ()
  :tags '(:benchmark)
  (ert-run-tests-batch "zk-test-reload-zk")
  (garbage-collect)
  (with-zk-test-environment :standard ()
    (should (benchmark-run 100 (gr/zk--parse-id 'file-path "202206052002")))
    (should (benchmark-run 100 (zk--parse-id 'file-path "202206052002")))))

;;;=============================================================================
;;; Zk-index-query-files (2023-07-10)
;;;=============================================================================

(ert-deftest zk-index-query-files ()
  (with-zk-test-environment :numerus ()
    (switch-to-buffer zk-index-buffer-name)
    (zk-index-refresh)
    (should (= 14 (length (zk-index-query-files "humor" 'zk-index-focus))))
    (zk-index-refresh)
    (should (= 83 (length (zk-index-query-files "humor" 'zk-index-search))))))

(ert-deftest benchmark/zk-index-query-files ()
  :tags '(:benchmark)
  (ert-run-tests-batch "zk-test-reload-zk")
  (with-zk-test-environment :numerus ()
    (switch-to-buffer zk-index-buffer-name)
    (should (benchmark-run 10           ; (26.718512 80 9.696325000000002)
              (zk-index-refresh)
              (garbage-collect)
              (zk-index-query-files "humor" 'zk-index-focus)))
    (should (benchmark-run 10           ; (34.558523 70 11.692167000000005)
              (zk-index-refresh)
              (garbage-collect)
              (zk-index-query-files "humor" 'zk-index-search)))))

(ert-deftest benchmark/rb/zk-index-query-files ()
  :tags '(:benchmark)
  (ert-run-tests-batch "zk-test-reload-zk")
  (with-zk-test-environment :numerus ()
    (switch-to-buffer zk-index-buffer-name)
    (should (benchmark-run 10           ; (21.83563 70 9.479023999999995)
              (zk-index-refresh)
              (garbage-collect)
              (zk-index-query-files/rb "humor" 'zk-index-focus)))
    (should (benchmark-run 10           ; (26.18411 70 9.764894999999996)
              (zk-index-refresh)
              (garbage-collect)
              (zk-index-query-files/rb "humor" 'zk-index-search)))))


;;;=============================================================================
;;; Regexps (pull request #63; 2023-07-14)
;;;=============================================================================

(ert-deftest zk--posix-regexp ()
  (let ((numerus "\\(?:[a-z]-[0-9]\\{4\\}\\)")
        (tempus "\\([0-9]\\{8\\}T[0-9]\\{4\\}\\)")
        (all "\\([a-z]-[0-9]\\{4\\}~[0-9][0-9]\\|[0-9]\\{8\\}T[0-9]\\{4\\}\\|[a-z]-[0-9]\\{4\\}\\)"))
    (should (string= (zk--posix-regexp "\\(.*\\)") "(.*)"))
    (should (string= (zk--posix-regexp "\\(.*\\)" 'basic) "\\(.*\\)"))
    (should (string= (zk--posix-regexp numerus)
                     "(?:[a-z]-[0-9]{4})"))
    (should (string= (zk--posix-regexp numerus 'basic)
                     "\\([a-z]-[0-9]\\{4\\}\\)"))
    (should (string= (zk--posix-regexp tempus)
                     "([0-9]{8}T[0-9]{4})"))
    (should
     (string= (zk--posix-regexp all)
              "([a-z]-[0-9]{4}~[0-9][0-9]|[0-9]{8}T[0-9]{4}|[a-z]-[0-9]{4})"))
    (should
     (string= (zk--posix-regexp all 'basic)
              "\\([a-z]-[0-9]\\{4\\}~[0-9][0-9]\\|[0-9]\\{8\\}T[0-9]\\{4\\}\\|[a-z]-[0-9]\\{4\\}\\)"))
    (should (benchmark 10000 (zk--posix-regexp all)))))

(ert-deftest zk--grep-commands ()
  (with-zk-test-environment :scriptum ()
    (should (= 13 (length (zk--grep-id-list "Taiwan"))))
    (should (= 13 (length (zk--grep-file-list "Taiwan"))))
    (should (= 13 (length (zk--grep-file-list "Taiwan"))))
    ;; extended regexp
    (should (= 16 (length (zk--grep-file-list "\\(garbage\\|waste\\)"))))
    ;; tags
    (should (equal '("#1" "#2" "#3" "#diss" "#metro2033" "#split" "#todo" "#102")
                   (zk--grep-tag-list))))
  (with-zk-test-environment :numerus ()
    (should (= (length (zk--backlinks-list "t-7019")) 13)) ; Yomi Braester [[t-7019]]
    (should (= (length (zk--backlinks-list "y-7690")) 1)))) ; Lee Yu-lin [[y-7690]]

;;;=============================================================================
;;; zk--id-list (2023-07-16)
;;;=============================================================================

(defun zk--id-list/orig (&optional str zk-alist) ; 2023-07-16
  "Return a list of zk IDs for notes in `zk-directory'.
Optional search for STR in note title, case-insenstive. Takes an
optional ZK-ALIST, for efficiency if `zk--id-list' is called in
an internal loop."
  (if str
      (let ((zk-alist (or zk-alist (zk--alist)))
            (case-fold-search t)
            (ids))
        (dolist (item zk-alist)
          (if str
              (when (string-match str (cadr item))
                (push (car item) ids))
            (push (car item) ids)))
        ids)
    (zk--parse-file 'id (zk--directory-files t))))

(ert-deftest zk--id-list ()
  :tags '()
  (with-zk-test-environment :tempus ()
    (should (= (length (zk--id-list)) (length (zk--id-list/orig)))) ; no args
    (should (= (length (zk--id-list "yomi")) (length (zk--id-list/orig "yomi")))) ; str
    (let ((zk-alist (zk--alist)))
      (should (= (length (zk--id-list nil zk-alist))
                 (length (zk--id-list/orig nil zk-alist)))) ; zk-alist
      (should (= (length (zk--id-list "yomi" zk-alist))
                 (length (zk--id-list/orig "yomi" zk-alist)))) ; zk-alist + str
      (should (= (length (zk--id-list "^2014" zk-alist))
                 (length (zk--id-list/orig "^2014" zk-alist))))
      )))

(defmacro zk-test-benchmark-run (n description &rest forms)
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

(ert-deftest bm/zk--id-list+search-ids ()
  :tags '(:benchmark)
  (ert-run-tests-batch "zk-test-reload-zk")
  (garbage-collect)
  (let ((n 10)
        results)
    (unwind-protect
        (with-zk-test-environment :tempus ()
          (push (zk-test-benchmark-run n nil (zk--id-list/orig)) results)
          (push (zk-test-benchmark-run n nil (zk--id-list)) results)
          (let ((zk-alist (zk--alist)))
            (push (zk-test-benchmark-run n nil (zk--id-list/orig nil zk-alist)) results)
            (push (zk-test-benchmark-run n nil (zk--id-list nil zk-alist)) results)
            (push (zk-test-benchmark-run n nil (zk--id-list/orig "yomi" zk-alist)) results)
            (push (zk-test-benchmark-run n nil (zk--id-list "yomi" zk-alist)) results)))
      (with-current-buffer (get-buffer-create "*ERT Results*")
        (goto-char (point-max))
        (insert (format "\n\n=== %s (%d reps) at %s ===\n"
                        "bm/zk--id-list+search-ids" n
                        (format-time-string "%F %R")))
        (insert (mapconcat #'identity (nreverse results) "\n"))
        (goto-char (point-max)))
      (switch-to-buffer-other-window "*ERT Results*"))))

(ert-deftest bm/zk--id-list+use-zk-alist ()
  :tags '(:benchmark)
  (ert-run-tests-batch "zk-test-reload-zk")
  (garbage-collect)
  (let ((n 10)
        results)
    (unwind-protect
        (with-zk-test-environment :tempus ()
          (push (zk-test-benchmark-run n nil (zk--id-list/orig)) results)
          (push (zk-test-benchmark-run n nil (zk--id-list)) results)
          (push (zk-test-benchmark-run n nil (zk--id-list/orig "yomi")) results)
          (push (zk-test-benchmark-run n nil (zk--id-list "yomi")) results)
          (push (zk-test-benchmark-run n nil (zk--id-list/orig "^2014")) results)
          (push (zk-test-benchmark-run n nil (zk--id-list "^2014")) results)
          (let ((zk-alist (zk--alist)))
            (push (zk-test-benchmark-run n nil (zk--id-list/orig nil zk-alist)) results)
            (push (zk-test-benchmark-run n nil (zk--id-list nil zk-alist)) results)
            (push (zk-test-benchmark-run n nil (zk--id-list/orig "yomi" zk-alist)) results)
            (push (zk-test-benchmark-run n nil (zk--id-list "yomi" zk-alist)) results)))
      (with-current-buffer (get-buffer-create "*ERT Results*")
        (goto-char (point-max))
        (insert (format "\n\n=== %s (%d reps) at %s ===\n"
                        "bm/zk--id-list+use-zk-alist" n
                        (format-time-string "%F %R")))
        (insert (mapconcat #'identity (nreverse results) "\n"))
        (goto-char (point-max)))
      (switch-to-buffer-other-window "*ERT Results*"))))

;;;=============================================================================
;;; zk-index-zk-index--current-id-list
;;;=============================================================================

(ert-deftest zk-index--current-id-list ()
  (with-zk-test-environment :numerus ()
    (should (= 6 (length (zk-index--current-id-list (get-buffer "*Zk-Index: Numerus*") 0 275))))
    (should (= 6 (length (zk-index--current-id-list (get-buffer "*Zk-Index: Numerus*") 0 310))))
    (should (= 4592 (length (zk-index--current-id-list (get-buffer "*Zk-Index: Numerus*")))))))

;;;=============================================================================
;;; zk--find-file and zk-find-file-functions
;;;=============================================================================

(ert-deftest zk-find-file-functions ()
  (with-zk-test-environment :numerus ()
    (let* ((stack '())
           (sample (car (zk-test-sample-files-for :numerus 'full-path)))
           (func (lambda (file)
                   (push file stack))))
      (add-to-list 'zk-find-file-functions func)
      (zk--find-file sample)
      (should (string= (car stack) sample)))))

(provide 'test-zk)
;;; zk-test.el ends here
