;; sort imports

;; inspired by Leonard Ehrenfried setup for vim:
;; http://leonard.io/blog/2013/04/editing-scala-with-vim/

(defvar si-group-keys '(:other :stdlib :own))
(defvar si-preamble-regex "^package ")
(defvar si-import-line-regex "^import ")
(defvar si-scala-stdlib-regex "\\(^import java\\.\\)\\|\\(^import scala\\.\\)")
(defvar si-stdlib-regex si-scala-stdlib-regex)
(defvar si-own-regex nil)

(defun si-looking-at-empty-line-p () (looking-at-p "^$"))

(defun si-group-for-import (import-line)
  (cond ((s-matches? si-stdlib-regex line)
         :stdlib)
        ((and si-own-regex (s-matches? si-own-regex line))
         :own)
        (t
         :other)))

(defun si-get-import-lines (all-lines)
  (with-temp-buffer
    (insert all-lines)
    (goto-char (point-min))
    (keep-lines si-import-line-regex (point-min) (point-max))
    (buffer-substring (point-min) (point-max))))

(defun si-construct-imports-string (import-groups)
  (with-temp-buffer
    (-each si-group-keys
           (lambda (group-key)
             (let* ((group-imports (cdr (assoc group-key import-groups)))
                    (sorted-import-group (-sort 'string< group-imports)))
               (when sorted-import-group
                 (insert (s-join "\n" sorted-import-group))
                 (insert "\n\n")))))
    (buffer-substring (point-min) (point-max))))

(defun si-get-sorted-imports ()
  (let* ((full-buffer-contents (buffer-substring-no-properties (point-min) (point-max)))
         (import-lines-string (si-get-import-lines full-buffer-contents))
         (import-lines (s-split "\n" import-lines-string t))
         (import-groups (-group-by (lambda (line) (si-group-for-import line)) import-lines)))
    (si-construct-imports-string import-groups)))

(defun si-find-place-to-insert-sorted-imports ()
  (goto-char (point-min))
  (when (looking-at-p si-preamble-regex)
    (forward-line 1)
    (if (si-looking-at-empty-line-p) (forward-line 1) (newline))))

(defun si-sort-imports ()
  (interactive)
  (let* ((sorted-imports (si-get-sorted-imports)))
    (when (not (s-blank? sorted-imports))
      (save-excursion
        (flush-lines si-import-line-regex  (point-min) (point-max))
        (si-find-place-to-insert-sorted-imports)
        (insert sorted-imports)
        (open-line 1)
        (while (si-looking-at-empty-line-p) (forward-line -1))
        (unless (eobp) (delete-blank-lines) (forward-line 1) (newline))))))

(provide 'sort-imports)

;; tests

(defun si-test-with-buffer-string (buffer-string)
  (with-temp-buffer
    (insert buffer-string)
    (si-sort-imports)
    (buffer-substring (point-min) (point-max))))

(ert-deftest sort-imports-tests-1 ()
  "test1"
  (should (equal
           (si-test-with-buffer-string
            "import org.b
import org.a")
           "import org.a
import org.b

")))

(ert-deftest sort-imports-tests-2 ()
  "test2"
  (should (equal
           (si-test-with-buffer-string
            "import org.b
import org.a
def greeting = 42")

           "import org.a
import org.b

def greeting = 42")))

(ert-deftest sort-imports-tests-3 ()
  "test3"
  (should (equal
           (si-test-with-buffer-string
            "package my.world

import org.b
import org.a
def greeting = 42")

           "package my.world

import org.a
import org.b

def greeting = 42")))

(ert-deftest sort-imports-tests-4 ()
  "test4"
  (should (equal
           (si-test-with-buffer-string
            "package my.world

def greeting = 42")

           "package my.world

def greeting = 42")))

(ert-deftest sort-imports-tests-5 ()
  "test5"

  (should   (let ((test-point 25))
              (equal
               (with-temp-buffer
                 (insert "package my.world

def greeting = 42")
                 (goto-char test-point)
                 (si-sort-imports)
                 (point))
               test-point))))

(ert-deftest sort-imports-tests-6 ()
  "test6"

  (should   (equal
             (with-temp-buffer
               (insert "package my.world
def greeting = 42
import x
def greeting2 = 82")
               (goto-char (- (point-max) 2))
               (si-sort-imports)
               (buffer-substring (point) (+ 2 (point))))
             "82")))

(ert-deftest sort-imports-tests-7 ()
  "test7"
  (should (equal
           (si-test-with-buffer-string
            "package my.world

import java.util.List

import scala.util.Try

import x

def greeting = 42")

           "package my.world

import x

import java.util.List
import scala.util.Try

def greeting = 42")))

(ert-deftest sort-imports-tests-8 ()
  "test8"
  (should (equal
           (let ((si-stdlib-regex "\\(^import java\\)\\|\\(^import scala\\)")
                 (si-own-regex "^import cool"))
             (si-test-with-buffer-string
              "package my.world

import java.util.List
import cool
import scala.util.Try

import x

def greeting = 42"))

           "package my.world

import x

import java.util.List
import scala.util.Try

import cool

def greeting = 42")))

(ert-deftest sort-imports-tests-9 ()
  "test9"
  (should (equal
           (let ((si-stdlib-regex "\\(^import java\\)\\|\\(^import scala\\)")
                 (si-own-regex "^import cool\\b"))
             (si-test-with-buffer-string
              "package my.world
import cool

import java.util.List
import scala.util.Try

import x
import cooler

def greeting = 42"))

           "package my.world

import cooler
import x

import java.util.List
import scala.util.Try

import cool

def greeting = 42")))

(ert-deftest sort-imports-tests-10 ()
  "test10"
  (should (equal
           (let ((si-stdlib-regex "\\(^import java\\)\\|\\(^import scala\\)")
                 (si-own-regex "^import cool")
                 (si-group-keys '(:own :other :stdlib)))
             (si-test-with-buffer-string
              "package my.world

import java.util.List
import cool
import scala.util.Try

import x

def greeting = 42"))

           "package my.world

import cool

import x

import java.util.List
import scala.util.Try

def greeting = 42")))
