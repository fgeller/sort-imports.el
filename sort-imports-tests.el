;; tests

(require 'sort-imports)

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


(ert t)
