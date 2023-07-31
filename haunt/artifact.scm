;;; Haunt --- Static site generator for GNU Guile
;;; Copyright © 2020 David Thompson <davet@gnu.org>
;;;
;;; This file is part of Haunt.
;;;
;;; Haunt is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Haunt is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Haunt.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Build artifact data type.
;; 构建成品
;;; Code:

(define-module (haunt artifact)
  #:use-module (haunt utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (srfi srfi-9)
  #:export (make-artifact
            artifact?
            artifact-file-name
            artifact-writer
            create-artifact
            serialized-artifact
            verbatim-artifact
            external-artifact))

(define-record-type <artifact>
  (make-artifact file-name writer)
  artifact?
  (file-name artifact-file-name)
  (writer artifact-writer))

(define (create-artifact artifact prefix)
  (let ((output (string-append prefix "/" (artifact-file-name artifact))))
    (mkdir-p (dirname output))
    ((artifact-writer artifact) output)
    (unless (file-exists? output)
      (error "failed to create artifact output file" output))))
;; 序列化到目标文件
(define (serialized-artifact destination obj serialize)
  (make-artifact destination ;;文件名
                 (lambda (output) ;;writer函数
                   (format #t "write '~a'~%" destination)
                   (call-with-output-file output
                     (lambda (port)
                       (serialize obj port))))))

(define (verbatim-artifact source destination)
  (unless (file-exists? source)
    (error "verbatim artifact source file does not exist" source))
  (make-artifact destination
                 (lambda (output)
                   (format #t "copy '~a' → '~a'~%" source destination)
                   (copy-file source output))))
