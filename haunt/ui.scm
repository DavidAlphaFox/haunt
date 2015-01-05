;;; Haunt --- Static site generator for GNU Guile
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;;
;;; This file is part of Haunt.
;;;
;;; Haunt is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Haunt is distributed in the hope that it will be useful, but
;;; WITnnnHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Haunt.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Haunt user interface.
;;
;;; Code:

(define-module (haunt ui)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:use-module (haunt config)
  #:export (program-name
            haunt-error
            show-version-and-exit
            option?
            haunt-main))

(define commands
  '(serve))

(define program-name (make-parameter 'haunt))

(define (haunt-error str . args)
  (format (current-error-port) "~a: " (program-name))
  (apply format (current-error-port) str args)
  (newline))

(define (show-haunt-help)
  (format #t "Usage: haunt COMMAND ARGS...
Run COMMAND with ARGS.~%~%")
  (format #t "COMMAND must be one of the sub-commands listed below:~%~%")
  (format #t "~{   ~a~%~}" (sort commands string<?)))

(define (show-haunt-usage)
  (format #t "Try `haunt --help' for more information.~%")
  (exit 1))

(define (show-version-and-exit)
  (let ((name (if (eq? (program-name) 'haunt)
                  "haunt"
                  (format #f "haunt ~a" (program-name)))))
    (format #t "~a ~a
Copyright (C) 2015 the Haunt authors
License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.~%"
            name %haunt-version)))

(define (option? str)
  (string-prefix? "-" str))

(define (run-haunt-command command . args)
  (let* ((module
          (catch 'misc-error
            (lambda ()
              (resolve-interface `(haunt ui ,command)))
            (lambda -
              (haunt-error "~a: command not found" command)
              (show-haunt-usage))))
         (command-main (module-ref module (symbol-append 'haunt- command))))
    (parameterize ((program-name command))
      (apply command-main args))))

(define* (haunt-main arg0 . args)
  (match args
    (()
     (show-haunt-usage))
    ((or ("-h") ("--help"))
     (show-haunt-help))
    (("--version")
     (show-version-and-exit))
    (((? option? opt) _ ...)
     (format (current-error-port)
             "haunt: unrecognized option '~a'~%"
             opt)
     (show-haunt-usage))
    ((command args ...)
     (apply run-haunt-command (string->symbol command) args))))
