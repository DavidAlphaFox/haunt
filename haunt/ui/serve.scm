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
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Haunt.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Haunt serve sub-command.
;;
;;; Code:

(define-module (haunt ui serve)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 threads)
  #:use-module (haunt site)
  #:use-module (haunt config)
  #:use-module (haunt ui)
  #:use-module (haunt serve web-server)
  #:export (haunt-serve))

(define (show-help)
  (format #t "Usage: haunt serve [OPTION]
Start an HTTP server for the current site.~%")
  (display "
  -p, --port             port to listen on")
  (display "
  -w, --watch            rebuild site when files change")
  (newline)
  (show-common-options-help)
  (newline)
  (display "
  -h, --help             display this help and exit")
  (display "
  -V, --version          display version and exit")
  (newline))

(define %options
  (cons* (option '(#\h "help") #f #f
                 (lambda _
                   (show-help)
                   (exit 0)))
         (option '(#\V "version") #f #f
                 (lambda _
                   (show-version-and-exit "haunt serve")))
         (option '(#\p "port") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'port (string->number* arg) result)))
         (option '(#\w "watch") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'watch? #t result)))
         %common-options))

(define %default-options
  (cons '(port . 8080)
        %default-common-options))

(define (call-with-error-handling thunk)
  (catch #t
    thunk
    (lambda (key . args)
      (let ((cep (current-error-port))
            (stack (make-stack #t 1)))
        (display "ERROR: site rebuild failed\n\n" cep)
        (display "Backtrace:\n" cep)
        (display-backtrace stack cep)
        (newline cep)
        (apply display-error (stack-ref stack 0) cep args)
        (newline cep)))))

;; XXX: Make this less naive.
(define (watch config-file check-dir? check-file?)
  "Watch the current working directory for changes to any of its files
that match CHECK-FILE? and any subdirectories that match CHECK-DIR?.
When a file has been changed, reload CONFIG-FILE and rebuild the
site."

  (define cwd (getcwd))

  (define (any-files-changed? time)
    (define (enter? name stat result)
      ;; Don't bother descending if we already know that a file has
      ;; changed.
      (and (not result) (check-dir? name)))

    (define (leaf name stat result)
      ;; Test if file has been modified since the last time we
      ;; checked.
      (or result
          (and (check-file? name)
               (or (>= (stat:mtime stat) time)
                   (>= (stat:ctime stat) time)))))

    (define (no-op name stat result) result)

    (file-system-fold enter? leaf no-op no-op no-op no-op #f cwd))

  (let loop ((time (current-time)))
    (when (any-files-changed? time)
      (display "rebuilding...\n")
      (call-with-error-handling
        (lambda ()
          (build-site (load-config config-file)))))
    (let ((next-time (current-time)))
      (sleep 1)
      (loop next-time))))

(define (haunt-serve . args)
  (let* ((opts     (simple-args-fold args %options %default-options))
         (port     (assq-ref opts 'port))
         (watch?   (assq-ref opts 'watch?))
         (config   (assq-ref opts 'config))
         (site     (load-config config))
         (doc-root (site-build-directory site)))
    (format #t "serving ~a on port ~d~%" doc-root port)

    (when watch?
      (call-with-new-thread
       (lambda ()
         (watch config
                (let ((cwd       (getcwd))
                      (build-dir (site-build-directory site)))
                  (lambda (dir)
                    (not
                     (string-prefix? (string-append cwd "/" build-dir) dir))))
                (site-file-filter site)))))

    (serve doc-root #:open-params `(#:port ,port))))
