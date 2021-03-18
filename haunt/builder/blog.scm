;;; Haunt --- Static site generator for GNU Guile
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2016 Christopher Allan Webber <cwebber@dustycloud.org>
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
;; Page builders
;;
;;; Code:

(define-module (haunt builder blog)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:use-module (haunt artifact)
  #:use-module (haunt site)
  #:use-module (haunt post)
  #:use-module (haunt page)
  #:use-module (haunt utils)
  #:use-module (haunt html)
  #:export (theme
            theme?
            theme-name
            theme-layout
            theme-post-template
            theme-collection-template
            with-layout
            render-post
            render-collection

            date->string*

            blog))

(define-record-type <theme>
  (make-theme name layout post-template collection-template)
  theme?
  (name theme-name)
  (layout theme-layout)
  (post-template theme-post-template)
  (collection-template theme-collection-template))

(define (ugly-default-layout site title body)
  `((doctype "html")
    (head
     (meta (@ (charset "utf-8")))
     (title ,(string-append title " — " (site-title site))))
    (body
     (h1 ,(site-title site))
     ,body)))

(define (ugly-default-post-template post)
  `((h2 ,(post-ref post 'title))
    (h3 "by " ,(post-ref post 'author)
        " — " ,(date->string* (post-date post)))
    (div ,(post-sxml post))))

(define (ugly-default-collection-template site title posts prefix)
  (define (post-uri post)
    (string-append (or prefix "") "/"
                   (site-post-slug site post) ".html"))

  `((h3 ,title)
    (ul
     ,@(map (lambda (post)
              `(li
                (a (@ (href ,(post-uri post)))
                   ,(post-ref post 'title)
                   " — "
                   ,(date->string* (post-date post)))))
            posts))))

(define* (theme #:key
                (name "Untitled")
                (layout ugly-default-layout)
                (post-template ugly-default-post-template)
                (collection-template ugly-default-collection-template))
  (make-theme name layout post-template collection-template))

(define (with-layout theme site title body)
  ((theme-layout theme) site title body))

(define (render-post theme site post)
  (let ((title (post-ref post 'title))
        (body ((theme-post-template theme) post)))
    (with-layout theme site title body)))

(define (render-collection theme site title posts prefix)
  (let ((body ((theme-collection-template theme) site title posts prefix)))
    (with-layout theme site title body)))

(define (date->string* date)
  "Convert DATE to human readable string."
  (date->string date "~a ~d ~B ~Y"))

(define ugly-theme
  (theme #:name "Ugly"
         #:layout ugly-default-layout
         #:post-template ugly-default-post-template
         #:collection-template ugly-default-collection-template))

(define* (blog #:key (theme ugly-theme) prefix
               (collections
                `(("Recent Posts" "index.html" ,posts/reverse-chronological))))
  "Return a procedure that transforms a list of posts into pages
decorated by THEME, whose URLs start with PREFIX."
  (define (make-file-name base-name)
    (if prefix
        (string-append prefix "/" base-name)
        base-name))

  (lambda (site posts)
    (define (post->page post)
      (let ((base-name (string-append (site-post-slug site post)
                                      ".html")))
        (serialized-artifact (make-file-name base-name)
                             (render-post theme site post)
                             sxml->html)))

    (define collection->page
      (match-lambda
       ((title file-name filter)
        (serialized-artifact (make-file-name file-name)
                             (render-collection theme site title (filter posts) prefix)
                             sxml->html))))

    (append (map post->page posts)
            (map collection->page collections))))
