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
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:use-module (haunt artifact)
  #:use-module (haunt site)
  #:use-module (haunt post)
  #:use-module (haunt utils)
  #:use-module (haunt html)
  #:export (theme
            theme?
            theme-name
            theme-layout
            theme-post-template
            theme-collection-template
            theme-pagination-template
            with-layout
            render-post
            render-collection

            date->string*

            blog))
;; 定义主题
(define-record-type <theme>
  (make-theme name layout post-template collection-template pagination-template)
  theme?
  (name theme-name) ;;主题名称
  (layout theme-layout);;主题的整体模板
  (post-template theme-post-template);;主题的文章模板，主要是内容
  (collection-template theme-collection-template);;主题的索引模板，用来做整个集合的目录
  (pagination-template theme-pagination-template));;分页用的模板

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

(define (ugly-default-pagination-template site body previous-page next-page)
  `(,@body
    (div
     ,(if previous-page
          `(a (@ (href ,previous-page)) "← Previous")
          '())
     " — "
     ,(if next-page
          `(a (@ (href ,next-page)) "Next →")
          '()))))

(define* (theme #:key
                (name "Untitled")
                (layout ugly-default-layout)
                (post-template ugly-default-post-template)
                (collection-template ugly-default-collection-template)
                (pagination-template ugly-default-pagination-template))
  (make-theme name layout post-template collection-template
              pagination-template))

(define (with-layout theme site title body)
  ((theme-layout theme) site title body))

(define (render-post theme site post)
  ((theme-post-template theme) post))

(define (render-collection theme site title posts prefix)
  ((theme-collection-template theme) site title posts prefix))

(define (render-pagination theme site body previous-page next-page)
  ((theme-pagination-template theme) site body previous-page next-page))

(define (date->string* date)
  "Convert DATE to human readable string."
  (date->string date "~a ~d ~B ~Y"))

(define ugly-theme
  (theme #:name "Ugly"
         #:layout ugly-default-layout
         #:post-template ugly-default-post-template
         #:collection-template ugly-default-collection-template
         #:pagination-template ugly-default-pagination-template))

(define* (blog #:key (theme ugly-theme) prefix
               (collections
                `(("Recent Posts" "index.html" ,posts/reverse-chronological)))
               posts-per-page)
  "Return a procedure that transforms a list of posts into pages
decorated by THEME, whose URLs start with PREFIX.  If POSTS-PER-PAGE
is specified, collections will be broken up into several pages with up
to POSTS-PER-PAGE posts on each page."
  (define (make-file-name base-name)
    (if prefix
        (string-append prefix "/" base-name)
        base-name))

  (lambda (site posts)
    (define (post->page post)
      (let ((base-name (string-append (site-post-slug site post)
                                      ".html"))
            (title (post-ref post 'title))
            (body ((theme-post-template theme) post)))
        (serialized-artifact (make-file-name base-name)
                             (with-layout theme site title body)
                             sxml->html)))

    (define (paginate base-name items)
      (define (make-page-file-name i)
        (make-file-name
         ;; First page does not get a page number added to the file
         ;; name.
         (if (= 0 i)
             (string-append base-name ".html")
             (string-append base-name "-"
                            (number->string i)
                            ".html"))))
      (define (make-page i items)
        (list (make-page-file-name i) (reverse items)))
      (let loop ((items items)
                 (n 0)
                 (i 0)
                 (page '()))
        (if (= n posts-per-page)
            (cons (make-page i page) (loop items 0 (+ i 1) '()))
            (match items
              (()
               (list (make-page i page)))
              ((item . rest)
               (loop rest (+ n 1) i (cons item page)))))))

    (define collection->page
      (match-lambda
        ((title file-name filter)
         ;; Earlier versions of Haunt, which did not have collection
         ;; pagination, told users to include a full file name, not
         ;; just a base name, so we continue to honor that style of
         ;; configuration.
         (let ((base-name (if (string-suffix? ".html" file-name)
                              (string-take file-name
                                           (- (string-length file-name) 5))
                              file-name))
               (filtered-posts (filter posts)))
           (define (make-collection-page current-page prev-page next-page)
             (match current-page
               ((file-name posts)
                (let* ((coll-sxml (render-collection theme site title
                                                     posts prefix))
                       (page-sxml (with-layout theme site title
                                               (render-pagination theme
                                                                  site
                                                                  coll-sxml
                                                                  (match prev-page
                                                                    (#f #f)
                                                                    ((file-name _) file-name))
                                                                  (match next-page
                                                                    (#f #f)
                                                                    ((file-name _) file-name))))))
                  (serialized-artifact file-name page-sxml sxml->html)))))
           (if posts-per-page
               (let loop ((pages (paginate base-name filtered-posts))
                          (prev-page #f))
                 (match pages
                   (()
                    '())
                   ((last-page)
                    (list (make-collection-page last-page prev-page #f)))
                   ((and (page . rest) (_ next-page . _))
                    (cons (make-collection-page page prev-page next-page)
                          (loop rest page)))))
               (list
                (serialized-artifact (string-append base-name ".html")
                                     (with-layout theme site title
                                                  (render-collection theme site title
                                                                     filtered-posts prefix))
                                     sxml->html)))))))

    (append (map post->page posts)
            (append-map collection->page collections))))
