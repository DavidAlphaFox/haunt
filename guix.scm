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
;; GNU Guix development package.  To build and install, run:
;;
;;   guix package -f guix.scm
;;
;; To use as the basis for a development environment, run:
;;
;;   guix environment -l guix.scm
;;
;;; Code:

(use-modules (guix packages)
             (guix licenses)
             (guix gexp)
             (guix git-download)
             (guix build-system gnu)
             (gnu packages)
             (gnu packages autotools)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (gnu packages pkg-config)
             (gnu packages texinfo))

(define %source-dir (dirname (current-filename)))

(package
  (name "haunt")
  (version "0.2.6")
  (source (local-file %source-dir
                      #:recursive? #t
                      #:select? (git-predicate %source-dir)))
  (build-system gnu-build-system)
  (arguments
   '(#:phases
     (modify-phases %standard-phases
       (add-after 'unpack 'bootstrap
         (lambda _ (zero? (system* "sh" "bootstrap")))))))
  (native-inputs (list autoconf automake pkg-config texinfo))
  (inputs (list guile-3.0))
  (propagated-inputs (list guile-commonmark guile-reader))
  (synopsis "Functional static site generator")
  (description "Haunt is a static site generator written in Guile
Scheme.  Haunt features a functional build system and an extensible
interface for reading articles in any format.")
  (home-page "http://haunt.dthompson.us")
  (license gpl3+))
