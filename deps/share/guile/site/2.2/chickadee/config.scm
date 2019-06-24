;;; Chickadee Game Toolkit
;;; Copyright © 2016 David Thompson <davet@gnu.org>
;;;
;;; Chickadee is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; Chickadee is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General
;;; Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Build time configuration.
;;
;;; Code:

(define-module (chickadee config)
  #:export (%datadir
            %chickadee-version
            scope-datadir))

(define %datadir
  (or (getenv "CHICKADEE_DATADIR") "/home/milo/guile-prefix/share/chickadee"))

(define %chickadee-version "0.4.0")

(define (scope-datadir file)
  "Append the Chickadee data directory to FILE."
  (string-append %datadir "/" file))
