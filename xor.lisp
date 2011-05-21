;;; xor.lisp --- simple space game

;; Copyright (C) 2010, 2011  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: games

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; THIS TUTORIAL EXAMPLE IS UNDER CONSTRUCTION.
;; TODO user control
;; TODO proper world use and bouncing ball

;;; Preamble

;; First, we set up a package (often called a "namespace" in other
;; languages) with `defpackage' and then enter it with `in-package'.

;; The `:use' declaration shows that we will be importing names from
;; IOFORMS and from the base Common Lisp package.

(defpackage :xor 
  (:use :ioforms :common-lisp))

(in-package :xor)

;; Now we should set a few system variables. In Common Lisp, globals
;; are named `*like-this*' with an asterisk on each end, to
;; distinguish them from normal variables.

(setf *screen-width* 640)
(setf *screen-height* 480)
(setf *window-title* "XOR")

;; A "resource" is an image, sound, piece of text, or some other asset
;; involved in gameplay. Often these are loaded from external files in
;; your project folder such as PNG images or WAV sound files. You can
;; define resources for a given project with `defresource':

(defresource 
    (:name "nebula" :type :image :file "nebula.png")
    (:name "cloud1" :type :image :file "cloud1.png")
  (:name "cloud2" :type :image :file "cloud2.png")
  (:name "cloud3" :type :image :file "cloud3.png")
  (:name "cloud4" :type :image :file "cloud4.png")
  (:name "flarestar" :type :image :file "flarestar.png")
  (:name "aquastar" :type :image :file "aquastar.png")
  (:name "greenstar" :type :image :file "greenstar.png")
  (:name "bluestar" :type :image :file "bluestar.png")
  (:name "cosmos" :type :music :file "cosmos.ogg"))

(defvar *cloud-images* (list "cloud1" "cloud2" "cloud3" "cloud4"))

(defsprite cloud 
  :image (random-choose *cloud-images*)
  :blend :additive ;; for nice transparency.
  :direction (random-direction)
  :x (+ 80 (random 500)) 
  :y (+ 80 (random 500)))

(define-method update cloud ()
  (with-fields (direction) self
    ;; move straight
    (move self direction 0.05) 
    ;; but with slight jitter
    (move self (random-direction)
	  (random 0.03)) 
    ;; occasionally change direction
    (percent-of-time 1 
      (setf direction (left-turn direction)))))

;;; We can define some stars as well; these should move all in the
;;; same direction, only very slightly.

(defvar *star-images* (list "flarestar" "greenstar" 
			    "bluestar" "aquastar"
			    "bluestar" "aquastar"
			    "bluestar" "aquastar"))

;; The repeated entries are to make the repeated star images more
;; common in the random selection.

(defsprite star 
  :image (random-choose *star-images*)
  :blend :additive
  :x (random 500)
  :y (random 500))

(define-method update star ()
  (move self :north 0.02))
    
;; Let's put it all together by writing the code that runs when your
;; game starts. We define it as a function (using `defun') to be
;; called later by IOFORMS.

;; (What happens is that IOFORMS loads this file before initializing
;; the system, which allows you to set system variables like
;; `*screen-height*' and `*screen-width*' before the window actually
;; opens. Once IOFORMS is fully initialized according to the
;; parameters you set, it will look for a function with the same name
;; as the module---in this case `xor'---and execute it, which
;; hands control back to you.
 
(defun xor ()
  (play-music "cosmos")
  (dotimes (n 6)
    (add-block (new cloud)))
  (dotimes (n 20)
    (add-block (new star))))
    
;; Once your startup function is finished, the game is running.

;;; xor.lisp ends here
