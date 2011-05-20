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

(defpackage :xor 
  (:use :ioforms :common-lisp))

(in-package :xor)

(setf *screen-width* 800)
(setf *screen-height* 600)
(setf *window-title* "XOR!")

(defresource 
    (:name "nebula" :type :image :file "nebula.png")
    (:name "cloud1" :type :image :file "cloud1.png")
  (:name "cloud2" :type :image :file "cloud2.png")
  (:name "cloud3" :type :image :file "cloud3.png")
  (:name "cloud4" :type :image :file "cloud4.png")
  (:name "flarestar" :type :image :file "flarestar.png")
  (:name "greenstar" :type :image :file "greenstar.png")
  (:name "bluestar" :type :image :file "bluestar.png")
  (:name "sparkle-1" :type :image :file "sparkle-1.png")
  (:name "sparkle-2" :type :image :file "sparkle-2.png"))

(defvar *clouds* (list "cloud1" "cloud2" "cloud3" "cloud4"))

(defvar *stars* (list "flarestar" "greenstar" "bluestar"))

(defsprite cloud 
  :image (random-choose *clouds*)
  :direction (random-direction :not-here)
  :x (+ 80 (random 500)) 
  :y (+ 80 (random 500)))

(define-method draw cloud ()  
  (with-fields (x y image) self
    (draw-image image x y)))

(define-method update cloud ()
  (move-toward self ^direction 0.05)
  (move-toward self (random-direction :not-here)
	       (random 0.05)))

(defsprite star 
  :image (random-choose *stars*)
  :x (+ 80 (random 500)) 
  :y (+ 80 (random 500)))

(defun xor ()
  (message "RUNNING XOR!")
  (dotimes (n 20)
    (add-block (new cloud))))


;; Now we define the code that runs when your game starts. We define
;; it as a function (using `defun') to be called later by IOFORMS.

;; (What happens is that IOFORMS loads this file before initializing
;; the system, which allows you to set system variables like
;; `*screen-height*' and `*screen-width*' before the window actually
;; opens. Once IOFORMS is fully initialized according to the
;; parameters you set, it will look for a function with the same name
;; as the module---in this case `xor'---and execute it, which
;; hands control back to you.

 
;(defworld whitespace :background "story")

;; (define-method initialize dot ()
;;   (bind-event self (:up) (move :north 5 :pixels))
;;   (bind-event self (:down) (move :south 5 :pixels))
;;   (bind-event self (:right) (move :east 5 :pixels))
;;   (bind-event self (:left) (move :west 5 :pixels)))

;; (play (new universe)
;;       :world (new whitespace)
;;       :dot (new dot))
      
;;; xor.lisp ends here
