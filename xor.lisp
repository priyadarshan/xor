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

;;; The walls of the reactor chamber

(defparameter *vertical-collision*
  '(:northeast :northwest
    :northwest :northeast
    :southeast :southwest
    :southwest :southeast))

(defparameter *horizontal-collision*
  '(:southeast :northeast
    :northeast :southeast
    :southwest :northwest
    :northwest :southwest))

;; A "resource" is an image, sound, piece of text, or some other asset
;; involved in gameplay. Often these are loaded from external files in
;; your project folder such as PNG images or WAV sound files. You can
;; define resources for a given project with `defresource':

(defresource
    (:name "wall" :type :image :file "wall.png"))

(defcell horizontal-wall
  :image "wall"
  :orientation :horizontal
  :categories '(:obstacle :oriented :wall))

(defcell vertical-wall
  :image "wall"
  :orientation :vertical
  :categories '(:obstacle :oriented :wall))

(defun is-wall (thing)
  (and (ioforms:object-p thing)
       (in-category thing :wall)))

(defun orientation (thing)
  (when (is-wall thing)
    (field-value :orientation thing)))

(defun bounce-direction (thing direction)
  (when (is-wall thing)
    (let ((map (ecase (orientation thing)
		 (:horizontal *horizontal-collision*)
		 (:vertical *vertical-collision*))))
      (getf direction map))))

;;; The radioactive particles that are the star of the game

(defresource
  (:name "alpha-particle" :type :image :file "alpha-particle.png")
  (:name "beta-particle" :type :image :file "beta-particle.png")
  (:name "gamma-particle" :type :image :file "gamma-particle.png")
  (:name "spark" :type :image :file "spark.png"))

(defvar *particle-colors*
  '(:alpha "alpha-particle"
    :beta "beta-particle"
    :gamma "gamma-particle"))

(defparameter *fast-split-bounces* 4)
(defparameter *medium-split-bounces* 8)

(defsprite particle
  :direction (random-direction)
  :color nil
  :speed 1
  :bounces *medium-split-bounces*
  :categories '(:particle))

(define-method set-color particle (color)
  (setf ^color color)
  (setf ^image (getf *particle-colors* color)))

(define-method initialize particle 
    (&key (color :alpha)
	  (bounces *medium-split-bounces*)
	  (speed 1)
	  (x 0)
	  (y 0))
  (setf ^x x ^y y)
  (setf ^bounces bounces)
  (setf ^speed speed)
  (set-color self color))

(define-method split particle ()
  (with-fields (x y color speed) self
    (drop (new particle 
	       :color color
	       :bounces *fast-split-bounces*
	       :speed (+ 2 speed))
	  x y)))

(define-method on-collide particle (thing)
  (when (is-wall thing)
    (with-fields (direction color bounces) self
      (setf direction (bounce-direction thing))
      (when (plusp bounces)  
	(decf bounces))
      (when (zerop bounces)
	(split self)))))

(define-method update particle ()
  (with-fields (direction speed) self
    (move self direction speed)))

;;; The player

(defresource
  (:name "robot" :type :image :file "robot.png"))
  ;; (:name "trail1" :type :image :file "trail1.png")
  ;; (:name "trail2" :type :image :file "trail2.png"))

(defsprite robot 
  :image "robot")

;; (define-method initialize robot ()
;;   (bind-event self (:up) (move :north 5 :pixels))
;;   (bind-event self (:down) (move :south 5 :pixels))
;;   (bind-event self (:right) (move :east 5 :pixels))
;;   (bind-event self (:left) (move :west 5 :pixels)))
    
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
  (dotimes (n 20)
    (add-block (new particle 
		    :color (random-choose (list :alpha :beta :gamma))
		    :x (random *screen-width*)
		    :y (random *screen-height*)))))
		    
;; Once your startup function is finished, the game is running.

;;; xor.lisp ends here
