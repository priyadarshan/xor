;;; xor.lisp --- space game mashup

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

(defworld whitespace :background "story")

(defsprite player 
  :image "blue-dot" 
  :x 20 :y 45)

(define-method initialize player ()
  (bind-event self (:up) (move :north 5 :pixels))
  (bind-event self (:down) (move :south 5 :pixels))
  (bind-event self (:right) (move :east 5 :pixels))
  (bind-event self (:left) (move :west 5 :pixels)))

(define-method bloop player ()
  (play-sound self "bloop"))

(play (new universe)
      :world (new whitespace)
      :player (new player))
      
;;; example.lisp ends here
