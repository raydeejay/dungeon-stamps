;;;; dungeon-stamps.lisp

(in-package #:dungeon-stamps)

;;; "dungeon-stamps" goes here. Hacks and glory await!
(defparameter *canvas-width* 400)
(defparameter *canvas-height* 600)

(defparameter *black* (gamekit:vec4 0 0 0 1))
(defparameter *origin* (gamekit:vec2 0 0))


(gamekit:defgame dungeon-stamps () ()
  (:viewport-width *canvas-width*)     ; window's width
  (:viewport-height *canvas-height*)   ; window's height
  (:viewport-title "Dungeon stamps"))  ; window's title


(defparameter *field* (make-array '(9)))
(defparameter *field-grid* nil)

(defun make-field ()
  (let ((field (make-array '(9))))
    (loop :for slot :from 0 :upto 8 :doing
       (setf (elt field slot) (random-elt '(:coin :demon))))
    field))

(defparameter *curve* (make-array 4 :initial-contents (list (gamekit:vec2 300 300)
                                                      (gamekit:vec2 375 300)
                                                      (gamekit:vec2 425 300)
                                                      (gamekit:vec2 500 300))))

(defparameter *current-box-position* (gamekit:vec2 0 0))
(defparameter *cursor-position* (gamekit:vec2 0 0))
(defparameter *head-grabbed-p* nil)

(defparameter *clicked-slot* -1)


(defun coords-to-slot (x y)
  (cond ((< x 120)
         (cond ((< y 160) 0)
               ((< y 320) 3)
               ((< y 480) 6)))
        ((< x 240)
         (cond ((< y 160) 1)
               ((< y 320) 4)
               ((< y 480) 7)))
        ((< x 360)
         (cond ((< y 160) 2)
               ((< y 320) 5)
               ((< y 480) 8)))))

(defparameter *valid-moves*
  `((0 . 1) (0 . 3)
    (1 . 0) (1 . 2) (1 . 4)
    (2 . 1) (2 . 5)
    (3 . 0) (3 . 4) (3 . 6)
    (4 . 1) (4 . 3) (4 . 5) (4 . 7)
    (5 . 2) (5 . 4) (5 . 8)
    (6 . 3) (6 . 7)
    (7 . 6) (7 . 4) (7 . 8)
    (8 . 7) (8 . 5)))

(defun can-move (from to)
  (find (cons from to) *valid-moves* :test #'equal))


(defmethod gamekit:act ((app dungeon-stamps))
  ;; possibly do something here
  )


(defmethod gamekit:draw ((app dungeon-stamps))
  (gamekit:print-text (format nil "Clicked slot: ~D" *clicked-slot*) 0 500)
  ;; let's center image position properly first
  ;; (let ((head-image-position (gamekit:subt (aref *curve* 3) (gamekit:vec2 32 32))))
  ;;   ;; then draw it where it belongs
  ;;   (gamekit:draw-image head-image-position :snake-head))

  (loop :for y :from 0 :upto 2 :do
     (loop :for x :from 0 :upto 2 :do
        (draw-image (vec2 (* 120 x) (* y 160))
                    (aref *field-grid* y x)))))

(defun load-resources ()
  (gamekit:register-resource-package :keyword "img/")
  ;; (gamekit:define-sound :snake-grab "snake-grab.ogg")
  (gamekit:define-image :demon "demon.png")
  (gamekit:define-image :coin "coin.png")
  )

(defun bind-controls ()
  (gamekit:bind-cursor (lambda (x y)
                         "When left mouse button is pressed, update snake's head position"
                         (setf *clicked-slot* (coords-to-slot x y))))
  (gamekit:bind-button :mouse-left :pressed
                       (lambda ()
                         "Try to move to another slot"
                         ;;(gamekit:play :snake-grab)
                         ))
  (gamekit:bind-button :mouse-left :released
                       (lambda () (setf *head-grabbed-p* nil))))

(defun init-game ()
  (setf *field* (make-field))
  (setf *field-grid* (make-array '(3 3) :displaced-to *field*)))

(defmethod gamekit:post-initialize ((app dungeon-stamps))
  (load-resources)
  (bind-controls)
  (init-game))

(defun launch ()
  (gamekit:start 'dungeon-stamps))

