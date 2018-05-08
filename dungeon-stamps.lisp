;;;; dungeon-stamps.lisp

(in-package #:dungeon-stamps)

;;; "dungeon-stamps" goes here. Hacks and glory await!

(defparameter *canvas-width* 400)
(defparameter *canvas-height* 600)

(gamekit:defgame dungeon-stamps () ()
                 (:viewport-width *canvas-width*)     ; window's width
                 (:viewport-height *canvas-height*)   ; window's height
                 (:viewport-title "Dungeon stamps"))  ; window's title

(gamekit:register-resource-package 'dungeon-stamps "/home/raydj/lisp/dungeon-stamps/img/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GAME STATE
(defparameter *hero* nil)
(defparameter *maxhp* 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI
(defparameter *cursor-position* (gamekit:vec2 0 0))

(defparameter *current-slot* 4)
(defparameter *clicked-slot* -1)

(defun coords-to-slot (coords)
  (let ((x (x coords))
        (y (y coords)))
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
                 ((< y 480) 8))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MOVEMENT
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

(defun move-stamp (from to)
  (setf (aref *field* to) (aref *field* from))
  (setf (aref *field* from) 'empty))

(defun move-to-target ()
  (setf (aref *field* *clicked-slot*) *hero*)
  (setf (aref *field* *current-slot*) (generate-stamp))
  (setf *current-slot* *clicked-slot*))

(defun try-to-move ()
  (when (can-move *current-slot* *clicked-slot*)
    (when (interact (aref *field* *clicked-slot*))
      (move-to-target)
      ;; pull the remaining card on the line/row
      ;; generate a new one
      ))
  (when (< (hp *hero*) 1)
    (switch-to-scene (make-instance 'menu))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SCENES
(defparameter *current-scene* nil)

(defclass scene () ())
(defgeneric on-enter (scene))
(defgeneric on-act (scene))
(defgeneric on-draw (scene))
(defgeneric on-exit (scene))

(defmethod on-enter ((scene scene))
  (gamekit:bind-cursor (lambda (x y)
                         "Keep track of the pointer position."
                         (setf *cursor-position* (vec2 x y)))))


;; menu
(defclass menu (scene) ())
(defmethod on-enter :after ((scene menu))
  (gamekit:bind-button :mouse-left :pressed
                       (lambda ()
                         "Activate the clicked widget, if there's one."
                         (when (eql 4 (coords-to-slot *cursor-position*))
                           (switch-to-scene (make-instance 'game))))))

(defmethod on-act ((scene menu)))
(defmethod on-draw ((scene menu))
  (print-text "START" 150 200))

(defmethod on-exit ((scene menu)))


;; game
(defclass game (scene) ())
(defmethod on-enter :after ((scene game))
  (gamekit:bind-button :mouse-left :pressed
                       (lambda ()
                         "Try to move to another slot"
                         ;;(gamekit:play :snake-grab)
                         (setf *clicked-slot* (coords-to-slot *cursor-position*))
                         (try-to-move)))
  (init-game))

(defmethod on-act ((scene game)))

(defmethod on-draw ((scene game))
  (gamekit:print-text (format nil "Coins: ~D / HP: ~D" (coins *hero*) (hp *hero*)) 0 500)
  (loop :for y :from 0 :upto 2 :do
     (loop :for x :from 0 :upto 2 :do
        ;; why are Y and X inverted here...
        ;; because the array is rows of columns? hmmmm
        (draw-stamp (aref *field-grid* y x) x y))))

(defmethod on-exit ((scene game)))


;; death scene
(defclass death (scene) ())
(defmethod on-enter ((scene death)))
(defmethod on-act ((scene death)))
(defmethod on-draw ((scene death)))
(defmethod on-exit ((scene death)))

(defun switch-to-scene (scene)
  (when *current-scene*
    (on-exit *current-scene*))
  (setf *current-scene* scene)
  (on-enter *current-scene*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INITIALISATION
(defparameter *field* (make-array '(9)))
(defparameter *field-grid* nil)

(defun load-resources ()
  ;; (gamekit:define-sound :snake-grab "snake-grab.ogg")
  (gamekit:define-image 'hero "hero.png")
  (gamekit:define-image 'demon "demon.png")
  (gamekit:define-image 'coin "coin.png")
  (gamekit:define-image 'potion "potion.png")
  (gamekit:define-image 'weapon "weapon.png")
  (gamekit:define-image 'chest "chest.png")
  (gamekit:define-image 'skeleton "skeleton.png")
  (gamekit:define-image 'goblin "goblin.png")
  (gamekit:define-image 'empty "empty.png")
  )

(defun init-game ()
  (setf *hero* (make-instance 'hero :weapon (make-instance 'weapon :damage 3))
        *field* (make-field)
        *field-grid* (make-array '(3 3) :displaced-to *field*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HOOKS
(defmethod gamekit:act ((app dungeon-stamps))
  ;; (when *current-scene* (on-act *current-scene*))
  )

(defmethod gamekit:draw ((app dungeon-stamps))
  (when *current-scene* (on-draw *current-scene*)))

(defmethod gamekit:post-initialize ((app dungeon-stamps))
  (load-resources)
  (switch-to-scene (make-instance 'menu)))

(defun launch ()
  (gamekit:start 'dungeon-stamps))
