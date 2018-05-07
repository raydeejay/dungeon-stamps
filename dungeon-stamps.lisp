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
;; MODEL
(defclass stamp ()
  ((name :accessor name :initarg :name :initform 'stamp))
  (:documentation "Base clase for stamps."))

(defclass weapon (stamp)
  ((name   :initform 'weapon)
   (damage :accessor damage :initarg  :damage :initform (+ 3 (random 8)))))

(defclass enemy (stamp)
  ((name :accessor name :initform 'enemy)
   (hp   :accessor hp   :initarg  :hp    :initform 10)))

(defclass demon (enemy)
  ((name :accessor name :initform 'demon)
   (hp   :accessor hp   :initarg  :hp    :initform (1+ (random 8)))))

(defclass skeleton (enemy)
  ((name :accessor name :initform 'skeleton)
   (hp   :accessor hp   :initarg  :hp    :initform (1+ (random 4)))))

(defclass goblin (enemy)
  ((name :accessor name :initform 'goblin)
   (hp   :accessor hp   :initarg  :hp    :initform (1+ (random 6)))))

(defclass coin (stamp)
  ((name  :initform 'coin)
   (value :accessor value :initarg  :value :initform (1+ (random 10)))))

(defclass potion (stamp)
  ((name :accessor name :initform 'potion)))

(defclass chest (stamp)
  ((name  :accessor name  :initform 'chest)))

(defclass hero (stamp)
  ((name   :accessor name   :initform 'hero)
   (coins  :accessor coins  :initarg  :coins  :initform 0)
   (hp     :accessor hp     :initarg  :hp     :initform *maxhp*)
   (weapon :accessor weapon :initarg  :weapon :initform nil)))


(defgeneric interact (stamp)
  (:documentation "Interact with a stamp. Returns T if the stamp should be discarded."))

(defmethod interact ((stamp weapon))
  (setf (weapon *hero*) stamp)
  T)

(defmethod interact ((stamp enemy))
  (if (weapon *hero*)
      (cond ((< (damage (weapon *hero*)) (hp stamp))
             (decf (hp stamp) (damage (weapon *hero*)))
             (setf (weapon *hero*) nil)
             nil)                       ; don't remove the enemy
            ((= (damage (weapon *hero*)) (hp stamp))
             (setf (weapon *hero*) nil)
             T)
            ((> (damage (weapon *hero*)) (hp stamp))
             (decf (damage (weapon *hero*)) (hp stamp))
             T))
      (progn (decf (hp *hero*) (hp stamp))
             T)))

(defmethod interact ((stamp coin))
  (incf (coins *hero*) (value stamp))
  T)

(defmethod interact ((stamp chest))
  (change-class stamp 'coin :name 'coin :value (1+ (random 10)))
  nil)

(defmethod interact ((stamp potion))
  (setf (hp *hero*) *maxhp*)
  T)

(defgeneric draw-stamp (stamp x y)
  (:documentation "Renders the stamp at the specified position. Specialise :after methods for
additional rendering, like health points or damage value."))

(defmethod draw-stamp ((stamp stamp) x y)
  (draw-image (vec2 (* 120 x) (* y 160))
              (name stamp)))

(defmethod draw-stamp :after ((stamp weapon) x y)
  (print-text (format nil "~D" (damage stamp)) (+ 10 (* 120 x)) (+ 10 (* y 160))))

(defmethod draw-stamp :after ((stamp enemy) x y)
  (print-text (format nil "~D" (hp stamp)) (+ 90 (* 120 x)) (+ 140 (* y 160))))

(defmethod draw-stamp :after ((stamp coin) x y)
  (print-text (format nil "~D" (value stamp)) (+ 90 (* 120 x)) (+ 10 (* y 160))))

(defmethod draw-stamp :after ((stamp hero) x y)
  (print-text (format nil "~D" (hp stamp)) (+ 90 (* 120 x)) (+ 140 (* y 160)))
  (when (weapon stamp)
    (print-text (format nil "~D" (damage (weapon stamp))) (+ 10 (* 120 x)) (+ 10 (* y 160)))))


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

(defun move-to-target ()
  (setf (aref *field* *clicked-slot*) *hero*)
  (setf (aref *field* *current-slot*) (generate-stamp))
  (setf *current-slot* *clicked-slot*))

(defun try-to-move ()
  (when (can-move *current-slot* *clicked-slot*)
    (when (interact (aref *field* *clicked-slot*))
      (move-to-target)))
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

(defun generate-stamp ()
  (make-instance (random-elt '(coin coin coin coin
                               skeleton skeleton skeleton
                               goblin goblin goblin
                               demon
                               potion
                               chest
                               weapon weapon weapon))))

(defun make-field ()
  (let ((field (make-array '(9))))
    (loop :for slot :from 0 :upto 8 :doing
       (setf (elt field slot) (generate-stamp)))
    (setf (elt field 4) (make-instance 'hero))
    field))

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
  (setf *field* (make-field)
        *field-grid* (make-array '(3 3) :displaced-to *field*)
        *hero* (make-instance 'hero)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HOOKS
(defmethod gamekit:act ((app dungeon-stamps))
  ;; (when *current-scene* (on-act *current-scene*))
  )

(defmethod gamekit:draw ((app dungeon-stamps))
  (when *current-scene* (on-draw *current-scene*)))

(defmethod gamekit:post-initialize ((app dungeon-stamps))
  (load-resources)
  (switch-to-scene (make-instance 'menu))
  ;; (bind-controls)
  ;; (init-game)
  )

(defun launch ()
  (gamekit:start 'dungeon-stamps))
