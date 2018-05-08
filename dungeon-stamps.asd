;;;; dungeon-stamps.asd

(asdf:defsystem #:dungeon-stamps
  :description "Describe dungeon-stamps here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:trivial-gamekit)
  :components ((:file "package")
               (:file "utils")
               (:file "model")
               (:file "field")
               (:file "dungeon-stamps")))

