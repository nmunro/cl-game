(defstruct entity
  name
  exp
  level
  atk
  def
  hp)

(defun create-entity (name)
  "Create an entity in the game"
  (let ((entity (make-entity)))
    (setf (entity-name entity) name
      (entity-exp entity) 0
      (entity-level entity) 1
      (entity-atk entity) 10
      (entity-def entity) 10
      (entity-hp entity) 100)
   entity))

(defun create-player ()
  "Create a player in terms of an entity"
  (format t "Please enter your name: ")
  (create-entity (read)))

(defun display-entity (entity)
  "Display information about an entity"
  (format t "Info:~%")
  (format t " -> Name: ~A~%" (entity-name entity))
  (format t " -> Level: ~A~%" (entity-level entity))
  (format t " -> ATK: ~A~%" (entity-atk entity))
  (format t " -> DEF: ~A~%" (entity-def entity))
  (format t " -> HP: ~A~%" (entity-hp entity)))

(defun attack (e1 e2 amount)
  "Function to attack an entity"
  (let ((dmg (- (+ amount (entity-atk e1)) (entity-def e2))))
    (format
      t
      "You attack: ~A hits ~A for DMG:~A with DEF:~A~%"
      (entity-name e1)
      (entity-name e2)
      dmg
      (entity-def e2))

      ;; Ensure negative numbers aren't subtracted, else health
      ;; will actually go up!
      (if (> dmg 0)
        (setf (entity-hp e2) (- (entity-hp e2) dmg)))))

(defun defend (e1 e2 amount)
  "Function to defend against an entity attack"
  (let ((dmg (- (+ amount (entity-atk e1)) (* 2 (entity-def e2)))))
    (format
      t
      "You defend: ~A hits ~A for DMG:~A with DEF:~A~%"
      (entity-name e1)
      (entity-name e2)
      dmg
      (entity-def e2))

      ;; Ensure negative numbers aren't subtracted, else health
      ;; will actually go up!
      (if (> dmg 0)
        (setf (entity-hp e2) (- (entity-hp e2) dmg)))))

(defun is-alive? (entity)
  (> (entity-hp entity) 0))

(defun user-input ()
  (format t "Please enter a command: ")
  (let ((cmd (string-downcase (read-line))))
    cmd))

(defun game-loop (player entity)
  (format
    t
    "Welcome ~A, you encounter a Goblin, it attacks.~%What do you do? "
    (entity-name player))

    (let ((action (user-input)))
      (cond
        ((equal action "attack") ((lambda ()
          (attack player entity (random 25)) (display-entity player))))

        ((equal action "defend") ((lambda ()
          (defend player entity (random 25)) (display-entity player))))

        (t "Dunno"))))

(game-loop (create-player) (create-entity "Gobin"))
