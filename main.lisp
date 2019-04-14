(defun create-entity (name)
  "Create an entity in the game"
  (let ((entity (make-hash-table)))
    (setf (gethash 'name entity) name)
    (setf (gethash 'exp entity) 0)
    (setf (gethash 'level entity) 1)
    (setf (gethash 'atk entity) 10)
    (setf (gethash 'def entity) 10)
    (setf (gethash 'hp entity) 100)
   entity))

(defun create-player ()
  "Create a player in terms of an entity"
  (format t "Please enter your name: ")
  (create-entity (read)))

(defun display-entity (entity)
  "Display information about an entity"
  (format t "Info:~%")
  (format t " -> Name: ~A~%" (gethash 'name entity))
  (format t " -> Level: ~A~%" (gethash 'level entity))
  (format t " -> ATK: ~A~%" (gethash 'atk entity))
  (format t " -> DEF: ~A~%" (gethash 'def entity))
  (format t " -> HP: ~A~%" (gethash 'hp entity)))

(defun attack (e1 e2 amount)
  "Function to attack an entity"
  (let ((dmg (- (+ amount (gethash 'atk e1)) (gethash 'def e2))))
    (format
      t
      "You attack: ~A hits ~A for DMG:~A with DEF:~A~%"
      (gethash 'name e1)
      (gethash 'name e2)
      dmg
      (gethash 'def e2))

      ;; Ensure negative numbers aren't subtracted, else health
      ;; will actually go up!
      (if (> dmg 0)
        (setf (gethash 'hp e2) (- (gethash 'hp e2) dmg)))))

(defun defend (e1 e2 amount)
  "Function to defend against an entity attack"
  (let ((dmg (- (+ amount (gethash 'atk e1)) (* 2 (gethash 'def e2)))))
    (format
      t
      "You defend: ~A hits ~A for DMG:~A with DEF:~A~%"
      (gethash 'name e1)
      (gethash 'name e2)
      dmg
      (gethash 'def e2))

      ;; Ensure negative numbers aren't subtracted, else health
      ;; will actually go up!
      (if (> dmg 0)
        (setf (gethash 'hp e2) (- (gethash 'hp e2) dmg)))))

(defun is-alive? (entity)
  (> (gethash 'hp entity) 0))

(defun user-input ()
  (format t "Please enter a command: ")
  (let ((cmd (string-downcase (read-line))))
    cmd))

(defun game-loop (player entity)
  (format
    t
    "Welcome ~A, you encounter a Goblin, it attacks.~%What do you do? "
    (gethash 'name player))

    (let ((action (user-input)))
      (cond
        ((equal action "attack") ((lambda ()
          (attack player entity (random 25)) (display-entity player))))

        ((equal action "defend") ((lambda ()
          (defend player entity (random 25)) (display-entity player))))

        (t "Dunno"))))

(game-loop (create-player) (create-entity "Gobin"))
