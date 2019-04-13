(defun create-entity (name)
  "Create an entity in the game"
  (let ((entity (make-hash-table)))
    (setf (gethash 'name entity) name)
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
      "~A hit ~A for DMG:~A with DEF:~A~%"
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

(defun game-loop (player entity)
  (display-entity player)
  (display-entity entity)

  (attack entity player (random 25))

  (if (is-alive? player)
    (attack player entity (random 25)))

  (cond
    ((not (is-alive? player)) (format t "Game Over: You Died~%"))
    ((not (is-alive? entity)) (format t "Game Over: You Won~%"))
    ((and (is-alive? player) (is-alive? entity)) (game-loop player entity))))

(game-loop (create-player) (create-entity "Gobin"))
