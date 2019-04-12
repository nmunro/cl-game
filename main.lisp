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
  "Function to attack with an entity"
  (format
    t
    "Player hit entity for ATK ~A with DEF ~A~%"
    (+ amount (gethash 'atk e1))
    (- (gethash 'def e2) amount))
  (setf (gethash 'hp e2) (- (gethash 'hp e2) (+ amount (gethash 'atk e1)))))

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
