(defstruct entity name ex level atk def hp)
(defstruct stage desc enemies items actions)

(defun create-entity (name)
  "Create an entity in the game"
  (let ((entity (make-entity)))
    (setf
      (entity-name entity) (string-capitalize name)
      (entity-ex entity) 0
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

(defun combat (action dmg e1 e2)
  (format
    t
    "~A: ~A hits ~A for DMG:~A with DEF:~A~%"
    action
    (entity-name e1)
    (entity-name e2)
    dmg
    (entity-def e2))

    ;; Ensure negative numbers aren't subtracted, else health
    ;; will actually go up!
    (if (> dmg 0)
      (setf (entity-hp e2) (- (entity-hp e2) dmg))))

(defun attack (e1 e2 amount)
  "Function to attack an entity"
  (let ((dmg (- (+ amount (entity-atk e1)) (entity-def e2))))
    (combat "attack" dmg e1 e2)))

(defun defend (e1 e2 amount)
  "Function to defend against an entity attack"
  (let ((dmg (- (+ amount (entity-atk e1)) (* 2 (entity-def e2)))))
    (combat "defend" dmg e1 e2)))

(defun is-alive? (entity)
  (> (entity-hp entity) 0))

(defun user-input ()
  (format t "What do you do? ")
  (let ((cmd (string-downcase (read-line))))
    cmd))

(defun help ()
  "Function to display help to the user"
  (format t "Help goes here.~%"))

(defun init-stage (stage)
  "A function to read in a stage and init enemies, items etc"
  (let ((stage (make-stage)))
    (setf (stage-desc stage) "This is a test")
    stage))

(defun game-loop (player data)
  (let ((stage (init-stage (car data))))
    ;; Print chapter
    (format t "~A~%" (stage-desc stage))

    ;; Bail out if data is empty, story is over.
    (if (null stage)
      "Game Over!"
      (progn
        (let ((action (user-input)))
          (cond
            ;; Help
            ((or (equal action "help") (equal action "?"))
              ((lambda ()
                (help)
                (game-loop player data))))

            ;; Quitting
            ((or (equal action "leave") (equal action "quit"))
             "Game Over")

            ;; Attack
            ((equal action "attack")
            ((lambda ()
              (format t "You attack!~%")
              (game-loop player (cdr data)))))

            ;; Defend
            ((equal action "defend")
              ((lambda ()
                (format t "You defend!~%")
                (game-loop player (cdr data)))))

            ;; Loop if nothing, don't advance story
            (t (game-loop player data))))))))

(defun main ()
  (let ((player (create-player)))
    (format t "Welcome, ~A.~%" (entity-name player))
    (with-input-from-string
      (script
        (uiop:read-file-string "~/dev/lisp/common/cl-game/script.json"))
      (game-loop player (json:decode-json script)))))

(main)
