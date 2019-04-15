(defstruct player name)
(defstruct stage text enemies actions end)

(defun create-player ()
  (format t "Please enter your name: ")
  (let ((player (make-player)))
    (setf (player-name player) (string-capitalize (read)))
   player))

(defun user-input (actions)
  (format t "You can: ~{'~A'~^, ~}, what do you do? " actions)
  (let ((cmd (string-downcase (read-line))))
    cmd))

(defun init-stage (stage-data)
  (let ((stage (make-stage)))
    (setf
      (stage-text stage) (rest (assoc :TEXT stage-data))
      (stage-enemies stage) (rest (assoc :ENEMIES stage-data))
      (stage-actions stage) (rest (assoc :ACTIONS stage-data)))
      (stage-end stage) (rest (assoc :END stage-data))
    stage))

(defun game-over? (stage)
  (not (cdr stage)))

(defun game-loop (player data)
  (let ((stage (init-stage (car data))))
    (format t "~A~%" (stage-text stage))

    (if (game-over? data)
      "Game Over!" ; Bail out if data is empty, story is over.
      (progn       ; Otherwise continue
        (let ((action (user-input (stage-actions stage))))
          (cond
            ((equal action "attack")
              ((lambda ()
                (format t "You attack!~%")
                (game-loop player (cdr data)))))

            ((equal action "explore")
              ((lambda ()
                (format t "You explore...~%")
                (game-loop player (cdr data)))))

            (t (game-loop player data))))))))

(defun main ()
  (let ((player (create-player)))
    (with-input-from-string
      (script
        (uiop:read-file-string "~/dev/lisp/common/cl-game/script.json"))
      (game-loop player (json:decode-json script)))))

(main)
