(setq notes '("C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B") )
(setq notes-flats '("C" "Db" "D" "Eb" "E" "F" "Gb" "G" "Ab" "A" "Bb" "B") )
(setq c-harp '(("1" . "C") ("-1'" . "C#") ("-1" . "D")  ("2" . "E") ("-2''". "F") ("-2'" . "F#") ("-2" . "G")
               ("3" . "G") ("-3'''" . "G#") ("-3''" . "A") ("-3'" . "A#") ("-3" . "B")))
(setq harp-holes '("1" "-1" "-1'" "2" "-2''" "-2'" "-2" "3" "-3'''" "-3''" "-3'" "-3"))

(defun cycle (seq n)
  "Returns a lazy sequence of the items in the given sequence repeated infinitely"
  (elt seq (mod n (length seq))))

(defun to-flat (note)
  (elt notes-flats (cl-position note notes :test 'string=)))

(to-flat "A#")

(cycle notes 2)
(cl-position "A" notes :test 'string=)

(defun fifth-above (note)
  (let ((note-index (cl-position note notes :test 'string=)))
    (cycle notes (+ note-index 7))))

(defun fifth-below (note)
  (let ((note-index (cl-position note notes :test 'string=)))
    (cycle notes (- note-index 7))))

(fifth-above "A")
(fifth-below "A")
(fifth-above (fifth-below "A"))

(defun shift-pitch (note delta)
  (let ((note-index (cl-position note notes :test 'string=)))
    (cycle notes (+ note-index delta))))

(defun harp-notes (key)
  (let ((note-index (cl-position key notes :test 'string=)))
    (mapcar  '(lambda (elem)  (shift-pitch (cdr elem) note-index)) c-harp)))

(defun harp-layout ("A"))

(message (cdr (car c-harp)))

((lambda (elem) (message (cdr elem))) (car c-harp))

;(message )


 
