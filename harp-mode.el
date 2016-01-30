(setq notes-sharps '("C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"))
(setq notes-flats '("C" "Db" "D" "Eb" "E" "F" "Gb" "G" "Ab" "A" "Bb" "B"))
(setq c-harp '(("1" . "C") ("-1'" . "C#") ("-1" . "D")  ("2" . "E") ("-2''". "F") ("-2'" . "F#") ("-2" . "G")
               ("3" . "G") ("-3'''" . "G#") ("-3''" . "A") ("-3'" . "A#") ("-3" . "B")
               ("4" . "C") ("-4'" . "C#") ("-4" . "D") ("5" . "E") ("-5" . "F") ("6" . "G") ("-6'" . "G#")
               ("-6" . "A") ("-7" . "B") ("7" . "C") ("-8" . "D") ("8'" . "D#") ("8" . "E") ("-9" . "F")
               ("9'" . "F#") ("9" . "G") ("-10" . "A") ("10''" . "A#") ("10'" . "B") ("10" . "C")))
(setq harp-holes '("1" "-1'" "-1" "2" "-2''" "-2'" "-2" "3" "-3'''" "-3''" "-3'" "-3" "4" "-4'" "-4" "5"
                   "-5" "6" "-6'" "-6" "-7" "7" "-8" "8'" "8" "-9" "9'" "9" "-10" "10''" "10'" "10"))
(setq first-octave 0)
(setq second-octave 12)
(setq third-octave 20)
(setq octave-indices (list first-octave second-octave third-octave))

(defun cycle (seq n)
  "Returns a lazy sequence of the items in the given sequence repeated infinitely"
  (elt seq (mod n (length seq))))

(defun to-flat (note)
  (if (string-suffix-p "#" note)
      (elt notes-flats (cl-position note notes-sharps :test 'string=))
    note))

(defun to-sharp (note)
  (if (string-suffix-p "b" note)
      (elt notes-sharps (cl-position note notes-flats :test 'string=))
    note))

(defun fifth-above (note)
  (let ((note-index (cl-position note notes-sharps :test 'string=)))
    (cycle notes-sharps (+ note-index 7))))

(defun fifth-below (note)
  (let ((note-index (cl-position note notes-sharps :test 'string=)))
    (cycle notes-sharps (- note-index 7))))

(defun shift-pitch (note delta)
  (let ((note-index (cl-position note notes-sharps :test 'string=)))
    (cycle notes-sharps (+ note-index delta))))

(defun harp-notes (key)
  (let ((note-index (cl-position key notes-sharps :test 'string=)))
    (mapcar  '(lambda (elem)  (shift-pitch (cdr elem) note-index)) c-harp)))

(defun harp-layout (key)
  (let ((key-notes (harp-notes key)))
    (cl-mapcar #'cons harp-holes key-notes)))

(defun note-to-hole (key note octave)
  (let* ((start (nth (decf octave) octave-indices))
         (sharp-key (to-sharp key)) ; sanitize the input
         (sub-harp (nthcdr start (harp-layout sharp-key))))
    (car (rassoc note sub-harp))))

(defun hole-to-note (key hole)
  (let* ((sharp-key (to-sharp key)))
    (cdr (assoc hole (harp-layout sharp-key)))))

(provide 'harp-mode)
