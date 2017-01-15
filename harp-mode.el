(setq harp-notes-sharps '("C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"))
(setq harp-notes-flats '("C" "Db" "D" "Eb" "E" "F" "Gb" "G" "Ab" "A" "Bb" "B"))
(setq harp-c-harp '(("1" . "C") ("-1'" . "C#") ("-1" . "D")  ("2" . "E") ("-2''". "F") ("-2'" . "F#") ("-2" . "G")
               ("3" . "G") ("-3'''" . "G#") ("-3''" . "A") ("-3'" . "A#") ("-3" . "B")
               ("4" . "C") ("-4'" . "C#") ("-4" . "D") ("5" . "E") ("-5" . "F") ("6" . "G") ("-6'" . "G#")
               ("-6" . "A") ("-7" . "B") ("7" . "C") ("-8" . "D") ("8'" . "D#") ("8" . "E") ("-9" . "F")
               ("9'" . "F#") ("9" . "G") ("-10" . "A") ("10''" . "A#") ("10'" . "B") ("10" . "C")))
(setq harp-holes '("1" "-1'" "-1" "2" "-2''" "-2'" "-2" "3" "-3'''" "-3''" "-3'" "-3" "4" "-4'" "-4" "5"
                   "-5" "6" "-6'" "-6" "-7" "7" "-8" "8'" "8" "-9" "9'" "9" "-10" "10''" "10'" "10"))
(setq harp-first-octave 0)
(setq harp-second-octave 12)
(setq harp-third-octave 20)
(setq harp-octave-indices (list harp-first-octave harp-second-octave harp-third-octave))

(defun harp-cycle (seq n)
  "Returns a lazy sequence of the items in the given sequence repeated infinitely"
  (elt seq (mod n (length seq))))

(defun harp-to-flat (note)
  (if (string-suffix-p "#" note)
      (elt harp-notes-flats (cl-position note harp-notes-sharps :test 'string=))
    note))

(defun harp-to-sharp (note)
  (if (string-suffix-p "b" note)
      (elt harp-notes-sharps (cl-position note harp-notes-flats :test 'string=))
    note))

(defun harp-fifth-above (note)
  (let ((note-index (cl-position note harp-notes-sharps :test 'string=)))
    (harp-cycle harp-notes-sharps (+ note-index 7))))

(defun harp-fifth-below (note)
  (let ((note-index (cl-position note harp-notes-sharps :test 'string=)))
    (harp-cycle harp-notes-sharps (- note-index 7))))

(defun harp-shift-pitch (note delta)
  (let ((note-index (cl-position note harp-notes-sharps :test 'string=)))
    (harp-cycle harp-notes-sharps (+ note-index delta))))

(defun harp-notes (key)
  (let ((note-index (cl-position key harp-notes-sharps :test 'string=)))
    (mapcar  '(lambda (elem)  (harp-shift-pitch (cdr elem) note-index)) harp-c-harp)))

(defun harp-layout (key)
  (let ((key-notes (harp-notes key)))
    (cl-mapcar #'cons harp-holes key-notes)))

(defun harp-note-to-hole (key note octave)
  (let* ((octave (if (string-prefix-p "^" note)
                     (+ octave 1)
                   (if (string-prefix-p "," note) (- octave 1) octave)))
         (note (if (or (string-prefix-p "^" note)
                       (string-prefix-p "," note))
                   (substring note 1)
                 note))
         (start (nth (decf octave) harp-octave-indices))
         (sharp-key (harp-to-sharp key)) ; sanitize the input
         (sub-harp (nthcdr start (harp-layout sharp-key))))
    (car (rassoc note sub-harp))))

(defun harp-hole-to-note (key hole)
  (let* ((sharp-key (harp-to-sharp key)))
    (cdr (assoc hole (harp-layout sharp-key)))))

(defun harp-notes-to-holes (key octave notes)
  (let* ((key-sharp (harp-to-sharp key)))
    (mapcar (lambda (note)
              (harp-note-to-hole key-sharp note octave))
            notes)))

(defun harp-notes-to-tab (key octave notes-str &optional from to)
  (interactive
   (when (use-region-p)
       (list nil nil nil (region-beginning) (region-end))))
  (let* ((workOnStringP (if notes-str t nil))
         (inputStr (if workOnStringP
                       notes-str
                     (buffer-substring-no-properties from to)))
         (args-and-notes (split-string inputStr "\n" t " "))
         (args (split-string (car args-and-notes) " "))
         (notes (if notes-str
                    (split-string notes-str " ")
                  (split-string (cadr args-and-notes) " ")))
         (key-sharp (if key
                        (harp-to-sharp key)
                      (harp-to-sharp (car args))))
         (octave (if octave
                     octave
                   (string-to-number (cadr args))))
         (outputStr
          (mapconcat (lambda (note)
                    (harp-note-to-hole key-sharp note octave))
                     notes
                     " ")))
    (if workOnStringP
        outputStr
      (save-excursion
        (newline)
        (insert outputStr)))))

(defvar harp-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `harp-mode'.")

(define-derived-mode harp-mode fundamental-mode "Harp"
  )

(add-to-list 'auto-mode-alist '("\\.harp\\'" . harp-mode))

(provide 'harp-mode)
