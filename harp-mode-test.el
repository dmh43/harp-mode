(require 'harp-mode)

(ert-deftest enharmonics-convert-test ()
  (should (equal "Bb" (harp-to-flat "A#")))
  (should (equal "A#" (harp-to-sharp "A#")))
  (should (equal "A#" (harp-to-sharp (to-flat "A#")))))

(ert-deftest pitch-shift-test ()
  (should (equal "E" (harp-fifth-above "A")))
  (should (equal "D" (harp-fifth-below "A")))
  (should (equal "A" (harp-fifth-above (fifth-below "A"))))
  (should (equal "C" (harp-shift-pitch "B" 1))))

(ert-deftest harp-layout-test ()
  (should (equal harp-c-harp (harp-layout "C"))))

(ert-deftest note-to-hole-test ()
  (should (equal "1" (harp-note-to-hole "A" "A" 1)))
  (should (equal "4" (harp-note-to-hole "A" "A" 2)))
  (should (equal "7" (harp-note-to-hole "A" "A" 3)))
  (should (equal "5" (harp-note-to-hole "C" "E" 2))))

(ert-deftest hole-to-note-test ()
  (should (equal "A" (harp-hole-to-note "A" "1")))
  (should (equal "A" (harp-hole-to-note "A" "4")))
  (should (equal "A" (harp-hole-to-note "A" "7")))
  (should (equal "E" (harp-hole-to-note "C" "5")))
  (should (equal "C#" (harp-hole-to-note "A" (harp-note-to-hole "A" "C#" 2)))))

(ert-deftest notes-to-tab-test ()
  (should (equal "4 5 6 7 1"
                 (harp-notes-to-tab "C" 2 "C E G ^C ,C"))))

;; C 1
;; A F A F F D C F F F F A ^C ^C ^C

;; C 3
;; E C E D C D E C ,A ,E
;; 8 7 8 -8 7 -8 8 7 -6 5
