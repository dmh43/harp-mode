(require 'harp-mode)

(ert-deftest enharmonics-convert-test ()
  (should (equal "Bb" (to-flat "A#")))
  (should (equal "A#" (to-sharp "A#")))
  (should (equal "A#" (to-sharp (to-flat "A#")))))

(ert-deftest pitch-shift-test ()
  (should (equal "E" (fifth-above "A")))
  (should (equal "D" (fifth-below "A")))
  (should (equal "A" (fifth-above (fifth-below "A"))))
  (should (equal "C" (shift-pitch "B" 1))))

(ert-deftest harp-layout-test ()
  (should (equal c-harp (harp-layout "C"))))

(ert-deftest note-to-hole-test ()
  (should (equal "1" (note-to-hole "A" "A" 1)))
  (should (equal "4" (note-to-hole "A" "A" 2)))
  (should (equal "7" (note-to-hole "A" "A" 3)))
  (should (equal "5" (note-to-hole "C" "E" 2))))

(ert-deftest hole-to-note-test ()
  (should (equal "A" (hole-to-note "A" "1")))
  (should (equal "A" (hole-to-note "A" "4")))
  (should (equal "A" (hole-to-note "A" "7")))
  (should (equal "E" (hole-to-note "C" "5")))
  (should (equal "C#" (hole-to-note "A" (note-to-hole "A" "C#" 2)))))

(ert-deftest notes-to-tab-test ()
  (should (equal '("4" "5" "6")
                 (notes-to-holes "C" 2 '("C" "E" "G")))))
