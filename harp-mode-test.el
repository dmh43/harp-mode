(require 'harp-mode)

(ert-deftest enharmonics-convert ()
  (should (string= "Bb" (to-flat "A#")))
  (should (string= "A#" (to-sharp "A#")))
  (should (string= "A#" (to-sharp (to-flat "A#")))))

(ert-deftest pitch-shift ()
  (should (string= "E" (fifth-above "A")))
  (should (string= "D" (fifth-below "A")))
  (should (string= "A" (fifth-above (fifth-below "A"))))
  (should (string= "C" (shift-pitch "B" 1))))
