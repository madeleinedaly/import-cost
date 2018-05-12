;;; import-cost-test.el --- Tests for import-cost

;; Author: Madeleine Daly <madeleine.faye.daly@gmail.com>
;; Last-Updated: <2018-05-12 15:02:59>

(ert-deftest import-cost-util/intern-keys ()
  (should
   (equal
    (import-cost--intern-keys
     '(("name" . "lodash/capitalize")
       ("line" . 2)
       ("string" . "require('lodash/capitalize')")
       ("size" . 3477)
       ("gzip" . 1346)
       ("filename" . "/Users/mdaly/Code/import-cost/buffer-file.js")))
    '((name . "lodash/capitalize")
      (line . 2)
      (string . "require('lodash/capitalize')")
      (size . 3477)
      (gzip . 1346)
      (filename . "/Users/mdaly/Code/import-cost/buffer-file.js")))))

(ert-deftest import-cost-lang/js ()
  (should (equal import-cost--lang-javascript (import-cost--language "index.js"))))

(ert-deftest import-cost-lang/jsx ()
  (should (equal import-cost--lang-javascript (import-cost--language "index.jsx"))))

(ert-deftest import-cost-lang/ts ()
  (should (equal import-cost--lang-typescript (import-cost--language "index.ts"))))

(ert-deftest import-cost-lang/tsx ()
  (should (equal import-cost--lang-typescript (import-cost--language "index.tsx"))))

(ert-deftest import-cost-ui/message ()
  (let* ((import-cost-bundle-size-decoration 'both)
         (package-info '((size . 203890) (gzip . 36890)))
         (decoration-message (import-cost--get-decoration-message package-info)))
    (should (equal " 203KB (gzipped: 36KB)" decoration-message))))

(ert-deftest import-cost-ui/color ()
  (let* ((import-cost-large-package-color "#d44e40")
         (size (+ import-cost-medium-package-size (* 128 1024)))
         (package-info (list (cons 'size size)))
         (decoration-color (import-cost--get-decoration-color package-info)))
    (should (equal import-cost-large-package-color decoration-color))))

;;; import-cost-test.el ends here
