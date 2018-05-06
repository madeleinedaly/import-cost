;;; import-cost-test.el --- Tests for import-cost.el

;; Author: Madeleine Daly <madeleine.faye.daly@gmail.com>
;; Last-Updated: <2018-05-06 19:03:30>

(ert-deftest import-cost-util/intern-keys ()
  (let ((actual (import-cost--intern-keys
                 '(("name" . "lodash/capitalize")
                   ("line" . 2)
                   ("string" . "require('lodash/capitalize')")
                   ("size" . 3477)
                   ("gzip" . 1346)
                   ("filename" . "/Users/mdaly/Code/import-cost.el/buffer-file.js"))))
        (expected '((name . "lodash/capitalize")
                    (line . 2)
                    (string . "require('lodash/capitalize')")
                    (size . 3477)
                    (gzip . 1346)
                    (filename . "/Users/mdaly/Code/import-cost.el/buffer-file.js")))))
  (should (equal actual expected)))

(ert-deftest import-cost-lang/js ()
  (should (string-equal import-cost--lang-javascript (import-cost--language "index.js"))))

(ert-deftest import-cost-lang/jsx ()
  (should (string-equal import-cost--lang-javascript (import-cost--language "index.jsx"))))

(ert-deftest import-cost-lang/ts ()
  (should (string-equal import-cost--lang-typescript (import-cost--language "index.ts"))))

(ert-deftest import-cost-lang/tsx ()
  (should (string-equal import-cost--lang-typescript (import-cost--language "index.tsx"))))

(ert-deftest import-cost-ui/message ()
  (let* ((import-cost-bundle-size-decoration 'both)
         (package-info '((size . 203890) (gzip . 36890)))
         (decoration-message (import-cost--get-decoration-message package-info)))
    (should (string-equal " 203KB (gzipped: 36KB)" decoration-message))))

(ert-deftest import-cost-ui/color ()
  (let* ((import-cost-large-package-color "#d44e40")
         (size (+ import-cost-medium-package-size (* 128 1024)))
         (package-info (list (cons 'size size)))
         (decoration-color (import-cost--get-decoration-color package-info)))
    (should (string-equal import-cost-large-package-color decoration-color))))

;;; import-cost-test.el ends here
