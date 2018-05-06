;;; import-cost-test.el --- Tests for import-cost.el

(ert-deftest import-cost-lang/js ()
  :tags '(utility languages)
  (should (string-equal import-cost--lang-javascript (import-cost--language "index.js"))))

(ert-deftest import-cost-lang/jsx ()
  :tags '(utility languages)
  (should (string-equal import-cost--lang-javascript (import-cost--language "index.jsx"))))

(ert-deftest import-cost-lang/ts ()
  :tags '(utility languages)
  (should (string-equal import-cost--lang-typescript (import-cost--language "index.ts"))))

(ert-deftest import-cost-lang/tsx ()
  :tags '(utility languages)
  (should (string-equal import-cost--lang-typescript (import-cost--language "index.tsx"))))

(ert-deftest import-cost-ui/message ()
  :tags '(utility ui)
  (let* ((import-cost-bundle-size-decoration 'both)
         (package-info '((size . 203890) (size . 36890)))
         (decoration-message (import-cost--get-decoration-message package-info)))
    (should (string-equal " 203KB (gzipped: 36KB)" decoration-message))))

(ert-deftest import-cost-ui/color ()
  :tags '(utility ui)
  (let* ((import-cost-large-package-color "#d44e40")
         (size (+ import-cost-medium-package-size (* 128 1024)))
         (package-info (list (cons 'size size)))
         (decoration-color (import-cost--get-decoration-color package-info)))
    (should (string-equal import-cost-large-package-color decoration-color))))

;;; import-cost-test.el ends here
