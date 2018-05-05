;;; test-helper.el --- Helpers for import-cost-test.el

(require 'seq)
(require 'f)

(defvar import-cost-test-path
  (f-dirname (f-this-file)))

(defvar import-cost-code-path
  (f-parent import-cost-test-path))

(require 'import-cost (f-expand "import-cost.el" import-cost-code-path))

;;; test-helper.el ends here
