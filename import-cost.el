;;; import-cost.el --- Display inline the sizes of imported JavaScript modules. -*- lexical-binding: t -*-

;; Copyright (C) 2018 Madeleine Daly

;; Author: Madeleine Daly <madeleine.faye.daly@gmail.com>
;; Version: 1.0
;; Package-Requires ((epc "0.1.1"))
;; URL: https://github.com/madeleinedaly/import-cost.el

;;; Commentary:

;;; Code:

;; TODO: can we do without ov.el and s.el?
(require 'epc)
(require 'ov)
(require 's)

;;;###autoload
(define-minor-mode import-cost-mode
  "Minor mode providing inline size displays for imported JavaScript modules."
  :lighter " $")

(defgroup import-cost nil
  "Minor mode providing inline size displays for imported JavaScript modules."
  :group 'tools
  :prefix "import-cost-"
  :link '(url-link :tag "Repository" "https://github.com/madeleinedaly/import-cost.el"))

(defcustom import-cost-small-package-size 50
  "Upper size limit, in KB, that will count a package as a small package."
  :group 'import-cost
  :type 'integer)

(defcustom import-cost-medium-package-size 100
  "Upper size limit, in KB, that will count a package as a medium package."
  :group 'import-cost
  :type 'integer)

;; TODO: update docstrings to specify that both strings and faces are valid types?
(defcustom import-cost-small-package-color "#7cc36e"
  "Decoration color for small packages."
  :group 'import-cost
  :type 'color)

(defcustom import-cost-medium-package-color "#7cc36e"
  "Decoration color for medium packages."
  :group 'import-cost
  :type 'color)

(defcustom import-cost-large-package-color "#d44e40"
  "Decoration color for large packages."
  :group 'import-cost
  :type 'color)

(defcustom import-cost-typescript-extensions '("\\.tsx?$")
  "File extensions to be parsed by the TypeScript parser."
  :group 'import-cost
  :type '(repeat string))

(defcustom import-cost-javascript-extensions '("\\.jsx?$")
  "File extensions to be parsed by the JavaScript parser."
  :group 'import-cost
  :type '(repeat string))

(defcustom import-cost-bundle-size-decoration 'both
  "Which bundle size to display."
  :group 'import-cost
  :type '(choice (const :tag "Both" both)
                 (const :tag "Minified" minified)
                 (const :tag "Gzipped" gzipped)))

(defcustom import-cost-show-calculating-decoration t
  "Whether to display the 'calculating' decoration."
  :group 'import-cost
  :type 'boolean)

(defconst import-cost--lang-typescript "typescript")
(defconst import-cost--lang-javascript "javascript")

(defun import-cost--regex-from-extensions (extensions)
  (s-concat "\\(" (s-join "\\|" extensions) "\\)"))

(defun import-cost--language (filename)
  (let ((typescript-regex (import-cost--regex-from-extensions import-cost-typescript-extensions))
        (javascript-regex (import-cost--regex-from-extensions import-cost-javascript-extensions)))
    (cond ((s-matches-p typescript-regex filename) import-cost--lang-typescript)
          ((s-matches-p javascript-regex filename) import-cost--lang-javascript))))

(defun import-cost--get (str-key alist)
  (cdr
   (seq-find
    (lambda (pair)
      (string-equal str-key (car pair)))
    alist)))

(defun import-cost--bytes-to-kilobytes (size)
  (/ size 1024))

(defun import-cost--get-decoration-color (size)
  (let ((size-in-kb (import-cost--bytes-to-kilobytes size)))
    (cond ((< size-in-kb import-cost-small-package-size) import-cost-small-package-color)
          ((< size-in-kb import-cost-medium-package-size) import-cost-medium-package-color)
          (t import-cost-large-package-color))))

(defun import-cost--get-decoration-message (package-info)
  (let ((size (format "%dKB" (import-cost--bytes-to-kilobytes (import-cost--get "size" package-info))))
        (gzip (format "%dKB" (import-cost--bytes-to-kilobytes (import-cost--get "gzip" package-info)))))
    (cond ((<= size 0) "")
          ((eq import-cost-bundle-size-decoration 'both) (format "%s (gzipped: %s)" size gzip))
          ((eq import-cost-bundle-size-decoration 'minified) size)
          ((eq import-cost-bundle-size-decoration 'gzipped) gzip))))

(defun import-cost--create-decoration (message color)
  (save-excursion
    (goto-char (point-min))
    (search-forward string)
    (let ((overlay (ov-create (point) (point))))
      (ov-set overlay 'after-string (propertize message 'font-lock-face '(:foreground color))))))

(defun import-cost--decorate (package-info)
  (let* ((message (import-cost--get-decoration-message package-info))
         (color (import-cost--get-decoration-color (import-cost--get "size" package-info)))
         (decoration (import-cost--create-decoration message color)))
    (add-to-list package-info (cons "decoration" decoration) t)))

(defvar import-cost--decorations-list nil)

(defvar import-cost--epc-server nil)

;; export function activate(context: ExtensionContext) {
;;   try {
;;     logger.init(context);
;;     logger.log('starting...');
;;     workspace.onDidChangeTextDocument(ev => processActiveFile(ev.document));
;;     window.onDidChangeActiveTextEditor(ev => ev && processActiveFile(ev.document));
;;     if (window.activeTextEditor) {
;;       processActiveFile(window.activeTextEditor.document);
;;     }
;;   } catch (e) {
;;     logger.log('wrapping error: ' + e);
;;   }
;; }
(defun import-cost--activate ()
  (when (not import-cost--epc-server)
    (setq import-cost--epc-server (epc:start-epc "node" '("server.js")))))

(defun import-cost--deactivate (&optional package-info)
  (when package-info
    (let ((filename (import-cost--get "fileName" package-info)))
      (epc:call-sync 'disconnect filename)
      (delq package-info import-cost--decorations-list)))
  (when (and (null import-cost--decorations-list) import-cost--epc-server)
    (epc:stop-epc import-cost--epc-server)
    (setq import-cost--epc-server nil)))

(defun import-cost--buffer-string-no-properties ()
  (save-restriction
    (widen)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun import-cost--process-active-buffer ()
  (let* ((filename (buffer-file-name))
         (contents (import-cost--buffer-string-no-properties))
         (language (import-cost--language filename))
         (args (list filename contents language)))
    (deferred:$
      (epc:call-deferred import-cost--epc-server 'calculate args)
      (deferred:nextc it
        (lambda (package-info-list)
          (dolist (package-info package-info-list import-cost--decorations-list)
            (push (import-cost--get "string" package-info) import-cost--decorations-list))
          (describe-variable 'import-cost--decorations-list)))
      (deferred:error it
        (lambda (err)
          (error err))))))

(provide 'import-cost)

;;; import-cost.el ends here
