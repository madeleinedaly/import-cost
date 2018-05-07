;;; import-cost.el --- Minor mode providing inline displays of JavaScript import sizes -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Madeleine Daly

;; Author: Madeleine Daly <madeleine.faye.daly@gmail.com>
;; Maintainer: Madeleine Daly <madeleine.faye.daly@gmail.com>
;; Created: <2018-04-08 21:28:52>
;; Last-Updated: <2018-05-06 22:32:30>
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.4") (epc "0.1.1") (ov "1.0.6"))
;; Keywords: javascript js
;; URL: https://github.com/madeleinedaly/import-cost.el

;;; Commentary:

;; Minor mode providing inline displays of JavaScript import sizes.

;;; Code:

(require 'epc)
(require 'ov)

(defconst import-cost-version "1.0.0"
  "The import-cost package version.")

(defgroup import-cost nil
  "Minor mode providing inline displays of JavaScript import sizes."
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

(defvar import-cost-cache-filename (expand-file-name "import-cost.cache" user-emacs-directory)
  "Where import size decorations get persisted across sessions.")

(defvar import-cost--decorations-list nil
  "A list of active import size decorations across buffers.")

(defvar import-cost--epc-server nil
  "A reference to the current EPC server instance.")

(defconst import-cost--lang-typescript "typescript" "A parser string constant for TypeScript.")
(defconst import-cost--lang-javascript "javascript" "A parser string constant for JavaScript.")

(unless (fboundp 'alist-get)
  (defun alist-get (key alist &optional default remove)
    (ignore remove) ;; silence byte-compiler
    (let ((x (assq key alist)))
      (if x (cdr x) default))))

(defun import-cost--intern-car (cell)
  "Converts the car of CELL from a string to a symbol."
  (cons (intern (car cell)) (cdr cell)))

(defun import-cost--intern-keys (package-info)
  "Converts the key of each cons cell in PACKAGE-INFO from a string to a symbol."
  (mapcar 'import-cost--intern-car package-info))

(defun import-cost--find-package-info (cell)
  "Returns the subset of `import-cost--decorations-list' where each element contains a cons cell equal to CELL."
  nil)

(defun import-cost--bytes-to-kilobytes (bytes)
  "Returns BYTES in kilobytes."
  (/ bytes 1000.0))

(defun import-cost--regex-from-extensions (extensions)
  "Returns a regexp that matches all EXTENSIONS."
  (concat "\\(" (mapconcat 'identity extensions "\\|") "\\)"))

(defun import-cost--language (filename)
  "Returns the language that the parser should use when analyzing the buffer contents of FILENAME."
  (let ((typescript-regex (import-cost--regex-from-extensions import-cost-typescript-extensions))
        (javascript-regex (import-cost--regex-from-extensions import-cost-javascript-extensions)))
    (cond ((string-match-p typescript-regex filename) import-cost--lang-typescript)
          ((string-match-p javascript-regex filename) import-cost--lang-javascript))))

(defun import-cost--get-decoration-color (package-info)
  "Returns the color that will be used to decorate the import size overlay."
  (let ((size-in-kb (import-cost--bytes-to-kilobytes (alist-get 'size package-info))))
    (cond ((< size-in-kb import-cost-small-package-size) import-cost-small-package-color)
          ((< size-in-kb import-cost-medium-package-size) import-cost-medium-package-color)
          (t import-cost-large-package-color))))

(defun import-cost--get-decoration-message (package-info)
  "Returns the string that will be used to decorate the line described by PACKAGE-INFO."
  (let* ((size (import-cost--bytes-to-kilobytes (alist-get 'size package-info)))
         (gzip (import-cost--bytes-to-kilobytes (alist-get 'gzip package-info))))
    (cond ((<= size 0) "")
          ((eq import-cost-bundle-size-decoration 'both) (format " %dKB (gzipped: %dKB)" size gzip))
          ((eq import-cost-bundle-size-decoration 'minified) (format " %dKB" size))
          ((eq import-cost-bundle-size-decoration 'gzipped) (format " %dKB" gzip)))))

(defun import-cost--decorate! (package-info)
  "Adds an overlay at the end of the line described by PACKAGE-INFO."
  (let ((calc-error (alist-get 'error package-info)))
    (if calc-error
        (message "Error calculating import cost: %S" package-info)
      (let* ((line (alist-get 'line package-info))
             (message-string (import-cost--get-decoration-message package-info))
             (color (import-cost--get-decoration-color package-info)))
        (save-excursion
          (goto-char (point-min))
          (forward-line line)
          (goto-char (line-end-position))
          (let ((overlay (ov-create (point) (point)))
                (after-string (propertize message-string 'font-lock-face (cons 'foreground-color color))))
            ;; (ov-set overlay 'after-string after-string)
            ;; (push (cons 'decoration (point)) package-info)
            (push (cons 'decoration (ov-set overlay 'after-string after-string)) package-info)))))))

(defun import-cost--activate! ()
  "Activates `import-cost-mode' in the current buffer, and instantiates a new EPC server if one is
not already running."
  (when (not import-cost--epc-server)
    (setq import-cost--epc-server (epc:start-epc "node" '("server.js")))))

;; ;; FIXME: must handle multiples
;; (defun import-cost--undecorate! (filename)
;;   (let* ((package-info
;;           (seq-find
;;            (lambda (alist) (equal filename (alist-get 'filename alist)))
;;            import-cost--decorations-list))
;;          (decoration (alist-get 'decoration package-info))))
;;   (ov-reset decoration)
;;   (delq package-info import-cost--decorations-list))

(defun import-cost--deactivate! (&optional filename)
  "Deactivates `import-cost-mode' in the current buffer.
If no other buffers are actively using this minor mode, the EPC server will be stopped and unlinked."
  (when filename
    ;; (import-cost--undecorate! filename)
    ;; FIXME: TypeError: CreateListFromArrayLike called on non-object
    (epc:call-sync import-cost--epc-server 'disconnect (list filename)))
  (when (and (null import-cost--decorations-list) import-cost--epc-server)
    (epc:stop-epc import-cost--epc-server)
    (setq import-cost--epc-server nil)))

(defun import-cost--buffer-string-no-properties ()
  "Returns the entire contents of the current buffer, without string properties."
  (save-restriction
    (widen)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun import-cost--process-active-buffer! (&rest _)
  "Passes the entire contents of the current buffer to the EPC server for processing, and on
successful response adds import size overlays to the buffer."
  (let* ((filename (buffer-file-name))
         (contents (import-cost--buffer-string-no-properties))
         (language (import-cost--language filename))
         (args (list filename contents language)))
    (deferred:$
      (epc:call-deferred import-cost--epc-server 'calculate args)
      (deferred:nextc it
        (lambda (package-info-list)
          (setq import-cost--decorations-list (mapcar 'import-cost--intern-keys package-info-list))
          ;; (setq import-cost--decorations-list (seq-map 'import-cost--decorate! package-info-list))
          (describe-variable 'import-cost--decorations-list)))
      (deferred:error it 'error))))

(defcustom import-cost-lighter " $"
  "Lighter used in the mode-line while `import-cost-mode' is active."
  :type 'string
  :group 'import-cost)

;; FIXME: buffer handling
;; FIXME: caching (cf. `import-cost-cache-filename')
;; FIXME: use `after-save-hook'

;;;###autoload
(define-minor-mode import-cost-mode
  "Minor mode providing inline displays of JavaScript import sizes."
  :lighter import-cost-lighter
  :group 'import-cost)

(provide 'import-cost)

;;; import-cost.el ends here
