;;; import-cost.el --- Inline display of JavaScript import sizes. -*- lexical-binding: t -*-

;; Copyright (C) 2018 Madeleine Daly

;; Author: Madeleine Daly <madeleine.faye.daly@gmail.com>
;; Maintainer: Madeleine Daly <madeleine.faye.daly@gmail.com>
;; Created: <2018-04-08 21:28:52>
;; Last-Updated: <2018-04-25 21:38:30>
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1") (epc "0.1.1") (ov "1.0.6"))
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

(defconst import-cost--lang-typescript "typescript" "A parser string constant for TypeScript.")
(defconst import-cost--lang-javascript "javascript" "A parser string constant for JavaScript.")

(defun import-cost--regex-from-extensions (extensions)
  "Returns a regexp that matches all EXTENSIONS."
  (concat "\\(" (mapconcat 'identity extensions "\\|") "\\)"))

(defun import-cost--language (filename)
  "Returns the language that the parser should use when analyzing the buffer contents of FILENAME."
  (let ((typescript-regex (import-cost--regex-from-extensions import-cost-typescript-extensions))
        (javascript-regex (import-cost--regex-from-extensions import-cost-javascript-extensions)))
    (cond ((string-match-p typescript-regex filename) import-cost--lang-typescript)
          ((string-match-p javascript-regex filename) import-cost--lang-javascript))))

(defun import-cost--get (str-key alist)
  "Returns the cdr of the element in ALIST that has STR-KEY as its car."
  (cdr
   (seq-find
    (lambda (pair) (string-equal str-key (car pair)))
    alist)))

(defun import-cost--bytes-to-kilobytes (bytes)
  "Returns the size of BYTES in kilobytes."
  (/ bytes 1000))

(defun import-cost--get-decoration-color (size)
  "Returns the color that will be used to decorate the import size overlay."
  (let ((size-in-kb (import-cost--bytes-to-kilobytes size)))
    (cond ((< size-in-kb import-cost-small-package-size) import-cost-small-package-color)
          ((< size-in-kb import-cost-medium-package-size) import-cost-medium-package-color)
          (t import-cost-large-package-color))))

(defun import-cost--get-decoration-message (package-info)
  "Returns the string that will be used to decorate the line described by PACKAGE-INFO."
  (let ((size (format "%dKB" (import-cost--bytes-to-kilobytes (import-cost--get "size" package-info))))
        (gzip (format "%dKB" (import-cost--bytes-to-kilobytes (import-cost--get "gzip" package-info)))))
    (cond ((<= size 0) "")
          ((eq import-cost-bundle-size-decoration 'both) (format "%s (gzipped: %s)" size gzip))
          ((eq import-cost-bundle-size-decoration 'minified) size)
          ((eq import-cost-bundle-size-decoration 'gzipped) gzip))))

(defun import-cost--decorate (package-info)
  "Adds an overlay at the end of the line described by PACKAGE-INFO, and returns PACKAGE-INFO with
a new element added that has the form (\"decoration\" . overlay)."
  (let* ((search-string (import-cost--get "string" package-info))
         (message-string (import-cost--get-decoration-message package-info))
         (color (import-cost--get-decoration-color (import-cost--get "size" package-info)))
         (decoration (save-excursion
                       (search-forward search-string)
                       (let ((overlay (ov-create (point) (point)))
                             (after-string (propertize message-string 'font-lock-face '(:foreground color))))
                         (ov-set overlay 'after-string after-string)))))
    (push (cons "decoration" decoration) package-info)))

(defvar import-cost--decorations-list nil
  "A list of active import size decorations across buffers.")

(defvar import-cost--epc-server nil
  "A reference to the current EPC server instance.")

(defun import-cost--activate ()
  "Activates `import-cost-mode' in the current buffer, and instantiates a new EPC server if one is
not already running."
  (when (not import-cost--epc-server)
    (setq import-cost--epc-server (epc:start-epc "node" '("server.js")))))

(defun import-cost--deactivate (&optional filename)
  "Deactivates `import-cost-mode' in the current buffer.
If no other buffers are actively using this minor mode, the EPC server will be stopped and unlinked."
  (when filename
    (epc:call-sync 'disconnect filename)
    (setq import-cost--decorations-list
          (seq-filter
           (lambda (package-info) (eq filename (import-cost--get "fileName" package-info)))
           import-cost--decorations-list)))
  (when (and (null import-cost--decorations-list) import-cost--epc-server)
    (epc:stop-epc import-cost--epc-server)
    (setq import-cost--epc-server nil)))

(defun import-cost--buffer-string-no-properties ()
  "Returns the entire contents of the current buffer, without string properties."
  (save-restriction
    (widen)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun import-cost--process-active-buffer (&rest _)
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
          (dolist (package-info package-info-list import-cost--decorations-list)
            (push (import-cost--decorate package-info) import-cost--decorations-list))
          (describe-variable 'import-cost--decorations-list)))
      (deferred:error it
        (lambda (err)
          (error err))))))

(defcustom import-cost-lighter " $"
  "Lighter used in the mode-line while `import-cost-mode' is active."
  :type 'string
  :group 'import-cost)

;;;###autoload
(define-minor-mode import-cost-mode
  "Minor mode providing inline displays of JavaScript import sizes."
  :lighter import-cost-lighter
  :group 'import-cost)

(provide 'import-cost)

;;; import-cost.el ends here
