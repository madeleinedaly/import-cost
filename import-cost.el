;;; import-cost.el --- Minor mode for displaying JavaScript module sizes inline. -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Madeleine Daly

;; Author: Madeleine Daly <madeleine.faye.daly@gmail.com>
;; Maintainer: Madeleine Daly <madeleine.faye.daly@gmail.com>
;; Created: <2018-04-08 21:28:52>
;; Last-Updated: <2018-05-12 22:41:10>
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.4") (epc "0.1.1") (ov "1.0.6"))
;; Keywords: javascript js
;; URL: https://github.com/madeleinedaly/import-cost

;;; Commentary:

;; Minor mode for displaying JavaScript module sizes inline.
;;
;; This is an Emacs port of the Import Cost plugin for Visual Studio Code.

;;; Code:

(require 'epc)
(require 'ov)

(defconst import-cost-version "1.0.0"
  "The import-cost package version.")

(defgroup import-cost nil
  "Minor mode for displaying JavaScript module sizes inline."
  :group 'tools
  :prefix "import-cost-"
  :link '(url-link :tag "Repository" "https://github.com/madeleinedaly/import-cost"))

(defcustom import-cost-small-package-size 50
  "Upper size limit, in KB, that will count a package as a small package."
  :group 'import-cost
  :type 'number)

(defcustom import-cost-medium-package-size 100
  "Upper size limit, in KB, that will count a package as a medium package."
  :group 'import-cost
  :type 'number)

(defcustom import-cost-small-package-color "#7cc36e"
  "Decoration color for small packages."
  :group 'import-cost
  :type '(choice color face))

(defcustom import-cost-medium-package-color "#ebbe3b"
  "Decoration color for medium packages."
  :group 'import-cost
  :type '(choice color face))

(defcustom import-cost-large-package-color "#d44e40"
  "Decoration color for large packages."
  :group 'import-cost
  :type '(choice color face))

(defcustom import-cost-typescript-extensions '("\\.tsx?$")
  "File extensions to be parsed by the TypeScript parser."
  :group 'import-cost
  :type '(repeat regexp))

(defcustom import-cost-javascript-extensions '("\\.jsx?$")
  "File extensions to be parsed by the JavaScript parser."
  :group 'import-cost
  :type '(repeat regexp))

(defcustom import-cost-bundle-size-decoration 'both
  "Which bundle size to display."
  :group 'import-cost
  :type '(choice (const :tag "Both" both)
                 (const :tag "Minified" minified)
                 (const :tag "Gzipped" gzipped)))

(defcustom import-cost-cache-path user-emacs-directory
  "The path to the file where import size decoration data gets persisted across sessions."
  :group 'import-cost
  :type 'string)

(defcustom import-cost-cache-filename "import-cost.cache"
  "The name of the file in which import size decoration data gets persisted across sessions."
  :group 'import-cost
  :type 'string)

(defcustom import-cost-lighter " $"
  "Lighter used in the mode-line while `import-cost-mode' is active."
  :type 'string
  :group 'import-cost)

(defvar import-cost--decorations-list nil
  "A list of active import size decorations across buffers.")

(defvar import-cost--epc-server nil
  "A reference to the current EPC server instance.")

(defconst import-cost--lang-typescript "typescript"
  "A parser string constant for TypeScript.")

(defconst import-cost--lang-javascript "javascript"
  "A parser string constant for JavaScript.")

(setf (symbol-function 'import-cost--alist-get)
      (if (fboundp 'alist-get)
          'alist-get
        (lambda (key alist &optional default remove)
          (ignore remove) ;; silence byte-compiler
          (let ((x (assq key alist)))
            (if x (cdr x) default)))))

(defun import-cost--filter (pred lst)
  "Returns the list of elements from LST that pass PRED."
  (delq nil (mapcar (lambda (elt) (and (funcall pred elt) elt)) lst)))

(defconst import-cost--package-directory
  (file-name-directory
   (file-truename
    (car
     (or
      ;; check the `load-path' first
      (import-cost--filter
       (lambda (path)
         (string-match-p "import-cost" path))
       load-path)
      ;; otherwise find the buffer directory in case of `eval-buffer' etc.
      (cl-loop for buffer being the buffers
               for buffer-file-path = (buffer-file-name buffer)
               when (and (stringp buffer-file-path)
                         (string-match-p "import-cost.el" buffer-file-path))
               collect buffer-file-path)))))
  "The path to the import-cost package directory.")

(defun import-cost--intern-car (cell)
  "Converts the car of CELL from a string to a symbol."
  (cons (intern (car cell)) (cdr cell)))

(defun import-cost--intern-keys (package-info)
  "Converts the key of each cons cell in PACKAGE-INFO from a string to a symbol."
  (mapcar 'import-cost--intern-car package-info))

(defun import-cost--find-package-infos (cell)
  "Returns a list elements from `import-cost--decorations-list' that each contain a cons cell equal to CELL."
  (cl-loop for package-info in import-cost--decorations-list
           when (member cell package-info)
           collect package-info))

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
  (let ((size-in-kb (import-cost--bytes-to-kilobytes (import-cost--alist-get 'size package-info))))
    (cond ((< size-in-kb import-cost-small-package-size) import-cost-small-package-color)
          ((< size-in-kb import-cost-medium-package-size) import-cost-medium-package-color)
          (t import-cost-large-package-color))))

(defun import-cost--get-decoration-message (package-info)
  "Returns the string that will be used to decorate the line described by PACKAGE-INFO."
  (let* ((size (import-cost--bytes-to-kilobytes (import-cost--alist-get 'size package-info)))
         (gzip (import-cost--bytes-to-kilobytes (import-cost--alist-get 'gzip package-info))))
    (cond ((<= size 0) "")
          ((eq import-cost-bundle-size-decoration 'both) (format " %dKB (gzipped: %dKB)" size gzip))
          ((eq import-cost-bundle-size-decoration 'minified) (format " %dKB" size))
          ((eq import-cost-bundle-size-decoration 'gzipped) (format " %dKB" gzip)))))

(defun import-cost--decorate! (package-info)
  "Adds an overlay at the end of the line described by PACKAGE-INFO."
  (let ((err (import-cost--alist-get 'error package-info)))
    (if err
        (message "Error calculating import cost: %S" package-info)
      (let* ((line (import-cost--alist-get 'line package-info))
             (message-string (import-cost--get-decoration-message package-info))
             (color (import-cost--get-decoration-color package-info))
             (buf (import-cost--alist-get 'buffer package-info)))
        (with-current-buffer (get-buffer buf)
          (save-excursion
            (goto-char (point-min))
            (forward-line line)
            (goto-char (line-end-position))
            (let* ((overlay (ov-create (point) (point)))
                   (after-string (propertize message-string 'font-lock-face (cons 'foreground-color color)))
                   (decoration (ov-set overlay 'after-string after-string)))
              (push (cons 'decoration decoration) package-info))))))))

(defun import-cost--activate! ()
  "Activates `import-cost-mode' in the current buffer, and instantiates a new EPC server if one is
not already running."
  (when (not import-cost--epc-server)
    (setq import-cost--epc-server (epc:start-epc "node" '("server.js")))))

;; ;; FIXME: must handle multiples
;; (defun import-cost--undecorate! (filename)
;;   (let* ((package-info
;;           (seq-find
;;            (lambda (alist) (equal filename (import-cost--alist-get 'filename alist)))
;;            import-cost--decorations-list))
;;          (decoration (import-cost--alist-get 'decoration package-info))))
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

(defun import-cost--is-valid (package-info)
  "Returns t if PACKAGE-INFO contains no errors, nil otherwise."
  (not (or (assq 'error package-info)
           (equal 0 (import-cost--alist-get 'size package-info))
           (equal 0 (import-cost--alist-get 'gzip package-info)))))

(defun import-cost--merge-buffer (buf package-info-list)
  "Add a new cons cell of the form (buffer . BUF) to each alist in PACKAGE-INFO-LIST."
  (mapcar (lambda (package-info) (push (cons 'buffer buf) package-info)) package-info-list))

(defun import-cost--process-active-buffer! (&rest _)
  "Passes the entire contents of the current buffer to the EPC server for processing, and on
successful response adds import size overlays to the buffer."
  (let* ((filename (buffer-file-name))
         (contents (import-cost--buffer-string-no-properties))
         (language (import-cost--language filename))
         (args (list filename contents language))
         (buf (current-buffer)))
    (deferred:$
      (epc:call-deferred import-cost--epc-server 'calculate args)
      (deferred:nextc it
        (lambda (package-info-list)
          (setq import-cost--decorations-list
                (let* ((package-infos (mapcar #'import-cost--intern-keys package-info-list))
                       (valid-package-infos (import-cost--filter #'import-cost--is-valid package-infos))
                       (buffer-package-infos (import-cost--merge-buffer buf valid-package-infos)))
                  (mapcar #'import-cost--decorate! buffer-package-infos)))
          ;; for debugging:
          (describe-variable 'import-cost--decorations-list)))
      (deferred:error it 'error))))

;;;###autoload
(define-minor-mode import-cost-mode
  "Minor mode for displaying JavaScript module sizes inline."
  :lighter import-cost-lighter
  :group 'import-cost)

(provide 'import-cost)

;;; import-cost.el ends here
