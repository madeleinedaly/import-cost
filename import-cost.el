;;; import-cost.el --- Minor mode for displaying JavaScript module sizes inline. -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Madeleine Daly

;; Author: Madeleine Daly <madeleine.faye.daly@gmail.com>
;; Maintainer: Madeleine Daly <madeleine.faye.daly@gmail.com>
;; Created: <2018-04-08 21:28:52>
;; Last-Updated: <2018-05-21 23:10:21>
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

(defcustom import-cost-lighter " ₵"
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
          (ignore remove) ; silence byte compiler
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

(defun import-cost--members-p (cells alist)
  "Returns t if all CELLS are members of ALIST, nil otherwise."
  (= (length cells)
     (length (cl-loop for cell in cells
                      when (member cell alist)
                      collect t into results
                      return results))))

(defun import-cost--find (cells list)
  "Returns a list elements (alists) from LIST that each contain cons cells equal to CELLS."
  (car (cl-loop for package-info in list
                when (import-cost--members-p cells package-info)
                collect package-info)))

(defun import-cost--find-file-buffer (filename)
  "Returns the buffer that is visiting FILENAME if it exists."
  (cl-loop for buffer being the buffers
           when (equal filename (buffer-file-name buffer))
           return buffer))

;; FIXME: see unit test
(defun import-cost--bytes-to-kilobytes (bytes)
  "Returns BYTES in kilobytes."
  (/ bytes 1000.0))

(defun import-cost--or-regexp-from-strings (strings)
  (mapconcat 'identity strings "\\|"))

(defun import-cost--regexp-from-extensions (extensions)
  "Returns a regexp that matches all EXTENSIONS."
  (concat "\\(" (import-cost--or-regexp-from-strings extensions) "\\)"))

(defun import-cost--language (filename)
  "Returns the language that the parser should use when analyzing the buffer contents of FILENAME."
  (let ((typescript-regexp (import-cost--regexp-from-extensions import-cost-typescript-extensions))
        (javascript-regexp (import-cost--regexp-from-extensions import-cost-javascript-extensions)))
    (cond ((string-match-p typescript-regexp filename) import-cost--lang-typescript)
          ((string-match-p javascript-regexp filename) import-cost--lang-javascript))))

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

(defun import-cost--strip-quotes (str)
  "Returns STR stripped of double-quotes."
  (replace-regexp-in-string (regexp-quote "\"") "" str t t))

(defun import-cost--decorate! (package-info)
  "Adds an overlay at the end of the line described by PACKAGE-INFO."
  (let* ((filename (import-cost--alist-get 'filename package-info))
         (line (import-cost--alist-get 'line package-info))
         (buffer (import-cost--find-file-buffer filename))
         (message-string (import-cost--get-decoration-message package-info))
         (color (import-cost--get-decoration-color package-info)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (forward-line line)
        (goto-char (line-end-position))
        (let* ((overlay (ov-create (point) (point)))
               (after-string (propertize message-string 'font-lock-face (cons 'foreground-color color)))
               (decoration (ov-set overlay 'after-string after-string)))
          (push (cons 'decoration (point)) package-info)
          (push (cons 'buffer (import-cost--strip-quotes (buffer-name buffer))) package-info))))))

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

(defun import-cost--valid-p (package-info)
  "Returns t if PACKAGE-INFO contains no errors, nil otherwise."
  (if (not (or (assq 'error package-info)
               (equal 0 (import-cost--alist-get 'size package-info))
               (equal 0 (import-cost--alist-get 'gzip package-info))))
      t
    (let ((package-name (import-cost--alist-get 'name package-info)))
      (message "Error calculating import cost for %S: %S" package-name package-info)
      nil)))

(defun import-cost--process-active-buffer! ()
  "Passes the entire contents of the current buffer to the EPC server for processing, and on
successful response adds import size overlays to the buffer."
  (let* ((buffer (current-buffer))
         (filename (buffer-file-name))
         (contents (import-cost--buffer-string-no-properties))
         (language (import-cost--language filename))
         (args (list filename contents language)))
    (deferred:$
      (epc:call-deferred import-cost--epc-server 'calculate args)
      (deferred:nextc it
        (lambda (package-info-list)
          (setq import-cost--decorations-list
                (let* ((package-infos (mapcar #'import-cost--intern-keys package-info-list))
                       (valid-package-infos (import-cost--filter #'import-cost--valid-p package-infos)))
                  (with-current-buffer buffer
                    (ov-clear (point-min) (point-max)))
                  (mapcar #'import-cost--decorate! valid-package-infos)))))
      (deferred:error it 'error))))

(defun import-cost--line-numbers-in-region (beg end)
  "Returns a list of the line numbers that are in the region between BEG and END."
  (save-excursion
    (goto-char beg)
    (let ((line-numbers '()))
      (while (not (= end (point)))
        (push (line-number-at-pos) line-numbers)
        (forward-line 1))
      line-numbers)))

(defconst import-cost--javascript-module-loader-keywords
  (import-cost--or-regexp-from-strings '("import" "require"))
  "A regexp to search for when naively checking the modified region for relevant changes on after-change.
AMD loaders like curl, define, etc. and are not supported.")

(defun import-cost-after-change-function (beg end len)
  (let ((modified-lines (if (and (= beg end) (= len 1))
                            (let ((line-deleted (save-excursion
                                                  (goto-char beg)
                                                  (line-number-at-pos))))
                              (list line-deleted))
                          (import-cost--line-numbers-in-region beg end))))
    (message "beg: %d, end: %d, len: %d" beg end len)
    (message "modified lines: %S" modified-lines)))

;;;###autoload
(define-minor-mode import-cost-mode
  "Minor mode for displaying JavaScript module sizes inline."
  :lighter import-cost-lighter
  :group 'import-cost
  (if import-cost-mode
      (add-hook 'after-change-functions #'import-cost-after-change-function)
    (remove-hook 'after-change-functions #'import-cost-after-change-function)))

(provide 'import-cost)

;;; import-cost.el ends here
