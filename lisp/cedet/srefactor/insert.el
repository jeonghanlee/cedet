;;; srefactor/insert.el --- Utilities for inserting code for srefactor
;;
;; Filename: srefactor/util.el
;; Description: Refactoring code gen tools for srefactor.
;; Author: Tu, Do Hoang <tuhdo1710@gmail.com
;; Maintainer: Tu, Do Hoang
;; Created: Wed Feb 11 21:25:51 2015 (+0700)
;; Version: 0.1
;; Package-Requires: ()
;; Last-Updated: Wed Feb 11 21:25:51 2015 (+0700)
;;           By: Tu, Do Hoang
;;     Update #: 1
;; URL:
;; Doc URL:
;; Keywords: c, languages, tools
;; Compatibility: GNU Emacs: 24.3+
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Utilities for inserting code for semantic refactor.
;;
;; Use the option `srefactor-use-srecode-p' to use srecode for tag
;; insertion vs hand written implementation.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:
(with-no-warnings
  (require 'cl))
(require 'cc-mode)
(require 'semantic)
(require 'semantic/tag)
(require 'srecode)
(require 'srecode/semantic)
(require 'srefactor/util)
(require 'srefactor/tags)


;; Declared in srefactor.el
(defvar srefactor--getter-prefix "get_")
(defvar srefactor--setter-prefix "set_")
(defvar srefactor--getter-setter-removal-prefix "")
(defvar srefactor--getter-setter-capitalize-p nil)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Developer Options
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar srefactor-use-srecode-p nil
  "Use experimental SRecode tag insertion ")


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insertion functions
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun srefactor--insert-tag (refactor-tag dest-tag insert-type &optional pos)
  "Insert a Semantic TAG to current buffer.

REFACTOR-TAG is selected Semantic tag to be refactored.

DEST-TAG is destination tag for refactored tag to be inserted
next to it. If nil, insert at the end of file.

POS is specific relative position to be inserted. POS is one of
the option \"Before|Inside|After\" that appears when a
destination tag can have its own members, such as a class or a
namespace.
"
  (let* ((parent-is-func-p (eq (semantic-tag-class (semantic-tag-calculate-parent dest-tag))
                               'function))
         (class (semantic-tag-class refactor-tag)))

    ;; if  refactor-tag dest-tag is nil, just insert at end of file
    (if dest-tag
        (progn
          (semantic-go-to-tag dest-tag)

          (if parent-is-func-p
              (srefactor--insert-function-as-parameter refactor-tag)

            ;; Handle selected position
            (cond
             ((string-equal pos "(Before)")
              (open-line 1))
             ((string-equal pos "(Inside)")
              (search-forward "{")
              (newline 1))
             (t (goto-char (semantic-tag-end dest-tag))
                (forward-line 1)))

            ;; handle insert type
            (cond
             ((eq insert-type 'gen-func-ptr)
              (srefactor--insert-function-pointer refactor-tag)
              (newline-and-indent)
              (recenter))
             ((eq insert-type 'gen-func-impl)
              (srefactor--insert-function-implementation refactor-tag))
             ((srefactor--tag-pointer refactor-tag)
              (semantic-insert-foreign-tag (srefactor--function-pointer-to-function refactor-tag)))
             ((eq insert-type 'move)
              (with-current-buffer (semantic-tag-buffer refactor-tag)
                (save-excursion
                  (goto-char (semantic-tag-start refactor-tag))
                  (delete-region (semantic-tag-start refactor-tag)
                                 (semantic-tag-end refactor-tag))
                  (delete-blank-lines)
                  )
                )
              (if (and (or (srefactor--tag-struct-p dest-tag)
                           (srefactor--tag-struct-p
                            (srefactor--calculate-parent-tag dest-tag)))
                       (eq class 'function)
                       (eq major-mode 'c-mode))
                  (progn
                    (insert (srefactor--function-to-function-pointer refactor-tag))
                    (insert ";"))
                (newline 1)
                (yank)
                (indent-according-to-mode))
              (newline-and-indent))
             (t (senator-yank-tag)))))
      (goto-char (point-max))
      (cond
       ((eq insert-type 'gen-func-ptr)
        (srefactor--insert-function-pointer refactor-tag))
       ((eq insert-type 'gen-func-impl)
        (srefactor--insert-function-implementation refactor-tag))
       ((semantic-tag-get-attribute refactor-tag :function-pointer)
        (semantic-insert-foreign-tag (srefactor--function-pointer-to-function refactor-tag)))
       (t (senator-yank-tag))))

    ;; indent after inserting refactor-tag
    (indent-according-to-mode)

    ;; post content insertion based on context
    (unless srefactor-use-srecode-p
      (unless parent-is-func-p
        (if (eq insert-type 'gen-func-impl)
            (progn
              (end-of-line)
              (insert " {")
              (newline 1)
              (save-excursion
                (srefactor--insert-initial-content-based-on-return-type
                 (if (or (srefactor--tag-function-constructor refactor-tag)
                         (srefactor--tag-function-destructor refactor-tag))
                     ""
                   (semantic-tag-type refactor-tag)))
                (insert "}")
                (srefactor--maybe-insert-function-end dest-tag insert-type)
                (indent-according-to-mode)
                (srefactor--indent-and-newline 1))
              (goto-char (line-end-position)))
          (srefactor--maybe-insert-function-end dest-tag insert-type))))
    ))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions that insert actual text or modify text
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; CLASS
;;
(defun srefactor-insert-class-getters-setters (tag file-option)
  "Insert getter-setter of a class TAG into file specified in FILE-OPTION."
  (semantic-fetch-tags-fast)
  (let ((tag (semantic-current-tag))
        (buffer (srefactor--contextual-open-file (srefactor--select-file file-option))))
    (when (eq (semantic-tag-class tag) 'type)
      (when (eq (semantic-tag-class tag) 'type)
        (let* ((members (srefactor--tag-filter 'semantic-tag-class
                                               '(variable label)
                                               (semantic-tag-type-members tag)))
               (variables (srefactor--tag-filter 'semantic-tag-class '(variable) members))
               (tag-start (semantic-tag-start tag)))
          (dolist (v variables)
            (when (srefactor--tag-private-p v)
              (srefactor--variable-insert-getter-setter t t v buffer)))
          (recenter))))))

(defun srefactor--insert-getter (tag &optional newline-before newline-after prototype-p)
  "Insert getter for TAG.
Add NEWLINE-BEFORE and NEWLINE-AFTER if t."
  (let ((tag-type (srefactor--tag-type-string tag))
        (tag-buffer (semantic-tag-buffer tag))
        (tag-parent-string "")
        tag-name beg)
    (setq beg (point))
    (unless (eq tag-buffer (current-buffer))
      (setq tag-parent-string (srefactor--tag-parents-string tag)))
    (when newline-before
      (newline newline-before))
    (when (and (or (listp (semantic-tag-type tag))
                   (semantic-tag-get-attribute tag :pointer))
               (not (semantic-tag-get-attribute tag :constant-flag)))
      (insert "const "))
    (insert tag-type)
    (setq tag-name (replace-regexp-in-string srefactor--getter-setter-removal-prefix
                                             ""
                                             (semantic-tag-name tag)))
    (insert (concat " "
                    tag-parent-string
                    srefactor--getter-prefix
                    (if srefactor--getter-setter-capitalize-p
                        (capitalize tag-name)
                      tag-name)))
    (insert "() const")
    (if prototype-p
        (insert ";")
      (insert " {")
      (srefactor--indent-and-newline 1)
      (insert (concat "return"
                      " "
                      (semantic-tag-name tag) ";"))
      (srefactor--indent-and-newline 1)
      (insert "}")
      (indent-according-to-mode)
      (when newline-after
        (newline newline-after)))
    (indent-region beg (point))))

(defun srefactor--insert-setter (tag newline-before newline-after &optional prototype-p)
  "Insert setter for TAG.
Add NEWLINE-BEFORE and NEWLINE-AFTER if t."
  (when newline-before
    (newline newline-before))
  (let ((tag-type (srefactor--tag-type-string tag))
        (tag-type (srefactor--tag-type-string tag))
        (tag-pointer (srefactor--tag-pointer tag))
        (tag-name (semantic-tag-name tag))
        (tag-type-string (srefactor--tag-type-string tag))
        (tag-buffer (semantic-tag-buffer tag))
        tag-parent-string modified-tag-name beg)
    (setq beg (point))
    (unless (eq tag-buffer (current-buffer))
      (setq tag-parent-string (srefactor--tag-parents-string tag)))
    (insert "void")
    (setq modified-tag-name (replace-regexp-in-string srefactor--getter-setter-removal-prefix
                                                      ""
                                                      (semantic-tag-name tag)))
    (insert (concat " "
                    tag-parent-string
                    srefactor--setter-prefix
                    (if srefactor--getter-setter-capitalize-p
                        (capitalize modified-tag-name)
                      modified-tag-name)))
    (insert (concat (insert "(")
                    (unless (semantic-tag-variable-constant-p tag)
                      "const ")
                    tag-type
                    (when (and (listp tag-type)
                               ;; (srefactor--tag-reference tag)
                               (not tag-pointer))
                      "&")
                    " "
                    tag-name
                    ")"))
    (if prototype-p
        (insert ";")
      (insert " {")
      (srefactor--indent-and-newline 1)
      (insert (concat "this->" tag-name " = " tag-name ";"))
      (srefactor--indent-and-newline 1)
      (insert "}")
      (indent-according-to-mode)
      (when newline-after
        (newline newline-after)))
    (indent-region beg (point))))

(defun srefactor--jump-or-insert-public-label (tag)
  "Check if TAG is a class or struct.
If so, check if any public label exists, jump to it.
Otherwise, insert one."
  (when (eq (semantic-tag-class tag) 'type)
    (goto-char (semantic-tag-start tag))
    (let* (label-pos
           (members (srefactor--tag-filter 'semantic-tag-class
                                           '(variable label)
                                           (semantic-tag-type-members tag)))
           (public-label (car (srefactor--tag-filter 'semantic-tag-name
                                                     '("public")
                                                     members))))
      (if public-label
          (progn
            (if (semantic-overlay-start (semantic-tag-overlay public-label))
                (progn
                  (goto-char (semantic-tag-end public-label))
                  (setq label-pos (semantic-tag-start public-label)))
              (search-forward "public:")
              (setq label-pos (point))))
        (goto-char (semantic-tag-end tag))
        (search-backward "}")
        (open-line 1)
        (insert "public:")
        (setq label-pos (point)))
      label-pos)))

(defun srefactor--variable-insert-getter-setter (insert-getter-p insert-setter-p tag buffer)
  "Insert getter if INSERT-GETTER-P is t, insert setter if INSERT-SETTER-P is t.
TAG is the current variable at point.
BUFFER is the destination buffer from file user selects from contextual menu."
  (with-current-buffer buffer
    (unless (srefactor--jump-or-insert-public-label (save-excursion
                                                      (goto-char (semantic-tag-start tag))
                                                      (semantic-current-tag-parent)))
      (goto-char (point-max)))
    (unless (eq buffer (semantic-tag-buffer tag))
      (with-current-buffer (semantic-tag-buffer tag)
        (srefactor--jump-or-insert-public-label (save-excursion
                                                  (goto-char (semantic-tag-start tag))
                                                  (semantic-current-tag-parent)))
        (when insert-getter-p (srefactor--insert-getter tag 1 1 t))
        (when insert-setter-p (srefactor--insert-setter tag 1 1 t))))
    (when insert-getter-p (srefactor--insert-getter tag 1 1))
    (when insert-setter-p (srefactor--insert-setter tag 1 1))))

;;
;; FUNCTION
;;
(defun srefactor--insert-function-implementation (func-tag)
  "Insert function implementations for FUNC-TAG at point, a tag that is a function."
  (forward-line 0)
  (open-line 1)
  (forward-line 1)
  (if srefactor-use-srecode-p
      ;; Try using SRecode as the mechanism for inserting a tag.
      (let* ((copy (semantic-tag-copy func-tag))
             ;; (parent (semantic-tag-calculate-parent func-tag))
             ;; TODO - below srefactor fcn should be a part of semantic or srecode.
             (parentstring1 (srefactor--tag-parents-string func-tag))
             (parentstring (substring parentstring1 0 (- (length parentstring1) 2)))
             (endofinsert nil))
        ;; Copied this line from original
        (semantic-tag-put-attribute func-tag :typemodifiers nil)
        (semantic-tag-put-attribute func-tag :parent parentstring)
        ;; Insert the tag
        (require 'srecode/semantic)
        ;; TODO - does it need any special dictionary entries?
        (setq endofinsert
              (srecode-semantic-insert-tag
               func-tag
               nil ;; Style
               (lambda (localtag)
                 (srefactor--insert-initial-content-based-on-return-type
                  (if (or (srefactor--tag-function-constructor copy)
                          (srefactor--tag-function-destructor copy))
                      ""
                    (semantic-tag-type copy)))
                 ) ;; Callbck for function body.
               ;; Dictionary entries go here.
               ))
        (goto-char endofinsert)
        (insert "\n\n"))
    (let ((func-tag-name (srefactor--tag-name func-tag))
          (parent (srefactor--calculate-parent-tag func-tag)))
      (when (srefactor--tag-function-modifiers func-tag)
        (semantic-tag-put-attribute func-tag :typemodifiers nil))
      (save-excursion
        (when (and (eq major-mode 'c++-mode) parent)
          (insert (srefactor--tag-templates-declaration-string parent)))
        (insert (srefactor--tag-function-string func-tag)))
      (unless (eq major-mode 'c-mode)
        (search-forward-regexp (regexp-quote func-tag-name) (line-end-position) t)
        (search-backward-regexp (regexp-quote func-tag-name) (line-beginning-position) t)

        (when (srefactor--tag-function-destructor func-tag)
          (forward-char -1))
        (unless (srefactor--tag-friend-p func-tag)
          (insert (srefactor--tag-parents-string func-tag)))
        (when (srefactor--tag-function-constructor func-tag)
          (let ((variables (srefactor--tag-filter #'semantic-tag-class
                                                  '(variable)
                                                  (semantic-tag-type-members parent))))
            (setq variables
                  (remove-if-not (lambda (v)
                                   (string-match "const" (srefactor--tag-type-string v)))
                                 variables))
            (when variables
              (goto-char (line-end-position))
              (insert ":")
              (mapc (lambda (v)
                      (when (string-match "const" (srefactor--tag-type-string v))
                        (insert (semantic-tag-name v))
                        (insert "()")))
                    variables))))))))

(defun srefactor--insert-function-pointer (tag)
  "Insert function pointer definition for TAG."
  (insert (concat "typedef "
                  (srefactor--tag-type-string tag)
                  " "
                  "("
                  (srefactor--tag-parents-string tag)
                  "*"
                  (semantic-tag-name tag)
                  ")"
                  "("))
  (mapc (lambda (tag)
          (let ((ptr-level (srefactor--tag-pointer tag))
                (ref-level (srefactor--tag-reference tag)))
            (insert (concat (srefactor--tag-type-string tag)
                            ", ") )))
        (semantic-tag-function-arguments tag))
  (search-backward ",")
  (replace-match "")
  (insert ");"))

(defun srefactor--insert-function-as-parameter (tag)
  "Insert TAG that is a function as a function parameter.
This means, the function is converted into a function pointer."
  (insert (srefactor--function-to-function-pointer tag))
  (insert ", "))

(defun srefactor--insert-new-function-from-region ()
  "Extract function from region."
  (semantic-force-refresh)
  (push-mark (region-beginning))
  (let ((reg-diff (- (region-end) (region-beginning)))
        (region (buffer-substring-no-properties (region-beginning) (region-end)))
        (tag (semantic-current-tag))
        (local-vars (semantic-get-all-local-variables))
        l orig p1 p2 name has-error)
    (unwind-protect
        (condition-case e
            (progn
              (setq orig (point))
              (setq region (with-temp-buffer
                             (let (p1 p2)
                               (insert (concat "void" " " "new_function"))
                               (insert "()")
                               (insert " {")
                               (newline 1)
                               (setq p1 (point))
                               (insert region)
                               (setq p2 (point))
                               (newline 1)
                               (insert "}")
                               (c-beginning-of-defun-1)
                               (search-forward "(" (point-max) t)
                               (dolist (v local-vars l)
                                 (when (srefactor--var-in-region-p v p1 p2)
                                   (push v l)))
                               (insert (srefactor--tag-function-parameters-string l))
                               (buffer-substring-no-properties (point-min) (point-max)))))
              (beginning-of-defun-raw)
              (recenter-top-bottom)
              (setq p1 (point))
              (insert region)
              (open-line 2)
              (setq p2 (point))
              (re-search-backward "new_function" nil t)
              (forward-char 1)
              (srefactor--mark-symbol-at-point)
              (setq name (read-from-minibuffer "Enter function name: "))
              (when (re-search-backward "new_function" nil t)
                (replace-match name))
              (indent-region (progn
                               (c-beginning-of-defun)
                               (point))
                             (progn
                               (c-end-of-defun)
                               (point))))
          (error "malform"
                 (setq has-error t)
                 (message "%s" "The selected region is malformed."))))
    (when has-error
      (unless (and (null p1) (null p2))
        (delete-region p1 p2))
      (kill-line 2)
      (goto-char orig)
      (pop-mark))
    (goto-char (car mark-ring))
    (delete-region (car mark-ring) (+ (car mark-ring) reg-diff))
    (insert name)
    (insert "(")
    (dolist (v l)
      (insert (concat (semantic-tag-name v) ", ")))
    (insert ");")
    (when (re-search-backward ", " nil t)
      (replace-match ""))
    (pop-mark)))

(defun srefactor--insert-initial-content-based-on-return-type (tag-type)
  "Insert initial content of function implementations.

TAG-TYPE is the return type such as int, long, float, double..."
  (cond
   ((listp tag-type)
    (insert (semantic-tag-name tag-type) " b;" )
    (indent-according-to-mode)
    (newline 2)
    (insert "return b;")
    (indent-according-to-mode))
   ((or (string-match "int" tag-type)
        (string-match "short" tag-type)
        (string-match "long" tag-type))
    (insert "return 0;"))
   ((or (string-match "double" tag-type)
        (string-match "float" tag-type))
    (insert "return 0.0;"))
   ((string-match "bool" tag-type)
    (insert "return true;"))
   ((string-match "char" tag-type)
    (insert "return 'a';"))
   (t))
  (srefactor--indent-and-newline 1)
  (forward-line 1))

;; TODO: work on this in next release
(defun srefactor--insert-new-macro-from-region ()
  "Assume region is marked."
  (let* ((region (buffer-substring (region-beginning) (region-end)))
         (beg (region-beginning))
         (end (region-end))
         (multiline-p (> (count-lines beg end) 1))
         (name (read-from-minibuffer "Enter a macro name: ")))
    (filter-buffer-substring beg end t)
    (insert (concat name "()"))
    (goto-char (semantic-tag-start (semantic-current-tag)))
    (search-backward-regexp "^$")
    (newline 1)
    (open-line 1)
    ;; (setq mark-active nil)
    (setq beg (point))
    (insert (concat "#define " name (if multiline-p "\n" " ")))
    (insert region)
    (forward-line 2)
    (setq end (point))
    (goto-char beg)
    (set-mark-command nil )
    (goto-char end)
    (setq deactivate-mark nil)
    (recenter)
    (when multiline-p
      (call-interactively 'c-backslash-region))
    (setq end (point))
    (indent-region beg end)
    (setq mark-active nil)))

(defun srefactor--maybe-insert-function-end (dest-tag function-insert-type)
  "Insert semicolon depend on the context of DEST-TAG and FUNCTION-INSERT-TYPE."
  ;; handle prototype insertion into a parent class
  (when (and (eq (semantic-tag-class (semantic-tag-calculate-parent dest-tag)) 'type)
             (not (eq function-insert-type 'move))
             (not (eq function-insert-type 'gen-func-ptr)))
    (insert ";")
    (newline-and-indent)))


(defun srefactor--extract-region (extract-type)
  "Extract region based on type.

EXTRACT-TYPE can be 'function or 'macro."
  (interactive)
  (if (region-active-p)
      (unwind-protect
          (progn
            ;; (narrow-to-region (region-beginning) (region-end))
            ;; (when (semantic-parse-region (region-beginning) (region-end))
            ;;   (error "Please select a region that is not a declaration or an implementation."))
            (save-excursion
              (narrow-to-region (region-beginning) (region-end))
              (c-beginning-of-defun)
              (c-end-of-defun))
            (widen)
            (cond
             ((eq extract-type 'function)
              (srefactor--insert-new-function-from-region))
             ((eq extract-type 'macro)
              (srefactor--insert-new-macro-from-region))
             (t)))
        (widen))
    (error "No active region.")))


;;
;; GENERAL
;;

(defun srefactor--indent-and-newline (&optional number)
  "Indent than insert a NUMBER of newline."
  (indent-according-to-mode)
  (newline (if number number 1)))

(provide 'srefactor/insert)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; srefactor/insert.el ends here
;; TODO : Note sure we should disable cl warnings here - Eric
;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "srefactor/insert"
;; End:
