;;; srefactor/tags.el --- Utilities for manipulating semantic tags for srefactor
;;
;; Filename: srefactor/util.el
;; Description: Tag manipulation tools for srefactor.
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
;; Utilities for manipulating semantic tags for srefactor.
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
(require 'semantic/tag-ls)
(require 'semantic/analyze)
(require 'semantic/doc)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions that operate on a Semantic tag and return information
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun srefactor--get-all-parents (tag)
  "Return a list of parent tags of a TAG.
The closer to the end of the list, the higher the parents."
  (let* ((tag-buffer (semantic-tag-buffer tag))
         (parent (with-current-buffer (if tag-buffer
                                          tag-buffer
                                        (current-buffer))
                   (save-excursion
                     (goto-char (semantic-tag-start tag))
                     (semantic-current-tag-parent)))))
    (when parent
      (cons parent (srefactor--get-all-parents parent)))))

(defun srefactor--tag-parents-string (tag)
  "Return parent prefix string of a TAG.

It is used for prepending to function or variable name defined
outside of a scope."
  (let ((parents (srefactor--get-all-parents tag)))
    (if parents
        (concat (mapconcat (lambda (T)
                             (concat (semantic-tag-name T)
                                     (srefactor--tag-templates-parameters-string T)))
                           (nreverse parents) "::") "::")
      "")))

(defun srefactor--tag-function-parameters-string (members)
  "Return function parameter string of a function.

MEMBERS is a list of tags that are parameters of a function.  The
parameters are retrieved by the function `semantic-tag-function-arguments'.

The returned string is formatted as \"param1, param2, param3,...\"."
  (string-trim-right
   (mapconcat (lambda (m)
                (concat (srefactor--tag-type-string m)
                        " "
                        (semantic-tag-name m)
                        ))
              members
              ", ")))

(defun srefactor--tag-function-string (tag)
  "Return a complete string representation of a TAG that is a function."
  (let ((return-type (srefactor--tag-type-string tag))
        (members (semantic-tag-function-arguments tag)))
    (string-trim-left (concat return-type
                              " "
                              (when (srefactor--tag-function-destructor tag)
                                "~")
                              (srefactor--tag-name tag)
                              "("
                              (srefactor--tag-function-parameters-string members)
                              ")"))))

(defun srefactor--tag-template-string-list (tag)
  "Return a list of templates as a list of strings from a TAG."
  (let ((templates (semantic-tag-template tag)))
    (unless templates
      (setq templates (semantic-tag-template (srefactor--calculate-parent-tag tag))))
    (when templates
      (mapcar #'car templates))))

(defun srefactor--calculate-parent-tag (tag)
  "An alternative version of `semantic-tag-calculate-parent'.

It is the same except does not check if a TAG is in current
buffer.  If such check is performed, even if a TAG has parent, nil
is returned."
  (let ((tag-buffer (semantic-tag-buffer tag)))
    (with-current-buffer (if tag-buffer
                             tag-buffer
                           (current-buffer))
      (save-excursion
        (goto-char (semantic-tag-start tag))
        (semantic-current-tag-parent)))))

(defun srefactor--tag-templates-parameters-string (tag)
  "Return a string with all template parameters from a TAG.

The returned string is formatted as \"<class T1, class T2, ...>\"."
  (let ((tmpl-list (srefactor--tag-template-string-list tag)))
    (if tmpl-list
        (concat "<"
                (mapconcat #'identity tmpl-list ", ")
                ">")
      ""))
  )

(defun srefactor--tag-templates-declaration-string (tag)
  "Return a string with all template declarations from a TAG.

The returned string is formatted as:

\"template <class T1, class T2>\"
\"template <class T3>\"
\"....\"."
  (let* ((parent (condition-case nil
                     (srefactor--calculate-parent-tag tag)
                   (error nil)))
         (tmpl-list (srefactor--tag-template-string-list tag)))
    (if tmpl-list
        (concat (if parent
                    (srefactor--tag-templates-declaration-string parent)
                  "")
                (concat "template <"
                        (mapconcat (lambda (T)
                                     (concat "class " T))
                                   tmpl-list
                                   ", ")
                        ">"
                        "\n"))
      "")))

(defun srefactor--function-pointer-to-function (tag)
  "Convert a function pointer from a function TAG."
  (let* ((new-tag (semantic-tag-copy tag))
         (args (semantic-tag-function-arguments new-tag))
         (i 1))
    (mapc (lambda (arg)
            (semantic-tag-set-name arg (concat "a" (number-to-string i)))
            (setq i (+ i 1)))
          args)
    (semantic-tag-set-name new-tag (semantic-tag-name new-tag))
    (semantic--tag-put-property new-tag :foreign-flag t)
    (semantic-tag-put-attribute new-tag :function-pointer nil)
    new-tag))

(defun srefactor--function-to-function-pointer (tag)
  "Convert a function to function pointer from a TAG"
  (let* ((type-string (srefactor--tag-type-string tag))
         (tag-name (concat "(*" (semantic-tag-name tag) ")"))
         (args (semantic-tag-function-arguments tag)))
    (concat type-string
            " "
            tag-name
            " "
            "("
            (mapconcat (lambda (arg)
                         (srefactor--tag-type-string arg))
                       args
                       ", ")
            ")")))

(defun srefactor--tag-function-modifiers (tag)
  "Return `:typemodifiers' attribute of a TAG."
  (semantic-tag-get-attribute tag :typemodifiers))

(defun srefactor--tag-function-destructor (tag)
  "Return `:destructor-flag' attribute of a TAG, that is either t or nil."
  (semantic-tag-get-attribute tag :destructor-flag))

(defun srefactor--tag-function-constructor (tag)
  "Return `:constructor-flag' attribute of a TAG, that is either t or nil."
  (semantic-tag-get-attribute tag :constructor-flag))

(defun srefactor--local-var-regexp (tag)
  "Return regexp for seraching local variable TAG."
  (format (concat "\\(\\_\<%s\\)[ ]*\\([^[:alnum:]_"
                  (unless (srefactor--tag-lambda-p tag) "(")
                  "]\\)")
          (regexp-quote (semantic-tag-name tag))))

(defun srefactor--tag-pointer (tag)
  "Return `:pointer' attribute of a TAG."
  (semantic-tag-get-attribute tag :pointer))

(defun srefactor--tag-typedef (tag)
  "Return `:typedef' attribute of a TAG."
  (semantic-tag-get-attribute tag :typedef))

(defun srefactor--tag-reference (tag)
  "Return `:reference' attribute of a TAG.

If it does not exist, perform additional check to make sure it
does not, since the actual text in buffer has it but for some
complicated language construct, Semantic cannot retrieve it."
  (let ((reference (semantic-tag-get-attribute tag :reference))
        (tag-buffer (semantic-tag-buffer tag)))
    (if reference
        reference
      (save-excursion
        (with-current-buffer (if tag-buffer
                                 tag-buffer
                               ;; only tag in current buffer does not
                               ;; carry buffer information
                               (current-buffer))
          (goto-char (semantic-tag-start tag))
          (re-search-forward (concat ".*&[ ]+.*" (regexp-quote (semantic-tag-name tag)))
                             (semantic-tag-end tag)
                             t))))))

(defun srefactor--tag-name (tag)
  "Return TAG name and handle edge cases."
  (let ((tag-name (semantic-tag-name tag)))
    (with-current-buffer (semantic-tag-buffer tag)
      (if (not (string-empty-p tag-name))
          (if (semantic-tag-get-attribute tag :operator-flag)
              (concat "operator " tag-name)
            tag-name)
        ""))))

(defun srefactor--tag-type-string (tag)
  "Return a complete return type of a TAG as string."
  (let* ((ptr-level (srefactor--tag-pointer tag))
         (ref-level (srefactor--tag-reference tag))
         (tag-type (semantic-tag-type tag))
         (const-p (semantic-tag-variable-constant-p tag))
         (template-specifier (when (semantic-tag-p tag-type)
                               (semantic-tag-template-specifier tag-type))))
    (cond
     ((semantic-tag-function-constructor-p tag)
      "")
     (template-specifier
      (replace-regexp-in-string ",>" ">"
                                (concat (when (semantic-tag-variable-constant-p tag)
                                          "const ")
                                        (when (srefactor--tag-struct-p tag)
                                          "struct ")
                                        (car (semantic-tag-type tag))
                                        "<"
                                        (srefactor--tag-type-string-inner-template-list template-specifier)
                                        ">"
                                        (cond
                                         (ptr-level
                                          (make-string ptr-level ?\*))
                                         (ref-level
                                          (make-string ref-level ?\&))
                                         (t ""))))
      )
     (t
      (if (listp tag-type)
          (concat (when const-p
                    "const ")
                  (when (srefactor--tag-struct-p tag)
                    "struct ")
                  (car tag-type)
                  (when (srefactor--tag-reference tag)
                    " &"))
        tag-type)))))

(defun srefactor--tag-type-string-inner-template-list (tmpl-spec-list)
  (mapconcat (lambda (tmpl)
               (let* ((templates (semantic-tag-template-specifier tmpl)))
                 (concat (if (listp tmpl)
                             (car tmpl)
                           tmpl)
                         (if (and (not (null templates)) (listp templates))
                             (concat "<"  (srefactor--tag-type-string-inner-template-list templates)) ",")
                         (when templates "> "))))
             tmpl-spec-list
             ""))

(defun srefactor--mark-symbol-at-point ()
  "Activate mark for a symbol at point."
  (interactive)
  (forward-sexp -1)
  (set-mark-command nil)
  (forward-sexp 1)
  (setq deactivate-mark nil))

;;; PREDICATES
;;
;; I copied the below two from a list of predicates, but they seemed
;; like they belonged with tag based functions
;; - Eric


(defun srefactor--tag-at-point ()
  "Retrieve current tag at point."
  (let* ((ctxt (semantic-analyze-current-context (point)))
	 (pf (when ctxt
	       ;; The CTXT is an EIEIO object.  The below
	       ;; method will attempt to pick the most interesting
	       ;; tag associated with the current context.
	       (semantic-analyze-interesting-tag ctxt)))
         )
    pf))

(defun srefactor--local-var-at-point ()
  "Retrieve current variable tag at piont."
  (let* ((ctxt (semantic-analyze-current-context (point)))
	 (pf (when ctxt
	       ;; The CTXT is an EIEIO object.  The below
	       ;; method will attempt to pick the most interesting
	       ;; tag associated with the current context.
	       (semantic-analyze-interesting-tag ctxt)))
         )
    (condition-case nil
        (catch 'found
          (mapc (lambda (var)
                  (when (or (semantic-equivalent-tag-p var pf)
                            (string-equal (semantic-tag-name var)
                                          (semantic-tag-name pf)))
                    (throw 'found var)))
                (semantic-get-all-local-variables))
          nil)
      (error nil))))

(defmacro srefactor--tag-filter (predicate tag-classes-or-names tags)
  "Filter TAGS based on PREDICATE that satisfies TAG-CLASSES-OR-NAMES.

TAG-CLASSES-OR-NAMES can be a list of Semantic tag classes, or a
list of Semantic tag names, but not both.

Based on the type of list passed above, either use
`semantic-tag-class' or `semantic-tag-name' as PREDICATE."
  `(let (l)
     (dolist (tag ,tags l)
       (when (member (funcall ,predicate tag) ,tag-classes-or-names)
         (setq l (cons tag l))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions - Predicates
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun srefactor--var-in-region-p (tag beg end)
  "Check if a local variable TAG is in a region from BEG to END."
  (save-excursion
    (goto-char beg)
    (search-forward-regexp (srefactor--local-var-regexp tag)
                           end t)))


(defun srefactor--tag-struct-p (tag)
  "Check if TAG is a C struct."
  (condition-case nil
      (let* ((type-tag (semantic-tag-type tag))
             (typedef-tag (srefactor--tag-typedef tag))
             type-type-tag struct-p)
        (when typedef-tag
          (setq struct-p (semantic-tag-type typedef-tag)))
        (unless struct-p
          (setq type-type-tag (semantic-tag-type type-tag))
          (setq struct-p (and (stringp type-type-tag)
                              (string-equal type-type-tag "struct"))))
        struct-p)
    (error nil)))

(defun srefactor--tag-private-p (tag)
  "Check whether a TAG is a private variable."
  (let* ((members (srefactor--tag-filter 'semantic-tag-class
                                         '(variable label)
                                         (semantic-tag-type-members (semantic-tag-calculate-parent tag))))
         (labels (srefactor--tag-filter 'semantic-tag-class
                                        '(label)
                                        members))
         (public-label (car (srefactor--tag-filter 'semantic-tag-name
                                                   '("public")
                                                   labels)))
         (private-label (car (srefactor--tag-filter 'semantic-tag-name
                                                    '("private")
                                                    labels)))
         (tag-start (semantic-tag-start tag))
         (private-pos (semantic-tag-start private-label))
         (public-pos (semantic-tag-start public-label)))
    (or (and private-label (> tag-start private-pos)
             public-label (< tag-start public-pos))
        (and public-label (> tag-start public-pos)
             private-label (> tag-start private-pos)
             (> private-pos public-pos)))))

(defun srefactor--tag-auto-p (tag)
  "Check whether a TAG is an auto variable."
  (let ((type (semantic-tag-type tag)))
    (and (listp type)
         (string-equal "auto" (car type)))))

(defun srefactor--tag-lambda-p (tag)
  "Check whether TAG is a lambda function."
  (condition-case nil
      (save-excursion
        (goto-char (semantic-tag-start tag))
        (and (srefactor--tag-auto-p tag)
             (search-forward-regexp "=[ ]*\\[.*\\][ ]*(.*)[ ]*"  (semantic-tag-end tag) t)))
    (error nil)))

(defun srefactor--tag-friend-p (tag)
  "Check whether a TAG is a friend to everyone."
  (condition-case nil
      (let ((tag-start (semantic-tag-start tag))
            (tag-end (semantic-tag-end tag))
            (tag-buffer (semantic-tag-buffer tag)))
        (with-current-buffer tag-buffer
          (save-excursion
            (goto-char tag-start)
            (search-forward-regexp "friend" tag-end t))))
    (error nil)))

(defun srefactor--unknown-symbol-at-point-p ()
  "Check whether a symbol at point is an unknown variable."
  (unless (and (semantic-ctxt-current-symbol)
               (srefactor--local-var-at-point))
    t))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions - Utilities
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun srefactor--collect-tag-occurrences (tag &optional parent-tag)
  "Collect all TAG occurrences.
PARENT-TAG is the tag that contains TAG, such as a function or a class or a namespace."
  (save-excursion
    (let ((matching-positions
           (srefactor--collect-var-positions tag
                                             (if parent-tag
                                                 (semantic-tag-start parent-tag)
                                               (point-min))
                                             (if parent-tag
                                                 (semantic-tag-end parent-tag)
                                               (point-max))
                                             nil))
          (parent-start (if parent-tag
                            (semantic-tag-start parent-tag)
                          (point-min)))
          (parent-end (if parent-tag
                          (semantic-tag-end parent-tag)
                        (point-max)))
          positions)
      (save-excursion
        (dolist (p matching-positions)
          (when (<= p parent-start)
            (delete p matching-positions))))

      (dolist (pos matching-positions positions)
        (goto-char pos)
        (when (or (semantic-equivalent-tag-p tag (srefactor--local-var-at-point))
                  (semantic-equivalent-tag-p tag (semantic-current-tag)))
          (push pos positions)))))
  )

(defun srefactor--collect-var-positions (local-var-tag &optional beg end with-content)
  "Return all lines that LOCAL-VAR-TAG occurs in FUNCTION-TAG.
If WITH-CONTENT is nil, returns a list of line numbers.  If
WITH-CONTENT is t, returns a list of pairs, in which each element
is a cons of a line and the content of that line."
  (save-excursion
    (goto-char (if beg beg (point-min)))
    (let ((local-var-regexp (srefactor--local-var-regexp local-var-tag))
          p lines)
      (while (re-search-forward local-var-regexp end t)
        (setq p (match-beginning 0))
        (push  (if with-content
                   (cons p (buffer-substring-no-properties (point)
                                                           (line-end-position)))
                 p)
               lines))
      lines)))

(defun srefactor--highlight-tag (tag &optional scope-tag face)
  "Highlight TAG in SCOPE-TAG with FACE."
  (let ((positions (srefactor--collect-tag-occurrences tag scope-tag))
        beg end)
    (mapc (lambda (p)
            (save-excursion
              (goto-char p)

              (search-forward-regexp (srefactor--local-var-regexp tag)
                                     (if scope-tag
                                         (semantic-tag-end scope-tag)
                                       (point-max))
                                     t)

              ;; if so, go back to the beginning
              (search-backward-regexp (srefactor--local-var-regexp tag)
                                      (if scope-tag
                                          (semantic-tag-start scope-tag)
                                        (point-min))
                                      t)
              (setq beg (point))
              (forward-sexp 1)
              (setq end (point))

              (let ((overlay (make-overlay beg end)))
                (overlay-put overlay 'srefactor-overlay t)
                (overlay-put overlay 'face 'match))))
          positions)))

(defun srefactor--unhighlight-tag (tag)
  "Unhighlight TAG."
  (remove-overlays))

(provide 'srefactor/tags)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; srefactor/tags.el ends here
;; TODO : Note sure we should disable cl warnings here - Eric
;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "srefactor/tags"
;; End:
