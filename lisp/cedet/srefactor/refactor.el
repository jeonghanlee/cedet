;;; srefactor/refactor.el --- Refatoring utilities
;;
;; Filename: srefactor/refactor.el
;; Description: Refactoring features for srefactor.
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
;; This package provides the core refactoring functions for srefactor.
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
(require 'srefactor/util)
(require 'srefactor/insert)
(require 'srefactor/tags)
(require 'srefactor/ui)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High level functions that select action to make
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun srefactor--refactor-based-on-tag-class (operation &optional file-option)
  "Refractor based on current tag in context.

OPERATION is a refactoring type user selected from the menu.
FILE-OPTION is a file destination associated with OPERATION."
  (let* ((refactor-tag (srefactor--copy-tag))
         (class (semantic-tag-class refactor-tag)))
    (cond
     ((eq class 'function)
      (cond
       ((eq operation 'extract-function)
        (srefactor--extract-region 'function))
       ((eq operation 'rename-local-var)
        (let* ((local-var (srefactor--local-var-at-point))
               prompt)
          (unwind-protect
              (condition-case nil
                  (progn
                    (srefactor--highlight-tag local-var refactor-tag 'match)
                    (setq prompt (format "Replace (%s) with: " (semantic-tag-name local-var)))
                    (srefactor--rename-local-var local-var
                                                 refactor-tag
                                                 (read-from-minibuffer prompt))
                    (srefactor--unhighlight-tag local-var))
                (error nil))
            (srefactor--unhighlight-tag local-var)
            (semantic-mode 1))
          (srefactor--unhighlight-tag local-var)
          ))
       (t
        (let ((other-file (srefactor--select-file file-option)))
          (srefactor--refactor-tag (srefactor--contextual-open-file other-file)
                                   refactor-tag
                                   operation
                                   t)))))
     ((eq class 'type)
      (cond
       ((eq operation 'gen-getters-setters)
        (srefactor-insert-class-getters-setters refactor-tag file-option)
        (message "Getter and setter generated."))
       ((eq operation 'move)
        (let ((other-file (srefactor--select-file file-option)))
          (srefactor--refactor-tag (srefactor--contextual-open-file other-file)
                                   refactor-tag
                                   operation
                                   t)))
       (t (srefactor--refactor-type (srefactor--contextual-open-file
                                     (srefactor--select-file file-option))
                                    refactor-tag))))
     ((eq class 'variable)
      (cond
       ((eq operation 'gen-getter-setter)
        (let ((buffer (srefactor--contextual-open-file (srefactor--select-file file-option))))
          (srefactor--variable-insert-getter-setter t t refactor-tag buffer))
        (message "Getter and setter generated."))
       ((eq operation 'gen-getter)
        (let ((buffer (srefactor--contextual-open-file (srefactor--select-file file-option))))
          (srefactor--variable-insert-getter-setter t nil refactor-tag buffer))
        (message "Getter generated."))
       ((eq operation 'gen-setter)
        (let ((buffer (srefactor--contextual-open-file (srefactor--select-file file-option))))
          (srefactor--variable-insert-getter-setter nil t refactor-tag buffer))
        (message "Setter generated."))
       ((eq operation 'move)
        (let ((other-file (srefactor--select-file file-option)))
          (srefactor--refactor-tag (srefactor--contextual-open-file other-file)
                                   refactor-tag
                                   operation
                                   t)))
       (t nil)))
     ((eq class 'package)
      (message "FIXME: 'package refactoring is not yet implemented."))
     ((eq class 'include)
      (message "FIXME: 'include refactoring is not yet implemented."))
     ((eq class 'label)
      (message "FIXME: 'label refactoring is not yet implemented."))
     (t))))

(defun srefactor--refactor-tag (buffer refactor-tag func-type &optional ask-place-p)
  "Refactor a tag.

BUFFER is a buffer from opening selected file chosen from the menu.

REFACTOR-TAG is selected tag to be refactored.

FUNC-TYPE is a refactoring action to be performed.

ASK-PLACE-P, if true, asks user to select a tag in BUFFER to insert next to it."
  (let (dest-tag
        (tag-list (nreverse (srefactor--fetch-candidates))))
    (setq srefactor-ui--func-type func-type)
    (with-current-buffer buffer
      (if (and ask-place-p tag-list)
          (progn
            (oset srefactor-ui--current-active-menu :items tag-list)
            (oset srefactor-ui--current-active-menu :action #'srefactor-ui--tag-action)
            (oset srefactor-ui--current-active-menu :shortcut-p nil)
            (oset srefactor-ui--current-active-menu :persistent-action 'srefactor--tag-persistent-action)
            (oset srefactor-ui--current-active-menu :post-handler
                  (lambda ()
                    (let ((tag (context srefactor-ui--current-active-menu))
                          tag-string)
                      (with-temp-buffer
                        (setq major-mode 'c++-mode)
                        (setq tag-string (semantic-format-tag-summarize tag nil nil)))
                      (search-forward-regexp (regexp-quote tag-string) (point-max) t)
                      (back-to-indentation))))
            (oset srefactor-ui--current-active-menu :keymap
                  (lambda ()
                    (define-key srefactor-ui-menu-mode-map "n" (lambda ()
                                                                 (interactive)
                                                                 (line-move-1 1 t)
                                                                 (srefactor--tag-persistent-action)))
                    (define-key srefactor-ui-menu-mode-map "p" (lambda ()
                                                                 (interactive)
                                                                 (line-move-1 -1 t)
                                                                 (srefactor--tag-persistent-action)))))
            (srefactor-ui-create-menu srefactor-ui--current-active-menu))
        (srefactor--insert-tag refactor-tag nil func-type)))))

(defun srefactor--refactor-type (dest-buffer refactor-tag)
  "Generate function implementations for all functions in a
class, including functions in nested classes.

DEST-BUFFER is the destination buffer to insert generated code.
REFACTOR-TAG is a Semantic tag that holds information of a C++ class."
  (let* ((members (semantic-tag-type-members refactor-tag))
         (dest-buffer-tags (with-current-buffer dest-buffer
                             (semantic-fetch-tags)))
         (diff (nreverse (set-difference members
                                         dest-buffer-tags
                                         :test #'semantic-equivalent-tag-p)))
         )
    (dolist (tag diff)
      (cond
       ((and (eq (semantic-tag-class tag) 'function)
             (semantic-tag-prototype-p tag))
        (srefactor--refactor-tag dest-buffer tag 'gen-func-impl))
       ((eq (semantic-tag-class tag) 'type)
        (srefactor--refactor-type dest-buffer tag))
       (t)))))


;;
;; VARIABLE
;;
(defun srefactor--rename-local-var (local-var-tag function-tag new-name)
  "Rename the name of a LOCAL-VAR-TAG in FUNCTION-TAG to NEW-NAME."
  (save-excursion
    (goto-char (semantic-tag-start function-tag))
    (let* ((distance (- (length new-name)
                        (length (semantic-tag-name local-var-tag))))
           (var-list (srefactor--collect-tag-occurrences local-var-tag function-tag))
           (var-list (loop for v in var-list
                           for i from 0 upto (1- (length var-list))
                           collect (if (consp v)
                                       (cons (+ (car v) (* 14 i)) (cdr v))
                                     (+ v (* distance i))))))
      (mapc (lambda (c)
              (goto-char c)
              (search-forward-regexp (srefactor--local-var-regexp local-var-tag)
                                     (semantic-tag-end function-tag)
                                     t)
              (replace-match new-name t t nil 1))
            var-list)
      (message (format "Renamed %d occurrences of %s to %s" (length var-list) (semantic-tag-name local-var-tag) new-name)))))

;;
;; Candidates
;;
(defun srefactor--fetch-candidates ()
  "Return a list of candidates in current buffer.

Each candidate is a list '(DISPLAY TAG OPTIONS).  This is a
wrapper for `srefactor--fetch-candidates-helper'.  See
`srefactor--fetch-candidates-helper' for more details."
  (srefactor--fetch-candidates-helper (semantic-fetch-tags) 0 nil))

(defun srefactor--fetch-candidates-helper (tags depth &optional class)
  "Return a list of lists '(DISPLAY TAG OPTIONS).

This function is intended to be used with `srefactor-ui-create-menu' to
be displayed as a list of menu items.

DISPLAY is the string to bepresented to user, TAG is a semantic
tag and OPTIONS is a list of possible choices for each menu item.

 TAGS are collection of Semantic tags in current buffer.
 DEPTH is current recursion depth.
 CLASS is the parent class."
  (let ((spaces (make-string (* depth 3) ?\s))
        (srefactor--tag-options (srefactor-ui--return-option-list 'tag))
        (dashes (make-string 1 ?\-))
        (class class)
        cur-type display tag-list)
    (cl-dolist (tag tags)
      (when (listp tag)
        (cl-case (setq cur-type (semantic-tag-class tag))
          ((function type)
           (let ((type-p (eq cur-type 'type)))
             (unless (and (> depth 0) (not type-p))
               (setq class nil))
             (setq display (concat (if (null class)
                                       spaces
                                     (format "%s|%s%s" spaces dashes "►"))
                                   (semantic-format-tag-summarize tag nil t)
                                   (if (eq cur-type 'type)
                                       " (Before)")))
             (and type-p
                  (setq class (car tag)))
             ;; Recurse to children
             (push (list display tag (if (eq cur-type 'type)
                                         srefactor--tag-options
                                       nil)) tag-list)
             (setq tag-list (append (srefactor--fetch-candidates-helper (semantic-tag-components tag)
                                                                        (1+ depth)
                                                                        class)
                                    tag-list))))

          ((package include label variable)
           (let* ((parent-tag (semantic-tag-calculate-parent tag))
                  (display (concat (if parent-tag
                                       (format "%s|%s%s" spaces dashes "►")
                                     spaces)
                                   (semantic-format-tag-summarize tag nil t))))
             (push (list display tag nil) tag-list)))
          ;; Catch-all
          (t))))
    tag-list))


(provide 'srefactor/refactor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; srefactor/refactor.el ends here
;; TODO : Note sure we should disable cl warnings here - Eric
;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "srefactor/refactor"
;; End:
