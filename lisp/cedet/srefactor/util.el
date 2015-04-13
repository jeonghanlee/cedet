;;; srefactor/util.el --- Utilities for srefactor
;;
;; Filename: srefactor/util.el
;; Description: Refactoring utilities for srefactor.
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
;; Utilities needed for SRefactor.
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
(require 'semantic)
(require 'semantic/senator)
(require 'srefactor/tags)

(defvar srefactor--current-local-var nil)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun srefactor--c-tag-start-with-comment (tag)
  (save-excursion
    (goto-char (semantic-tag-start tag))
    (if (eq (semantic-tag-class tag) 'function)
        (if (semantic-documentation-comment-preceeding-tag tag)
            (search-backward-regexp "/\\*")
          (goto-char (semantic-tag-end tag))
          (c-beginning-of-statement-1))
      (when (semantic-documentation-comment-preceeding-tag tag)
        (search-backward-regexp "/\\*")))
    (point)))

(defun srefactor--copy-tag ()
  "Take the current tag, and place it in the tag ring."
  (interactive)
  (semantic-fetch-tags)
  (let ((ft (semantic-obtain-foreign-tag)))
    (when ft
      (ring-insert senator-tag-ring ft)
      (semantic-tag-set-bounds ft
                               (srefactor--c-tag-start-with-comment ft)
                               (semantic-tag-end ft))
      (kill-ring-save (semantic-tag-start ft)
                      (semantic-tag-end ft))
      (when (called-interactively-p 'interactive)
        (message "Use C-y to yank text.  \
Use `senator-yank-tag' for prototype insert.")))
    ft))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File Selection
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun srefactor--select-file (option)
  "Select a file based on OPTION selected by a user."
  (let ((projectile-func-list '(projectile-get-other-files
                                projectile-current-project-files
                                projectile-project-root
                                projectile-find-file))
        other-files file l)
    (when  (and (featurep 'projectile)
                (cl-reduce (lambda (acc f)
                             (and (fboundp f) acc))
                           projectile-func-list
                           :initial-value t))
      (cond
       ((string-equal option "(Other file)")
        (setq other-files (projectile-get-other-files (buffer-file-name)
                                                      (projectile-current-project-files)
                                                      nil))
        (setq l (length other-files))
        (setq file (concat (projectile-project-root)
                           (cond ((> l 1)
                                  (completing-read "Select a file: "
                                                   other-files))
                                 ((= l 1)
                                  (car other-files))
                                 (t (projectile-find-file))))))
       ((and (string-equal option "(Project file)") (featurep 'projectile))
        (setq file (concat (projectile-project-root)
                           (completing-read "Select a file: "
                                            (projectile-current-project-files)))))
       ))

    (when (string-equal option "(Current file)")
      (setq file (buffer-file-name (current-buffer))))

    (when (string-equal option "(File)")
      (setq file (with-current-buffer (call-interactively 'find-file-other-window)
                   (buffer-file-name (current-buffer)))))
    file))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions - IO
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun srefactor--contextual-open-file (other-file)
  "If the current buffer is also the selected file, don't open
the file in another window but use the current buffer and window
instead.

OTHER-FILE is the selected file from the menu."
  (if other-file
      (cond
       ((srefactor--switch-to-window other-file)
        (current-buffer))
       ((equal other-file (buffer-file-name (current-buffer)))
        (find-file other-file))
       (t (find-file-other-window other-file)
          (current-buffer)))
    ;; use ff-find-other-file if no file is chosen When no file is
    ;; chosen, it means that user selected (Other file) option, but
    ;; does not install Projectile so he cannot use its function to
    ;; return the filename of other file. In this case, he simply gets
    ;; nil, which mean it's the job for `ff-find-other-file'. This needs
    ;; fixing in the future
    (ff-find-other-file t t)

    ;; `ff-find-other-file' does not return a buffer but switching to
    ;; the opened buffer instantly. We must return a buffer from this
    ;; function otherwise things go wrong
    (current-buffer)))

(defun srefactor--switch-to-window (file-path)
  "Switch to window that contains FILE-PATH string."
  (catch 'found
    (dolist (w (window-list))
      (when (equal file-path (buffer-file-name (window-buffer w)))
        (select-window w)
        (throw 'found "Found window.")))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions - Menu Predicates
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun srefactor--menu-add-function-proto-p (tag)
  "Check whether to add generate function prototype menu item for a TAG."
  (let ((class (semantic-tag-class tag)))
    (and (eq class 'function)
         (not (semantic-tag-prototype-p tag))
         (and (not (srefactor--tag-function-constructor tag))
              (not (srefactor--tag-function-destructor tag)))
         (not (region-active-p))
         (null srefactor--current-local-var) )))

(defun srefactor--menu-add-function-implementation-p (tag)
  "Check whether to add generate function implementation menu item for a TAG."
  (let ((class (semantic-tag-class tag)))
    (and (or (eq class 'type)
             (and (eq class 'function)
                  (semantic-tag-prototype-p tag)))
         (not (region-active-p))
         (null srefactor--current-local-var))))

(defun srefactor--menu-add-rename-local-p ()
  "Check whether to add rename menu item."
  (let ((local-var (srefactor--local-var-at-point)))
    (when (and local-var
               (eq (semantic-tag-class (semantic-current-tag)) 'function)
               (not (semantic-tag-prototype-p (semantic-current-tag)))
               (not (region-active-p)))
      local-var)))

(defun srefactor--menu-add-function-pointer-p (tag)
  "Check whether to add generate function pointer menu item for a TAG."
  (and (eq (semantic-tag-class tag) 'function)
       (not (semantic-tag-get-attribute tag :pointer))
       (and (not (srefactor--tag-function-constructor tag))
            (not (srefactor--tag-function-destructor tag)))
       (not (region-active-p))
       (null srefactor--current-local-var)))

(defun srefactor--menu-add-getters-setters-p (tag)
  "Check whether to add generate getters and setters menu item for a TAG."
  (and (eq (semantic-tag-class tag) 'type)
       (srefactor--tag-filter 'semantic-tag-class '(variable) (semantic-tag-type-members tag))
       (not (region-active-p))))

(defun srefactor--menu-add-getter-setter-p (tag)
  "Check whether to add generate getter and setter menu item for a TAG."
  (and (eq (semantic-tag-class tag) 'variable)
       (eq (semantic-tag-class (semantic-current-tag-parent)) 'type)
       (not (region-active-p))))

(defun srefactor--menu-add-move-p ()
  "Check whether to add move menu."
  (and (semantic-current-tag)
       (not (region-active-p))
       (semantic-equivalent-tag-p (srefactor--tag-at-point)
                                  (semantic-current-tag))))

(defun srefactor--activate-region (beg end)
  "Activate a region from BEG to END."
  (interactive)
  (goto-char beg)
  (set-mark-command nil)
  (goto-char end)
  (setq deactivate-mark nil))

(defun srefactor--menu-for-region-p ()
  "Check whether to add exclusive menu item for a region."
  (region-active-p))

;;; TODO This wasn't used anywhere.  Can it be deleted? - Eric

(defun srefactor--introduce-variable-at-point ()
  (save-excursion
    ;;
    (let ((var (save-excursion
                 (c-end-of-statement)
                 (semantic-ctxt-current-assignment)))
          var-string)
      (unless var
        (setq var (semantic-ctxt-current-symbol)))
      (setq var-string (read-from-minibuffer "New variable: " var))
      (goto-char (semantic-tag-end (car (last (semantic-get-all-local-variables)))))
      (newline-and-indent)
      (insert (concat var-string ";")))))

(provide 'srefactor/util)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; srefactor/refactor.el ends here
;; TODO : Note sure we should disable cl warnings here - Eric
;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "srefactor/refactor"
;; End:
