;;; srefactor.el --- A refactoring tool based on Semantic parser framework
;;
;; Filename: srefactor.el
;; Description: A refactoring tool based on Semantic parser framework
;; Author: Tu, Do Hoang <tuhdo1710@gmail.com>
;; URL      : https://github.com/tuhdo/semantic-refactor
;; Maintainer: Tu, Do Hoang
;; Created: Wed Feb 11 21:25:51 2015 (+0700)
;; Version: 0.3
;; Package-Requires: ((emacs "24.4"))
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
;; Semantic is a package that provides a framework for writing
;; parsers. Parsing is a process of analyzing source code based on
;; programming language syntax. This package relies on Semantic for
;; analyzing source code and uses its results to perform smart code
;; refactoring that based on code structure of the analyzed language,
;; instead of plain text structure.
;;
;; To use this package, user only needs to use this single command:
;; `srefactor-refactor-at-point'
;;
;; This package includes the following features:
;;
;; - Context-sensitive menu: when user runs the command, a menu
;; appears and offer refactoring choices based on current scope of
;; semantic tag. For example, if the cursor is inside a class, the
;; menu lists choices such as generate function implementations for
;; the class, generate class getters/setters... Each menu item also
;; includes its own set of options, such as perform a refactoring
;; option in current file or other file.
;;
;; - Generate class implementation: From the header file, all function
;; prototypes of a class can be generated into corresponding empty
;; function implementation in a source file. The generated function
;; implementations also include all of their (nested) parents as
;; prefix in the names, if any. If the class is a template, then the
;; generated functions also includes all templates declarations and in
;; the parent prefix properly.
;;
;; - Generate function implementation: Since all function
;; implementations can be generated a class, this feature should be
;; present.
;;
;; - Generate function prototype: When the cursor is in a function
;; implementation, a function prototype can be generated and placed in
;; a selected file. When the prototype is moved into, its prefix is
;; stripped.
;;
;; - Convert function to function pointer: Any function can be
;; converted to a function pointer with typedef. The converted
;; function pointer can also be placed as a parameter of a function.
;; In this case, all the parameter names of the function pointer is
;; stripped.
;;
;; - Move semantic units: any meaningful tags recognized by Semantic
;; (class, function, variable, namespace...) can be moved relative to
;; other tags in current file or any other file.
;;
;; - Extract function: select a region and turn it into a function,
;; with relevant variables turned into function parameters and
;; preserve full type information.
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

;; TODO: Is this cheating the rule about no CL?  May need to change to cl-macs?
;;       -Eric
(with-no-warnings
  (require 'cl))
(require 'srefactor/refactor)
(require 'srefactor/util)
(require 'srefactor/tags)
(require 'srefactor/ui)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User options
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom srefactor--getter-prefix "get_"
  "Prefix for inserting getter."
  :group 'srefactor)

(defcustom srefactor--setter-prefix "set_"
  "Prefix for inserting getter."
  :group 'srefactor)

(defcustom srefactor--getter-setter-removal-prefix ""
  "Prefix for inserting getter."
  :group 'srefactor)

(defcustom srefactor--getter-setter-capitalize-p nil
  "Prefix for inserting getter."
  :group 'srefactor
  :type 'boolean)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State Variables used to track user actions
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar srefactor--current-local-var nil
  "Current local variable at point")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands - only one currently
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun srefactor-refactor-at-point ()
  "Offer contextual menu with actions based on current tag in scope.

Each menu item added returns a token for what type of refactoring
to perform."
  (interactive)
  (semantic-parse-changes-default)
  (let (menu-item-list
        (srefactor--file-options (srefactor-ui--return-option-list 'file))
        (tag (srefactor--copy-tag))
        (menu (srefactor-ui-menu "menu")))
    (setq srefactor--current-local-var (srefactor--menu-add-rename-local-p))
    (when (srefactor--menu-add-function-implementation-p tag)
      (add-to-list 'menu-item-list `("Generate Function Implementation (Other file)"
                                     gen-func-impl
                                     ,srefactor--file-options)))
    (when (srefactor--menu-add-function-proto-p tag)
      (add-to-list 'menu-item-list `("Generate Function Prototype (Other file)"
                                     gen-func-proto
                                     ,srefactor--file-options)))
    (when (srefactor--menu-add-function-pointer-p tag)
      (add-to-list 'menu-item-list `("Generate Function Pointer (Current file)"
                                     gen-func-ptr
                                     ,srefactor--file-options)))
    (when (srefactor--menu-add-getters-setters-p tag)
      (add-to-list 'menu-item-list `("Generate Getters and Setters (Current file)"
                                     gen-getters-setters
                                     ,srefactor--file-options)))
    (when (srefactor--menu-add-getter-setter-p tag)
      (add-to-list 'menu-item-list `("Generate Setter (Current file)"
                                     gen-setter
                                     ,srefactor--file-options))
      (add-to-list 'menu-item-list `("Generate Getter (Current file)"
                                     gen-getter
                                     ,srefactor--file-options))
      (add-to-list 'menu-item-list `("Generate Getter and Setter (Current file)"
                                     gen-getter-setter
                                     ,srefactor--file-options)))
    (when srefactor--current-local-var
      (setq tag srefactor--current-local-var)
      (add-to-list 'menu-item-list `("Rename local variable (Current file)"
                                     rename-local-var
                                     ("(Current file)"))))
    (when (srefactor--menu-add-move-p)
      (add-to-list 'menu-item-list `("Move (Current file)"
                                     move
                                     ,srefactor--file-options)))
    (when (region-active-p)
      (add-to-list 'menu-item-list `("Extract function (Current file)"
                                     extract-function
                                     nil)))
    (oset menu :items menu-item-list)
    (oset menu :action #'srefactor-ui--refactor-action)
    (oset menu :context tag)
    (oset menu :shortcut-p t)
    (srefactor-ui-create-menu menu)))


(provide 'srefactor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; srefactor.el ends here
;; TODO : Note sure we should disable cl warnings here - Eric
;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
