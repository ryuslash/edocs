;;; edocs.el --- Extract and format documentation from file(s)

;; Copyright (C) 2013  Tom Willemse

;; Author: Tom Willemse <tom@ryuslash.org>
;; Keywords: docs
;; Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Generate formatted description of a module.  Currently it makes a
;; simple HTML export of the Commentary and all the docstrings in a
;; file.  It is meant to be used as a batch operation, like so:

;; : emacs -batch -l edocs.el -f edocs-generate-batch file.el

;;; Code:

(require 'help-fns)
(require 'lisp-mnt)
(require 'ox-html)
(require 'package)

(defvar edocs-stylesheet-location "style.css"
  "Where to find the Cascading Style Sheet for the exported docs.")

(defvar edocs-generate-only-body nil
  "Whether to genereate only the body and no header/footer info.")

(defconst edocs--symbol-type-map
  #s(hash-table size 8 test equal
                data ("defclass" "Class"
                      "defconst" "Constant"
                      "defcustom" "Customization option"
                      "defgeneric" "Method"
                      "defgroup" "Customization group"
                      "define-minor-mode" "Minor mode"
                      "defun" "Function"
                      "defvar" "Variable"))
  "Type -> name map for symbol types.")

(defun edocs--list-symbols ()
  "Get a list of all symbols in the buffer.

The results also contain a specification of what was found for
each symbol, for example a `defun', `defvar' or `defcustom',
etc."
  (let (ls)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (rx (and bol ?\(
                       (group (or "defun" "defgroup" "defcustom" "defvar"
                                  "defclass" "defgeneric" "defconst"
                                  "define-minor-mode"))
                       " "
                       (group (1+ (not (any space ?\n ?\)))))))
              nil :noerror)
        (setq ls (cons (cons (buffer-substring-no-properties
                              (match-beginning 1) (match-end 1))
                             (buffer-substring-no-properties
                              (match-beginning 2) (match-end 2))) ls))))
    (reverse ls)))

(defun edocs--get-docs (type name)
  "Get docs of TYPE for symbol NAME."
  (let ((type (intern type))
        (obj (intern name)))
    (cond
     ((memq type '(defun define-minor-mode))
      (cons (format "%s" (or (help-function-arglist obj :preserve-names)
                             "()"))
            (documentation obj)))
     ((memq type '(defcustom defvar defconst defclass))
      (documentation-property obj 'variable-documentation))
     ((eql type 'defgroup)
       (documentation-property obj 'group-documentation))
     ((eql type 'defgeneric)
       (mapcar (lambda (itm)
                 (cons (format "%s" (cons (list (car (nth 2 itm))
                                                (car itm))
                                          (cdr (nth 2 itm))))
                       (nth 3 itm)))
               (aref (plist-get (symbol-plist obj)
                                'eieio-method-tree) 2))))))

(defun edocs--get-type-display (type-name)
  "Get the display text for TYPE-NAME."
  (gethash type-name edocs--symbol-type-map type-name))

(defun edocs--insert-header ()
  "Insert necessary header information into the current buffer."
  (insert "<!DOCTYPE html>\n"
          "<html><head>"
          "<link href=\"" edocs-stylesheet-location
          "\" rel=\"stylesheet\"></head><body>"))

(defun edocs--insert-footer ()
  "Insert necessary footer information into the current buffer."
  (insert "</body></html>"))

(defun edocs--insert-title (title sub)
  "Insert a formatted TITLE and SUB into the current buffer."
  (insert "<h1>" title " <small>&mdash; " sub "</small></h1>"))

(defmacro edocs--with-tag (tag attrs &rest contents)
  "Put insertion of TAG (possibly with ATTRS) around CONTENTS."
  (declare (indent 2))
  `(progn
     (insert "<" ,tag)
     (insert (mapconcat
              (lambda (itm) (format " %s=%S" (car itm) (cdr itm))) ,attrs ""))
     (insert ">")
     ,@contents
     (insert "</" ,tag ">")))

(defun edocs--format-text (txt)
  "Perform formatting operations on TXT."
  (let ((org-export-with-toc nil)
        (org-export-with-section-numbers nil))
    (org-export-string-as
     (replace-regexp-in-string "`\\([^']+\\)'" "~\\1~" txt)
     'html t)))

(defun edocs--format-commentary (cmt)
  "Perform special commentary formatting operations on CMT."
  (edocs--format-text
   (replace-regexp-in-string
    ";; " "" (replace-regexp-in-string
              ";;; Commentary:\n+" "" cmt))))

(defun edocs--format-doc (doc)
  "Perform formatting operations on DOC or on DOC's `cdr'."
  (edocs--format-text (if (consp doc) (cdr doc) doc)))

(defun edocs--package-desc-p (package-info)
  "Check to see if PACKAGE-INFO is a package-desc struct."
  (and (fboundp 'package-desc-p)
       (package-desc-p package-info)))

(defun edocs--module-name (package-info)
  "Extract the module name from PACKAGE-INFO.

The location of this information seems to have changed since
Emacs 24.3. If the function `package-desc-p' is bound and returns
t for PACKAGE-INFO, it is the new style and we should get it
accordingly.  Otherwise we assume we're dealing with an old-style
package description and return the first element."
  (if (edocs--package-desc-p package-info)
      (symbol-name (package-desc-name package-info))
    (aref package-info 0)))

(defun edocs--module-summary (package-info)
  "Extract a short description from PACKAGE-INFO.

See the docstring for `edocs--module-name' for more information."
  (if (edocs--package-desc-p package-info)
      (package-desc-summary package-info)
    (aref package-info 2)))

(defun edocs--normalize (docs)
  "Make sure DOCS is a properly formatted list."
  (if (or (not (listp docs))
          (not (listp (cdr docs))))
      (list docs)
    docs))

(defun edocs--format-symbol (symbol)
  "Format the information in SYMBOL."
  (let ((docs (edocs--get-docs (car symbol) (cdr symbol))))
    (mapc (lambda (doc)
            (edocs--with-tag "div" nil
              (insert "&ndash; ")
              (edocs--with-tag "strong" nil
                (insert (edocs--get-type-display (car symbol))))
              (insert ": ")
              (edocs--with-tag "tt" nil
                (insert (cdr symbol)))
              (insert " " (if (consp doc) (car doc) ""))
              (edocs--with-tag "div" '(("class" . "docstring"))
                (edocs--with-tag "p" nil
                  (insert (or (edocs--format-doc doc)
                              "Not documented."))))))
          (edocs--normalize docs))))

(defun edocs-generate ()
  "Generate nice-looking documentation for a module or file."
  (interactive)
  (let ((buffer (get-buffer-create "*edocs*"))
        (binfo (package-buffer-info))
        (commentary (lm-commentary))
        (symbols (edocs--list-symbols)))
    (with-current-buffer buffer
      (unless edocs-generate-only-body (edocs--insert-header))
      (edocs--with-tag "div" '(("class" . "container"))
        (edocs--insert-title (edocs--module-name binfo)
                             (edocs--module-summary binfo))
        (edocs--with-tag "p" nil
          (insert (edocs--format-commentary commentary)))
        (mapc #'edocs--format-symbol symbols))
      (unless edocs-generate-only-body
        (edocs--insert-footer)))
    (switch-to-buffer buffer)))

(defun edocs--generate-batch-1 (file)
  "Generate docs for FILE."
  (with-current-buffer (find-file file)
    (eval-buffer)
    (edocs-generate)
    (write-file (concat (file-name-sans-extension file) ".html"))))

(defun edocs-generate-batch ()
  "Generate module docs using batch operations."
  (mapc #'edocs--generate-batch-1 command-line-args-left))

(provide 'edocs)
;;; edocs.el ends here
