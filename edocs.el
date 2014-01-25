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

;; Each paragraph of text is exported using `org-mode', currently
;; requiring at least org version 8.

;; There is `edocs-generate' which can be used to generate a buffer of
;; the HTML output of the module it's run in interactively, but it is
;; not meant as the main entry-point for edocs.  The function
;; `edocs-generate-batch' is the main entry-point of edocs, it is
;; supposed to be run as a batch procedure as shown above.

;;; Code:

(require 'help-fns)
(require 'lisp-mnt)
(require 'ox-html)
(require 'package)

(eval-when-compile
  (require 'cl-macs))

(defvar edocs-stylesheet-location "style.css"
  "The location of the CSS used by the exported HTML.

By default it uses `style.css'.  This can be changed to any value
which can be used as the `href' attribute of a `style' tag as it
is placed verbatim in one.")

(defvar edocs-generate-only-body nil
  "Whether to generate only the body and no header/footer info.

In case the output of edocs is to be embedded into some other
HTML or similar file this option can be changed so that no HTML
header of footer tags are output.  Any non-nil value will
suppress these tags.  The default is nil.")

(defvar edocs-private-regexp "--"
  "Regular expression to identify private parts of a module's API.

Some modules (such as this one) differentiate between public and
private parts of the API.  This regular expression is used to
identify symbols that are supposed to be private to the module,
and are not meant to be used outside the module.  The default is
`--', which matches any symbol with two hyphens such as
`edocs--symbol-type-map'.")

(defconst edocs--symbol-type-map
  #s(hash-table size 8 test equal
                data ("defclass" "Class"
                      "defconst" "Constant"
                      "defcustom" "Customization option"
                      "defface" "Face"
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
                                  "define-minor-mode" "defface"))
                       " "
                       (group (1+ (not (any space ?\n ?\)))))))
              nil :noerror)
        (let ((type (buffer-substring-no-properties
                     (match-beginning 1) (match-end 1)))
              (name (buffer-substring-no-properties
                     (match-beginning 2) (match-end 2))))
          (unless (string-match edocs-private-regexp name)
            (setq ls (cons (cons type name) ls))))))
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
     ((eql type 'defface)
      (documentation-property obj 'face-documentation))
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
  (let ((tag-sym (cl-gensym))
        (attrs-sym (cl-gensym)))
    `(let ((,tag-sym ,tag)
           (,attrs-sym ,attrs))
       (insert "<" ,tag-sym)
       (insert (mapconcat
                (lambda (itm)
                  (format " %s=%S" (car itm) (cdr itm)))
                ,attrs-sym ""))
       (insert ">")
       ,@contents
       (insert "</" ,tag-sym ">"))))

(defun edocs--format-text (txt known-symbols)
  "Perform formatting operations on TXT.

KNOWN-SYMBOLS is used for referencing symbols found in other
parts of the module."
  (let ((org-export-with-toc nil)
        (org-export-with-section-numbers nil))
    (org-export-string-as
     (replace-regexp-in-string
      "`\\([^']+\\)'"
      (lambda (match)
        (if (member (substring match 1 -1) known-symbols)
            "@@html:<a href=\"#\\1\"><code>@@\\1@@html:</code></a>@@"
          "~\\1~"))
      txt)
     'html t)))

(defun edocs--format-commentary (cmt known-symbols)
  "Perform special commentary formatting operations on CMT.

KNOWN-SYMBOLS is used for referencing symbols found in other
parts of the module."
  (edocs--format-text
   (replace-regexp-in-string
    ";" "*" (replace-regexp-in-string
             "^;; " "" (replace-regexp-in-string
                        ";;; Commentary:\n+" "" cmt))) known-symbols))

(defun edocs--format-doc (doc known-symbols)
  "Perform formatting operations on DOC or on DOC's `cdr'.

KNOWN-SYMBOLS is used for referencing symbols found in other
parts of the module."
  (edocs--format-text (if (consp doc) (cdr doc) doc) known-symbols))

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

(defun edocs--format-symbol (symbol known-symbols)
  "Format the information in SYMBOL.

KNOWN-SYMBOLS is used for referencing symbols found in other
parts of the module."
  (let* ((type (car symbol))
         (name (cdr symbol))
         (docs (edocs--get-docs type name)))
    (mapc (lambda (doc)
            (edocs--with-tag "div" nil
              (insert "&ndash; ")
              (edocs--with-tag "strong" nil
                (insert (edocs--get-type-display type)))
              (insert ": ")
              (edocs--with-tag "tt" nil
                (edocs--with-tag "a" `(("name" . ,name)
                                       ("href" . ,(concat "#" name)))
                  (insert name)))
              (insert " " (if (consp doc) (car doc) ""))
              (edocs--with-tag "blockquote" '(("class" . "docstring"))
                (insert (or (edocs--format-doc doc known-symbols)
                            "Not documented.")))))
          (edocs--normalize docs))))

(defun edocs-generate ()
  "Generate nice-looking documentation for a module or file.

Markup is handled by `org-mode' exporting functions.  This
command is used both as an interactive command to test the output
of this module and called by the `edocs-generate-batch' function
to generate the actual output.  This command outputs its result
into a buffer called `*edocs*' and switches to that buffer."
  (interactive)
  (let* ((buffer (get-buffer-create "*edocs*"))
         (binfo (package-buffer-info))
         (commentary (lm-commentary))
         (symbol-specs (edocs--list-symbols))
         (symbols (mapcar #'cdr symbol-specs)))
    (with-current-buffer buffer
      (unless edocs-generate-only-body (edocs--insert-header))
      (edocs--with-tag "div" '(("class" . "container"))
        (edocs--insert-title (edocs--module-name binfo)
                             (edocs--module-summary binfo))
        (insert (edocs--format-commentary commentary symbols))
        (insert "<h2>API</h2>")
        (mapc (lambda (spec) (edocs--format-symbol spec symbols))
              symbol-specs))
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
  "Generate module docs as a batch operation.

This function maps over `command-line-args-left' and tries to
export the documentation for each file to a file with the same
name, except for the extension replaced with `.html'.  This
function uses the `edocs-generate' command to actually generate
the HTML.

Options which affect the export of module documentation (such as
`edocs-stylesheet-location') can be changed using the `-eval'
command line argument to Emacs.  For example:

: emacs -batch -l edocs.el -eval \"(setq edocs-generate-only-body t)\" \\
:     -f edocs-generate-batch file.el"
  (mapc #'edocs--generate-batch-1 command-line-args-left))

(provide 'edocs)
;;; edocs.el ends here
