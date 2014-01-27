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

(require 'eieio)
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

(defvar edocs-exporter 'edocs-html-exporter
  "The exporter to use when exporting docs.")

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

(defclass edocs-ascii-exporter ()
  ((extension :reader exporter-extension :initform ".txt"))
  :documentation "An exporter that produces ascii text.")

(defclass edocs-html-exporter ()
  ((extension :reader exporter-extension :initform ".html"))
  :documentation "An exporter that produces html text.")

(defun edocs--get-doc (expr)
  "Get a docstring from EXPR."
  (cl-case (car expr)
    ((defun defcustom defgroup defface)
     (let ((doc (nth 3 expr)))
       (when (stringp doc) doc)))
    ((defvar defconst defgeneric)
      (let ((doc (car (last expr))))
        (when (and (= (length expr) 4)
                   (stringp doc))
          doc)))
    (define-minor-mode
      (let ((doc (nth 2 expr)))
        (when (stringp doc) doc)))
    (defclass
      (let ((doc (or (plist-get expr :documentation)
                     (nth 4 expr))))
        (when (stringp doc) doc)))
    (defmethod
      (let ((doc1 (nth 3 expr))
            (doc2 (nth 4 expr)))
        (or (and (stringp doc1) doc1)
            (and (stringp doc2) doc2))))))

(defun edocs--get-arg-list (expr)
  "Get argument list from EXPR if possible."
  (when (eql (car expr) 'defun)
    (format "%s" (or (nth 2 expr) "()"))))

(cl-defstruct edocs-symbol name type doc args)

(defun edocs--list-symbols ()
  "Get a list of all symbols in the buffer.

The results also contain a specification of what was found for
each symbol, for example a `defun', `defvar' or `defcustom',
etc."
  (let (ls)
    (save-excursion
      (goto-char (point-min))
      (condition-case nil
          (while t
            (let ((expr (read (current-buffer))))
              (when (member (car expr)
                            '(defun defgroup defcustom defvar
                                    defclass defgeneric defconst
                                    define-minor-mode defface))
                (let ((type (symbol-name (car expr)))
                      (name (symbol-name (cadr expr)))
                      (docs (edocs--get-doc expr)))
                  (unless (or (string-match edocs-private-regexp name)
                              (not docs))
                    (push (make-edocs-symbol
                           :name name :type type :doc docs
                           :args (edocs--get-arg-list expr)) ls))))))
        (end-of-file nil))
      (reverse ls))))

(defun edocs--get-type-display (type-name)
  "Get the display text for TYPE-NAME."
  (gethash type-name edocs--symbol-type-map type-name))

(defmethod edocs--export-insert-headers ((exporter edocs-html-exporter))
  "Insert an HTML header."
  (insert "<!DOCTYPE html>\n"
          "<html><head>"
          "<link href=\"" edocs-stylesheet-location
          "\" rel=\"stylesheet\"></head><body>"))

(defmethod edocs--export-insert-title ((exporter edocs-html-exporter)
                                       title subtitle)
  "Insert TITLE and SUBTITLE as html."
  (edocs--with-tag "div" '(("class" . "container"))
    (insert "<h1>" title " <small>&mdash; " subtitle "</small></h1>")))

(defmethod edocs--export-insert-headers ((exporter edocs-ascii-exporter))
  "Don't insert anything."
  nil)

(defmethod edocs--export-insert-title ((exporter edocs-ascii-exporter)
                                       title subtitle)
  "Insert TITLE and SUBTITLE."
  (insert title " --- " subtitle "\n\n"))

(defmethod edocs--export-insert-footer ((exporter edocs-html-exporter))
  "Insert necessary footer information into the current buffer."
  (insert "</body></html>"))

(defmethod edocs--export-insert-footer ((exporter edocs-ascii-exporter))
  "Don't insert anything."
  nil)

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

(defmethod edocs--export-format-text ((exporter edocs-html-exporter)
                                      txt known-symbols)
  "Perform formatting operations on TXT.

KNOWN-SYMBOLS is used for referencing symbols found in other
parts of the module."
  (let ((org-export-with-toc nil)
        (org-export-with-section-numbers nil))
    (edocs--with-tag "div" '(("class" . "container"))
      (insert
       (org-export-string-as
        (replace-regexp-in-string
         "`\\([^']+\\)'"
         (lambda (match)
           (if (member (substring match 1 -1) known-symbols)
               "@@html:<a href=\"#\\1\"><code>@@\\1@@html:</code></a>@@"
             "~\\1~"))
         txt)
        'html t)))))

(defmethod edocs--export-format-text ((exporter edocs-ascii-exporter)
                                      txt known-symbols)
  "Perform formatting operations on TXT."
  (insert txt))

(defmethod edocs--export-insert-header ((exporter edocs-html-exporter)
                                        level txt)
  "Format a header."
  (edocs--with-tag "div" '(("class" . "container"))
    (insert (format "<h%d>%s</h%d>" level txt level))))

(defmethod edocs--export-insert-header ((exporter edocs-ascii-exporter)
                                        level txt)
  "Format a header"
  (insert (make-string level ?=) " " txt "\n\n"))

(defun edocs--format-commentary (exporter cmt known-symbols)
  "Make EXPORTER perform special commentary formatting operations on CMT.

KNOWN-SYMBOLS is used for referencing symbols found in other
parts of the module."
  (edocs--export-format-text
   exporter
   (replace-regexp-in-string
    ";" "*" (replace-regexp-in-string
             "^;; " "" (replace-regexp-in-string
                        ";;; Commentary:\n+" "" cmt))) known-symbols))

(defun edocs--format-doc (exporter doc known-symbols)
  "Make EXPORTER perform formatting operations on DOC or on DOC's `cdr'.

KNOWN-SYMBOLS is used for referencing symbols found in other
parts of the module."
  (edocs--export-format-text
   exporter
   (if (consp doc) (cdr doc) doc) known-symbols))

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

(defmethod edocs--export-insert-definition
  ((exporter edocs-html-exporter) type name args doc known-symbols)
  "Insert definition."
  (edocs--with-tag "div" '(("class" . "container"))
    (insert "&ndash; ")
    (edocs--with-tag "strong" nil
      (insert (edocs--get-type-display type)))
    (insert ": ")
    (edocs--with-tag "tt" nil
      (edocs--with-tag "a" `(("name" . ,name)
                             ("href" . ,(concat "#" name)))
        (insert name)))
    (insert " " (or args ""))
    (edocs--with-tag "blockquote" '(("class" . "docstring"))
      (edocs--format-doc exporter doc known-symbols))))

(defmethod edocs--export-insert-definition
  ((exporter edocs-ascii-exporter) type name args doc known-symbols)
  "Insert definition."
  (insert "-- " (edocs--get-type-display type) ": "
          name " " (or args "") "\n\n"
          (edocs--format-doc exporter doc known-symbols) "\n\n"))

(defun edocs--format-symbol (exporter symbol known-symbols)
  "Make EXPORTER format the information in SYMBOL.

KNOWN-SYMBOLS is used for referencing symbols found in other
parts of the module."
  (let ((type (edocs-symbol-type symbol))
        (name (edocs-symbol-name symbol))
        (docs (edocs-symbol-doc symbol))
        (args (edocs-symbol-args symbol)))
    (mapc (lambda (doc)
            (edocs--export-insert-definition
             exporter type name args doc known-symbols))
          (edocs--normalize docs))))

(defun edocs-generate (&optional exporter)
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
         (symbols (mapcar #'edocs-symbol-name symbol-specs))
         (exporter (or exporter (make-instance edocs-exporter))))
    (with-current-buffer buffer
      (unless edocs-generate-only-body
        (edocs--export-insert-headers exporter))
      (edocs--export-insert-title exporter
                                  (edocs--module-name binfo)
                                  (edocs--module-summary binfo))
      (edocs--format-commentary exporter commentary symbols)
      (edocs--export-insert-header exporter 2 "API")
      (mapc (lambda (spec) (edocs--format-symbol exporter spec symbols))
            symbol-specs)
      (unless edocs-generate-only-body
        (edocs--export-insert-footer exporter)))
    (switch-to-buffer buffer)))

(defun edocs--generate-batch-1 (file)
  "Generate docs for FILE."
  (let ((exporter (make-instance edocs-exporter)))
    (with-current-buffer (find-file file)
      (edocs-generate )
      (write-file (concat (file-name-sans-extension file)
                          (exporter-extension exporter))))))

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
