;;; edocs.el --- Extract documentation from file(s)

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

;;     emacs -batch -l edocs.el -f edocs-generate-docs file.el

;;; Code:

(require 'package)
(require 'help-fns)

(defvar edocs-stylesheet-location "style.css"
  "Where to find the Cascading Style Sheet for the exported docs.")

(defvar edocs-generate-only-body nil
  "Whether to genereate only the body and no header/footer info.")

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
  (cond
   ((string= type-name "defun") "Function")
   ((string= type-name "define-minor-mode") "Minor mode")
   ((string= type-name "defgeneric") "Method")
   ((string= type-name "defclass") "Class")
   ((string= type-name "defcustom") "Customization option")
   ((string= type-name "defgroup") "Customization group")
   (t type-name)))

(defun edocs-generate ()
  "Generate nice-looking documentation for a module or file."
  (interactive)
  (let ((buffer (get-buffer-create "*edocs*"))
        (binfo (package-buffer-info)))
    (with-current-buffer buffer
      (unless edocs-generate-only-body
        (insert "<!DOCTYPE html>\n"
                "<html><head>"
                "<link href=\"" edocs-stylesheet-location
                "\" rel=\"stylesheet\"></head><body>"))
      (insert "<div class=\"container\">"
              "<h1>" (aref binfo 0) " <small>&mdash; " (aref binfo 2)
              "</small></h1><p>"
              (replace-regexp-in-string
               ";; " "" (replace-regexp-in-string
                         ";;; Commentary:\n+" "" (aref binfo 4)))
              "</p>"))
    (mapc (lambda (itm)
            (let ((docs (edocs--get-docs (car itm) (cdr itm))))
              (with-current-buffer buffer
                (mapc (lambda (doc)
                        (insert "<div>&ndash; "
                                (edocs--get-type-display (car itm))
                                " <tt>" (cdr itm) "</tt> "
                                (if (consp doc) (car doc) "")
                                "<p>"
                                (or (if (consp doc)
                                        (replace-regexp-in-string
                                         "\n\n" "</p><p>" (cdr doc))
                                      (replace-regexp-in-string
                                       "\n\n" "</p><p>" doc))
                                    "Not documented.") "</p></div>"))
                      (if (or (not (listp docs))
                              (not (listp (cdr docs))))
                          (list docs)
                        docs)))))
          (edocs--list-symbols))
    (with-current-buffer buffer
      (insert "</div>")
      (unless edocs-generate-only-body
        (insert "</body></html>")))
    (switch-to-buffer buffer)))

(defun edocs-generate-batch ()
  "Generate module docs using batch operations."
  (mapc (lambda (file)
          (with-current-buffer (find-file file)
            (eval-buffer)
            (edocs-generate)
            (write-file (concat (file-name-sans-extension file) ".html"))))
        command-line-args-left))

(provide 'edocs)
;;; edocs.el ends here
