;;; company-anaconda.el --- Anaconda backend for company-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015 by Artem Malyshev

;; Author: Artem Malyshev <proofit404@gmail.com>
;; URL: https://github.com/proofit404/anaconda-mode
;; Version: 0.1.0
;; Package-Requires: ((company "0.8.0") (anaconda-mode "0.1.1") (cl-lib "0.5.0") (dash "2.6.0") (s "1.9"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See the README for more details.

;;; Code:

(require 'cl-lib)
(require 'company)
(require 'company-capf)
(require 'anaconda-mode)
(require 'dash)
(require 's)

(defgroup company-anaconda nil
  "Company back-end for Python code completion."
  :group 'programming)

(defcustom company-anaconda-annotation-function
  'company-anaconda-description-in-chevrons
  "Function that returns candidate annotations.

This function takes a single argument, a completion candidate
produced by anaconda-mode.  It should return an annotation string
to be displayed by company-mode as the annotation for this
candidate, or nil if no annotation should be displayed.

The candidate is a string, a possible completion.  The candidate
may also have some text properties containing additional
information.  These properties are:

- description: Jedi's description, typically the type of
  completion optionally followed by a fully-qualified name for
  the candidate.  For example, \"class: foo.bar.Baz\" or
  \"statement\".

- module-path: The path to the file that contains this candidate.

- line: The line within that file where the candidate is defined.

- docstring: The candidate's docstring.

You can retrieve any of these properties with
`get-text-property', such as
\(get-text-property 0 'description candidate).  Bear in mind that
any of these properties may be absent for any candidate."
  :group 'company-anaconda
  :type 'function)

(defun company-anaconda-description-in-chevrons (candidate)
  "Return the description property of CANDIDATE inside chevrons.

This will return a string such as,
\"<function: mod.Klass.a_function>\".  This is primarily for use
as a possible value for `company-anaconda-annotation-function'."
  (--when-let (get-text-property 0 'description candidate)
    (concat "<" it ">")))

(defcustom company-anaconda-case-insensitive t
  "Use case insensitive candidates match."
  :group 'company-anaconda
  :type 'boolean)

(defun company-anaconda-prefix ()
  "Grab prefix at point.
Properly detect strings, comments and attribute access."
  (and anaconda-mode
       (not (company-in-string-or-comment))
       (--if-let (company-capf 'prefix)
           (if (looking-back "\\." (- (point) 1))
               (cons it t)
             it)
         'stop)))

(defun company-anaconda-candidates (callback)
  "Obtain candidates list from anaconda asynchronously.
Apply passed CALLBACK to extracted collection."
  (anaconda-mode-call "complete"
                      (lambda (result)
                        (funcall callback
                                 (anaconda-mode-complete-extract-names result)))))

(defun company-anaconda-doc-buffer (candidate)
  "Return documentation buffer for chosen CANDIDATE."
  (let ((docstring (get-text-property 0 'docstring candidate)))
    (unless (s-blank? docstring)
      (anaconda-mode-with-view-buffer
       (insert docstring)))))

(defun company-anaconda-meta (candidate)
  "Return short documentation string for chosen CANDIDATE."
  (let ((docstring (get-text-property 0 'docstring candidate)))
    (unless (s-blank? docstring)
      (car (s-split-up-to "\n" docstring 1)))))

(defun company-anaconda-location (candidate)
  "Return location (path . line) for chosen CANDIDATE."
  (-when-let* ((module-path (get-text-property 0 'module-path candidate))
               (line (get-text-property 0 'line candidate)))
    (cons module-path line)))

;;;###autoload
(defun company-anaconda (command &optional arg &rest _args)
  "Anaconda backend for company-mode.
See `company-backends' for more info about COMMAND and ARG."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-anaconda))
    (prefix (company-anaconda-prefix))
    (candidates (cons :async
                      (lambda (callback)
                        (company-anaconda-candidates callback))))
    (doc-buffer (company-anaconda-doc-buffer arg))
    (meta (company-anaconda-meta arg))
    (annotation (funcall company-anaconda-annotation-function arg))
    (location (company-anaconda-location arg))
    (ignore-case company-anaconda-case-insensitive)
    (sorted t)))

(provide 'company-anaconda)

;;; company-anaconda.el ends here
