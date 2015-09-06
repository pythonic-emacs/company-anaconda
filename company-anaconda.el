;;; company-anaconda.el --- Anaconda backend for company-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015 by Artem Malyshev

;; Author: Artem Malyshev <proofit404@gmail.com>
;; URL: https://github.com/proofit404/anaconda-mode
;; Version: 0.1.0
;; Package-Requires: ((company "0.8.0") (anaconda-mode "0.1.0") (cl-lib "0.5.0") (dash "2.6.0") (s "1.9"))

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
(require 'anaconda-mode)
(require 'dash)
(require 's)

(defun company-anaconda-prefix ()
  "Grab prefix at point.
Properly detect strings, comments and attribute access."
  (and anaconda-mode
       (not (company-in-string-or-comment))
       (or (company-grab-symbol-cons "\\." 1)
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

(defun company-anaconda-annotation (candidate)
  "Return annotation string for chosen CANDIDATE."
  (--when-let (get-text-property 0 'description candidate)
    (concat "<" it ">")))

(defun company-anaconda-location (candidate)
  "Return location (path . line) for chosen CANDIDATE."
  (-when-let* ((module-path (get-text-property 0 'module-path candidate))
               (line (get-text-property 0 'line candidate)))
    (cons module-path line)))

;;;###autoload
(defun company-anaconda (command &optional arg)
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
    (annotation (company-anaconda-annotation arg))
    (location (company-anaconda-location arg))
    (ignore-case t)
    (sorted t)))

(provide 'company-anaconda)

;;; company-anaconda.el ends here
