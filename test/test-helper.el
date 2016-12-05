;;; test-helper.el --- ert-runner test helper

;;; Commentary:

;;; Code:

(require 'cask)

(let ((source-directory (locate-dominating-file load-file-name "Cask")))
  (cask-initialize source-directory)
  (add-to-list 'load-path source-directory))

(undercover "company-anaconda.el")

(setq python-indent-guess-indent-offset nil)

(require 'cl-lib)
(require 'company-anaconda)

(provide 'test-helper)

;;; test-helper.el ends here
