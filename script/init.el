;;; init.el --- minimal company-anaconda configuration

;;; Commentary:

;;; Code:

(require 'cask)

(let ((source-directory (locate-dominating-file load-file-name "Cask")))
  (cask-initialize source-directory)
  (add-to-list 'load-path source-directory))

(global-company-mode)

(add-to-list 'company-backends 'company-anaconda)

(pyenv-mode)

(add-hook 'python-mode-hook 'anaconda-mode)

;;; init.el ends here
