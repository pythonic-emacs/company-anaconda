;;; init.el --- minimal company-anaconda configuration

;;; Commentary:

;;; Code:

(require 'cask)

(let ((source-directory (locate-dominating-file load-file-name "Cask")))
  (cask-initialize source-directory)
  (add-to-list 'load-path source-directory))

;; Anaconda settings.

(setq anaconda-mode-debug t)

;; Company settings.

(global-company-mode)

(add-to-list 'company-backends 'company-anaconda)

;; Pyenv settings.

(pyenv-mode)

(provide 'init)

;;; init.el ends here
