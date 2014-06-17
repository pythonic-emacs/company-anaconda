(require 'cask)

(let ((source-directory (locate-dominating-file load-file-name "Cask")))
  (cask-initialize source-directory)
  (add-to-list 'load-path source-directory))

;; Anaconda settings.

(setq anaconda-mode-port 8887)

(setq anaconda-mode-debug t)

;; Company settings.

(global-company-mode)

(add-to-list 'company-backends 'company-anaconda)

;; Pyenv settings.

(pyenv-mode)
