(package "company-anaconda"
         "0.2.0"
         "Anaconda backend for company-mode")

(website-url "https://github.com/proofit404/anaconda-mode")
(keywords "convenience" "company" "anaconda")

(package-file "company-anaconda.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)
(source 'melpa)

(depends-on "emacs" "25.1")
(depends-on "anaconda-mode")
(depends-on "company")
(depends-on "dash")
(depends-on "s")

(development
 (depends-on "cask")
 (depends-on "zenburn-theme"))

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
