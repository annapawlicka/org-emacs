;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is the minimal config needed to get org-mode from melpa and
;; get it up and running so that we can load our emacs config from a
;; .org file in a literate manner. The basis for this can be found
;; here:
;;
;; http://orgmode.org/worg/org-contrib/babel/intro.html
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                        ;; ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")))

;; This means we prefer things from ~/.emacs.d/elpa over the standard packages.
(package-initialize)

;; This bootstraps us if we don't have anything
(when (not package-archive-contents)
  (package-refresh-contents))

;; This installs elpa packages if we haven't done that yet
(defun maybe-install-and-require (p)
  (when (not (package-installed-p p))
    (package-install p))
  (require p))

(setq desktop-restore-forces-onscreen nil)
(setq magit-last-seen-setup-instructions "1.4.0")

;; org-mode always needs to be installed in an emacs where it isn't loaded.
(require 'org)

(org-babel-load-file (concat (getenv "HOME") "/.emacs.d/org/config.org"))
(put 'erase-buffer 'disabled nil)
(defvar clojure--prettify-symbols-alist nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("7c14c764f15a135f12824c322f2ec7b576fd06f1ca634c58538c6b47f53b069a" default)))
 '(safe-local-variable-values
   (quote
    ((cider-refresh-after-fn . "user/go")
     (cider-refresh-before-fn . "user/stop")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
