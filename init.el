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
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                        ;; ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
                         ))

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
    ("addfaf4c6f76ef957189d86b1515e9cf9fcd603ab6da795b82b79830eed0b284" "7c14c764f15a135f12824c322f2ec7b576fd06f1ca634c58538c6b47f53b069a" default)))
 '(helm-ff-lynx-style-map t)
 '(horizontal-scroll-bar-mode nil)
 '(package-selected-packages
   (quote
    (restclient git-link graphql company flycheck-color-mode-line flycheck markdown-mode tagedit org-magit helm-orgcard refheap gist js2-mode helm-cider yasnippet use-package smartscan rainbow-mode rainbow-delimiters projectile paredit multiple-cursors magit highlight-symbol helm git-gutter-fringe exec-path-from-shell discover diminish cyberpunk-theme cider buffer-move align-cljlet ace-jump-mode)))
 '(safe-local-variable-values
   (quote
    ((cider-refresh-after-fn . "user/go")
     (cider-refresh-before-fn . "user/stop"))))
 '(scroll-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-buffer-directory ((t (:background "gray8" :foreground "firebrick3"))))
 '(helm-ff-directory ((t (:background "gray17" :foreground "OrangeRed3"))))
 '(helm-selection ((t (:background "purple" :foreground "black" :underline nil)))))
