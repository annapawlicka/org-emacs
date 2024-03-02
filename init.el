;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is the minimal config needed to get org-mode from melpa and
;; get it up and running so that we can load our emacs config from a
;; .org file in a literate manner. The basis for this can be found
;; here:
;;
;; http://orgmode.org/worg/org-contrib/babel/intro.html

(require 'package)
(setq package-enable-at-startup nil)

(when (equal emacs-version "27.2")
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; https://emacs.stackexchange.com/a/2989
(setq package-archives
      '(("elpa"     . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")
	("org" . "http://orgmode.org/elpa/")
	("gnu" . "http://elpa.gnu.org/packages/"))
      package-archive-priorities
      '(("melpa-stable" . 10)
        ("elpa"     . 5)
        ("melpa"        . 0)))

(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Pin all the things
(setq package-pinned-packages
      '((aggressive-indent . "melpa-stable")
        (bind-key . "melpa")
        (cider . "melpa")
        (cider-eval-sexp-fu . "melpa-stable")
        (clj-refactor . "melpa-stable")
        (clojure-mode . "melpa-stable")
        (company . "melpa-stable")
        ;; (dash . "melpa-stable")
        ;; (diminish . "melpa-stable")
        (epl . "melpa-stable")
        (exec-path-from-shell . "melpa-stable")
        (flx . "melpa-stable")
        (flx-ido . "melpa-stable")
        (git-commit . "melpa-stable")
        ;; stable version is broken (doesn't compile in Emacs 29.x that doesn't have linum anymore.
        ;; there's a fix in the latest pockage in melpa, so let's use that for now.
        (git-gutter . "melpa")
        (hydra . "melpa-stable")
        (ido . "melpa-stable")
        (ido-completing-read+ . "melpa-stable")
        (ido-ubiquitous . "melpa-stable")
        (ido-vertical-mode . "melpa-stable")
        (flycheck-pos-tip . "melpa-stable")
        (flycheck . "melpa-stable")
        (highlight . "melpa") ;; woo! from the wiki https://www.emacswiki.org/emacs/highlight.el
        (highlight-symbol . "melpa-stable")
        (inflections . "melpa-stable")
        (magit . "melpa-stable")
        (magit-popup . "melpa-stable")
        (multiple-cursors . "melpa-stable")
        (org . "org")
        (org-plus-contrib . "org")
        (paredit . "melpa-stable")
        (peg . "melpa-stable")
        (pkg-info . "melpa-stable")
        (pos-tip . "melpa-stable")
        (projectile . "melpa-stable")
        (rainbow-delimiters . "melpa-stable")
        (s . "melpa-stable")
        (seq . "elpa")
        (smex . "melpa-stable")
        (swiper . "melpa-stable")
        (use-package . "melpa")
        (with-editor . "melpa-stable")
        (yasnippet . "melpa-stable")))

;; org-mode always needs to be installed in an emacs where it isn't loaded.
(use-package org
  :pin org
  :ensure t)

(org-babel-load-file (concat user-emacs-directory "org/config.org"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-buffer-directory ((t (:background "gray8" :foreground "firebrick3"))))
 '(helm-ff-directory ((t (:background "gray17" :foreground "OrangeRed3"))))
 '(helm-selection ((t (:background "purple" :foreground "black" :underline nil)))))

(setq desktop-restore-forces-onscreen nil)
(setq magit-last-seen-setup-instructions "1.4.0")

(put 'erase-buffer 'disabled nil)
(defvar clojure--prettify-symbols-alist nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(persistent-scratch flycheck-clj-kondo flycheck-color-mode-line flycheck markdown-mode js2-mode helm-cider cider clojure-mode smartscan yasnippet highlight-symbol rainbow-mode rainbow-delimiters paredit projectile buffer-move ace-jump-mode magit helm git-gutter-mode cyberpunk-theme exec-path-from-shell discover multiple-cursors diminish company use-package)))
