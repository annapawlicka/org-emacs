;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is the minimal config needed to get org-mode from melpa and
;; get it up and running so that we can load our emacs config from a
;; .org file in a literate manner. The basis for this can be found
;; here:
;;
;; http://orgmode.org/worg/org-contrib/babel/intro.html
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			;; ("marmalade" . "http://marmalade-repo.org/packages/")
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
