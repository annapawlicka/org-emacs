(defun load-if-exists (file)
  (if (file-exists-p file)
      (progn
        (load file)
        (message (format "Loading file: %s" file)))
    (message (format "No %s file. So not loading one." file))))

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

(add-hook 'after-init-hook 'global-company-mode)

(global-set-key (kbd "M-RET") 'hippie-expand)

(maybe-install-and-require 'diminish)

(setq-default indent-tabs-mode nil)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

(maybe-install-and-require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(maybe-install-and-require 'discover)
(global-discover-mode 1)

(defun my-bell-function ()
  (unless (memq this-command
    '(isearch-abort abort-recursive-edit exit-minibuffer
          keyboard-quit mwheel-scroll down up next-line previous-line
          backward-char forward-char))
        (ding)))
 (setq ring-bell-function 'my-bell-function)

(global-set-key (kbd "M-o") 'other-window)

(desktop-save-mode 1)

(defun git-commit-template ()
  (let ((branch (magit-get-current-branch)))
    (when (setq pivotal-id (cadr (s-match "\\([0-9]+\\)-" branch)))
      (newline 2)
      (insert (format "Story: https://www.pivotaltracker.com/story/show/%s"
                      pivotal-id))
      (goto-char (point-min))
      (insert (format "[#%s] " pivotal-id)))))
(add-hook 'git-commit-mode-hook #'git-commit-template)

(setq cider-repl-history-file "~/.emacs.d/cache/cider-history")

(setq cider-repl-display-help-banner nil)

(setq auto-save-visited-file-name t)

(add-hook 'before-save-hook
          (lambda nil
           (delete-trailing-whitespace)))

(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] '(lambda ()
                             (interactive)
                             (scroll-down 1)))
  (global-set-key [mouse-5] '(lambda ()
                             (interactive)
                             (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t))

(defun insert-pragma-block ()
  (interactive)
  (insert ";;-------------------------------------------------------------------------------
;; ## Pragma-block-name"))

(global-set-key (kbd "<f5> p") 'insert-pragma-block)

(setq split-height-threshold nil
      split-width-threshold nil)

(add-to-list 'load-path (concat user-emacs-directory "non-elpa/"))

(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

(require 'window-number-super)
(window-number-mode 1) ;; for the window numbers
(window-number-super-mode 1) ;; for the super key binding

(maybe-install-and-require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(when (window-system)
    (set-default-font "Fira Code"))
  (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
                 (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
                 (36 . ".\\(?:>\\)")
                 (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
                 (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
                 (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
                 (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
                 (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
                 (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
                 (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
                 (48 . ".\\(?:x[a-zA-Z]\\)")
                 (58 . ".\\(?:::\\|[:=]\\)")
                 (59 . ".\\(?:;;\\|;\\)")
                 (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
                 (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
                 (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
                 (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
                 (91 . ".\\(?:]\\)")
                 (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
                 (94 . ".\\(?:=\\)")
                 (119 . ".\\(?:ww\\)")
                 (123 . ".\\(?:-\\)")
                 (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
                 (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
                 )
                 ))
(dolist (char-regexp alist)
  (set-char-table-range composition-function-table (car char-regexp)
                        `([,(cdr char-regexp) 0 font-shape-gstring]))))

(setq custom-theme-directory (concat user-emacs-directory "themes"))

(maybe-install-and-require 'grandshell-theme)

(when (memq window-system '(mac ns))
  (set-default-font "-apple-Menlo-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1"))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(line-number-mode 1)
(column-number-mode 1)

(setq inhibit-startup-screen t)

(defun toggle-transparency ()
  (interactive)
  (let ((param (cadr (frame-parameter nil 'alpha))))
    (if (and param (/= param 100))
        (set-frame-parameter nil 'alpha '(100 100))
      (set-frame-parameter nil 'alpha '(85 50)))))
(global-set-key (kbd "C-c t") 'toggle-transparency)

(setq dired-listing-switches "-alh")

(maybe-install-and-require 'helm)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x")     'helm-M-x)
(helm-mode 1)

(diminish 'helm-mode)

(maybe-install-and-require 'magit)

(global-set-key (kbd "C-x g") 'magit-status)

(maybe-install-and-require 'git-gutter-fringe+)
(global-git-gutter+-mode t)

(global-set-key (kbd "s-n") 'git-gutter+-next-hunk)
(global-set-key (kbd "s-p") 'git-gutter+-previous-hunk)

(diminish 'git-gutter+-mode)

(maybe-install-and-require 'ace-jump-mode)
(global-set-key (kbd "C-c j") 'ace-jump-mode)
(global-set-key (kbd "C-c k") 'ace-jump-mode-pop-mark)

(windmove-default-keybindings)

(maybe-install-and-require 'buffer-move)
(global-set-key (kbd "<s-up>")     'buf-move-up)
(global-set-key (kbd "<s-down>")   'buf-move-down)
(global-set-key (kbd "<s-left>")   'buf-move-left)
(global-set-key (kbd "<s-right>")  'buf-move-right)

(global-set-key (kbd "s-=") 'shrink-window)
(global-set-key (kbd "s-+") 'enlarge-window)

(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.saves"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

(global-set-key (kbd "C-x C-b") 'ibuffer)

(maybe-install-and-require 'projectile)
(projectile-global-mode)

(diminish 'projectile-mode)

(show-paren-mode +1)

(maybe-install-and-require 'paredit)
(diminish 'paredit-mode "()")
(add-hook 'prog-mode-hook 'paredit-mode)

(maybe-install-and-require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(maybe-install-and-require 'rainbow-mode)
(add-hook 'prog-mode-hook 'rainbow-mode)
(diminish 'rainbow-mode)

(maybe-install-and-require 'highlight-symbol)
(add-hook 'prog-mode-hook 'highlight-symbol-mode)

(maybe-install-and-require 'yasnippet)

(setq yas/root-directory (concat user-emacs-directory "snippets"))

(yas-global-mode 1)

(diminish 'yas-minor-mode)

(maybe-install-and-require 'smartscan)
(add-hook 'prog-mode-hook
          '(lambda () (smartscan-mode 1)))

(setq lisp-hooks (lambda ()
                   (eldoc-mode +1)
                   (diminish 'eldoc-mode)
                   (define-key paredit-mode-map
                     (kbd "{") 'paredit-open-curly)
                   (define-key paredit-mode-map
                     (kbd "}") 'paredit-close-curly)))

(add-hook 'emacs-lisp-mode-hook lisp-hooks)

(require 'popup)

(defun describe-thing-in-popup ()
  (interactive)
  (let* ((thing (symbol-at-point))
         (help-xref-following t)
         (description (with-temp-buffer
                        (help-mode)
                        (help-xref-interned thing)
                        (buffer-string))))
    (popup-tip description
               :point (point)
               :around t
               :height 30
               :scroll-bar t
               :margin t)))

(add-hook 'emacs-lisp-mode-hook
          (lambda () (local-set-key (kbd "C-c C-d") 'describe-thing-in-popup)))

(maybe-install-and-require 'cider)

(setq cider-repl-print-length 100)

(add-hook 'clojure-mode-hook lisp-hooks)

(setq cider-history-file (concat user-emacs-directory "cider-history"))

(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)

(setq cider-show-error-buffer nil)

(defun cider-ediff-hack ()
 (interactive)
 (let ((expected (get-text-property (point) 'actual))
  (tmp-buffer (generate-new-buffer " *tmp*"))
  (expected-buffer (generate-new-buffer " *expected*"))
  (actual-buffer   (generate-new-buffer " *actual*")))
 (with-current-buffer tmp-buffer
  (insert expected)
  (goto-char (point-min))
  (re-search-forward "= ")
  (let ((opoint (point)))
    (forward-sexp 1)
    (let* ((tpoint (point))
           (our-exp (buffer-substring-no-properties opoint (point)))
           (_ (forward-sexp 1))
           (our-act (buffer-substring-no-properties tpoint (point) )))
      (with-current-buffer expected-buffer
        (insert our-exp)
        (delete-trailing-whitespace))
      (with-current-buffer actual-buffer
        (insert our-act)
        (delete-trailing-whitespace))
      (apply 'ediff-buffers
             (setq cider-test-ediff-buffers
                   (list (buffer-name expected-buffer)
                         (buffer-name actual-buffer)))))))))

(maybe-install-and-require 'align-cljlet)

(defun helm-clojure-headlines ()
  "Display headlines for the current Clojure file."
  (interactive)
  (helm :sources '(((name . "Clojure Headlines")
                    (volatile)
                    (headline "^[;(]")))))

(add-hook 'clojure-mode-hook
          (lambda () (local-set-key (kbd "s-h") 'helm-clojure-headlines)))

(maybe-install-and-require 'clojure-cheatsheet)
(add-hook 'clojure-mode-hook
          (lambda () (local-set-key [s-f1] 'clojure-cheatsheet)))

(defun sw1nn-nrepl-current-server-buffer ()
  (let ((nrepl-server-buf (replace-regexp-in-string "connection" "server" (nrepl-current-connection-buffer))))
    (when nrepl-server-buf
      (get-buffer nrepl-server-buf))))

(defun sw1nn-cider-perspective ()
  (interactive)
  (delete-other-windows)
  (split-window-below)
  (windmove-down)
  (shrink-window 25)
  (switch-to-buffer (sw1nn-nrepl-current-server-buffer))
  (windmove-up)
  (pop-to-buffer (cider-find-or-create-repl-buffer)))

(maybe-install-and-require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(maybe-install-and-require 'gist)

(maybe-install-and-require 'refheap)

(setq ispell-program-name "aspell"
      ispell-dictionary "english")

(setq org-src-fontify-natively t)

(setq ispell-program-name (executable-find "aspell"))

(add-hook 'org-mode-hook
          (lambda () (local-set-key (kbd "s-h") 'helm-org-headlines)))

(maybe-install-and-require 'helm-orgcard)
(add-hook 'org-mode-hook
                 (lambda () (local-set-key [s-f1] 'helm-orgcard)))

(setq org-feed-retrieve-method 'curl)

(load-if-exists (concat user-emacs-directory "local/blog-roll.el"))

(maybe-install-and-require 'ox-reveal)

(setq org-reveal-root "reveal.js-2.5.0/")

(maybe-install-and-require 'org-magit)

(maybe-install-and-require 'tagedit)
(eval-after-load "sgml-mode"
  '(progn
     (require 'tagedit)
     (tagedit-add-paredit-like-keybindings)
     (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))

(tagedit-add-experimental-features)

(add-hook 'css-mode-hook 'paredit-mode)

(add-hook 'css-mode-hook 'rainbow-mode)

(maybe-install-and-require 'css-eldoc)

(maybe-install-and-require 'helm-css-scss)
(add-hook 'css-mode-hook
          (lambda () (local-set-key (kbd "s-h") 'helm-css-scss)))

(maybe-install-and-require 'markdown-mode)

(add-to-list 'auto-mode-alist '(".md$" . gfm-mode))

(setq markdown-open-command "open")

(defun helm-markdown-headlines ()
  "Display headlines for the current Clojure file."
  (interactive)
  (helm :sources '(((name . "Markdown Headlines")
                    (volatile)
                    (headline "^[#]")))))

(add-hook 'markdown-mode-hook
          (lambda () (local-set-key (kbd "s-h") 'helm-markdown-headlines)))

(maybe-install-and-require 'erc)
(setq erc-hide-list '("JOIN" "PART" "QUIT"))
(setq erc-autojoin-channels-alist
     '(("freenode.net" "#emacs" "#clojure" "#clojurescript")))
(erc :server "irc.freenode.net" :port 6667 :nick "annapawlicka")
