
(add-hook 'before-save-hook
          (lambda nil
           (delete-trailing-whitespace)))

(defun load-if-exists (file)
  (if (file-exists-p file)
      (progn
        (load file)
        (message (format "Loading file: %s" file)))
    (message (format "No %s file. So not loading one." file))))

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

(add-to-list 'load-path (concat user-emacs-directory "non-elpa/"))

(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

(require 'window-number-super)
(window-number-mode 1) ;; for the window numbers
(window-number-super-mode 1) ;; for the super key binding

(maybe-install-and-require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(setq custom-theme-directory (concat user-emacs-directory "themes"))

(maybe-install-and-require 'grandshell-theme)

;; (load-theme 'emacslive-cyberpunk t)

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

(global-set-key [M-s-up] 'windmove-up)
(global-set-key [M-s-down] 'windmove-down)
(global-set-key [M-s-right] 'windmove-right)
(global-set-key [M-s-left] 'windmove-left)

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

;;  (maybe-install-and-require 'helm-projectile)
;;  (global-set-key (kbd "s-t") 'helm-projectile)

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

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-interaction-mode-hook 'cider-turn-on-eldoc-mode)

(setq cider-history-file (concat user-emacs-directory "cider-history"))

(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)

(setq cider-show-error-buffer nil)

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

(add-hook 'text-mode-hook
          (lambda ()
                  (flyspell-mode 1)
                  (diminish 'flyspell-mode)
                  (auto-fill-mode 1)
                  (diminish 'auto-fill-function)))

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

(setq org-agenda-include-diary t)

(setq org-agenda-window-setup 'current-window)

(setq org-agenda-files
      (quote ("~/org/refile.org"
              "~/org/personal.org"
              "~/org/work.org"
              "~/org/community.org"
              "~/org/work/world-domination.org")))

(setq org-agenda-custom-commands
      '(("D" "Doing Now" todo "DOING|WAITING|BLOCKED|CCC"
         ((org-agenda-sorting-strategy '(todo-state-down tag-up priority-up effort-down))
          (org-agenda-overriding-columns-format
           "%60ITEM(Task) %8CATEGORY %8Owner %8Effort(Estimated Effort){:} %CLOCKSUM")
          (org-agenda-view-columns-initially t)))
        ("N" "Me Now!" tags-todo "TODO={^[DCWB].+[^E]$}+Owner=\"Bruce\""
         ((org-agenda-sorting-strategy '(todo-state-down tag-up priority-up effort-down))))
        ("F" "Me in the Future!" tags-todo "TODO={^[TDCWB].+[^E]$}+Owner=\"Bruce\""
         ((org-agenda-sorting-strategy '(todo-state-down tag-up deadline-up priority-up effort-down))))
        ("C" . "Current Cake Countdown Searches")
        ("Cm" "My Current Cake Countdown"
         ((agenda "" ((org-agenda-span 'week)
                      (org-agenda-start-on-weekday 3)))
          (tags-todo "TODO={^[DCWB].+[^E]$}+Owner=\"Bruce\""
                ((org-agenda-sorting-strategy '(todo-state-down tag-up deadline-up priority-up effort-down))))))
        ("Ch" "My Current Cake Countdown History"
         ((agenda "" ((org-agenda-span 'week)
                      (org-agenda-start-on-weekday 3)))
          (tags-todo "TODO={^[DCWB].+}+Owner=\"Bruce\""
                ((org-agenda-sorting-strategy '(todo-state-down tag-up deadline-up priority-up effort-down))))))
        ("Ct" "Team Current Cake Countdown"
         ((agenda "CATEGORY=\"MC\"" ((org-agenda-files '("~/org/work/world-domination.org"))
                                     (org-agenda-span 'week)
                                     (org-agenda-start-on-weekday 3)))
          (tags-todo "+CATEGORY=\"MC\"+TODO={^[DCWB].+[^E]$}"
                     ((org-agenda-sorting-strategy '(todo-state-down tag-up deadline-up priority-up effort-down))))))
        ("Cl" "Team Current Cake Countdown Log"
         ((agenda "Current Cake Countdow" ((org-agenda-files '("~/org/work/world-domination.org"))
                                           (org-agenda-span 'week)
                                           (org-agenda-start-on-weekday 3)
                                           (org-agenda-show-log t)))
          (tags-todo "+CATEGORY=\"MC\"+TODO={^[DCWB].+}"
                     ((org-agenda-sorting-strategy '(todo-state-down tag-up deadline-up priority-up effort-down))))))
        ("Cf" "Team Next Cake Countdown Log"
         ((agenda "Next Cake Countdown" ((org-agenda-files '("~/org/work/world-domination.org"))
                                         (org-agenda-span 'week)
                                         (org-agenda-start-on-weekday 3)
                                         (org-agenda-show-log t)))
          (tags-todo "+CATEGORY=\"MC\"+TODO={^[TDCWB].+[^E]$}"
                     ((org-agenda-sorting-strategy '(todo-state-down tag-up deadline-up priority-up effort-down))))))))

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "CCC(c)" "DOING(g)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "BLOCKED(b@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

(setq org-log-into-drawer t)

(setq org-clock-into-drawer t)

(setq org-enforce-todo-dependencies t)

(defun captains-chair ()
  (interactive)
  (delete-other-windows)

  (if (< (frame-width) 240)
      ;; Small frame
      (progn
        ;; create our 2 columns
        (split-window-right)

        ;; split 1st column vertically
        (split-window-below)

        ;; move to the rightmost and split into 3 verticalally
        (window-number-select 3)
        (split-window-below)
        (split-window-below))
    ;; Big frame
    (progn
      ;; create our 3 columns
      (split-window-right)
      (split-window-right)

      ;; move to the rightmost and split
      (window-number-select 3)
      (split-window-below)
      (split-window-below)))

  (balance-windows)

  ;; set up the buffers as we want
  (window-number-select 1)
  (org-agenda nil "Cm")
  (window-number-select 2)
  (switch-to-buffer (find-file (concat org-directory "/work/world-domination.org")))
  (window-number-select 3)
  (switch-to-buffer "#kixi")
  (window-number-select 4)
  (switch-to-buffer "#ldnclj")
  (window-number-select 5)
  (switch-to-buffer "*-jabber-roster-*")

  ;; And go to window 1
  (window-number-select 1))

(global-set-key [M-f12] 'captains-chair)

(setq org-default-notes-file (concat org-directory "/refile.org"))

(global-set-key [C-M-f12] 'org-capture)

(setq org-capture-templates
      '(("t" "Doing RIGHT NOW" entry (file+datetree org-default-notes-file)
         "* DOING %?\n%^{Owner}p\n%U\n%a\n" :clock-in t :clock-resume t :empty-lines-after 1)
        ("f" "Do in the Future" entry (file+datetree org-default-notes-file)
         "* TODO %?\n%^{Owner}p\n%U\n%a\n" :empty-lines-after 1)
        ("r" "respond" entry (file+datetree org-default-notes-file)
         "* TODO Respond to %:from on %:subject\nSCHEDULED: %t\n%^{Owner}p\n%U\n%a\n"
         :clock-in t :clock-resume t :empty-lines-after 1)
        ("n" "note" entry (file+datetree org-default-notes-file)
         "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t :empty-lines-after 1)
        ("j" "Journal" entry (file+datetree (concat org-directory "/refile.org"))
         "* %?\n%U\n" :clock-in t :clock-resume t :empty-lines-after 1 :empty-lines-after 1)
        ("m" "Meeting" entry (file+datetree org-default-notes-file)
         "* MEETING with %? :MEETING:\n%^{Owner}p\n%U" :clock-in t :clock-resume t :empty-lines-after 1)
        ("s" "Sit Down" entry (file+datetree org-default-notes-file)
         "* MEETING with Mastodon C :MEETING:\n%^{Owner}p\n%U\n" :clock-in t :clock-resume t :empty-lines-after 1)
        ("p" "Phone call" entry (file+datetree org-default-notes-file)
         "* PHONE %? :PHONE:\n%^{Owner}p\n%U" :clock-in t :clock-resume t :empty-lines-after 1)))

(setq org-refile-targets
      '((nil :maxlevel . 9)
        (org-agenda-files :maxlevel . 9)))

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

(diminish 'auto-revert-mode)
