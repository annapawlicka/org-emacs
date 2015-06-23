;;; org-magit-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "org-magit" "org-magit.el" (21895 65472 0 0))
;;; Generated autoloads from org-magit.el

(autoload 'org-magit-open "org-magit" "\


\(fn STR)" nil nil)

(autoload 'org-magit-export "org-magit" "\


\(fn PATH DESC FORMAT)" nil nil)

(autoload 'org-magit-store-link "org-magit" "\


\(fn)" nil nil)

(eval-after-load "org" '(progn (org-add-link-type "magit" 'org-magit-open 'org-magit-export) (add-hook 'org-store-link-functions 'org-magit-store-link)))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; org-magit-autoloads.el ends here
