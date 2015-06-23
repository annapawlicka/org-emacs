;;; refheap-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "refheap" "refheap.el" (21895 65389 0 0))
;;; Generated autoloads from refheap.el

(autoload 'refheap-paste-region "refheap" "\
Paste the current region to refheap. With prefix arg, paste privately.

\(fn BEGIN END &optional PRIVATE)" t nil)

(autoload 'refheap-paste-region-private "refheap" "\
Paste the current region to a private refheap entry.

\(fn BEGIN END)" t nil)

(autoload 'refheap-paste-buffer "refheap" "\
Paste the current buffer to refheap. With prefix arg, paste privately.

\(fn &optional PRIVATE)" t nil)

(autoload 'refheap-paste-buffer-private "refheap" "\
Paste the current buffer to a private refheap entry.

\(fn)" t nil)

(autoload 'refheap-paste-region-or-buffer "refheap" "\
Paste the current region (or buffer, if no region is active) to refheap.
With prefix arg, paste privately.

\(fn &optional PRIVATE)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; refheap-autoloads.el ends here
