This is a replacement for the ff-find-other-file.  If the helm is loaded,
uses it to provide possible multiple choices; otherwise just picks the
first one.

Usage:
Add the following to your .emacs file:

(require 'cff)
defines shortcut for find source/header file for the current
file
(add-hook 'c++-mode-hook
          '(lambda ()
             (define-key c-mode-base-map (kbd "M-o") 'cff-find-other-file)))
(add-hook 'c-mode-hook
          '(lambda ()
             (define-key c-mode-base-map (kbd "M-o") 'cff-find-other-file)))
