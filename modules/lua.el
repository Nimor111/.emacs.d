;;; lua.el --- Personal configuration for working with Lua files  -*- lexical-binding: t; -*-

;;; Commentary:

;; Personal configuration for working with Lua files.  Also includes Love2d and Fennel configuration.

;;; Code:

(use-package lua-mode
  :straight t
  :config
  (setq lua-indent-level 2))

(use-package love-minor-mode
  :straight t
  :hook (lua-mode . love-minor-mode)
  :config
  (setq love-exe "/usr/bin/love"))

;; Does not work for some reason
;;(use-package fennel-mode
;;  :straight t)

(autoload 'fennel-mode (concat user-emacs-directory "lisp/fennel-mode/fennel-mode") nil t)
(add-to-list 'auto-mode-alist '("\\.fnl\\'" . fennel-mode))

(provide 'lua)
;;; lua.el ends here
