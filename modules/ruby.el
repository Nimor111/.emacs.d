;;; ruby.el --- Personal configuration for Ruby development  -*- lexical-binding: t; -*-

;;; Commentary:

;; Personal configuration for Ruby development.  Mainly includes rails stuff.

;;; Code:

(use-package projectile-rails
  :straight t
  :config
  (projectile-rails-global-mode))

(provide 'ruby)
;;; ruby.el ends here
