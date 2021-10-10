;;; python.el ---Personal configuration for Python development  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Personal configuration for Python development.  Also includes Hy.

;;; Code:

(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(use-package company-jedi
  :straight t
  :hook ((python-mode . jedi:setup)
         (python-mode . my/python-mode-hook)))

(use-package virtualenvwrapper
  :straight t
  :defer t
  :init
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))

(my/language-leader-def
  "p" '(:ignore t :which-key "python")
  "pf" 'elpy-autopep8-fix-code)

;; poetry
(use-package poetry
  :straight t)

;; hy
(use-package hy-mode
  :straight t)

(provide 'python)
;;; python.el ends here
