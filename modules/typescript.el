;;; typescript.el --- Personal configuration for typescript development  -*- lexical-binding: t; -*-

;;; Commentary:

;;;  Personal configuration for typescript development

;;; Code:

(use-package typescript-mode
  :straight t
  :mode
  ("\\.ts\\'" . typescript-mode))

(use-package tide
  :straight t
  :after (typescript-mode company)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(provide 'typescript)
;;; typescript.el ends here
