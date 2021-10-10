;;; web.el --- Personal configuration for html and css files  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Configuration for tools for working with web related files

;;; Code:

;; Add emmet for tag completion and stuff

(use-package emmet-mode
  :straight t)

;; For different types of templates and web markups and the like
(use-package web-mode
  :straight t
  :hook (web-mode . emmet-mode)
  :mode
  ("\\.erb\\'" . web-mode)
  ("\\.html?\\'" . web-mode)
  :config
  (setq web-mode-engines-alist '(("django" . "\\.html\\'"))))

;; Impatient mode for live preview of html

(use-package impatient-mode
  :straight t)


(provide 'web)
;;; web.el ends here
