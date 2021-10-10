;;; elixir.el --- Personal configuration for Elixir development  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Personal configuration for Elixir development

;;; Code:

;; Elixir mode for basic interactions
(use-package elixir-mode
  :straight t)

;; Alchemist
(use-package alchemist
  :straight t)

;; Interaction with mix
(use-package mix
  :straight t)

(provide 'elixir)
;;; elixir.el ends here
