;;; rust.el --- Personal configuration for Rust development  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Configuration for tools for working with rust

;;; Code:

(use-package rust-mode
  :straight t
  :config
  (setq rust-format-on-save t))

(provide 'rust)
;;; rust.el ends here
