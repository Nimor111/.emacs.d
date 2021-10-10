;;; scala.el --- Personal configuration for scala development  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Personal configuration for scala development

;;; Code:

(use-package scala-mode
  :straight t
  :mode "\\.s\\(cala\\|bt\\)$"
  :config
  (load-file (concat user-emacs-directory "/lisp/ob-scala.el"))) ;; for org babel evaluating of scala code blocks

(use-package lsp-metals
  :straight t
  :defer t
  :config (setq lsp-metals-treeview-show-when-views-received t))

;; sbt
(use-package sbt-mode
  :straight t
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

;; Ammonite
(use-package ob-ammonite
  :straight t
  :config
  (setq ammonite-term-repl-auto-detect-predef-file nil)
  (setq ammonite-term-repl-program-args '("--no-default-predef" "--no-home-predef")))

(provide 'scala)
;;; scala.el ends here
