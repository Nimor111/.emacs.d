;;; clojure.el --- Personal configuration for clojure development -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; Configuration for tools for working with clojure
;;;
;;; Code:

(use-package cider
  :straight t)

;; Flycheck clj-kondo for linting
(use-package flycheck-clj-kondo
  :straight t)

;; Use it in clojure mode
(use-package clojure-mode
  :straight t
  :config
  (require 'flycheck-clj-kondo))

;; Handy functions for refactoring clojure code
(use-package clj-refactor
  :straight t)

(defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import statements
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

(provide 'clojure)
;;; clojure.el ends here
