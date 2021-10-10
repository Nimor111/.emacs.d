;;; haskell.el --- Personal configuration for haskell development  -*- lexical-binding: t; -*-

;;; Commentary:

;; Personal configuration for haskell development

;;; Code:

(use-package haskell-mode
  :straight t
  :config
  (add-hook 'haskell-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 (append '((company-capf company-dabbrev-code))
                         company-backends))))
  (setq haskell-stylish-on-save t)

 ;; This changes the evil "O" and "o" keys for haskell-mode to make sure that
 ;; indentation is done correctly. See
 ;; https://github.com/haskell/haskell-mode/issues/1265#issuecomment-252492026.
  (defun haskell-evil-open-above ()
    (interactive)
    (evil-digit-argument-or-evil-beginning-of-line)
    (haskell-indentation-newline-and-indent)
    (evil-previous-line)
    (haskell-indentation-indent-line)
    (evil-append-line nil))

  (defun haskell-evil-open-below ()
    (interactive)
    (evil-append-line nil)
    (haskell-indentation-newline-and-indent))

  (evil-define-key 'normal haskell-mode-map
    "o" 'haskell-evil-open-below
    "O" 'haskell-evil-open-above))

(provide 'haskell)
;;; haskell.el ends here
