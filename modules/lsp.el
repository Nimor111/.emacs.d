;;; lsp.el --- Personal configuration for lsp        -*- lexical-binding: t; -*-

;;; Commentary:

;;; Personal configuration for lsp

;;; Code:

;; For your general language needs.
(use-package lsp-mode
  :straight t
  :custom
  (lsp-rust-server 'rust-analyzer)
  (lsp-rust-analyzer-server-command "/usr/bin/rust-analyzer")
  :hook
  ;;(if (fboundp 'rust-mode)
      (rust-mode     . lsp)
  ;;(if (fboundp 'ruby-mode)
      (ruby-mode     . lsp)
  ;;(if (fboundp 'gdscript-mode')
      (gdscript-mode . lsp)
  ;;(if (fboundp 'scala-mode)
      (scala-mode    . lsp)
  :commands lsp
  :config
  (advice-add 'lsp :before #'direnv-update-environment))

(defun lsp--gdscript-ignore-errors (original-function &rest args)
  "Ignore the error message resulting from Godot not replying to the `JSONRPC' request."
  (if (string-equal major-mode "gdscript-mode")
      (let ((json-data (nth 0 args)))
        (if (and (string= (gethash "jsonrpc" json-data "") "2.0")
                 (not (gethash "id" json-data nil))
                 (not (gethash "method" json-data nil)))
            nil ; (message "Method not found")
          (apply original-function args)))
    (apply original-function args)))

;; Runs the function `lsp--gdscript-ignore-errors` around `lsp--get-message-type` to suppress unknown notification errors.
(advice-add #'lsp--get-message-type :around #'lsp--gdscript-ignore-errors)

;; LSP UI

(use-package lsp-ui
  :straight t
  :hook (prog-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-position 'bottom))

(provide 'lsp)
;;; lsp.el ends here
