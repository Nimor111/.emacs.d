;;; gdscript.el --- Personal configuration for gdscript development  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Personal configuration for gdscript development

;;; Code:

(use-package gdscript-mode
  :ensure-system-package godot
  :straight
  (gdscript-mode
     :type git
     :host github
     :repo "GDQuest/emacs-gdscript-mode")
  :defer t)

(provide 'gdscript)
;;; gdscript.el ends here
