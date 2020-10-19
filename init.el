;; [[file:init.org::*Custom variables][Custom variables:1]]
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("aaf300544667970333366f2bad847899f193fcfe96172ec325dbc3195b797220" default))
  '(safe-local-variable-values
     (quote
       ((eval add-hook
	     (quote after-save-hook)
	     (lambda nil
	       (org-babel-tangle))
	     nil t)))))
;; Custom variables:1 ends here

(defvar bootstrap-version)
(let ((bootstrap-file
        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
    (bootstrap-version 5))
(unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
    (goto-char (point-max))
    (eval-print-last-sexp)))
(load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(let* ((home-dir (getenv "HOME"))
     (custom-emacs-directory (concat home-dir "/.emacs.d")))
  (setq user-emacs-directory custom-emacs-directory))

(if (eq system-type 'gnu/linux)
  (set-frame-font "Jet Brains Mono 18")
  (set-frame-font "JetBrains Mono 18"))

(use-package doom-themes
  :straight t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(display-battery-mode t)

(setq display-time-format "%H:%M %a,%d %b %Y")
(display-time)

(setq-default indent-tabs-mode nil)

(use-package evil-nerd-commenter
  :straight t
  :config
  (evilnc-default-hotkeys))

(use-package command-log-mode
  :straight t
  :config
  (global-command-log-mode))

(global-set-key (kbd "C-c l") 'org-store-link)

;; (use-package smart-mode-line-atom-one-dark-theme
;;   :straight t)

;; ;; blacklist removes minor modes from the mode line that don't contribute any info
;; (use-package smart-mode-line
;;   :straight t
;;   :config
;;   (setq sml/theme 'atom-one-dark)
;;   (setq sml/short-directory t
;;         sml/shorten-modes t)
;;   ;; (setq sml/name-width 40
;;         ;; sml/mode-width "full")
;;   (setq rm-blacklist
;;       (format "^ \\(%s\\)$"
;;               (mapconcat #'identity
;;                          '("Fly.*" "Projectile.*" "Helm" "Org-roam" "Undo-Tree" "company" "yas")
;;                          "\\|")))
;;   (sml/setup))

;; dependency
(use-package all-the-icons
  :straight t)

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1))

(use-package evil
  :straight t
  :config
  (evil-mode 1)
  (evil-define-key 'normal 'global
    (kbd "C-h") 'evil-window-left
    (kbd "C-l") 'evil-window-right
    (kbd "C-k") 'evil-window-up
    (kbd "C-j") 'evil-window-down))

(use-package evil-org
  :straight t
  :after (evil org)
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme '(navigation insert textobjects additional calendar))))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (define-key evil-ex-map "e" 'find-file))

(use-package org
    :straight t
    :hook
    (org-mode . visual-line-mode)
    :config
    (require 'org-tempo)
    (eval-after-load 'org-agenda
    '(progn
        (evil-set-initial-state 'org-agenda-mode 'normal)
        (evil-define-key 'normal org-agenda-mode-map
          "vd" 'org-agenda-day-view
          "vw" 'org-agenda-week-view
          "I"  'org-agenda-clock-in
          "O"  'org-agenda-clock-out)))
    (setq org-refile-targets
        '(("~/Nextcloud/Orgzly/gtd.org" :maxlevel . 3)
          ("~/Nextcloud/Orgzly/someday.org" :level . 1)
          ("~/Nextcloud/Orgzly/tickler.org" :maxlevel . 2)))
    (setq org-capture-templates
        '(("t" "Todo [inbox]" entry
          (file+headline "~/Nextcloud/Orgzly/inbox.org" "Tasks")
          "* TODO %i%?")
          ("T" "Tickler" entry
          (file+headline "~/Nextcloud/Orgzly/tickler.org" "Tickler")
          "* TODO %i%? \n SCHEDULED: %T")))
    (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
    (setq org-agenda-files
    '("~/Nextcloud/Orgzly/gtd.org"
      "~/Nextcloud/Orgzly/tickler.org"
      "~/Nextcloud/Orgzly/inbox.org"
      "~/Nextcloud/org/work"))

    (setq org-startup-indented t)
    (setq org-startup-folded t)
    (setq org-indent-mode t)
    (setq org-log-done 'note)
    (setq org-tags-column 0)
    (setq org-agenda-tags-column 0)
    (setq org-src-preserve-indentation nil)
    (setq org-edit-src-content-indentation 0))

(use-package org-superstar
  :straight t
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

(use-package org-roam
  :straight t
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/Nextcloud/org-roam")
  :bind (:map org-roam-mode-map
    (("C-c n l" . org-roam)
     ("C-c n f" . org-roam-find-file)
     ("C-c n g" . org-roam-graph-show))
    :map org-mode-map
    (("C-c n i" . org-roam-insert))
    (("C-c n I" . org-roam-insert-immediate))))

(use-package deft
  :straight t
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/Nextcloud/org-roam"))

(use-package ox-hugo
  :straight t
  :after ox)

(use-package org-books
  :straight t
  :after org
  :config
  (setq org-books-file "~/Nextcloud/org/reading_list.org"))

(defun org-advance ()
  (interactive)
  (when (buffer-narrowed-p)
    (beginning-of-buffer)
    (widen)
    (org-forward-heading-same-level 1))
  (org-narrow-to-subtree))

(defun org-retreat ()
  (interactive)
  (when (buffer-narrowed-p)
    (beginning-of-buffer)
    (widen)
    (org-backward-heading-same-level 1))
  (org-narrow-to-subtre))

;;(org-reload)

(use-package magit
  :straight t)

(setq-default with-editor-emacsclient-executable "emacsclient")

(use-package helm
  :init
    (require 'helm-config)
    (setq helm-split-window-in-side-p t
          helm-move-to-line-cycle-in-source t)
  :config
    (helm-mode 1) ;; Most of Emacs prompts become helm-enabled
    (helm-autoresize-mode 1) ;; Helm resizes according to the number of candidates
    (global-set-key (kbd "C-x b") 'helm-buffers-list) ;; List buffers ( Emacs way )
    (define-key evil-ex-map "b" 'helm-buffers-list) ;; List buffers ( Vim way )
    (global-set-key (kbd "C-x r b") 'helm-bookmarks) ;; Bookmarks menu
    (global-set-key (kbd "C-x C-f") 'helm-find-files) ;; Finding files with Helm
    (global-set-key (kbd "M-c") 'helm-calcul-expression) ;; Use Helm for calculations
    (global-set-key (kbd "C-s") 'helm-occur)  ;; Replaces the default isearch keybinding
    (global-set-key (kbd "C-h a") 'helm-apropos)  ;; Helmized apropos interface
    (global-set-key (kbd "M-x") 'helm-M-x)  ;; Improved M-x menu
    (global-set-key (kbd "M-y") 'helm-show-kill-ring)  ;; Show kill ring, pick something to paste
  :straight t)

(use-package ranger
  :straight t
  :config
  (setq ranger-show-hidden t)
  (setq ranger-cleanup-on-disable t))

(use-package company
  :straight t
  :hook
  (after-init . global-company-mode))

(use-package company-lua
  :straight t
  :after (company)
  :hook (lua-mode my-lua-mode-company-init))

;; FIXME this is deprecated and now inside org-roam
(use-package company-org-roam
  :straight (:host github :repo "org-roam/company-org-roam")
  :config
  (push 'company-org-roam company-backends))

(use-package yasnippet
  :straight t
  :demand t
  :custom
  (yas-also-auto-indent-first-line t)
  (yas-also-indent-empty-lines t)
  :config
  (yas-global-mode t))

(use-package yasnippet-snippets
  :straight t)

(use-package toc-org
  :straight t
  :hook
  (org-mode . toc-org-mode))

(use-package howdoyou
  :straight t)

(use-package web-mode
  :straight t
  :hook (web-mode . emmet-mode)
  :mode
  ("\\.erb\\'" . web-mode)
  ("\\.html?\\'" . web-mode))

(use-package emmet-mode
  :straight t)

(use-package lsp-mode
  :straight t
  :custom
  (lsp-rust-server 'rust-analyzer)
  (lsp-rust-analyzer-server-command "/usr/bin/rust-analyzer")
  :hook
  (rust-mode . lsp)
  (ruby-mode . lsp)
  :commands lsp)

(use-package helm-lsp
  :straight t
  :commands helm-lsp-workspace-symbol)

(use-package lsp-ui 
  :straight t
  :commands lsp-ui-mode)

(use-package rust-mode
  :straight t
  :config
  (setq rust-format-on-save t))

(use-package glsl-mode
  :straight t)

(defun set-company-backends-for-lua()
  "Set lua company backend."
  (setq-local company-backends '(
                                 (
                                  company-lsp
                                  company-lua
                                  company-keywords
                                  company-gtags
                                  company-yasnippet
                                  )
                                 company-capf
                                 company-dabbrev-code
                                 company-files
                                 )))

(use-package lua-mode
  :straight t
  :hook (lua-mode . set-company-backends-for-lua))

(use-package love-minor-mode
  :straight t
  :hook (lua-mode . love-minor-mode)
  :config
  (setq love-exe "/usr/bin/love"))

(use-package projectile-rails
  :straight t
  :config
  (projectile-rails-global-mode))

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

(use-package projectile
  :straight t
  :config 
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-track-known-projects-automatically nil)
  (projectile-mode +1))

(use-package helm-projectile
  :straight t)

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package flycheck
  :straight t
  :init (global-flycheck-mode))

(use-package flycheck-pos-tip
  :straight t
  :after flycheck
  :hook (flycheck-mode . flycheck-pos-tip-mode))

(use-package emojify
  :straight t
  :init (global-emojify-mode))
