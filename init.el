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
  (set-frame-font "JetBrainsMono 18"))

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
    (setq org-agenda-tags-column 0))

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

(use-package ox-hugo
  :straight t
  :after ox)

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

;; FIXME this is deprecated and now inside org-roam
(use-package company-org-roam
  :straight (:host github :repo "org-roam/company-org-roam")
  :config
  (push 'company-org-roam company-backends))

(use-package toc-org
  :straight t
  :hook
  (org-mode . toc-org-mode))

(use-package howdoyou
  :straight t)

;; [[file:init.org::*Custom set variables][Custom set variables:1]]
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(safe-local-variable-values
     (quote
       ((eval add-hook
	     (quote after-save-hook)
	     (lambda nil
	       (org-babel-tangle))
	     nil t)))))
;; Custom set variables:1 ends here
