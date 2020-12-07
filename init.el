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

(use-package use-package-ensure-system-package
  :straight t)

(defun nimor/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/init.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook
  (lambda () (add-hook 'after-save-hook #'nimor/org-babel-tangle-config)))

(setq-default
  custom-file "~/.emacs.d/custom.el")

(when (file-exists-p custom-file)
  (load custom-file t))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

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
  (load-theme 'doom-dracula t)

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
  :straight t)

(global-set-key (kbd "C-c l") 'org-store-link)

(use-package which-key
  :straight t
  :config
  (which-key-mode))

(use-package undo-tree
  :straight t
  :init
  (global-undo-tree-mode))

(setq display-buffer-alist
  `(;; Messages, errors, processes, Calendar in the bottom side window
     (,(rx bos (or "*Emacs Log*"))  ; interaction log *Emacs Log*
       (display-buffer-reuse-window display-buffer-in-side-window)
       (side . right)
       (reusable-frames . visible)
       (window-height . 0.45))
       ;; Let `display-buffer' reuse visible frames for all buffers. This must
       ;; be the last entry in `display-buffer-alist', because it overrides any
       ;; previous entry with more actions.
     ("." nil (reusable-frames . visible))))

(use-package simple
  :hook (before-save . delete-trailing-whitespace))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq user-init-file-org "~/.emacs.d/init.org")
(setq gtd-inbox-file "~/Nextcloud/Orgzly/inbox.org")
(setq gtd-file "~/Nextcloud/Orgzly/gtd.org")
(setq gtd-someday-file "~/Nextcloud/Orgzly/someday.org")
(setq gtd-tickler-file "~/Nextcloud/Orgzly/tickler.org")
(setq daily-file "~/Nextcloud/Orgzly/daily.org")
(setq tech-notebook-file "~/Nextcloud/org/tech_notebook.org")
(setq work-file "~/Nextcloud/org/work/work.org")

;; would love to be able to do it like this but it doesn't work for some reason
(defun nimor/open-file (file-name)
  "Open a specific file"
  (lambda ()
    (interactive)
    (find-file file-name)))

(use-package general
  :straight t
  :config
  (general-evil-setup t)

  ;; general leader key
  (general-create-definer nimor/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  ;; leader key for language specific bindings
  (general-create-definer nimor/language-leader-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC l"
    ;; for insert mode
    :global-prefix "C-SPC l"
    "" '(:ignore t :which-key "language"))

  (nimor/leader-keys
    "b"  'counsel-bookmark
    "SPC" 'find-file
    "/"  'swiper

    "p"  '(:ignore t :which-key "file")
    "pf"  (list (lambda () (interactive) (find-file user-init-file-org)) :which-key "config")

    "e"  'mu4e

    "g"  '(:ignore t :which-key "gtd")
    "gi"  (list (lambda () (interactive) (find-file gtd-inbox-file))   :which-key "inbox")
    "gg"  (list (lambda () (interactive) (find-file gtd-file))         :which-key "gtd")
    "gs"  (list (lambda () (interactive) (find-file gtd-someday-file)) :which-key "someday")
    "gt"  (list (lambda () (interactive) (find-file gtd-tickler-file)) :which-key "tickler")

    "fd"  (list (lambda () (interactive) (find-file daily-file)) :which-key "daily")

    "fw"  (list (lambda () (interactive) (find-file work-file)) :which-key "work")

    "o"   '(:ignore t :which-key "org")
    "oc"  'org-capture
    "oa"  'org-agenda
    "op"  'org-pomodoro
    "or"  'org-refile
    "os"  'org-archive-subtree
    "ok"  '(:ignore t :which-key "kanban")
    "oki" 'org-kanban/initialize-at-end
    "oks" 'org-kanban/shift

    "ot" '(:ignore t :which-key "timestamp")
    "otu" 'org-timestamp-up-day
    "otd" 'org-timestamp-down-day

    "tn" (list (lambda () (interactive) (find-file tech-notebook-file)) :which-key "tech-notebook")

    "m" '(:ignore t :which-key "todo")
    "mt" 'org-todo
    "ms" 'org-schedule

    "w" '(:ignore t :which-key "window")
    "wh" 'evil-window-left
    "wl" 'evil-window-right
    "wk" 'evil-window-up
    "wj" 'evil-window-down

    "hd" 'howdoyou-query

    "x"   '(:ignore t :which-key "buffer")
    "xk"  'kill-buffer
    "xs"  '(:ignore t :which-key "split-window")
    "xsr" 'split-window-right
    "xsb" 'split-window-below

    "ie"  'emojify-insert-emoji))

(defun nimor/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :straight t
  :hook (org-mode . nimor/org-mode-visual-fill))

;; dependency
(use-package all-the-icons
  :straight t)

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1)
  :config
  ;; Whether display the mu4e notifications. It requires `mu4e-alert' package.
  (setq doom-modeline-mu4e t))

(use-package dashboard
  :straight t
  :config
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)))
  (dashboard-setup-startup-hook))

(use-package evil
  :straight t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (evil-set-undo-system 'undo-tree)
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line))

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
  (define-key evil-ex-map "e" 'counsel-find-file))

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

(defun nimor/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1))

(use-package org
  :straight t
  :hook
  (org-mode . nimor/org-mode-setup)
  :config
  ;; make org-agenda respect evil
  (evil-set-initial-state 'org-agenda-mode 'normal)

  ;; org-agenda custom bindings
  (evil-define-key 'normal org-agenda-mode-map
    "vd" 'org-agenda-day-view
    "vw" 'org-agenda-week-view
    "I"  'org-agenda-clock-in
    "O"  'org-agenda-clock-out)

  ;; files that org-agenda will read from
  (setq org-agenda-files
  '("~/Nextcloud/Orgzly/gtd.org"
    "~/Nextcloud/Orgzly/tickler.org"
    "~/Nextcloud/Orgzly/inbox.org"
    "~/Nextcloud/Orgzly/daily.org"
    "~/Nextcloud/org/work/work.org"))

  ;; org-agenda custom views
  (setq org-agenda-custom-commands
    '(("g" "GTD" tags-todo "@gtd"
      ((org-agenda-overriding-header "GTD")))))

  ;; files to refile to
  (setq org-refile-targets
    '(("~/Nextcloud/Orgzly/gtd.org"     :maxlevel . 9)
      ("~/Nextcloud/Orgzly/someday.org" :maxlevel . 9)
      ("~/Nextcloud/Orgzly/tickler.org" :maxlevel . 9)))

  ;; quick templates for org files
  (setq org-capture-templates
    '(("t" "Todo [inbox]" entry
      (file+headline "~/Nextcloud/Orgzly/inbox.org" "Tasks")
     "* TODO %i%?")
      ("T" "Tickler" entry
      (file+headline "~/Nextcloud/Orgzly/tickler.org" "Tickler")
      "* TODO %i%? \n SCHEDULED: %T")
      ("M" "Mail Todo with link" entry
      (file+headline "~/Nextcloud/Orgzly/inbox.org" "Tasks")
      "* TODO %i%? \n:PROPERTIES: \n:CREATED: %U \n:END: \n %a\n")
      ("W" "Finnish word of the day" entry
      (file+headline "~/Nextcloud/Orgzly/inbox.org" "Tasks")
      "* TODO Word of the day - %t \n:PROPERTIES: \n:CREATED: %U \n:END: \n %a\n")
      ("d" "Todo [daily]" entry
        (file+olp+datetree "~/Nextcloud/Orgzly/daily.org")
        "* TODO %i%? \n SCHEDULED: %t")
      ("D" "Todo with link [daily]" entry
        (file+olp+datetree "~/Nextcloud/Orgzly/daily.org")
        "* TODO %a \n SCHEDULED: %t")))

  ;; TODO keywords that I use - the ones after the | are the done states
  (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "IN PROGRESS(p)" "|" "DONE(d)" "CANCELLED(c)")))

  ;; Clocking settings
  (setq org-pretty-entities t)

  ;; indentation settings and other misc stuff
  (setq org-startup-indented t)
  (setq org-startup-folded t)
  (setq org-log-done 'note)
  (setq org-tags-column 0)
  (setq org-agenda-tags-column 0)
  (setq org-src-fontify-natively t
        org-src-preserve-indentation t ;; do not put two spaces on the left
        org-src-tab-acts-natively t)

  (setq yt-iframe-format
    ;; You may want to change your width and height.
    (concat "<iframe width=\"440\""
            " height=\"335\""
            " src=\"https://www.youtube.com/embed/%s\""
            " frameborder=\"0\""
            " allowfullscreen>%s</iframe>"))

  (org-add-link-type
   "yt"
   (lambda (handle)
     (browse-url
      (concat "https://www.youtube.com/embed/"
              handle)))
   (lambda (path desc backend)
     (cl-case backend
       (html (format yt-iframe-format
                   path (or desc "")))
       (latex (format "\href{%s}{%s}"
                    path (or desc "video")))))))

(use-package org-superstar
  :straight t
  :after org
  :hook (org-mode . org-superstar-mode))

(use-package org-tempo
  :after org)

(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode 0)))

(use-package org-roam
  :straight
  (:host github :repo "jethrokuan/org-roam" :branch "master")
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/Nextcloud/org-roam")
  :config
  (setq org-roam-directory "~/Nextcloud/org-roam")
  (nimor/leader-keys
    "n" '(:ignore t :which-key "org-roam")
    "nl" 'org-roam
    "nf" 'org-roam-find-file
    "ng" 'org-roam-graph-show
    "ni" 'org-roam-insert
    "nI" 'org-roam-insert-immediate
    "no" 'org-roam-dailies-capture-today)

  (setq org-roam-completion-everywhere t)

  (setq org-roam-dailies-directory "daily/")

  (setq org-roam-dailies-capture-templates
    '(("d" "daily" entry
       #'org-roam-capture--get-point
       "* %<%H:%M> %?"
       :file-name "daily/%<%Y-%m-%d>"
       :head "#+title: %<%Y-%m-%d>\n"
       :olp ("Daily notes"))

      ("j" "journal" entry
       #'org-roam-capture--get-point
       "* %<%H:%M> %?"
       :file-name "daily/%<%Y-%m-%d>"
       :head "#+title: %<%Y-%m-%d>\n"
       :olp ("Journal")))))

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

(use-package toc-org
  :straight t
  :after org
  :hook
  (org-mode . toc-org-enable))

;; TODO Remove this as it's in org roam now
(use-package org-journal
  :straight t
  :after org
  :config
  (setq org-journal-dir "~/Documents/journal")
  (setq org-journal-date-format "%A, %d %B %Y")
  (nimor/leader-keys
    "nj" 'org-journal-new-entry))

(use-package ob
  :defer t
  :after org
  :config
  (org-babel-do-load-languages
    'org-babel-load-languages
      '((python . t)
        (shell  . t)
      )))

(use-package alert
  :straight t
  :defer t
  :config
  (setq alert-default-style
    (if (eq system-type 'gnu/linux)
        'notifications
        'notifier)))

(use-package org-wild-notifier
  :straight t
  :after org
  :config
  (org-wild-notifier-mode 1)
  (setq org-wild-notifier-alert-time '(10 0)))

(use-package org-pomodoro
  :straight t
  :after org
  :config
  (setq org-pomodoro-finished-sound (concat user-emacs-directory "/eraser.mp3"))
  (setq org-pomodoro-short-break-sound (concat user-emacs-directory "/eraser.mp3"))
  (setq org-pomodoro-long-break-sound (concat user-emacs-directory "/eraser.mp3")))

(use-package org-kanban
  :straight t
  :after org)

(use-package magit
  :straight t
  :defer t)

(setq-default with-editor-emacsclient-executable "emacsclient")

;; (use-package helm
;;   :init
;;     (require 'helm-config)
;;     (setq helm-split-window-in-side-p t
;;           helm-move-to-line-cycle-in-source t)
;;   :config
;;     (helm-mode 1) ;; Most of Emacs prompts become helm-enabled
;;     (helm-autoresize-mode 1) ;; Helm resizes according to the number of candidates
;;     (global-set-key (kbd "C-x b") 'helm-buffers-list) ;; List buffers ( Emacs way )
;;     (define-key evil-ex-map "b" 'helm-buffers-list) ;; List buffers ( Vim way )
;;     (global-set-key (kbd "C-x r b") 'helm-bookmarks) ;; Bookmarks menu
;;     (global-set-key (kbd "C-x C-f") 'helm-find-files) ;; Finding files with Helm
;;     (global-set-key (kbd "M-c") 'helm-calcul-expression) ;; Use Helm for calculations
;;     (global-set-key (kbd "C-s") 'helm-occur)  ;; Replaces the default isearch keybinding
;;     (global-set-key (kbd "C-h a") 'helm-apropos)  ;; Helmized apropos interface
;;     (global-set-key (kbd "M-x") 'helm-M-x)  ;; Improved M-x menu
;;     (global-set-key (kbd "M-y") 'helm-show-kill-ring)  ;; Show kill ring, pick something to paste
;;   :straight t)

;; (use-package helm-lsp
;;   :straight t
;;   :commands helm-lsp-workspace-symbol)

;; (use-package helm-projectile
;;   :straight t)

(use-package counsel
  :straight t
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1))

(use-package counsel-projectile
  :straight t
  :config
  (counsel-projectile-mode 1))

(use-package ivy
  :straight t
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  ;;(define-key evil-ex-map "b" 'ivy-switch-buffer) ;; List buffers ( Vim way )
  (define-key evil-ex-map "b" 'counsel-ibuffer) ;; List buffers ( Vim way )
  ;; Press M-o when inside the ivy minibuffer for the actions to show
  (ivy-set-actions
    'counsel-find-file
    '(("d" delete-file "delete")))
  (ivy-mode 1))

(use-package ivy-rich
  :straight t
  :init
  (ivy-rich-mode 1))

(use-package company
  :straight t
  :hook
  (after-init . global-company-mode))

(use-package company-lua
  :straight t
  :after (company)
  :hook (lua-mode my-lua-mode-company-init))

;; (use-package yasnippet
;;   :straight t
;;   :custom
;;   (yas-also-auto-indent-first-line t)
;;   (yas-also-indent-empty-lines t)
;;   :config
;;   (yas-global-mode t))

;; (use-package yasnippet-snippets
;;   :straight t)

(use-package howdoyou
  :straight t
  :defer t)

(use-package web-mode
  :straight t
  :hook (web-mode . emmet-mode)
  :mode
  ("\\.erb\\'" . web-mode)
  ("\\.html?\\'" . web-mode)
  :config
  (setq web-mode-engines-alist '(("django" . "\\.html\\'"))))

(use-package emmet-mode
  :straight t)

(use-package lsp-mode
  :straight t
  :custom
  (lsp-rust-server 'rust-analyzer)
  (lsp-rust-analyzer-server-command "/usr/bin/rust-analyzer")
  :hook
  (rust-mode     . lsp)
  (ruby-mode     . lsp)
  (gdscript-mode . lsp)
  (scala-mode    . lsp)
  :commands lsp)

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode)

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

(use-package scala-mode
  :straight t
  :mode "\\.s\\(cala\\|bt\\)$"
  :config
  (load-file (concat user-emacs-directory "/lisp/ob-scala.el"))) ;; for org babel evaluating of scala code blocks

(use-package lsp-metals
  :straight t
  :defer t
  :config (setq lsp-metals-treeview-show-when-views-received t))

(use-package gdscript-mode
  :ensure-system-package godot
  :straight
  (gdscript-mode
     :type git
     :host github
     :repo "GDQuest/emacs-gdscript-mode")
  :defer t)

(use-package elpy
  :straight t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

(nimor/language-leader-def
  "p" '(:ignore t :which-key "python")
  "pf" 'elpy-autopep8-fix-code)

(use-package cider
  :straight t)

(use-package projectile
  :straight t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-track-known-projects-automatically nil)
  (projectile-mode +1))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :straight t
  :config
  (smartparens-global-mode)
  (show-smartparens-global-mode))

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

(use-package helpful
  :straight t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; dependency of elpl
(use-package edit-indirect
  :straight t)

(use-package elpl
  :straight t
  :config
  (nimor/leader-keys
    "rl" 'elpl-clean
    "re" 'elpl-edit))

(use-package link-hint
  :straight t
  :config
  (nimor/leader-keys
    "fo" 'link-hint-open-link))

(if (eq system-type 'gnu/linux)
    (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e"))

(use-package mu4e
  :if (eq system-type 'gnu/linux)
  :ensure-system-package mu
  :config
  (setq mu4e-sent-messages-behaviour 'delete)
  (setq mu4e-get-mail-command "/usr/bin/mbsync -Va")
  (setq mu4e-change-filenames-when-moving t)
  (setq mu4e-update-interval 60)
  (setq mu4e-use-fancy-chars t)
  (setq mu4e-view-show-addresses t)
  (setq mu4e-view-show-images t)
  (add-to-list 'mu4e-view-actions '("view in browser" . mu4e-action-view-in-browser))
  (setq mu4e-contexts
    `( ,(make-mu4e-context
          :name "Gmail"
          :enter-func (lambda () (mu4e-message "Entering Gmail context"))
          :match-func (lambda (msg)
                        (when msg
                          (string-match-p "^/gmail" (mu4e-message-field msg :maildir))))
          :vars '( ( user-mail-address        . "nimor784@gmail.com" )
                   ( user-full-name           . "Georgi Bozhinov")
                   ( mu4e-sent-folder         . "/gmail/[Gmail]/Sent Mail")
                   ( mu4e-trash-folder        . "/gmail/[Gmail]/Trash")
                   ( mu4e-drafts-folder       . "/gmail/[Gmail]/Drafts")
                   (smtpmail-smtp-server      . "smtp.gmail.com")
                   (smtpmail-smtp-service     . 587)
                   (smtpmail-stream-type      . starttls)
                   (smtpmail-debug-info       . t)))
       ,(make-mu4e-context
          :name "Outlook"
          :enter-func (lambda () (mu4e-message "Entering Outlook context"))
          :match-func (lambda (msg)
                        (when msg
                          (string-match-p "^/outlook" (mu4e-message-field msg :maildir))))
          :vars '( ( user-mail-address    . "georgi.bojinov@hotmail.com" )
                   ( user-full-name       . "Georgi Bozhinov")
                   ( mu4e-sent-folder     . "/outlook/Sent")
                   ( mu4e-trash-folder    . "/outlook/Deleted")
                   ( mu4e-drafts-folder   . "/outlook/Drafts")
                   (smtpmail-smtp-server  . "smtp.office365.com")
                   (smtpmail-smtp-service . 587)
                   (smtpmail-stream-type  . starttls)
                   (smtp-debug-info       . t)))
  ))
  (setq mu4e-context-policy 'pick-first)
  (require 'org-mu4e)

;; do not put a trashed flag on messages moved to deleted because then mu4e will delete them forever
  (setf (alist-get 'trash mu4e-marks)
        (list :char '("d" . "▼")
              :prompt "dtrash"
              :dyn-target (lambda (target msg)
                          (mu4e-get-trash-folder msg))
              :action (lambda (docid msg target)
                      ;; Here's the main difference to the regular trash mark,
                      ;; no +T before -N so the message is not marked as
                      ;; IMAP-deleted:
                      (mu4e~proc-move docid (mu4e~mark-check-target target) "-N"))))

  (mu4e t))

;; Configure desktop notifs for incoming emails:
(when (eq system-type 'gnu/linux)
  (use-package mu4e-alert
    :straight t
    :after mu4e
    :hook
    ((after-init . mu4e-alert-enable-mode-line-display)
     (after-init . mu4e-alert-enable-notifications))
    :config
    (mu4e-alert-set-default-style 'libnotify)))

(use-package persistent-scratch
  :straight t
  :config
  (persistent-scratch-setup-default))

(use-package esup
  :straight t
  :config
  ;; don't try to follow symlinks in straight.el repos
  (setq esup-depth 0)
  (setq esup-user-init-file (file-truename "~/.emacs.d/init.el")))

(use-package emms
  :straight t
  :config
  (emms-all)
  (emms-default-players))
