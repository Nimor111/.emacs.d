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

(setq gtd-table-file "~/Nextcloud/org/gtd-notion/gtd.org")

(defun my/org-babel-tangle-config ()
  "Tangles the org config file to init.el"
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/init.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(defun my/gtd-update-dblocks ()
  "Updates the org-columns dynamic blocks in the gtd file"
  (interactive)
  (when (string-equal (buffer-file-name)
                      (expand-file-name gtd-table-file))
    (progn
      (org-update-all-dblocks))))

(add-hook 'org-mode-hook
  (lambda () (add-hook 'after-save-hook #'my/org-babel-tangle-config)))

(add-hook 'org-mode-hook
  (lambda () (add-hook 'before-save-hook #'my/gtd-update-dblocks)))

(setq-default
  custom-file "~/.emacs.d/custom.el")

(when (file-exists-p custom-file)
  (load custom-file t))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(let* ((home-dir (getenv "HOME"))
     (custom-emacs-directory (concat home-dir "/.emacs.d")))
  (setq user-emacs-directory custom-emacs-directory))

;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
;(setq user-emacs-directory "~/.cache/emacs")

(use-package no-littering
  :straight t)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(if (eq system-type 'gnu/linux)
  (set-frame-font "Jet Brains Mono 18")
  (set-frame-font "JetBrains Mono 18"))

;; (use-package doom-themes
;;   :straight t
;;   :config
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   (load-theme 'doom-dracula t)

;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)

;;   ;; Enable custom neotree theme (all-the-icons must be installed!)
;;   (doom-themes-neotree-config)
;;   ;; or for treemacs users
;;   (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
;;   (doom-themes-treemacs-config)

;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))

(use-package challenger-deep-theme
  :straight t
  :config
  (load-theme 'challenger-deep t))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(display-battery-mode t)

(setq display-time-format "%H:%M %a,%d %b %Y")
(setq display-time-default-load-average nil)
(display-time)

(setq-default indent-tabs-mode nil)

(use-package evil-nerd-commenter
  :straight t
  :config
  (evilnc-default-hotkeys))

(use-package command-log-mode
  :straight t)

(use-package which-key
  :straight t
  :config
  (which-key-mode))

(use-package undo-tree
  :straight t
  :init
  (global-undo-tree-mode))

;; (setq display-buffer-alist
;;   `(;; Messages, errors, processes, Calendar in the bottom side window
;;      (,(rx bos (or "*Emacs Log*"))  ; interaction log *Emacs Log*
;;        (display-buffer-reuse-window display-buffer-in-side-window)
;;        (side . right)
;;        (reusable-frames . visible)
;;        (window-height . 0.45))
;;        ;; Let `display-buffer' reuse visible frames for all buffers. This must
;;        ;; be the last entry in `display-buffer-alist', because it overrides any
;;        ;; previous entry with more actions.
;;      ("." nil (reusable-frames . visible))))

(use-package simple
  :hook (before-save . delete-trailing-whitespace))

(use-package restart-emacs
  :straight t)

(fset 'yes-or-no-p 'y-or-n-p)

(column-number-mode 1)

(defun my/org-mode-visual-fill ()
  (setq visual-fill-column-width 120
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :straight t
  :hook (org-mode . my/org-mode-visual-fill))

(use-package simple
  :hook ((prog-mode . turn-on-auto-fill)
         (text-mode . turn-on-auto-fill))
  :config
  (setq-default fill-column 112))

(global-hl-line-mode)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(display-battery-mode t)

(setq display-time-format "%H:%M %a,%d %b %Y")
(setq display-time-default-load-average nil)
(display-time)

(setq-default indent-tabs-mode nil)

(use-package evil-nerd-commenter
  :straight t
  :config
  (evilnc-default-hotkeys))

(use-package command-log-mode
  :straight t)

(use-package which-key
  :straight t
  :config
  (which-key-mode))

(use-package undo-tree
  :straight t
  :init
  (global-undo-tree-mode))

;; (setq display-buffer-alist
;;   `(;; Messages, errors, processes, Calendar in the bottom side window
;;      (,(rx bos (or "*Emacs Log*"))  ; interaction log *Emacs Log*
;;        (display-buffer-reuse-window display-buffer-in-side-window)
;;        (side . right)
;;        (reusable-frames . visible)
;;        (window-height . 0.45))
;;        ;; Let `display-buffer' reuse visible frames for all buffers. This must
;;        ;; be the last entry in `display-buffer-alist', because it overrides any
;;        ;; previous entry with more actions.
;;      ("." nil (reusable-frames . visible))))

(use-package simple
  :hook (before-save . delete-trailing-whitespace))

(use-package restart-emacs
  :straight t)

(fset 'yes-or-no-p 'y-or-n-p)

(column-number-mode 1)

(defun my/org-mode-visual-fill ()
  (setq visual-fill-column-width 120
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :straight t
  :hook (org-mode . my/org-mode-visual-fill))

(use-package simple
  :hook ((prog-mode . turn-on-auto-fill)
         (text-mode . turn-on-auto-fill))
  :config
  (setq-default fill-column 112))

(global-hl-line-mode)

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
(defun my/open-file (file-name)
  "Open a specific file"
  (lambda ()
    (interactive)
    (find-file file-name)))

(use-package general
  :straight t
  :config
  (general-evil-setup t)

  ;; general leader key
  (general-create-definer my/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  ;; leader key for language specific bindings
  (general-create-definer my/language-leader-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC l"
    ;; for insert mode
    :global-prefix "C-SPC l"
    "" '(:ignore t :which-key "language"))

  (my/leader-keys
    "b"  'counsel-bookmark
    "SPC" 'find-file
    "/"  'swiper

    "fp"  (list (lambda () (interactive) (find-file user-init-file-org)) :which-key "config")

    "p"   '(:ignore t :which-key "projectile")
    "pp"  'counsel-projectile-switch-project
    "pk"  'projectile-kill-buffers
    "pa"  'projectile-add-known-project
    "pr"  'projectile-remove-known-project
    "psr" 'projectile-ripgred
    "pxe" 'projectile-run-eshell
    "pf"  'counsel-projectile-find-file
    "pS"  'projectile-save-project-buffers
    "pD"  'projectile-dired

    "em"  'mu4e

    "g"  '(:ignore t :which-key "gtd")
    "gi"  (list (lambda () (interactive) (find-file gtd-inbox-file))   :which-key "inbox")
    "gg"  (list (lambda () (interactive) (find-file gtd-file))         :which-key "gtd")
    "gs"  (list (lambda () (interactive) (find-file gtd-someday-file)) :which-key "someday")
    "gt"  (list (lambda () (interactive) (find-file gtd-tickler-file)) :which-key "tickler")

    "fd"  (list (lambda () (interactive) (find-file daily-file)) :which-key "daily")

    "fw"  (list (lambda () (interactive) (find-file work-file)) :which-key "work")

    "o"    '(:ignore t :which-key "org")
    "oa"   'org-agenda
    "or"   'org-refile
    "os"   'org-archive-hierarchically

    "oc"   '(:ignore t :which-key "org-clock")
    "occ"  'org-capture
    "oci"  'org-clock-in
    "oco"  'org-clock-out
    "ocl"  'org-clock-in-last
    "ocr"  'org-clock-report
    "ocg"  'org-clock-goto

    "ok"   '(:ignore t :which-key "kanban")
    "oki"  'org-kanban/initialize-at-end
    "oks"  'org-kanban/shift

    "op"   '(:ignore t :which-key "org-projectile")
    "opt"  'org-projectile-project-todo-completing-read
    "opg"  'org-projectile-goto-location-for-project
    "opp"  'org-pomodoro

    "ot" '(:ignore t :which-key "timestamp")
    "otu" 'org-timestamp-up-day
    "otd" 'org-timestamp-down-day
    "otl" 'org-toggle-link-display

    "od" 'org-decrypt-entry

    "tn" (list (lambda () (interactive) (find-file tech-notebook-file)) :which-key "tech-notebook")

    "m" '(:ignore t :which-key "todo")
    "mt" 'org-todo
    "ms" 'org-schedule
    "md" 'org-deadline

    "w"  '(:ignore t :which-key "window")
    "wh" 'evil-window-left
    "wl" 'evil-window-right
    "wk" 'evil-window-up
    "wj" 'evil-window-down

    "q"  'howdoyou-query

    "h"  '(:ignore t :which-key "describe")
    "hf" 'counsel-describe-function
    "hv" 'counsel-describe-variable
    "hk" 'helpful-key

    "x"   '(:ignore t :which-key "buffer")
    "xh"  'previous-buffer
    "xl"  'next-buffer
    "xk"  'kill-buffer
    "xs"  '(:ignore t :which-key "split-window")
    "xsr" 'split-window-right
    "xsb" 'split-window-below

    "ie"  'emojify-insert-emoji

    "re"  'restart-emacs

    "eli" 'ielm

    "ed"  '(:ignore t :which-key "elfeed-dashboard")
    "edd" 'elfeed-dashboard
    "edi" 'elfeed-dashboard-edit

    "d" '(:ignore t :which-key "dired")
    "dd" 'dired
    "dj" 'dired-jump

    "c" '(:ignore t :which-key "store link")
    "cc" 'sl-store-link
    "cp" 'sl-insert-link))

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
                          (projects . 5)))
  (dashboard-setup-startup-hook))

(use-package dashboard-hackernews
  :straight t)

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

(use-package ov
  :straight t)

(defun my/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1))

(use-package org
  :straight t
  :hook
  (org-mode . my/org-mode-setup)
  :config
  ;; TODO keywords that I use - the ones after the | are the done states
  (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "NEXT(n)" "|" "DONE(d)" "CANCELLED(c)")))

  ;; Clocking settings
  (setq org-pretty-entities t)

  ;; indentation settings and other misc stuff
  (setq org-startup-indented t)
  (setq org-startup-folded t)
  (setq org-log-done 'note)
  (setq org-tags-column 0)
  (setq org-agenda-tags-column 0)

  ;; org habit
  (setq org-habit-show-all-today t
        org-habit-show-habits-only-for-today t
        org-habit-show-done-always-green t
        org-habit-graph-column 80
        org-habit-preceding-days 28
        org-habit-following-days 7)

  ;; log when an item is rescheduled
  (setq org-log-reschedule (quote time))

  ;; some safeguards against accidentally deleting entire sections of an org file
  (setq org-catch-invisible-edits 'smart)
  (setq org-ctrl-k-protect-subtree t)

  (setq org-src-fontify-natively t
        org-src-preserve-indentation t ;; do not put two spaces on the left
        org-src-tab-acts-natively t)

  (setq yt-iframe-format
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
                    path (or desc "video"))))))

  ;; coloured text
  (load-file (concat user-emacs-directory "/lisp/org-colored-text.el")))

(use-package org-superstar
  :straight t
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-hide-leading-stars nil)
  (setq org-superstar-leading-bullet ?\s))

(use-package org-tempo
  :after org)

(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode 0)))

(use-package org-capture
  :after org
  :config
  ;; quick templates for org files
  (setq org-capture-templates
    '(("t" "Todo [inbox]" entry
      (file+headline "~/Nextcloud/Orgzly/inbox.org" "Tasks")
      "* TODO %i%? \n SCHEDULED: %t")
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
        "* TODO %a \n SCHEDULED: %t"))))

(use-package org-agenda
  :after org
  :config
  ;; make org-agenda respect evil
  (evil-set-initial-state 'org-agenda-mode 'normal)

  ;; org-agenda custom bindings
  (evil-define-key 'normal org-agenda-mode-map
    "vd" 'org-agenda-day-view
    "vw" 'org-agenda-week-view
    "I"  'org-agenda-clock-in
    "O"  'org-agenda-clock-out
    "vR" 'org-agenda-clockreport-mode)

  ;; files that org-agenda will read from
  (setq org-agenda-files
  '("~/Nextcloud/Orgzly/gtd.org"
    "~/Nextcloud/Orgzly/tickler.org"
    "~/Nextcloud/Orgzly/inbox.org"
    "~/Nextcloud/Orgzly/hobbies.org"
    "~/Nextcloud/org/work/work.org"
    "~/Nextcloud/org/reading_list.org"))

  ;; show logs during the day - closed tasks and times, clocks
  (setq org-agenda-start-with-log-mode t))

(use-package org-refile
  :after org
  :config
  ;; files to refile to
  (setq org-refile-targets
    '(("~/Nextcloud/Orgzly/gtd.org"     :maxlevel . 9)
      ("~/Nextcloud/Orgzly/someday.org" :maxlevel . 9)
      ("~/Nextcloud/Orgzly/tickler.org" :maxlevel . 9))))

(use-package org-clock
  :after org
  :config
  ;; Resume clocking task when emacs is restarted
  (org-clock-persistence-insinuate)
  ;; Save the running clock and all clock history when exiting Emacs, load it on startup
  (setq org-clock-persist t)
  ;; Resume clocking task on clock-in if the clock is open
  (setq org-clock-in-resume t)
  ;; Do not prompt to resume an active clock, just resume it
  (setq org-clock-persist-query-resume nil)
  ;; If idle for more than 15 minutes, resolve the things by asking what to do
  ;; with the clock time
  (setq org-clock-idle-time 15)
  ;; remove zero time clocks
  (setq org-clock-out-remove-zero-time-clocks t)
  ;; Include current clocking task in clock reports
  (setq org-clock-report-include-clocking-task t)
  ;; Regular clock report parameters
  (setq org-clock-clocktable-default-properties
    '(:block day :maxlevel 9 :scope agenda :link t :compact t :step day :narrow 80 :fileskip0 t :stepskip0 t :formula %))
  ;; Agenda clock report parameters
  (setq org-agenda-clockreport-parameter-plist
    '(:link t :maxlevel 6 :fileskip0 t :compact t :narrow 60 :score 0)))

(use-package counsel-org-clock
  :straight t)

(use-package org-roam
  :straight
  (:host github :repo "org-roam/org-roam" :branch "master")
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/Nextcloud/org-roam")
  :config
  (setq org-roam-directory "~/Nextcloud/org-roam")
  (my/leader-keys
    "n" '(:ignore t :which-key "org-roam")
    "nl" 'org-roam
    "nf" 'org-roam-find-file
    "ng" 'org-roam-graph-show
    "ni" 'org-roam-insert
    "nI" 'org-roam-insert-immediate
    "no" 'org-roam-dailies-capture-today
    "nt" 'org-roam-dailies-find-today)

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
  :init
  (setq org-journal-enable-encryption t)
  :config
  (setq org-journal-dir "~/Nextcloud/journal")
  (setq org-journal-date-format "%A, %d %B %Y")
  (my/leader-keys
    "nj" 'org-journal-new-entry))

(use-package ob-python   :after org)
(use-package ob-shell    :after org)
(use-package ob-js       :after org)
(use-package ob-java     :after org)

(use-package google-translate
  :straight t
  :custom
  (google-translate-backend-method 'curl)
  :config
  ;; some weird workaround so google translate will actually work
  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130)))

(use-package ob-translate
  :straight t)

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
  (setq org-pomodoro-finished-sound (concat user-emacs-directory "/eraser.wav"))
  (setq org-pomodoro-short-break-sound (concat user-emacs-directory "/eraser.wav"))
  (setq org-pomodoro-long-break-sound (concat user-emacs-directory "/eraser.wav")))

(use-package org-kanban
  :straight t
  :after org)

(use-package org-super-agenda
  :straight t
  :after org
  :config
  (org-super-agenda-mode 1)
  ;; conflicts of header bindings with evil-mode
  (setq org-super-agenda-header-map nil)
  (setq org-super-agenda-groups
       '((:auto-category t)
         (:discard (:not  ; Is it easier to read like this?
                    (:and
                     (:todo "READING" :file-path "reading_list")))))))

(use-package org-super-links
  :straight (:host github :repo "toshism/org-super-links" :branch "master")
  :after org)

(use-package org-web-tools
  :straight t
  :after org)

(use-package org-crypt
  :after org
  :init
  (org-crypt-use-before-save-magic)
  :custom
  (org-crypt-key "C7F48F25C1B7378F6111676E50390E6011771685")
  :config
  (setq org-tags-exclude-from-inheritance '("crypt")))

(use-package org-re-reveal
  :straight t
  :after org
  :config
  (setq org-reveal-mathjax t)
  (setq org-re-reveal-root "https://cdnjs.cloudflare.com/ajax/libs/reveal.js/3.9.2"))

(use-package org-projectile
  :straight t
  :after org
  :init
  (org-projectile-per-project)
  :config
  (setq org-projectile-per-project-filepath "todos.org")
	(setq org-agenda-files (seq-filter 'file-readable-p (delete-dups (append org-agenda-files (org-projectile-todo-files))))))

(use-package org-archive-hierarchically
  :straight (:host gitlab :repo "andersjohansson/org-archive-hierarchically" :branch "master"))

(use-package anki-editor
  :ensure-system-package anki
  :straight t)

(use-package magit
  :straight t
  :defer t)

(setq-default with-editor-emacsclient-executable "emacsclient")

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

(use-package counsel-web
  :straight t)

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

(use-package impatient-mode
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

(use-package sbt-mode
  :straight t
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

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

(use-package virtualenvwrapper
  :straight t
  :defer t
  :init
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))

(my/language-leader-def
  "p" '(:ignore t :which-key "python")
  "pf" 'elpy-autopep8-fix-code)

(use-package cider
  :straight t)

(use-package nix-mode
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

(use-package link-hint
  :straight t
  :config
  (my/leader-keys
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
  (setq mu4e-update-interval 300)
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

(use-package elfeed
  :straight t)

(use-package elfeed-org
  :straight t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org")))

(use-package elfeed-dashboard
  :load-path "~/.emacs.d/lisp/elfeed-dashboard/"
  :config (progn
            (setq elfeed-dashboard-file "~/.emacs.d/lisp/elfeed-dashboard/elfeed-dashboard.org")
             ;; to update feed counts automatically
            (advice-add 'elfeed-search-quit-window :after #'elfeed-dashboard-update-links)))

(use-package dired
  :config

  (defun dw/dired-link (path)
    (lexical-let ((target path))
      (lambda () (interactive) (message "Path: %s" target) (dired target))))

  (my/leader-keys
   "drm" `(,(dw/dired-link "/run/media/gbojinov") :which-key "Media")
   "fin" `(,(dw/dired-link "~/Nextcloud/org/finnish") :which-key "Finnish")))

(use-package all-the-icons-dired
  :straight t
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  (setq dired-auto-revert-buffer t)
  (setq dired-dwim-target t)
  (setq dired-listing-switches "-lah"))

(use-package dired-open
  :straight t
  :config
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv")
                                ("avi" . "mpv")
                                ("mp4" . "mpv"))))

(use-package snow
  :straight (:host github :repo "alphapapa/snow.el" :branch "master"))

(use-package engine-mode
  :straight (:host github :repo "hrs/engine-mode" :branch "main")
  :defer t
  :config
  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")

  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "g")

  (defengine google-images
    "http://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s"
    :keybinding "i")

  (defengine google-maps
    "http://maps.google.com/maps?q=%s"
    :keybinding "m"
    :docstring "Mappin' it up.")

  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "s")

  (defengine youtube
    "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
    :keybinding "y")

  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w"
    :docstring "Searchin' the wikis.")
  (engine-mode t))
