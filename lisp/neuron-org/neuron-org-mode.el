;;; neuron-org.el --- These functions are slightly modified versions of the ones in neuron-mode to work with org-mode

;;; Commentary:

;; Largely inspired by and using
;; https://github.com/felko/neuron-mode

;;; Code:

(require 'neuron-mode)
(require 'xah-get-thing)
(require 's)

(defgroup neuron nil
  "An org mode extension to neuron-mode"
  :prefix "neuron-org-"
  :link '(url-link "https://github.com")
  :group 'org)

(defun neuron-org-insert-zettel-link ()
  "Insert a org link to another zettel."
  (interactive)
  (neuron-check-if-zettelkasten-exists)
  (let ((label (read-string "Label: ")))
    (neuron-org--insert-zettel-link-from-id (map-elt (neuron-select-zettel "Link zettel: ") 'zettelID) label)))

(defun neuron-org--insert-zettel-link-from-id (id &optional label)
  "Insert a zettel org mode folgezettel link with ID.
You can also optionally specify a LABEL."
  (progn
    (if (not (string= label ""))
        (insert (format "[[z:/%s?type=branch][%s]]" id label))
        (insert (format "[[z:/%s?type=branch]]" id)))))

(defun neuron-org-new-zettel (&optional title id)
  "Create a new zettel and open it in a new buffer.
The new zettel will be generated with the given TITLE and ID if specified.
When TITLE is nil, prompt the user."
  (interactive)
  (if-let (buffer (call-interactively #'neuron-org-create-zettel-buffer t (vector title id)))
          (neuron--pop-to-buffer-same-window buffer)
          (user-error "Unable to create zettel %s" id)))

(defun neuron-org--make-command (cmd title &rest args)
  "Construct a neuron command CMD with TITLE and ARGS."
  (concat
   neuron-executable
   " "
   (mapconcat
    #'shell-quote-argument
    (append (list "-d" neuron--current-zettelkasten cmd title "-f" "org") args) " ")))

(defun neuron-org--make-new-command (&optional id title)
  "Construct a neuron command to execute, optionally with ID and TITLE."
  (neuron-check-if-zettelkasten-exists)
  (unless id
    (setq id (pcase neuron-id-format
               ('prompt
                (if-let* ((id (read-string "ID: "))
                          ((neuron--is-valid-id id)))
                    id
                  (user-error "Invalid zettel ID: %S" id)))
               ((pred functionp)
                (let ((id (funcall neuron-id-format title)))
                  (if (neuron--is-valid-id id)
                      id
                    (user-error "Invalid zettel ID: %S" id)))))))
  (let ((args (if id (list id) nil)))
    (apply #'neuron-org--make-command "new" title args)))

(defun neuron-org-create-zettel-buffer (title &optional id no-default-tags)
  "Create a new zettel in the current zettelkasten.
The new zettel will be generated with the given TITLE and ID if specified.
When TITLE is nil, prompt the user.
If NO-DEFAULT-TAGS is non-nil, don't add the tags specified the variable
`neuron-default-tags'."
  (interactive (list (read-string "Title: ")))
  (neuron-check-if-zettelkasten-exists)
  (when (or (not id) (and id (not (neuron--get-cached-zettel-from-id id))))
    (let* ((underscore-title (s-downcase (replace-regexp-in-string "[?',|;\s/]" "_" title)))
           (cmd     (neuron-org--make-new-command id underscore-title))
           (path    (neuron--run-command cmd))
           (buffer  (find-file-noselect path))
           (buffer-read-only nil))
      (with-current-buffer buffer
        (unless no-default-tags
          (dolist (tag neuron-default-tags)
            (neuron-add-tag tag)))
        (when title
          (goto-char (point-min))
          (kill-word 1)
          (insert (format "* %s" title)))
        (save-buffer))
      (neuron--rebuild-cache)
      (message "Created %s" (f-filename path))
      buffer)))

(defun neuron-org-follow-link ()
  "Follow the link at point in the current zettelkasten."
  (interactive)
  (let* ((link (neuron-org--get-link-at-point))
         (file-name (concat (neuron--get-zettelkasten) "/" (neuron-org--parse-zettel-link link) ".org")))
         (find-file file-name)))

(defun neuron-org--parse-zettel-link (link)
  "Extract the name of the file that is being linked to from LINK.
Assume a link of type z:/file_name?type=branch"

  (car (split-string (cadr (split-string link "z:/")) "?")))

(defun neuron-org--get-link-at-point ()
  "Get the link to follow."
  (xah-get-thing-at-point 'line))

;;; Completion

(defun company-neuron-org--prefix ()
  "Extract the completion prefix, triggered by entering an opening square bracket."
  (and (derived-mode-p 'neuron-org-mode)
       (when (looking-back (rx "[[" (group (+ (not (any "]]"))))) nil)
         (match-string 1))))

(defun company-neuron-org--fuzzy-match-title (prefix candidate)
  "Return whether PREFIX fuzzily matches the title of the CANDIDATE zettel."
  (let ((full-title (alist-get 'zettelTitle (get-text-property 0 'zettel candidate))))
    (cl-subsetp (string-to-list prefix)
                (string-to-list full-title))))

(defun company-neuron-org--propertize-completion-candidate (zettel)
  "Propertize a zettel title to contain all information about ZETTEL.
The resulting title is truncated and padded to fit the width given by
`neuron-max-completion-width'."
  (let* ((title (alist-get 'zettelTitle zettel))
         (padded (s-pad-right neuron-max-completion-width " " title))
         (truncated (s-truncate neuron-max-completion-width padded)))
    (propertize truncated 'zettel zettel)))

(defun company-neuron-org--all-candidates ()
  "Propertize all cached zettels to provide completion candidates."
  (mapcar #'company-neuron--propertize-completion-candidate neuron--zettel-cache))

(defun company-neuron-org--candidates (prefix)
  "Filter the candidates by fuzzily matching PREFIX against the candidates."
  (cl-remove-if-not
   (lambda (candidate) (company-neuron--fuzzy-match-title prefix candidate))
   (company-neuron--all-candidates)))

(defun company-neuron-org--completion-annotation (candidate)
  "Annotate the completion CANDIDATE so that it includes the ID of the underlying zettel."
  (let* ((zettel (get-text-property 0 'zettel candidate))
         (annot (format "<%s>" (alist-get 'zettelID zettel))))
    (concat " " (propertize annot 'face 'neuron-link-face))))

(defun company-neuron-org--completion-meta (candidate)
  "Display information about the underlying zettel of CANDIDATE.
The resulting string contains the ID, the full title of the zettel, as well as
the list of its tags."
  (let ((zettel (get-text-property 0 'zettel candidate)))
    (neuron--propertize-zettel zettel)))

(defun company-neuron-org--post-completion-action (candidate)
  "Delete the completed zettel title CANDIDATE and replace it with an actual neuron link."
  (let ((begin (point))
        (zettel (get-text-property 0 'zettel candidate)))
    (when (re-search-backward (rx "[["))
      (goto-char begin)
      (delete-region begin (match-end 0))
      (insert "z:/" (concat (alist-get 'zettelID zettel) "?type=branch"))
      (neuron--setup-overlays))))

;;;###autoload
(defun company-neuron-org (command &optional arg &rest ignored)
  "Defines a company completion backend that completes zettels by title.
COMMAND is the relevant command provided by company.
ARG is the command argument, depending on which command was received.
IGNORED is the rest of the arguments, not sure why it's there."
  (interactive (list 'interactive))
  (cl-case command
    ((interactive) (company-begin-backend 'company-neuron-org-backend))
    ((prefix) (company-neuron-org--prefix))
    ((candidates) (company-neuron-org--candidates arg))
    ((annotation) (company-neuron-org--completion-annotation arg))
    ((meta) (company-neuron-org--completion-meta arg))
    ((post-completion) (company-neuron-org--post-completion-action arg))
    ((no-cache) 't)
    ((ignore-case) t)))

;;;###autoload
(defun company-neuron-org-setup ()
  "Setup company to use the neuron org backend."
  (add-to-list 'company-backends 'company-neuron-org))

(defvar neuron-org-mode-hook nil
  "Hook run when entering `neuron-org-mode'.")

;;;###autoload
(define-derived-mode neuron-org-mode org-mode "Neuron"
  "A major mode to edit Zettelkasten notes with neuron and org mode."
  (neuron-check-if-zettelkasten-exists)
  (when neuron-generate-on-save
    (add-hook 'after-save-hook #'neuron-rib-generate t t)))
  ;;(add-hook 'after-save-hook #'neuron--setup-overlays t t))
  ;;(neuron--setup-overlays)
  ;;(use-local-map neuron-org-mode-map))
  ;;(neuron--name-buffer))

(defun neuron-org--auto-enable-when-in-zettelkasten ()
  "Automatically switch to neuron-org-mode when located in a zettelkasten."
  (when (and (eq major-mode 'org-mode)
             (neuron--detect-zettelkasten (f-parent buffer-file-name))
    (neuron-org-mode))))


(add-hook 'find-file-hook #'neuron-org--auto-enable-when-in-zettelkasten t nil)

(provide 'neuron-org-mode)

;;; neuron-org-mode ends here
