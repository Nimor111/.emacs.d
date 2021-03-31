;;; neuron-org.el --- These functions are slightly modified versions of the ones in neuron-mode to work with org-mode

;;; Commentary:

;; Largely inspired by and using
;; https://github.com/felko/neuron-mode

;;; Code:

(require 'neuron-mode)
(require 'xah-get-thing)
(require 's)

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
    (let* ((underscore-title (s-downcase (replace-regexp-in-string "[?',|;\s]" "_" title)))
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

(provide 'neuron-org)

;;; neuron-org ends here
