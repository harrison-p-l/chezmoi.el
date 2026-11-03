;;; chezmoi.el --- A package for interacting with chezmoi -*- lexical-binding: t -*-

;; Author: Harrison Pielke-Lombardo
;; Assisted-by: Claude Code
;; Maintainer: Harrison Pielke-Lombardo
;; Version: 1.2.0
;; Package-Requires: ((emacs "27.1"))
;; Homepage: http://www.github.com/tuh8888/chezmoi.el
;; Keywords: vc


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Chezmoi is a dotfile management system that uses a source-target state
;; architecture.  This package provides convenience functions for maintaining
;; synchronization between the source and target states when making changes to
;; your dotfiles through Emacs.  It provides alternatives to `find-file' and
;; `save-buffer' for source state files which maintain synchronization to the
;; target state.  It also provides diff/ediff tools for resolving when dotfiles
;; get out of sync.  Dired and magit integration is also provided.

;;; Code:
(require 'chezmoi-core)
(require 'chezmoi-template)
(require 'cl-lib)
(require 'custom)
(require 'shell)
(require 'subr-x)

;; Let these commands work for a plain `require' without generated autoloads.
(autoload 'chezmoi-ediff "chezmoi-ediff" nil t)
(autoload 'chezmoi-dired-add-marked-files "chezmoi-dired" nil t)
(autoload 'chezmoi-dired-forget-marked-files "chezmoi-dired" nil t)

(defun chezmoi--dispatch (args)
  "Dispatch chezmoi command to shell, passing ARGS."
  (let ((b (get-buffer-create "*chezmoi*")))
    (with-current-buffer b
      (erase-buffer)
      (let ((output
             (chezmoi--locally
              (chezmoi--command-result-output
               (chezmoi--run-command (chezmoi--command-line args))))))
        (insert output)
        (let ((result (split-string (string-trim output) "\n")))
          (unless (cl-some (lambda (line)
                             (string-match-p chezmoi-command-error-regex line))
                           result)
            result))))))

(defun chezmoi-managed ()
  "List all files and directories managed by chezmoi."
  (thread-last "managed"
	       chezmoi--dispatch
	       (cl-map 'list (lambda (file) (concat "~/" file)))))

(defun chezmoi-managed-files ()
  "List only files managed by chezmoi."
  (thread-last (chezmoi-managed)
	       (cl-remove-if #'file-directory-p)))

(declare-function magit-status-setup-buffer "magit-status" (&optional directory))

;;;###autoload
(defun chezmoi-magit-status ()
  "Show the Magit status of the chezmoi source repository.
Requires Magit, which `chezmoi' does not depend on."
  (interactive)
  (if (and (require 'magit nil t)
           (fboundp 'magit-status-setup-buffer))
      (magit-status-setup-buffer (cl-first (chezmoi--dispatch "source-path")))
    (user-error "`chezmoi-magit-status' requires Magit")))

(defun chezmoi-target-file-p (file)
  "Return non-nil if FILE is in the target state."
  (thread-last (chezmoi-managed-files)
	       (cl-mapcar #'expand-file-name)
	       (member (expand-file-name file))))

;;;###autoload
(defun chezmoi-diff (arg)
  "View output of =chezmoi diff= in a diff-buffer.
If ARG is non-nil, switch to the diff-buffer."
  (interactive "i")
  (let ((b (get-buffer-create "*chezmoi-diff*")))
    (with-current-buffer b
      (erase-buffer)
      (insert
       (chezmoi--locally
        (chezmoi--command-result-output
         (chezmoi--run-command (chezmoi--command-line "diff --use-builtin-diff"))))))
    (unless arg
      (switch-to-buffer b)
      (diff-mode)
      (whitespace-mode 0))
    b))

(defun chezmoi-changed-files ()
  "Use chezmoi status to return the files that have changed."
  (let ((lines (split-string
		(chezmoi--locally
		 (chezmoi--command-result-output
		  (chezmoi--run-command (chezmoi--command-line "status"))))
		"\r?\n" t)))
    (cl-loop for line in lines
	     when (string-match "^[ ADMR][ADMR] \\(.+\\)$" line)
	     collect (concat "~/" (match-string 1 line)))))

(defun chezmoi-changed-p (file)
  "Return non-nil of FILE has changed."
  (member (if (chezmoi-target-file-p file)
	      (chezmoi-target-file file)
	    file)
	  (chezmoi-changed-files)))

(defun chezmoi-version ()
  "Get version number of chezmoi."
  (let* ((s (cl-first (chezmoi--dispatch "--version")))
	 (dev-re "\\(version \\(dev\\)\\)")
	 (v-re " \\(v\\(\\([0-9]+\\.\\)?\\([0-9]+\\.\\)?\\(\\*\\|[0-9]+\\)\\)\\)")
	 (re (concat dev-re "\\|" v-re)))
    (when (string-match re s)
      (or (match-string 4 s)
	  (match-string 2 s)))))

(defun chezmoi-get-data ()
  "Return chezmoi data."
  (json-parse-string (apply #'concat (chezmoi--dispatch "data"))))

(defun chezmoi-get-config()
  (let ((v (chezmoi-version)))
    (when (or (and v (string-match-p "^[0-9]" v) (version<= "2.27.0" v)) (string= "dev" v))
      (let ((config-string (apply #'concat (chezmoi--dispatch "dump-config"))))
	(json-parse-string config-string
			   :array-type 'list
			   :null-object nil)))))

(defun chezmoi--unchezmoi-source-file-name (source-file)
  "Remove chezmoi attributes from SOURCE-FILE."
  (let* ((base-name (file-name-base source-file))
	 (ext (file-name-extension source-file))
	 (base-name (if ext
			(concat (file-name-sans-extension base-name) "." ext)
		      base-name))
	 (base-name (cl-reduce (lambda (s attr) (replace-regexp-in-string attr "" s))
			       chezmoi-source-state-suffix-attrs
			       :initial-value base-name))
	 (dir (file-name-directory source-file))
	 (dir (when dir (cl-reduce (lambda (s attr) (let ((replacement (if (string= "dot_" attr) "." "")))
						 (replace-regexp-in-string attr replacement s)))
				   chezmoi-source-state-prefix-attrs
				   :initial-value dir)))
	 (stop-parsing nil)
	 attr)
    (while (and (not stop-parsing) (setq attr (cl-some (lambda (attr) (when (string-prefix-p attr base-name) attr)) chezmoi-source-state-prefix-attrs)))
      (when (string= "literal_" attr) (setq stop-parsing t))
      (setq base-name (substring base-name (length attr)))
      (when (string= "dot_" attr) (setq base-name (concat "." base-name))))

    (concat dir "/" base-name)))

(defun chezmoi--manual-target-file (source-file)
  "Return the target file corresponding to SOURCE-FILE."
  (let* ((to-find (chezmoi--unchezmoi-source-file-name source-file))
	 (potential-targets (cl-remove-if-not (lambda (f)
						(let* ((dir (replace-regexp-in-string "~" "~/.local/share/chezmoi" (file-name-directory f)))
						       (base (file-name-base f))
						       (ext (file-name-extension f))
						       (corrected-f (concat dir
									    "/"
									    (if ext
										(concat (file-name-sans-extension base) "." ext)
									      base)))
						       (trying (expand-file-name corrected-f)))
						  (string= trying to-find)))
					      (chezmoi-managed-files))))
    (cond ((zerop (length potential-targets)) (progn
						(message "No target found")
						nil))
	  ((not (= 1 (length potential-targets)))
	   (progn
	     (message "Multiple targets found: %s. Using first" potential-targets)
	     (cl-first potential-targets)))
	  (t (cl-first potential-targets)))))

(make-obsolete-variable 'chezmoi--manual-target-file 'chezmoi-target-file "0.0.1")

(defun chezmoi-target-file (file)
  "Return the target file corresponding to FILE."
  (unless (chezmoi-target-file-p file)
    (let ((v (chezmoi-version)))
      (if (or (and v (string-match-p "^[0-9]" v) (version<= "2.12.0" v)) (string= "dev" v))
	  (thread-last (when file (shell-quote-argument file))
		       (format "target-path %s")
		       chezmoi--dispatch
		       cl-first)
	(chezmoi--manual-target-file file)))))

(defun chezmoi-source-file (file)
  "Return the source file corresponding to FILE."
  (when (chezmoi-target-file-p file)
    (thread-last (when file (shell-quote-argument file))
		 (format "source-path %s")
		 chezmoi--dispatch
		 cl-first)))

;;;###autoload
(defun chezmoi-write (&optional file arg)
  "Sync FILE.  How it syncs depends if FILE is in source or target.
If FILE is in source state, run =chezmoi apply= on the target to overwrite it.
With prefix ARG, use `shell' to run =chezmoi apply= command.  This is helpful
for resolving some issues.

If FILE is in target state, copy it to the source buffer without saving.
With prefix ARG, save the source buffer."
  (interactive (list (buffer-file-name)
		     current-prefix-arg))
  (let ((file (if file file (buffer-file-name))))
    (if (chezmoi-target-file-p file)
	;; File is in target state
	(let* ((target-file file)
	       (source-file (chezmoi-source-file target-file)))
	  (with-current-buffer (find-file-noselect source-file)
	    (replace-buffer-contents (find-file-noselect target-file))
	    (if arg
		(progn
		  (save-buffer)
		  (message "Wrote source: %s" source-file))
	      (message "Wrote source (unsaved): %s" source-file))
	    source-file))

      ;; File is in source state
      (let* ((source-file file)
             (target-file (chezmoi-target-file source-file))
             (cmd (format "apply %s%s"
			  (shell-quote-argument target-file)
			  (if arg " --force" ""))))
        (if (chezmoi--dispatch cmd)
	    (progn
              (message "Wrote target: %s" target-file)
              target-file)
          (progn
            (message "Failed to write %s. Use chezmoi-write with prefix arg to resolve with chezmoi."
                     target-file)
            nil))))))

(defun chezmoi-add (files)
  "Add FILES to the chezmoi source state.
FILES can be a single file name or a list of file names.  When called
interactively, prompt for a file, defaulting to the current buffer's file.
Return the list of added files, or nil if =chezmoi add= failed."
  (interactive
   (list (read-file-name "Add file to chezmoi: " nil (buffer-file-name) t
			 (when (buffer-file-name)
			   (file-name-nondirectory (buffer-file-name))))))
  (let* ((files (cl-mapcar #'expand-file-name (if (listp files) files (list files))))
	 (cmd (concat "add " (mapconcat #'shell-quote-argument files " "))))
    (if (chezmoi--dispatch cmd)
	(progn
	  (message "Added to chezmoi: %s" (string-join files ", "))
	  files)
      (progn
	(message "Failed to add %s. See the *chezmoi* buffer for details."
		 (string-join files ", "))
	nil))))

(defun chezmoi--completing-read (prompt choices category)
  "Completing read with meta data.
PROMPT, CHOICES, and CATEGORY are passed to `complete-with-action'."
  (completing-read prompt
		   (lambda (string predicate action)
		     (if (eq action 'metadata)
			 `(metadata (category . ,category))
		       (complete-with-action action choices string predicate)))
		   nil t))

;;;###autoload
(defun chezmoi-find (file)
  "Edit a source FILE managed by chezmoi.
If the target file has the same state as the source file,add a hook to
`save-buffer' that applies the source state to the target state.  This way, when
the buffer editing the source state is saved the target state is kept in sync.
Note: Does not run =chezmoi edit=."
  (interactive
   (list (chezmoi--completing-read "Select a dotfile to edit: "
				   (chezmoi-managed-files)
				   'project-file)))
  (let ((source-file (chezmoi-source-file file)))
    (when source-file
      (find-file source-file)
      (let ((target-file (chezmoi-target-file source-file)))
	(when-let ((mode (thread-first target-file
				       file-name-nondirectory
				       (assoc-default auto-mode-alist 'string-match))))
	  (funcall mode))
	(message target-file)
	(unless chezmoi-mode (chezmoi-mode))
	source-file))))

;;;###autoload
(defun chezmoi-sync-files (files &optional arg)
  "Iteratively select file from FILES to sync.
Interactively select whether to sync the source state or the target state.
Prefix ARG is passed to `chezmoi-write'."
  (interactive
   (list (let ((choice (completing-read "Source or target?"
					'("Source" "Target")))
	       (files (chezmoi-changed-files)))
	   (if (string-equal "Source" choice)
	       (cl-mapcar #'chezmoi-source-file files)
	     files))
	 current-prefix-arg))
  (let (file)
    (while (and (setq file (chezmoi--completing-read "Select file to sync. (C-g to stop): "
						     files
						     'project-file))
		(chezmoi-write file arg))
      (setq files (remove file files)))))

;;;###autoload
(defun chezmoi-open-other (file)
  "Open buffer's target FILE."
  (interactive (list (buffer-file-name)))
  (if (chezmoi-target-file-p file)
      (chezmoi-find file)
    (thread-first file
		  chezmoi-target-file
		  find-file)))

(defun chezmoi--data-at-keys (keys)
  "Return the chezmoi data found by following KEYS, or nil."
  (cl-reduce (lambda (data key) (when (hash-table-p data) (gethash key data)))
	     keys
	     :initial-value (chezmoi-get-data)))

;;;###autoload
(defun chezmoi-capf ()
  "Complete chezmoi template keys at point using =chezmoi data=.
Meant for `completion-at-point-functions'.  Works with the default
completion UI, Corfu, and Company (through `company-capf')."
  (when (looking-back "{{-? *\\(\\.[^[:space:]{}]*\\)" (line-beginning-position))
    (let* ((keys (split-string (match-string-no-properties 1) chezmoi-template-key-regex))
	   (end (point))
	   (beg (- end (length (car (last keys)))))
	   (data 'unfetched)
	   (get-data (lambda ()
		       (when (eq data 'unfetched)
			 (setq data (chezmoi--data-at-keys (remove "" (butlast keys)))))
		       data)))
      (list beg end
	    (completion-table-dynamic
	     (lambda (_)
	       (let ((data (funcall get-data)))
		 (when (hash-table-p data) (hash-table-keys data)))))
	    :annotation-function
	    (lambda (candidate)
	      (let* ((data (funcall get-data))
		     (value (when (hash-table-p data) (gethash candidate data))))
		(cond ((stringp value) (format " (%s)" value))
		      ((hash-table-p value) " <Object>")
		      (t ""))))
	    :company-kind (lambda (_) 'keyword)
	    :exclusive 'no))))

(defun chezmoi-font-lock-keywords ()
  "Keywords for font lock."
  `((,chezmoi-template-regex 0 'chezmoi-template-face prepend)))

(defun chezmoi--template-file-p (file)
  "Return non-nil if source FILE is interpreted by chezmoi as a template."
  (and file
       (or (string-suffix-p ".tmpl" file)
           (string-match-p "/\\.chezmoitemplates/" file))))

;;;###autoload
(define-minor-mode chezmoi-mode
  "Chezmoi mode for source files."
  :group 'chezmoi
  (if chezmoi-mode
      (progn
	(unless (chezmoi-changed-p (buffer-file-name))
	  (add-hook 'after-save-hook #'chezmoi-write 0 t))
	(when (chezmoi--template-file-p (buffer-file-name))
	  (add-hook 'after-change-functions #'chezmoi-template--after-change nil 1)
	  (add-hook 'completion-at-point-functions #'chezmoi-capf nil t)
	  (chezmoi-template-buffer-display t))

	(font-lock-add-keywords nil (chezmoi-font-lock-keywords) 'append)
	(font-lock-ensure (point-min) (point-max)))
    (progn
      (chezmoi-template-buffer-display nil)

      (remove-hook 'after-save-hook #'chezmoi-write t)
      (remove-hook 'after-change-functions #'chezmoi-template--after-change t)
      (remove-hook 'completion-at-point-functions #'chezmoi-capf t)

      (font-lock-remove-keywords nil (chezmoi-font-lock-keywords))
      (font-lock-ensure (point-min) (point-max)))))

(provide 'chezmoi)

;;; chezmoi.el ends here
