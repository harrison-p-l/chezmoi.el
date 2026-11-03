;;; chezmoi-ediff.el --- Ediff integration for chezmoi -*- lexical-binding: t -*-

;; Author: Harrison Pielke-Lombardo
;; Assisted-by: Claude Code
;; Maintainer: Harrison Pielke-Lombardo
;; Version: 1.2.0
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

;; Provides `ediff' integration for `chezmoi'.

;;; Code:

(require 'chezmoi)
(require 'ediff)

(defcustom chezmoi-ediff-force-overwrite t
  "Whether to force file overwrite when ediff finishes with identical buffers."
  :type '(boolean)
  :group 'chezmoi)

(defvar-local chezmoi-ediff--source-file nil
  "Current ediff source-file.")

;; From age.el, which is optional.
(defvar age-always-use-default-keys)
(defvar age-default-identity)
(defvar age-default-recipient)

(defun chezmoi-ediff--age-config ()
  "Return the age section of chezmoi's config, or nil."
  (let ((config (chezmoi-get-config)))
    (and (hash-table-p config) (gethash "age" config))))

(defun chezmoi-ediff-age-recipients ()
  "Return the age recipients from chezmoi's config."
  (when-let ((age (chezmoi-ediff--age-config)))
    (let ((recipient (gethash "recipient" age))
          (recipients (gethash "recipients" age)))
      (append (and (stringp recipient) (not (string-empty-p recipient))
                   (list recipient))
              recipients))))

(defun chezmoi-ediff-age-identity ()
  "Return the age identity from chezmoi's config."
  (when-let ((age (chezmoi-ediff--age-config)))
    (gethash "identity" age)))

(defun chezmoi-ediff--age-default-keys-p ()
  "Return non-nil if chezmoi's age keys are age.el's defaults."
  (let ((recips (chezmoi-ediff-age-recipients)))
    (and (equal (chezmoi-ediff-age-identity) age-default-identity)
         (equal (if (equal 1 (length recips)) (car recips) recips)
                age-default-recipient))))

(defun chezmoi-ediff--ediff-files (source-file file)
  "Run `ediff-files' on SOURCE-FILE and FILE.
When age.el is installed and chezmoi uses age's default keys, bind
`age-always-use-default-keys' so encrypted files open without prompting."
  (if (require 'age nil t)
      (let ((age-always-use-default-keys (chezmoi-ediff--age-default-keys-p)))
        (ediff-files source-file file))
    (ediff-files source-file file)))

(defun chezmoi-ediff--ediff-get-region-contents (n buf-type ctrl-buf &optional start end)
  "An overriding fn for `ediff-get-region-contents'.
Converts and applies template diffs from the source-file.
N, BUF-TYPE, CTRL-BUF, START, and END are all passed to `ediff'."
  (ediff-with-current-buffer
      (ediff-with-current-buffer ctrl-buf (ediff-get-buffer buf-type))
    (if (string-equal chezmoi-ediff--source-file (buffer-file-name))
        (chezmoi-template-execute (buffer-substring-no-properties
                                   (or start (ediff-get-diff-posn buf-type 'beg n ctrl-buf))
                                   (or end (ediff-get-diff-posn buf-type 'end n ctrl-buf))))
      (buffer-substring
       (or start (ediff-get-diff-posn buf-type 'beg n ctrl-buf))
       (or end (ediff-get-diff-posn buf-type 'end n ctrl-buf))))))

(defun chezmoi-ediff--ediff-cleanup-hook ()
  "Write the source file if the ediff buffers ended up identical."
  (when chezmoi-ediff-force-overwrite
    (when-let (source-file (or (with-current-buffer ediff-buffer-A chezmoi-ediff--source-file)
			       (with-current-buffer ediff-buffer-B chezmoi-ediff--source-file)))
      (when (equal (with-current-buffer ediff-buffer-A (buffer-string))
		   (with-current-buffer ediff-buffer-B (buffer-string)))
	(chezmoi-write source-file t)))))

(defun chezmoi-ediff--ediff-quit-hook ()
  "Remove the `ediff-get-region-contents' override when ediff quits."
  (advice-remove 'ediff-get-region-contents #'chezmoi-ediff--ediff-get-region-contents))

;;;###autoload
(defun chezmoi-ediff (file)
  "Choose a FILE to merge with its source using `ediff'.
Note: Does not run =chezmoi merge=."
  (interactive
   (list (chezmoi--completing-read "Select a dotfile to merge: "
				   (chezmoi-changed-files)
				   'project-file)))
  (let* ((source-file (chezmoi-find file)))
    (advice-add 'ediff-get-region-contents :override #'chezmoi-ediff--ediff-get-region-contents)
    (setq chezmoi-ediff--source-file source-file)
    (chezmoi-ediff--ediff-files source-file file)
    (add-hook 'ediff-cleanup-hook #'chezmoi-ediff--ediff-cleanup-hook nil t)
    (add-hook 'ediff-quit-hook #'chezmoi-ediff--ediff-quit-hook nil t)))

(provide 'chezmoi-ediff)
;;; chezmoi-ediff.el ends here
