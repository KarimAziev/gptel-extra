;;; gptel-extra.el --- Enhancers for Gptel ChatGPT client -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/gptel-extra
;; Version: 0.1.0
;; Keywords: convenience tools
;; Package-Requires: ((emacs "29.1") (gptel "0.4.0"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Enhancers for Gptel ChatGPT client:

;; `gptel-extra-org-markdown-block-mode': a global mode, that will advise
;; the gptel stream functions `gptel-curl--stream-cleanup' and
;; `gptel-curl-get-response' to insert the streaming response from ChatGPT as an
;; Org src markdown block.

;; Usage:
;;
;; (require 'gptel-extra)
;; (gptel-extra-org-markdown-block-mode 1)

;;
;;; Code:

(require 'gptel)
(require 'gptel-curl)

(defcustom gptel-extra-ext-to-org-langs-alist '(("ts" . "typescript")
                                                ("tsx" . "typescript")
                                                ("js" . "javascript")
                                                ("el" . "elisp")
                                                ("jsx" . "javascript")
                                                ("c" . "C")
                                                ("c++" . "C++")
                                                ("py" . "python")
                                                ("md" . "markdown")
                                                ("toml" . "toml")
                                                ("sh" . "shell"))
  "Alist of file extensions to Org mode languages."
  :group 'gptel-extra
  :type '(alist
          :key-type string
          :value-type string))

(defcustom gptel-extra-default-save-dir (locate-user-emacs-file ".gptel-extra")
  "Default directory for saving GPT-EL extra state files.

The default directory for saving extra GPT-EL state files.

Specifies the directory where GPT-EL state files are saved by default. The
directory is resolved relative to the user's Emacs directory. If the directory
does not exist, it will be created when saving a state file for the first time.

To change the default save directory, set the value to a string representing the
desired directory path. Ensure that the path is valid and that Emacs has the
necessary permissions to create and write to the directory."
  :group 'gptel-extra
  :type 'directory)

(defun gptel-extra--after-begin-block-p ()
  "Check if the cursor is immediately after a begin block in a buffer."
  (save-excursion
    (when (re-search-backward "[^\s\t\n\r\f]" nil t 1)
      (let* ((line-beg (line-beginning-position))
             (str (string-trim
                   (buffer-substring-no-properties
                    line-beg
                    (1+ (point)))))
             (word (car (split-string str nil t))))
        (and word
             (let ((case-fold-search t))
               (string-match-p ",?#\\+begin_" (string-trim word)))
             line-beg)))))

(defun gptel-extra--before-end-block-p ()
  "Check if the cursor is before an Org block end line."
  (save-excursion
    (when (re-search-forward "[^\s\t\n\r\f]" nil t 1)
      (let* ((line-beg (line-beginning-position))
             (str (string-trim
                   (buffer-substring-no-properties
                    line-beg
                    (line-end-position))))
             (word (car (split-string str nil t))))
        (and word
             (let ((case-fold-search t))
               (string-match-p ",?#\\+end_" (string-trim word)))
             line-beg)))))

(defun gptel-extra-curl-stream-cleanup (fn &rest args)
  "Clean up the stream after a successful HTTP request.

This function should be used as advice for `gptel-curl--stream-cleanup':

\\=(advice-add \\='gptel-curl--stream-cleanup :around
               \\=#'gptel-extra-curl-stream-cleanup)

Argument FN is expected to be a function `gptel-curl--stream-cleanup',
that will be applied to the rest of the arguments.
Optional argument ARGS is a list of arguments that will be passed to the
function FN."
  (let ((gptel-prompt-prefix-alist nil))
    (let* ((info (alist-get (car args) gptel-curl--process-alist))
           (http-status (plist-get info :http-status))
           (tracking-marker (plist-get info :tracking-marker))
           (start-marker (plist-get info :position)))
      (apply fn args)
      (when (and (equal http-status "200"))
        (with-current-buffer (marker-buffer start-marker)
          (pulse-momentary-highlight-region (+ start-marker 2)
                                            tracking-marker)
          (when gptel-mode (save-excursion
                             (goto-char tracking-marker)
                             (when-let* ((end (gptel-extra--before-end-block-p))
                                         (beg (save-excursion
                                                (let ((e))
                                                  (while (and (looking-at "\n")
                                                              (looking-back
                                                               "\n\n"
                                                               0))
                                                    (forward-line -1)
                                                    (setq e (point)))
                                                  e))))
                               (delete-region beg end))
                             (move-marker tracking-marker
                                          (save-excursion
                                            (forward-line 1)
                                            (end-of-line)
                                            (point)))
                             (let ((pos (if (markerp start-marker)
                                            (marker-position start-marker)
                                          start-marker)))
                               (add-text-properties
                                pos
                                tracking-marker
                                '(gptel response rear-nonsticky t))))))))))

(defun gptel-extra-curl-get-response (fn &optional info callback)
  "Apply a function in `org-mode' with a CALLBACK or without it.

This function should be used as advice for `gptel-curl-get-response':

\\=(advice-add \\='gptel-curl-get-response :around
               \\=#'gptel-extra-curl-get-response).

Argument FN is expected to be a function `gptel-curl-get-response' that is
passed as the first argument to `gptel-extra-curl-get-response'.
Optional argument INFO is a property list (plist) that can be optionally
passed to the function.
If not provided, its default value is nil.

Optional argument CALLBACK is a function that is optionally passed as the
third argument. If not provided, the default value is
`gptel-extra-curl-stream-insert-response'."
  (if (eq (buffer-local-value
           'major-mode
           (plist-get info :buffer))
          'org-mode)
      (apply fn info (list
                      (or callback #'gptel-extra-curl-stream-insert-response)))
    (apply fn (delq nil (list info callback)))))

(defun gptel-extra-curl-stream-insert-response (response info)
  "Insert streaming RESPONSE from ChatGPT as Org src markdown block.

INFO is a mutable plist containing information relevant to this buffer.

Argument RESPONSE is a string that represents the RESPONSE from the curl
command."
  (let ((start-marker (plist-get info :position))
        (tracking-marker (plist-get info :tracking-marker)))
    (when response
      (with-current-buffer (marker-buffer start-marker)
        (save-excursion
          (unless tracking-marker
            (when-let
                ((update-fn
                  (seq-find #'fboundp
                            '(gptel--update-status
                              gptel--update-header-line))))
              (funcall update-fn " Typing..." 'success))
            (goto-char start-marker)
            (unless (or (bobp)
                        (plist-get info :in-place))
              (let* ((beg-block-p (gptel-extra--after-begin-block-p))
                     (end-block-p (gptel-extra--before-end-block-p))
                     (pos (point))
                     (left-block-line (and beg-block-p
                                           (let* ((count
                                                   (save-excursion
                                                     (goto-char
                                                      beg-block-p)
                                                     (forward-line 1)
                                                     (count-lines pos
                                                                  (point))))
                                                  (diff (- 1 count)))
                                             (when (> diff 0)
                                               (make-string diff (string-to-char
                                                                  "\n"))))))
                     (end-block-line (and end-block-p
                                          (let* ((count (save-excursion
                                                          (goto-char end-block-p)
                                                          (forward-line -1)
                                                          (count-lines pos
                                                                       (point))))
                                                 (diff (- 1 count)))
                                            (when (> diff 0)
                                              (make-string diff (string-to-char
                                                                 "\n"))))))
                     (str (string-join (delq nil (list
                                                  (or left-block-line "\n\n")
                                                  (unless beg-block-p
                                                    "#+begin_src markdown\n")
                                                  end-block-line
                                                  (unless end-block-p
                                                    "\n#+end_src")))
                                       "")))
                (insert (apply #'propertize str
                               '(gptel response rear-nonsticky t)))
                (when (string-match-p str "#\\+end_src")
                  (re-search-backward "#\\+end_src" nil t 1))
                (forward-line -1)))
            (setq tracking-marker (set-marker (make-marker) (point)))
            (set-marker-insertion-type tracking-marker t)
            (plist-put info :tracking-marker tracking-marker))
          (add-text-properties
           0 (length response) '(gptel response rear-nonsticky t)
           response)
          (goto-char tracking-marker)
          (insert response))))))

;;;###autoload
(define-minor-mode gptel-extra-org-markdown-block-mode
  "Toggle handling of org-markdown blocks in `gptel' streams.

When this mode is on, it will advise the gptel stream functions
`gptel-curl--stream-cleanup' and `gptel-curl-get-response' to insert the
streaming response from ChatGPT as an Org src markdown block."
  :lighter " gptel-extra-org-stream-org-markdown-block"
  :group 'gptel
  :global t
  (advice-remove 'gptel-curl--stream-cleanup
                 #'gptel-extra-curl-stream-cleanup)
  (advice-remove 'gptel-curl-get-response
                 #'gptel-extra-curl-get-response)
  (when gptel-extra-org-markdown-block-mode
    (advice-add 'gptel-curl--stream-cleanup :around
                #'gptel-extra-curl-stream-cleanup)
    (advice-add 'gptel-curl-get-response :around
                #'gptel-extra-curl-get-response)))

(defun gptel-extra-save-state ()
  "Write the gptel state to the buffer.

This enables saving the chat session when writing the buffer to
disk.  To restore a chat session, turn on `gptel-mode' after
opening the file."
  (pcase major-mode
    ('org-mode
     (save-excursion
       (save-restriction
         (widen)
         (dolist (item '(gptel-model gptel-temperature gptel--system-message
                         gptel-max-tokens
                         (eval . (progn
                                   (require 'gptel)
                                   (gptel-mode 1)))))
           (pcase item
             ((pred (symbolp))
              (add-file-local-variable item (symbol-value item)))
             (`(eval . ,value)
              (let ((regex
                     (concat "^" (string-trim (or comment-start "#")) " eval: "
                             (regexp-quote
                              (prin1-to-string
                               value)))))
                (unless (save-excursion
                          (save-restriction
                            (widen)
                            (goto-char (point-max))
                            (re-search-backward regex nil t 1)))
                  (add-file-local-variable (car item) value))))))
         (add-file-local-variable 'gptel--bounds (gptel--get-buffer-bounds)))))
    (_ (save-excursion
         (save-restriction
           (add-file-local-variable 'gptel-model gptel-model)
           (unless (equal (default-value 'gptel-temperature) gptel-temperature)
             (add-file-local-variable 'gptel-temperature gptel-temperature))
           (unless (string= (default-value 'gptel--system-message)
                            gptel--system-message)
             (add-file-local-variable 'gptel--system-message
                                      gptel--system-message))
           (when gptel-max-tokens
             (add-file-local-variable 'gptel-max-tokens gptel-max-tokens))
           (add-file-local-variable 'gptel--bounds (gptel--get-buffer-bounds)))))))

(defun gptel-extra-restore ()
  "Restore text properties for response regions in the `gptel--bounds' list."
  (mapc (pcase-lambda (`(,beg . ,end))
          (put-text-property beg end 'gptel 'response))
        gptel--bounds))


(defun gptel-extra-trim-org-src-markdown-block (content)
  "Trim Markdown source blocks in Org content.

Argument CONTENT is a string containing the Org source block to be trimmed."
  (when-let* ((lines (split-string content "\n" t))
              (prefix (pop lines))
              (suffix (car (last lines)))
              (case-fold-search t))
    (when (and
           (string-match-p "^#?\\+?begin_src markdown$" (string-trim prefix))
           (string-match-p "^#?\\+?end_src$" (string-trim suffix)))
      (string-join
       (nbutlast lines 1)
       "\n"))))

(declare-function org-export-expand-include-keyword "ox")

(defun gptel-extra--include-files (content)
  "Expand included files in Org CONTENT and return result.

Argument CONTENT is a string containing the text with `org-mode' include
keywords to be expanded."
  (require 'org)
  (require 'ox)
  (with-temp-buffer
    (let ((tab-width 8))
      (insert content)
      (org-export-expand-include-keyword)
      (buffer-substring-no-properties (point-min)
                                      (point-max)))))

(defun gptel-extra-denormalize-prompt-item (plist)
  "Modify PLIST based on :role and content trimming.

Argument PLIST is a property list containing the data to denormalize."
  (let ((content (plist-get plist :content)))
    (or
     (pcase (plist-get plist :role)
       ("assistant"
        (when-let ((new-content
                    (gptel-extra-trim-org-src-markdown-block content)))
          (plist-put plist :content new-content)))
       ("user"
        (let ((case-fold-search t))
          (if-let ((new-content
                    (and (string-match-p "#\\+include:" content)
                         (gptel-extra--include-files content))))
              (plist-put plist :content new-content)
            plist)))
       (_ plist))
     plist)))

(defun gptel-extra-normalize-prompts (plist)
  "Normalize prompt in PLIST by denormalizing each message.

Argument PLIST is a property list containing the data to normalize."
  (plist-put plist :messages
             (vconcat
              (mapcar
               #'gptel-extra-denormalize-prompt-item
               (plist-get plist :messages)))))

(defun gptel-extra-filter-curl--get-args (args)
  "Normalize prompt in ARGS for Curl call.

Argument ARGS is a list of prompts to be normalized for Curl command execution."
  (setcar args (gptel-extra-normalize-prompts (car args)))
  args)

;;;###autoload
(define-minor-mode gptel-extra-mode
  "Enhance GPT-EL with additional functionality.

Enable or disable additional features for GPT-EL by overriding certain functions
and cleaning up temporary files on exit. When enabled, modify the behavior of
`gptel-curl--get-args' to use custom arguments, override the state saving and
restoring functions with enhanced versions, and ensure temporary files are
cleaned up when Emacs is closed. When disabled, revert these modifications to
their original behavior."
  :lighter " gptel+"
  :group 'gptel
  :global t
  (advice-remove 'gptel-curl--get-args #'gptel-extra-filter-curl--get-args)
  (advice-remove 'gptel--save-state #'gptel-extra-save-state)
  (advice-remove 'gptel--restore-state #'gptel-extra-restore)
  (when gptel-extra-mode
    (advice-add 'gptel-curl--get-args
                :filter-args #'gptel-extra-filter-curl--get-args)
    (advice-add 'gptel--save-state :override #'gptel-extra-save-state)
    (advice-add 'gptel--restore-state :override #'gptel-extra-restore)))

(defun gptel-extra-get-files (files-or-dirs)
  "Retrieve all files from given directories and files list recursively.

Argument FILES-OR-DIRS is a list of files or directories."
  (let ((files))
    (dolist (file files-or-dirs)
      (if (file-directory-p file)
          (setq files (nconc files
                             (gptel-extra-get-files
                              (directory-files file t directory-files-no-dot-files-regexp))))
        (push file files)))
    (nreverse files)))

;;;###autoload
(defun gptel-extra-copy-dir-tree-as-org-block (dir)
  "Copy directory tree to Org block.

Argument DIR is a string representing the directory path to be processed by the
function."
  (interactive (list (read-directory-name "Directory: ")))
  (with-temp-buffer (if (zerop
                         (call-process "tree" nil t nil dir))
                        (let ((str (buffer-string)))
                          (kill-new
                           (format
                            "Here is files in %s\n#+begin_example\n%s\n#+end_example"
                            (gptel-extra-get-project-relative-name dir)
                            str))
                          (message "Copied")
                          str)
                      (message "An error occured"))))

(defun gptel-extra-magit-staged-files ()
  "List expanded paths of staged Git files."
  (require 'magit-git)
  (require 'magit)
  (let ((repo
         (when (fboundp 'magit-toplevel)
           (magit-toplevel)))
        (files
         (when (fboundp 'magit-staged-files)
           (magit-staged-files))))
    (mapcar (lambda (it)
              (expand-file-name it repo))
            files)))

(defun gptel-extra-get-dired-marked-files ()
  "Retrieve marked files from the active `dired-mode' buffer."
  (require 'dired)
  (when (fboundp 'dired-get-marked-files)
    (when-let ((buff (seq-find (lambda
                                 (buff)
                                 (and (eq (buffer-local-value 'major-mode buff)
                                          'dired-mode)
                                      (get-buffer-window buff)
                                      (with-current-buffer buff
                                        (dired-get-marked-files))))
                               (delete-dups (append (mapcar #'window-buffer
                                                            (window-list))
                                                    (buffer-list))))))
      (with-current-buffer buff
        (dired-get-marked-files)))))

;;;###autoload
(defun gptel-extra-copy-staged-files-contents-as-org-blocks ()
  "Copy staged files contents into Org blocks."
  (interactive)
  (let* ((files (gptel-extra-magit-staged-files))
         (str (gptel-extra-copy-file-contents-as-org-blocks files)))
    (kill-new str)
    (message "Copied content of %s files" (length files))
    str))

;;;###autoload
(defun gptel-extra-copy-files-contents-as-org-blocks ()
  "Copy file contents into Org-mode blocks."
  (interactive)
  (let* ((files (or (gptel-extra-get-files
                     (gptel-extra-get-dired-marked-files))
                    (and buffer-file-name (list buffer-file-name))))
         (str (gptel-extra-copy-file-contents-as-org-blocks files)))
    (kill-new str)
    (message "Copied content of %s files" (length files))
    str))


(defun gptel-extra-get-file-content (file)
  "Read and return the content of a specified FILE.

Argument FILE is a string representing the path of the FILE to be read."
  (with-temp-buffer (insert-file-contents file)
                    (buffer-substring-no-properties
                     (point-min)
                     (point-max))))

(defun gptel-extra-get-project-relative-name (file)
  "Retrieve the relative name of a FILE within its project.

Argument FILE is a string representing the FILE path."
  (require 'project)
  (let ((proj
         (ignore-errors
           (when (fboundp 'project-root)
             (project-root
              (project-current nil (if (file-directory-p file)
                                       file
                                     (file-name-parent-directory file))))))))
    (if proj
        (file-relative-name file proj)
      (abbreviate-file-name file))))

;;;###autoload
(defun gptel-extra-set-default-dir-to-project-dir ()
  "Set `default-directory' to a selected known project directory."
  (interactive)
  (require 'project)
  (when-let ((proj (completing-read "Project: " (project-known-project-roots))))
    (setq default-directory (expand-file-name proj))))

;;;###autoload
(defun gptel-extra-save ()
  "Save buffer content with timestamped filename."
  (interactive)
  (gptel-extra-save-state)
  (unless (file-exists-p gptel-extra-default-save-dir)
    (make-directory gptel-extra-default-save-dir t))
  (let ((content (buffer-substring-no-properties (point-min)
                                                 (point-max)))
        (name (expand-file-name (concat (file-name-nondirectory
                                         (or buffer-file-name (buffer-name)))
                                        "~"
                                        (format-time-string
                                         "%Y_%m_%d_%H_%M_%S"))
                                gptel-extra-default-save-dir)))
    (write-region content nil name nil nil)))

;;;###autoload
(defun gptel-extract-saved ()
  "Prompt user to select a file from saved GPT-EL states."
  (interactive)
  (when (file-exists-p gptel-extra-default-save-dir)
    (completing-read "File: "
                     (directory-files gptel-extra-default-save-dir nil
                                      directory-files-no-dot-files-regexp))))


(defun gptel-extra-file-content-as-org-block (file)
  "Get FILE content as org block with language based on file extension.

Argument FILE is a string representing the path to the file whose content will
be extracted and formatted as an org block."
  (require 'project)
  (let* ((parent-dir (file-name-parent-directory file))
         (proj (ignore-errors
                 (when (fboundp 'project-root)
                   (project-root
                    (project-current nil parent-dir)))))
         (title (if proj
                    (substring-no-properties
                     (expand-file-name file)
                     (length (expand-file-name proj)))
                  (abbreviate-file-name file)))
         (file-ext (file-name-extension file))
         (ob-lang (and file-ext
                       (cdr (assoc-string file-ext
                                          gptel-extra-ext-to-org-langs-alist))))
         (content
          (concat
           (format "#+INCLUDE: %s" file)
           " "
           (if ob-lang (format "SRC %s" ob-lang) "EXAMPLE"))))
    (concat "- " title "\n"
            "\n"
            content
            "\n\n")))

(defun gptel-extra-copy-file-contents-as-org-blocks (files)
  "Convert file contents into org blocks based on file extension.

Argument FILES is a list of strings, each representing the path to a file whose
content will be extracted and formatted as an org block."
  (mapconcat #'gptel-extra-file-content-as-org-block
             files
             "\n\n"))

(provide 'gptel-extra)
;;; gptel-extra.el ends here