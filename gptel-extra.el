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

(defcustom gptel-extra-curl-args-file-threshold 150000
  "The size threshold for using a temporary file to pass curl arguments.

If the length of the generated curl arguments string exceeds this threshold,
the arguments are written to a temporary file instead of being passed directly
on the command line.

This can be useful for avoiding
command line length limitations when a large number of arguments are needed.

The value should be a positive integer."
  :group 'gptel-extra
  :type 'integer)

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


(defun gptel-extra-stream-normalize-markdown (str)
  "Convert bullet points to headings in a Markdown string.

Argument STR is a string that represents the markdown content to be normalized."
;; relint suppression: REGEXP
  (if (string-match-p "^[\s\t]*\\([*]+\\)" str)
      (with-temp-buffer (insert str)
                        (while
                        ;; relint suppression: REGEXP
                            (re-search-backward "^[\s\t]*\\([*]+\\)[\s\t]" nil
                                                t 1)
                          (let ((beg (match-beginning 1))
                                (end (match-end 1))
                                (count (length (match-string-no-properties 1))))
                            (replace-region-contents beg end (lambda ()
                                                               (make-string
                                                                count (string-to-char
                                                                       "#"))))))
                        (buffer-string))
    str))

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
                             (add-text-properties
                              start-marker tracking-marker
                              '(gptel response rear-nonsticky t)))))))))

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
            (gptel--update-header-line " Typing..." 'success)
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
          (setq response (gptel-extra-stream-normalize-markdown response))
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
                                     (eval . (gptel-mode 1))
                                     (gptel--bounds . gptel--get-bounds)))
           (cond ((symbolp item)
                  (add-file-local-variable item (symbol-value item)))
                 ((consp item)
                  (add-file-local-variable (car item)
                                           (if (functionp (cdr item))
                                               (funcall
                                                (cdr item))
                                             (cdr item)))))))))
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
           (add-file-local-variable 'gptel--bounds (gptel--get-bounds)))))))

(defun gptel-extra-restore ()
  "Restore text properties for response regions in the `gptel--bounds' list."
  (mapc (pcase-lambda (`(,beg . ,end))
          (put-text-property beg end 'gptel 'response))
        gptel--bounds))

(defun gptel-extra-adjust-curl-args (curl-args)
  "Modify curl arguments for binary data.

Argument CURL-ARGS is a list of strings representing the command-line arguments
to be passed to curl."
  (when-let* ((pos (seq-position curl-args "-D-"))
              (data-pos (and (length> curl-args pos)
                             (1+ pos)))
              (data-arg (nth data-pos curl-args))
              (data (and (string-prefix-p "-d" data-arg)
                         (substring-no-properties data-arg 2))))
    (append (seq-subseq curl-args 0 data-pos)
            (list "--data-binary"
                  (format "@%s"
                          (make-temp-file "gptel-curl-data"
                                          nil ".json" data)))
            (seq-subseq curl-args (1+ data-pos)))))

(defun gptel-extra-curl-get-args (old-fn &rest args)
  "Customize curl arguments based on length threshold.

Argument OLD-FN is a function to be called with ARGS.

Remaining arguments ARGS are passed to OLD-FN."
  (let ((curl-args (apply old-fn args)))
    (if (not (length> (string-join curl-args " ")
                      gptel-extra-curl-args-file-threshold))
        curl-args
      (or (gptel-extra-adjust-curl-args curl-args)
          curl-args))))


(defun gptel-extra-cleanup-temp-files ()
  "Delete temporary `gptel-curl-data' files."
  (dolist (file
           (directory-files (temporary-file-directory) t "\\`gptel-curl-data"))
    (delete-file file)))

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
  (advice-remove 'gptel-curl--get-args #'gptel-extra-curl-get-args)
  (advice-remove 'gptel--save-state #'gptel-extra-save-state)
  (advice-remove 'gptel--restore-state #'gptel-extra-restore)
  (remove-hook 'kill-emacs-hook #'gptel-extra-cleanup-temp-files)
  (when gptel-extra-mode
    (advice-add 'gptel-curl--get-args
                :around #'gptel-extra-curl-get-args)
    (advice-add 'gptel--save-state :override #'gptel-extra-save-state)
    (advice-add 'gptel--restore-state :override #'gptel-extra-restore)
    (add-hook 'kill-emacs-hook #'gptel-extra-cleanup-temp-files)))




(provide 'gptel-extra)
;;; gptel-extra.el ends here