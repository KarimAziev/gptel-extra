;;; gptel-extra.el --- Enhancers for Gptel ChatGPT client -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/gptel-extra
;; Version: 0.1.0
;; Keywords: convenience tools
;; Package-Requires: ((emacs "26.1") (gptel "0.4.0"))
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
                               (delete-region beg end)))))))))

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
                (insert str)
                (when (string-match-p str "#\\+end_src")
                  (re-search-backward "#\\+end_src" nil t 1))
                (forward-line -1)))
            (setq tracking-marker (set-marker (make-marker) (point)))
            (set-marker-insertion-type tracking-marker t)
            (plist-put info :tracking-marker tracking-marker))
          (setq response (gptel-extra-stream-normalize-markdown response))
          (put-text-property 0 (length response) 'gptel 'response response)
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



(provide 'gptel-extra)
;;; gptel-extra.el ends here