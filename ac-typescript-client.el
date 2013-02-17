;;; ac-typescript-client.el --- interface of client for TypeScript completion service
;; Copyright (C) 2013 derui

;; Author: derui <derutakayu@gmail.com>
;; Keywords: completion, convenience
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Many code are from Kensuke Matsuzaki's typescript-tss.el

(require 'json)

(defcustom ac-typescript-client/client-executable
  (executable-find "curl")
  "*Location of http client executable"
  :group 'auto-complete
  :type 'file)

(defvar ac-typescript-client/server-port-variable
  'ac-typescript-server/server-port
  "Variable symbol to be used as port for isense server")

(defvar ac-typescript-client/server-address
  "http://localhost"
  "Server address")

(defvar ac-typescript-client/timeout-second
  3
  "Second to connection timeout from server")

(defvar ac-typescript-client/add-file-request-format
  "/?method=add&file=%s"
  "Format when send request to server to add file for using completion")

(defvar ac-typescript-client/completion-request-format
  "/?method=completion&file=%s&line=%d&column=%d&member=%d"
  "Format when send request to server to get completion list")

;; connect to isense-client.
(defvar ac-typescript-client/proc nil)
(make-variable-buffer-local 'ac-typescript-client/proc)
(defvar ac-typescript-client/result nil)
(make-variable-buffer-local 'ac-typescript-client/result)

(defun ac-typescript-client/run-process (query)
  (unless (and ac-typescript-client/proc
               (member (process-status ac-typescript-client/proc) '(run stop)))
    (with-temp-buffer
      (let ((ret (call-process-shell-command
                  ac-typescript-client/client-executable
                  nil t nil
                  (concat "--connect-timeout " ac-typescript-client/timeout-second)
                  (concat ac-typescript-client/server-address
                          ":"
                          (symbol-value ac-typescript-client/server-port-variable)
                          query))))
        (unless (zerop ret)
          (error (concat "Failed request action as " query)))
        (json-read-from-string (buffer-substring-no-properties (buffer-end -1) (buffer-end 1))
                               )))))

(defun ac-typescript-client/add-file (filename)
  "Send request that add given file to completion server"
  (when (file-exists-p (expand-file-name filename))
    (unwind-protect
        (progn
          (ac-typescript-client/run-process (format ac-typescript-client/add-file-request-format
                                                    (expand-file-name filename)))
          )
      (progn
        (message "Failed adding file to server")))
    ))

(defun ac-typescript-client/get-completion-list (filename line column member)
  "Get completion list for filename at position that is composed of line and column."
  (when (file-exists-p (expand-file-name filename))
    (unwind-protect
        (ac-typescript-client/run-process
         (format ac-typescript-client/completion-request-format
                 filename line column (cond ((not member)
                                             "0")
                                            (t
                                             "1"))))
      (message "Failed get completion list from server"))
    ))

(defun ac-typescript-client/current-pos ()
  "Get current buffer position as line and column"
  (ac-typescript-client/pos-to-line-col (point)))

(defun ac-typescript-client/pos-to-line-col (pos)
  "Get current buffer position as line and column"
  `((line . ,(line-number-at-pos pos))
    (column . ,(- (point) (line-beginning-position))))
  )

(defun ac-typescript-client/update-buffer-file (buffer)
  (when buffer
    (let ((buffer-file (buffer-file-name buffer)))
      ;; TODO: implement update method on server
      (ac-typescript-client/add-file buffer-file)))
  )

;; (defun ac-typescript-client/show-type ()
;;   (interactive)
;;   (message "Type: %s" (ac-typescript-client/default-command "type")))

;; (defun ac-typescript-client/show-symbol ()
;;   (interactive)
;;   (message "Symbol: %s" (ac-typescript-client/default-command "symbol")))

;; (defun ac-typescript-client/show-definition ()
;;   (interactive)
;;   (message "Definition: %s" (ac-typescript-client/default-command "definition")))

;; (defun ac-typescript-client/goto-definition ()
;;   (interactive)
;;   (let ((def (ac-typescript-client/default-command "definition")))
;;     (if def
;;         (let* ((file (cdr (assoc 'file def)))
;;                (min (cdr (assoc 'min def)))
;;                (l (elt min 0))
;;                (c (elt min 1)))
;;           (find-file file)
;;           (beginning-of-line))
;;       (message "Not found"))))

(provide 'ac-typescript-client)
