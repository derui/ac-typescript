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

(eval-when-compile
  (require 'cl))
(require 'json)
(require 'url)
(require 'websocket)

(defcustom ac-typescript-client/client-executable
  (executable-find "curl")
  "*Location of http client executable"
  :group 'auto-complete
  :type 'file)

(defvar ac-typescript-client/server-port-variable
  'ac-typescript-server/server-port
  "Variable symbol to be used as port for isense server")

(defvar ac-typescript-client/ws nil
  "Websocket structure for typescript completion server")

(defvar ac-typescript-client/server-address
  "ws://localhost"
  "Server address")

(defvar ac-typescript-client/log-buffer "*ac-typescript-client Logs"
  "Log that is used to output log from client")

(defvar ac-typescript-client/debug-mode nil
  "Enable or disable debugging log for ac-typescript-client")

(defvar ac-typescript-client/timeout-second
  3
  "Second to connection timeout from server")

(defvar ac-typescript-client/add-file-message-format
  "{\"method\":\"add\", \"file\": \"%s\"}"
  "Format when send request to server to add file for using completion")

(defvar ac-typescript-client/update-buffer-message-format
  "{\"method\": \"update\", \"file\": \"%s\", \"next\":%d, \"prev\": %d, \"text\":\"%s\"}"
  "Format when send request to server to update region of file for using completion")

(defvar ac-typescript-client/update-file-message-format
  "{\"method\": \"update-file\", \"file\": \"%s\", \"content\": \"%s\"}"
  "Format when send request to server to update file for using completion")

(defvar ac-typescript-client/completion-message-format
  "{\"method\": \"completion\", \"file\":\"%s\",\"line\":%d,\"column\":%d, \"member\":%d}"
  "Format when send request to server to get completion list")

(defvar ac-typescript-client/candidate nil)
(defvar ac-typescript-client/completion-sync nil)

(defun ac-typescript-client/logger (log)
  "output log to client's log buffer"
  (when ac-typescript-client/debug-mode
    (with-current-buffer (get-buffer-create ac-typescript-client/log-buffer)
      (goto-char (point-max))
      (insert log)
      (newline))
    ))

(defun ac-typescript-client/ws-message (websocket frame)
  (let ((completion (websocket-frame-payload frame)))
    (setq ac-typescript-client/candidate completion))
  (when ac-typescript-client/completion-sync
    (setq ac-typescript-client/completion-sync nil)))

(defun ac-typescript-client/ws-opened (websocket)
  "Execute when websocket connection is opened."
  (ac-typescript-client/logger "websocket opened"))

(defun ac-typescript-client/ws-closed (websocket)
  "Execute when websocket connection is closed."
  (message "connection closed")
  (ac-typescript-client/logger websocket)
  (setq ac-typescript-client/ws nil))

(defun ac-typescript-client/ws-open-p ()
  "Return whether websocket connection open or not."
  (and ac-typescript-client/ws
       (websocket-openp ac-typescript-client/ws))
  )

(defun ac-typescript-client/make-address ()
  "Make address for websocket connection."
  (concat ac-typescript-client/server-address ":"
          (symbol-value ac-typescript-client/server-port-variable)
          "/websocket"
          )
  )

(defun ac-typescript-client/open-websocket ()
  "Open websocket protocol to completion server.
If connecting it failed, websocket structure set nil
"
  (when (not (ac-typescript-client/ws-open-p))
    (let ((ws (websocket-open
               (ac-typescript-client/make-address)
               :on-open (lambda (websocket) (ac-typescript-client/ws-opened websocket))
               :on-message (lambda (websocket frame) (ac-typescript-client/ws-message websocket frame))
               :on-close (lambda (websocket) (ac-typescript-client/ws-closed websocket)))))
      (setq ac-typescript-client/ws ws)
      )
    ))

(defun ac-typescript-client/send-message (message)
  "Send message to websocket opened."
  (when (and (not (ac-typescript-client/ws-open-p))
             (ac-typescript-server/server-running-p))
    (ac-typescript-client/open-websocket))
  (websocket-send-text ac-typescript-client/ws
                       message)
  )

(defun ac-typescript-clinet/close-websocket ()
  "Close websocket."
  (interactive)
  (websocket-close ac-typescript-client/ws))

(defun ac-typescript-client/update-buffer-region (buffer prev next text)
  "Send request that update region of given file to completion server"
  (ac-typescript-client/logger (format "Update file to completion server : file : %s %d %d %s"
                                       buffer prev next text))
  (let* ((original (expand-file-name (buffer-file-name buffer)))
         (query))
    (when (file-exists-p original)
      (ac-typescript-client/send-message (format ac-typescript-client/update-buffer-message-format original
                                                 prev next (url-hexify-string text)))
      ))
  )

(defun ac-typescript-client/add-file (filename)
  "Send request that add given file to completion server"
  (ac-typescript-client/logger (format "Add file to completion server : file : %s"
                                       filename))
  (when (file-exists-p (expand-file-name filename))
    (ac-typescript-client/send-message
     (format ac-typescript-client/add-file-message-format
             (expand-file-name filename))))
  )

(defun ac-typescript-client/get-completion-list (filename line column member)
  "Get completion list for filename at position that is composed of line and column."

  (when (file-exists-p (expand-file-name filename))
    (let ((query (format ac-typescript-client/completion-message-format
                         (url-hexify-string (expand-file-name filename)) line column
                         (if (not member) 0 1))))
      (ac-typescript-client/logger (format "Get completion list : query : %s" query))
      (setq ac-typescript-client/completion-sync t)
      (ac-typescript-client/send-message
       (format ac-typescript-client/completion-message-format
               (expand-file-name filename) line column (if (not member)
                                                           0 1)))
      (let ((timeout 3) (time 0))
        (while (and (> timeout time) ac-typescript-client/completion-sync)
          (sit-for 0.1)
          (setq time (+ time 0.1))))
      (setq ac-typescript-client/completion-sync nil)
      (let ((candidate ac-typescript-client/candidate))
        (setq ac-typescript-client/candidate nil)
        (json-read-from-string candidate))
      )
    ))

(defun ac-typescript-client/current-pos ()
  "Get current buffer position as line and column"
  (ac-typescript-client/pos-to-line-col (point)))

(defun ac-typescript-client/pos-to-line-col (pos)
  "Get current buffer position as line and column"
  `((line . ,(line-number-at-pos pos))
    (column . ,(- (point) (line-beginning-position))))
  )

(defun ac-typescript-client/update-file (original content)
  "Send request that update given file to completion server"
  (ac-typescript-client/logger (format "Update file to completion server"))
  (let* ((original (expand-file-name original)))
    (when (file-exists-p original)
      (ac-typescript-client/send-message
       (format ac-typescript-client/update-file-message-format original
               (url-hexify-string content))))
    ))

(provide 'ac-typescript-client)
