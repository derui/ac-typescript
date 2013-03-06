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

(defvar ac-typescript-client/add-file-request-format
  "/?method=add&file=%s"
  "Format when send request to server to add file for using completion")

(defvar ac-typescript-client/update-buffer-request-format
  "/?method=update&file=%s&prev=%d&next=%d&text=%s"
  "Format when send request to server to update region of file for using completion")

(defvar ac-typescript-client/update-file-request-format
  "/?method=update-file&file=%s"
  "Format when send request to server to update file for using completion")

(defvar ac-typescript-client/completion-request-format
  "/?method=completion&file=%s&line=%d&column=%d&member=%d"
  "Format when send request to server to get completion list")

(defun ac-typescript-client/logger (log)
  "output log to client's log buffer"
  (when ac-typescript-client/debug-mode
    (with-current-buffer (get-buffer-create ac-typescript-client/log-buffer)
      (goto-char (point-max))
      (insert log)
      (newline))
    ))

(defun ac-typescript-client/make-query (query)
  (concat "\"" ac-typescript-client/server-address
          ":"
          (symbol-value ac-typescript-client/server-port-variable)
          query "\""))

(defun ac-typescript-client/ws-message (websocket frame)
  (push (websocket-frame-payload frame) wstest-msgs)
  (message "ws frame: %S" (websocket-frame-payload frame))
  (error "Test error (expected)"))

(defun ac-typescript-client/ws-opened ()
  "Execute when websocket connection is opened."
  (ac-typescript-client/logger "websocket opened"))

(defun ac-typescript-client/ws-closed ()
  "Execute when websocket connection is closed."
  (setq ac-typescript-client/ws nil))

(defun ac-typescript-client/ws-open-p ()
  "Return whether websocket connection open or not."
  (and ac-typescript-client/ws
       (websocket-openp ac-typescript-client/ws))
  )

(defun ac-typescript-client/make-address ()
  "Make address for websocket connection."
  (concat ac-typescript-client/server-address ":"
          (symbol-value ac-typescript-client/server-port-variable))
  )

(defun ac-typescript-client/open-websocket ()
  "Open websocket protocol to completion server.
If connecting it failed, websocket structure set nil
"
  (let ((ws (websocket-open
             (ac-typescript-client/make-address)
             :on-open 'ac-typescipt-client/ws-opened
             :on-message 'ac-typescript-client/ws-message
             :on-close 'ac-typescript-client/ws-closed)))
    (setq ac-typescript-client/ws ws)
    )
  )

(defun ac-typescript-client/run-process (query &rest callback)
  (ac-typescript-client/logger
   (format "Running http client with : %s" query))

  (lexical-let ((q query))
    (deferred:$
      (deferred:process-shell ac-typescript-client/client-executable
        "--connect-timeout" (int-to-string ac-typescript-client/timeout-second)
        "-s"
        q)
      (deferred:nextc it
        (lambda (result)
          (when ac-typescript-client/debug-mode
            (ac-typescript-client/logger result))
          (when callback
            (funcall callback result))))
      (deferred:error it
        (lambda (err)
          (ac-typescript-client/logger err)))))
  )

(defun ac-typescript-client/run-process-sync (query)
  (ac-typescript-client/logger
   (format "Running http client with : %s" query))
  (with-temp-buffer
    (let ((ret (call-process-shell-command ac-typescript-client/client-executable
                             nil t nil
                             "--connect-timeout" (int-to-string ac-typescript-client/timeout-second)
                             "-s"
                             query)))
      (unless (zerop ret)
        (error "Running synchronous process occured error!"))

      (let ((result (buffer-substring-no-properties (point-min) (point-max))))
        (ac-typescript-client/logger result)
        (json-read-from-string result)
      )
    )
  ))

(defun ac-typescript-client/update-buffer-region (buffer prev next text)
  "Send request that update region of given file to completion server"
  (ac-typescript-client/logger (format "Update file to completion server : file : %s %d %d %s"
                                       buffer prev next text))
  (let* ((original (expand-file-name (buffer-file-name buffer)))
         (query (ac-typescript-client/make-query
                 (format ac-typescript-client/update-buffer-request-format original
                         prev next (json-encode text)))))
    (when (file-exists-p original)
      (ac-typescript-client/run-process query)))
  )


(defun ac-typescript-client/add-file (filename)
  "Send request that add given file to completion server"
  (ac-typescript-client/logger (format "Add file to completion server : file : %s"
                                       filename))
  (when (file-exists-p (expand-file-name filename))
    (ac-typescript-client/run-process
     (ac-typescript-client/make-query (format ac-typescript-client/add-file-request-format
                                              (url-hexify-string (expand-file-name filename)))))
    )
  )

(defun ac-typescript-client/get-completion-list (filename line column member)
  "Get completion list for filename at position that is composed of line and column."

  (when (file-exists-p (expand-file-name filename))
    (let ((query (format ac-typescript-client/completion-request-format
                         (url-hexify-string (expand-file-name filename)) line column
                         (if (not member) 0 1))))
      (ac-typescript-client/logger (format "Get completion list : query : %s" query))
      (ac-typescript-client/run-process-sync
       (ac-typescript-client/make-query
        (format ac-typescript-client/completion-request-format
                (url-hexify-string (expand-file-name filename)) line column (if (not member)
                                                                                 0 1)))
        )
       )))

(defun ac-typescript-client/current-pos ()
  "Get current buffer position as line and column"
  (ac-typescript-client/pos-to-line-col (point)))

(defun ac-typescript-client/pos-to-line-col (pos)
  "Get current buffer position as line and column"
  `((line . ,(line-number-at-pos pos))
    (column . ,(- (point) (line-beginning-position))))
  )

(defun ac-typescript-client/update-file (original filename)
  "Send request that update given file to completion server"
  (ac-typescript-client/logger (format "Update file to completion server"))
  (let* ((original (expand-file-name original))
         (query (ac-typescript-client/make-query
                 (format ac-typescript-client/update-file-request-format original))))
    (when (file-exists-p original)
      (ac-typescript-client/run-process
       (concat "--data-binary '@" filename "' " query)
       )))
  )

(provide 'ac-typescript-client)
