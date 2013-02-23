;;; ac-typescript-server.el --- interface of server for TypeScript completion service
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

(defcustom ac-typescript-server/node-executable
  (executable-find "node")
  "*Location of node.js executable"
  :group 'auto-complete
  :type 'file)

(defcustom ac-typescript-server/isense-location
  "./bin/isense.js"
  "*Location of typescript completion service"
  :group 'auto-complete
  :type 'file)

(defcustom ac-typescript-server/server-port
  "8124"
  "*Port of server for typescript completion service"
  :group 'auto-complete
  :type 'int)

(defvar ac-typescript-server/dir
  (concat (file-name-as-directory (file-name-directory load-file-name))
          "bin")
  "The root dir of the ac-typescript completion service distribution.")

(defconst ac-typescript-server/error-buffer-name "*ac-typescript-server error*")

;; connect to isense-server.
(defvar ac-typescript-server/proc nil)
(defvar ac-typescript-server/result nil)

(defun ac-typescript-server/run-process ()
  (unless (and ac-typescript-server/proc
               (member (process-status ac-typescript-server/proc) '(run stop exit)))
    (let ((isense (expand-file-name ac-typescript-server/isense-location))
          (port (concat "--port " ac-typescript-server/server-port)))
      (let ((current-dir default-directory))
        (cd ac-typescript-server/dir)
        (setq ac-typescript-server/proc (start-process "typescript-isense"
                                                       "*typescript-isense*"
                                                       ac-typescript-server/node-executable
                                                       isense port))
        (cd current-dir))
      )))

(defun ac-typescript-server/delete-process ()
  "delete process to ac-typescript-server"
  (let ((process ac-typescript-server/proc))
    (setq ac-typescript-server/proc nil)
    (and process (delete-process process)))
  )

(defun ac-typescript-server/server-running-p ()
  "Return server running or not."
  (and ac-typescript-server/proc
       (eq 'run (process-status ac-typescript-server/proc)))
  )

(provide 'ac-typescript-server)
