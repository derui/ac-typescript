;;; ac-typescript.el ---  Source for AutoComplete for typescript
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

;; Many code are from Kensuke Matsuzaki's auto-complete-ts.el

(require 'auto-complete)
(require 'ac-typescript-server)
(require 'ac-typescript-client)

(defcustom ac-typescript/auto-register t
  "*Determines whether to save the buffer when retrieving completions."
  :group 'auto-complete
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom ac-typescript/auto-register-interval 1
  "*Interval timer executing auto update each interval when emacs idle while this time"
  :group 'auto-complete)

(defvar ac-typescript/debug-mode nil)
(defvar ac-typescript/timer-handle nil)

(defface ac-typescript/candidate-face
  '((t (:background "lightgray" :foreground "navy")))
  "Face for ts candidate"
  :group 'auto-complete)

(defface ac-typescript/selection-face
  '((t (:background "navy" :foreground "white")))
  "Face for the ts selected candidate."
  :group 'auto-complete)

(defun ac-typescript/start-server ()
  "Running server for prepareation to use completion."
  (interactive)
  (ac-typescript-server/run-process))

(defun ac-typescript/stop-server ()
  "Stop server if it is running."
  (interactive)
  (ac-typescript-server/delete-process))

(defun ac-typescript/document (item)
  (if (stringp item)
      (let (s)
        (setq s (get-text-property 0 'ac-typescript/help item))
        s))
  )

(defsubst ac-in-string/comment ()
  "Return non-nil if point is in a literal (a comment or string)."
  (nth 8 (syntax-ppss)))

(defun ac-typescript/get-completions (info)
  (mapcar (lambda (ent)
            (let ((name (cdr (assoc 'name ent)))
                  (kind (cdr (assoc 'kind ent)))
                  (type (cdr (assoc 'type ent))))
              (propertize name 'ac-typescript/help
                          (concat kind ":" type))))
          info))

(defun ac-typescript/inner-candidate-callback (result)
  (if (vectorp result)
      (ac-typescript/get-completions result)
    nil))

(defun ac-typescript/candidate ()
  (when ac-typescript/debug-mode
    (message "ac-typescript/candidate '%s'" ac-prefix))
  (unless (ac-in-string/comment)
    (let ((file-name (expand-file-name (buffer-file-name)))
          (callback 'ac-typescript/inner-candidate-callback))
      (prog1
          (save-restriction
            (widen)
            (let* ((point (- (point) (length ac-prefix)))
                   (pos (ac-typescript-client/pos-to-line-col point))
                   (member (eq  ?\. (char-before ac-point)))
                   (result (ac-typescript-client/get-completion-list
                            file-name
                            (cdr (assoc 'line pos)) (cdr (assoc 'column pos))
                            member)))
              (ac-typescript/inner-candidate-callback result)))))))

(defun ac-typescript/prefix ()
  "Define prefix is given timing when fire ac-complete"
  (or (ac-prefix-symbol)
      (let ((c (char-before)))
        (when (or (eq ?\. c))
          (point)))))

(ac-define-source typescript
  '((candidates . ac-typescript/candidate)
    (candidate-face . ac-typescript/candidate-face)
    (selection-face . ac-typescript/selection-face)
    (prefix . ac-typescript/prefix)
    (requires . 0)
    (document . ac-typescript/document)
    (cache)
    (symbol . "t")))

(defun ac-typescript/register ()
  "Register current buffer file to completion server."
  (interactive)
  (let* ((buffer (current-buffer))
         (filename (buffer-file-name buffer)))
    (when (and (ac-typescript-server/server-running-p)
               (string-match ".+\\.ts$" filename))
      (ac-typescript-client/add-file filename)
      ))
  )

(defun ac-typescript/update (target-buffer)
  "Register current buffer file to completion server."
  (interactive)
  (with-current-buffer target-buffer
    (let* ((buffer (if target-buffer target-buffer (current-buffer)))
           (buffer-filename (buffer-file-name buffer))
           (contents (buffer-substring-no-properties (point-min) (point-max)))
           (filename (make-temp-file "tscomp")))
      (when (and (ac-typescript-server/server-running-p)
                 (ac-typescript/typescriptp buffer))
        (with-temp-file filename
          (insert contents)
          )
        (ac-typescript-client/update-file buffer-filename contents)
        ))
    ))

(defun ac-typescript/typescriptp (buffer)
  "Return whether given buffer is typescript or not"
  (and buffer
       (buffer-file-name buffer)
       (if (string-match "^.+\\.ts$" (buffer-file-name buffer)) t nil))
  )

(defun ac-typescript/auto-update-each-buffer ()
  "Update all buffer which are typescript"
  (interactive)
  (mapcar (lambda (buffer)
            (when (and (ac-typescript/typescriptp buffer)
                       (buffer-modified-p buffer))
              (ac-typescript/update buffer))
            )
          (buffer-list))
  )

(defun ac-typescript/update-range (begin end old-text-length)
  "Update registered script with previous and current point what edit on the buffer"
  (when (ac-typescript/typescriptp (current-buffer))
    (let ((contents (buffer-substring-no-properties begin end)))
       (if (= begin end)
           ;; if remove some text
           (ac-typescript-client/update-buffer-region
            (current-buffer) begin (+ end old-text-length) "")
         (ac-typescript-client/update-buffer-region
            (current-buffer) begin (- end (length contents)) contents))
    )))

(defun ac-typescript/ac-enable ()
  "Enable auto-complete and setting to use ac-typescript."
  (interactive)
  (setq ac-sources (append '(ac-source-typescript) ac-sources))
  (auto-complete-mode 1)

  (add-hook 'after-save-hook 'ac-typescript/register)
  (make-variable-buffer-local 'after-change-functions)
  (add-hook 'after-change-functions 'ac-typescript/update-range)

  (when (and ac-typescript/auto-register
             (ac-typescript-server/server-running-p))
    (ac-typescript-client/add-file (buffer-file-name (current-buffer)))
    )
  )

(provide 'ac-typescript)
