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

(defvar ac-typescript/debug-mode nil)

(defface ac-typescript/candidate-face
  '((t (:background "lightgray" :foreground "navy")))
  "Face for ts candidate"
  :group 'auto-complete)

(defface ac-typescript/selection-face
  '((t (:background "navy" :foreground "white")))
  "Face for the ts selected candidate."
  :group 'auto-complete)

(defun ac-typescript/init-server ()
  "Running server for prepareation to use completion."
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
  (let ((member-entries (cdr (assoc 'entries info))))
    (mapcar (lambda (ent)
              (let ((name (cdr (assoc 'name ent)))
                    (kind (cdr (assoc 'kind ent)))
                    (type (cdr (assoc 'type ent))))
                (propertize name 'ac-typescript/help
                            (concat kind ":" type))))
            member-entries)))

(defun ac-typescript/candidate ()
  (when ac-typescript/debug-mode
    (message "ac-typescript/candidate '%s'" ac-prefix))
  (unless (ac-in-string/comment)
    (let ((file-name (expand-file-name (buffer-file-name))))
      (prog1
          (save-restriction
            (widen)
            (let* ((point (- (point) (length ac-prefix)))
                   (pos (ac-typescript-client/pos-to-line-col point))
                   (member (eq  ?\. (char-before ac-point)))
                   (result (ac-typescript-client/get-completion-list
                            file-name
                            (assoc 'line pos) (assoc 'column pos)
                            member)))
              (if (listp result)
                  (ac-typescript/get-completions result)
                nil)))))))

(defun ac-typescript/prefix ()
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

(defun ac-typescript/ac-enable ()
  "Enable auto-complete and setting to use ac-typescript."
  (interactive)
  (setq ac-sources (append ('ac-source-typescrip9t) ac-sources))
  (auto-complete-mode 1)
  )

(provide 'ac-typescript)
