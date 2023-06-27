;;; keycaster.el --- Visualize last pressed key-binding and command -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/keycaster
;; Version: 0.1.0
;; Keywords: multimedia
;; Package-Requires: ((emacs "25.1"))
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

;; Visualize last pressed key-binding and command

;;; Code:

(defcustom keycaster-single-use-separator nil
  "Whether to draw a line between the keycaster window."
  :group 'keycaster
  :type 'boolean)

(defcustom keycaster-window-position 'below
  "Where to show key and command."
  :group 'keycaster
  :type '(radio (const :tag "Above" above)
                (const :tag "Below" below)))

(defface keycaster-command '((t (:weight bold)))
  "Face used the displaying command in the keycaster window."
  :group 'keycaster)

(defface keycaster-key
  '((t (:inherit fixed-pitch
                 :weight bold
                 :background "#eeeedddd8282"
                 :foreground "#000000"
                 :box (:line-width -2
                                   :style released-button))))
  "Face used for displaying keybinding in the keycaster window."
  :group 'keycaster)

(defvar keycaster-window nil
  "Current keycaster window.")

(defun keycaster-get-window-create ()
  "Show and select keycaster window creating a new one if needed."
  (if (window-live-p keycaster-window)
      keycaster-window
    (let ((orig-wind (selected-window))
          buff)
      (prog1 (setq keycaster-window
                   (select-window
                    (let ((ignore-window-parameters t))
                      (split-window
                       (frame-root-window) -2
                       keycaster-window-position))
                    'norecord))
        (if (setq buff (get-buffer " *Keycaster*"))
            (switch-to-buffer buff 'norecord)
          (switch-to-buffer " *Keycaster*" 'norecord)
          (fundamental-mode)
          (set-window-hscroll keycaster-window 0)
          (setq window-size-fixed t)
          (setq mode-line-format nil)
          (setq header-line-format nil)
          (setq tab-line-format nil)
          (setq cursor-type nil)
          (setq display-line-numbers nil)
          (setq display-fill-column-indicator nil)
          (set-window-dedicated-p keycaster-window t)
          (set-window-parameter keycaster-window 'no-other-window t)
          (when (boundp 'aw-ignored-buffers)
            (add-to-list 'aw-ignored-buffers " *Keycaster*")))
        (select-window orig-wind 'norecord)))))

(defvar golden-ratio-mode)

(defun keycaster-message (str)
  "Show STR in keycaster window."
  (let* ((n-lines (length (split-string str "\n")))
         golden-ratio-mode
         deactivate-mark)
    (with-selected-window (keycaster-get-window-create)
      (unless (string= (buffer-string) str)
        (delete-region (point-min)
                       (point-max))
        (goto-char (point-max))
        (insert str)
        (when (and (window-system) keycaster-single-use-separator)
          (unless (looking-back "\n" nil)
            (insert "\n")))
        (set (make-local-variable 'window-min-height) n-lines)
        (setq truncate-lines (> n-lines 1))
        (let ((window-resize-pixelwise t)
              (window-size-fixed nil))
          (fit-window-to-buffer nil nil 1)))
      (goto-char (point-min)))))

(defun keycaster-delete-window ()
  "Delete keycaster window and buffer."
  (when (and keycaster-window
             (window-live-p keycaster-window))
    (let ((buf (window-buffer keycaster-window)))
      (delete-window keycaster-window)
      (kill-buffer buf)))
  (setq keycaster-window nil))

(defvar keycaster-self-insert-stack nil)

(defun keycaster-show ()
  "Update key description for single command."
  (let ((key (this-single-command-keys))
        (cmd this-command)
        (description))
    (if (not (memq this-command '(self-insert-command
                                  org-self-insert-command)))
        (setq keycaster-self-insert-stack nil)
      (setq keycaster-self-insert-stack (push key keycaster-self-insert-stack)))
    (setq description
          (cond (keycaster-self-insert-stack
                 (format-spec
                  "%2s%k %c"
                  `((?s . "")
                    (?k . ,(propertize "Typing: " 'face 'keycaster-key))
                    (?c . ,(propertize (mapconcat
                                        (lambda (it)
                                          (replace-regexp-in-string
                                           "SPC" " " (or
                                                      (ignore-errors
                                                        (key-description
                                                         it))
                                                      "")))
                                        (reverse keycaster-self-insert-stack))
                                       'face 'keycaster-key)))))
                (t
                 (let ((count
                        (length (append
                                 (seq-take-while
                                  (lambda (it)
                                    (eq this-command (cdr-safe it)))
                                  (reverse (seq-filter 'consp (recent-keys
                                                               t))))
                                 nil))))
                   (format-spec
                    "%2s%k %c %n"
                    `((?s . "")
                      (?k . ,(propertize (or
                                          (ignore-errors
                                            (key-description key))
                                          "")
                                         'face 'keycaster-key))
                      (?c . ,(concat (propertize
                                      (pcase cmd
                                        ((pred symbolp)
                                         (symbol-name cmd))
                                        ((guard
                                          (and (eq (car-safe cmd) 'lambda)))
                                         "<lambda>")
                                        (_ (format "<%s>" (type-of cmd))))
                                      'face
                                      'keycaster-command)))
                      (?n ,(or
                            (when (> count 0)
                              count)
                            ""))))))))
    (keycaster-message description)))

(defun keycaster-gif-screencast-modes-toggle ()
  "Run presentation and keycaster mode if `gif-screencast-mode' is on."
  (require 'presentation-mode nil t)
  (when (and (boundp 'presentation-mode)
             (symbol-value 'presentation-mode)
             (fboundp 'presentation-mode))
    (presentation-mode -1)
    (keycaster-mode -1))
  (when (and (boundp 'gif-screencast-mode)
             (boundp 'symbol-value))
    (when (fboundp 'presentation-mode)
      (presentation-mode 1))
    (keycaster-mode 1)))

;;;###autoload
(define-minor-mode keycaster-gif-screencast-mode
  "Toggle running keycaster when `gif-screencast-mode-hook' is turned on."
  :group 'keycaster
  :global t
  (remove-hook 'gif-screencast-mode-hook 'keycaster-gif-screencast-modes-toggle)
  (when keycaster-gif-screencast-mode
    (add-hook 'gif-screencast-mode-hook 'keycaster-gif-screencast-modes-toggle)))

;;;###autoload
(define-minor-mode keycaster-mode
  "Toggle showing current command and keybinding when this mode on."
  :group 'keycaster
  :global t
  (keycaster-delete-window)
  (remove-hook 'pre-command-hook #'keycaster-show)
  (when keycaster-mode
    (add-hook 'pre-command-hook #'keycaster-show)))

(provide 'keycaster)
;;; keycaster.el ends here