;;; conceal.el --- Minor mode for concealing buffer content -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>
;; Version: 0.0.1
;; Package-Requires: ((emacs "26") (s "1.12.0"))
;; URL: https://github.com/lepisma/conceal.el

;;; Commentary:

;; Minor mode for concealing buffer content
;; This file is not a part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'overlay)
(require 's)

(defcustom conceal-buffer-p #'conceal-buffer-gpg-p
  "Function that tells which buffers to conceal when conceal-mode
is active.")

(defvar-local conceal-overlays nil
  "Buffer local variable holding the line overlays.")

(defun conceal-buffer-gpg-p (buffer)
  (s-ends-with-p ".gpg" (buffer-file-name buffer)))

(defun conceal-text (text)
  (s-replace-regexp "[[:graph:]]" "▃" text))

(defun conceal-current-line ()
  "Hide current line and return overlay."
  (let* ((start (line-beginning-position))
         (end (line-end-position))
         (ov (make-overlay start end)))
    (overlay-put ov 'display (conceal-text (buffer-substring-no-properties start end)))
    ov))

(defun conceal-current-buffer ()
  (let ((ovs))
    (save-excursion
      (dotimes (i (count-lines (point-min) (point-max)))
        (goto-line (+ i 1))
        (push (conceal-current-line) ovs)))
    (setq conceal-overlays ovs)))

(defun conceal-clear-buffer ()
  (dolist (ov conceal-overlays)
    (delete-overlay ov))
  (setq conceal-overlays nil))

(defun conceal-mode-enable ()
  (dolist (buffer (buffer-list))
    (when (funcall conceal-buffer-p buffer)
      (with-current-buffer buffer
        (conceal-current-buffer)))))

(defun conceal-mode-disable ()
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (conceal-clear-buffer))))

;;;###autoload
(define-minor-mode conceal-mode
  "Minor mode to hide content from specific buffers."
  :init-value nil
  :global t
  (if conceal-mode (conceal-mode-enable) (conceal-mode-disable)))

(provide 'conceal)

;;; conceal.el ends here
