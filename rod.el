;;; rod.el --- Stupidly simple non-persistent bookmarks for buffers

;; Author: Harvey Lin

;;; Commentary:

;;; This plugin provides a dead simple bookmark system
;;; for quickly navigating to open buffers inspired by
;;; the functionality of harpoon.

;;; Code:

(defconst *rod-num-slots* 5)

(defvar rod-buffer-list ["", "", "", "", ""])


(defun rod--set-slot (slot)
  "Set the given slot in the buffer list with the current buffer name.
SLOT: the slot to set."
  (aset rod-buffer-list slot (buffer-name))
  (message "Set %s to slot %d" (buffer-name) (+ 1 slot)))

(defun rod--go-to-slot (slot)
  "Go to the buffer, if any, stored in the slot.
SLOT: the slot containing the buffer to go to."
  (let ((buf-name (aref rod-buffer-list slot)))
    (if (or (not (stringp buf-name))
         (string-empty-p buf-name))
        (error "Slot %d is empty" (+ 1 slot))
      (switch-to-buffer buf-name nil t))))

;;;###autoload
(defun rod-set-1 ()
  "Set slot 1 in rod."
  (interactive)
  (rod--set-slot 0))

;;;###autoload
(defun rod-set-2 ()
  "Set slot 2 in rod."
  (interactive)
  (rod--set-slot 1))

;;;###autoload
(defun rod-set-3 ()
  "Set slot 3 in rod."
  (interactive)
  (rod--set-slot 2))

;;;###autoload
(defun rod-set-4 ()
  "Set slot 4 in rod."
  (interactive)
  (rod--set-slot 3))

;;;###autoload
(defun rod-set-5 ()
  "Set slot 5 in rod."
  (interactive)
  (rod--set-slot 4))

;;;###autoload
(defun rod-go-to-1 ()
  "Go to buffer in slot 1 in rod."
  (interactive)
  (push-mark)
  (rod--go-to-slot 0))

;;;###autoload
(defun rod-go-to-2 ()
  "Go to buffer in slot 2 in rod."
  (interactive)
  (push-mark)
  (rod--go-to-slot 1))

;;;###autoload
(defun rod-go-to-3 ()
  "Go to buffer in slot 3 in rod."
  (interactive)
  (push-mark)
  (rod--go-to-slot 2))

;;;###autoload
(defun rod-go-to-4 ()
  "Go to buffer in slot 4 in rod."
  (interactive)
  (push-mark)
  (rod--go-to-slot 3))

;;;###autoload
(defun rod-go-to-5 ()
  "Go to buffer in slot 5 in rod."
  (interactive)
  (push-mark)
  (rod--go-to-slot 4))

(provide 'rod)
;;; rod.el ends here
