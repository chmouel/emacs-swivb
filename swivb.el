;;; swivb.el --- Switch to buffer/recent/bookmark/functions

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Chmouel Boudjnah <chmouel@chmouel.com>
;; URL: https://github.com/chmouel/emacs-swivb
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.0"))
;; Keywords: matching

;; This file is part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; This package extract the ivy-virtual-buffer feature to its own, and add a
;;; function mode, where you can switch to a buffer or launch a function i.e:
;;; (setq swivb-actions '((:name "*Gnus*" :function gnus :buffer "*Group*")))
;;; Will launch the function gnus if you choose the *Gnus* in selection, if the
;;; buffer Group doesn't exist. By default it does a switch-to-bufer, you can
;;; override this with somethign like this :
;;;
;;; (swivb 'find-file-other-tab 'swivb-switch-to-buffer-or-focus-tab)
;;;
;;; The swivb-switch-to-buffer-or-focus-tab function will focus a tab if the
;;; buffer name exist or switch-to-bufer it.

(defvar swivb-user-virtual-buffers nil)
(defvar swivb-use-virtual-buffers t)
(defvar swivb-virtual-abbreviate t)
(defvar swivb--virtual-buffers nil)
(defvar swivb-actions nil)

(defun swivb--virtual-buffers ()
  "Adapted from `ido-add-virtual-buffers-to-list'."
  (require 'bookmark)
  (unless recentf-mode
    (recentf-mode 1))
  (bookmark-maybe-load-default-file)
  (let* ((vb-bkm (delete "   - no file -"
                         (delq nil (mapcar #'bookmark-get-filename
										   bookmark-alist))))
         (vb-list (cond ((eq swivb-use-virtual-buffers 'recentf)
                         recentf-list)
                        ((eq swivb-use-virtual-buffers 'bookmarks)
                         vb-bkm)
                        (swivb-use-virtual-buffers
                         (append recentf-list vb-bkm))
                        (t nil)))
         virtual-buffers)
    (dolist (head vb-list)
	  (let* ((file-name (if (stringp head)
                            head
						  (cdr head)))
             (name (cond ((eq swivb-virtual-abbreviate 'name)
						  (file-name-nondirectory file-name))
                         ((eq swivb-virtual-abbreviate 'abbreviate)
						  (abbreviate-file-name file-name))
                         (t
						  (expand-file-name file-name)))))
        (when (equal name "")
		  (setq name
                (if (consp head)
                    (car head)
				  (file-name-nondirectory (directory-file-name file-name)))))
        (unless (or (equal name "")
                    (get-file-buffer file-name)
                    (assoc name virtual-buffers))
		  (push (cons (copy-sequence name) file-name) virtual-buffers))))
    (when virtual-buffers
	  (dolist (comp virtual-buffers)
        (put-text-property 0 (length (car comp))
						   'face 'swivb-virtual
						   (car comp)))
	  (setq swivb--virtual-buffers (nreverse virtual-buffers))
	  (mapcar #'car swivb--virtual-buffers))))

(defun swivb--buffer-list (str &optional virtual predicate)
  "Return the buffers that match STR.
If VIRTUAL is non-nil, add virtual buffers.
If optional argument PREDICATE is non-nil, use it to test each
possible match.  See `all-completions' for further information."
  (delete-dups
   (nconc
	(mapcar
	 (lambda (elt)
	   (plist-get elt :name)) swivb-actions)	
    (all-completions str #'internal-complete-buffer predicate)
    (and virtual
         (swivb--virtual-buffers)))))

(defun swivb-switch-to-buffer-or-focus-tab (buffer &optional func)
  "Focus tab if we find it or switch to a new tab on buffer"
  (let* ((recent-tabs
	      (mapcar
	       (lambda (tab)
			 (alist-get 'name tab))
	       (tab-bar--tabs-recent))))
    (if (member buffer recent-tabs)
		(tab-bar-switch-to-tab buffer)
	  (if func (funcall (intern func))
		(switch-to-buffer buffer)))))

;;;###autoload
(defun swivb (&optional find-file-func switch-buffer-func)
  (interactive)
  (let* ((swivb-virtual-abbreviate 'abbreviate)
		 (swivb-use-virtual-buffers t)
		 (selection
	      (completing-read
	       "☕ Switch to: " (swivb--buffer-list "" t) nil t)))
    (if (zerop (length selection))
		(switch-to-buffer swivb-text)
	  (let ((virtual (assoc selection swivb--virtual-buffers))
			funcelt)
		(dolist (helt swivb-actions)
		  (if (string= (plist-get helt :name) selection)
			  (setq funcelt helt)))
		(cond (funcelt
			   (swivb-switch-to-buffer-or-focus-tab
				(plist-get funcelt :buffer)
				(plist-get funcelt :function)))
			  ((and virtual
					(not (get-buffer selection)))
			   (funcall
				(if find-file-func
					find-file-func #'find-file) (cdr virtual)))
			  (t (funcall
				  (if switch-buffer-func switch-buffer-func #'switch-to-buffer) selection)))))))

(provide 'swivb)
