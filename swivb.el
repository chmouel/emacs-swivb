(defvar swivb-user-virtual-buffers nil)
(defvar swivb-use-virtual-buffers t)
(defvar swivb-virtual-abbreviate t)

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

(defvar swivb-actions '((:name "*switch-to-gnus*" :function "my-switch-to-gnus" :buffer "*Group*")))

(defun swivb--buffer-list (str &optional virtual predicate)
  "Return the buffers that match STR.
If VIRTUAL is non-nil, add virtual buffers.
If optional argument PREDICATE is non-nil, use it to test each
possible match.  See `all-completions' for further information."
  (delete-dups
   (nconc
	swivb-actions)))
    ;; (all-completions str #'internal-complete-buffer predicate)
    ;; (and virtual
    ;;      (swivb--virtual-buffers)))))

(defun swivfb-switch-to-buffer-or-focus-tab (buffer)
  "Focus tab if we find it or switch to a new tab on buffer"
  (let* ((recent-tabs
	      (mapcar
	       (lambda (tab)
			 (alist-get 'name tab))
	       (tab-bar--tabs-recent))))
    (if (member buffer recent-tabs)
		(tab-bar-switch-to-tab buffer)
	  (switch-to-buffer buffer))))

(defun swivb (&optional find-file-func switch-buffer-func)
  (interactive)
  (let* ((swivb-virtual-abbreviate 'abbreviate)
		 (swivb-use-virtual-buffers t)
		 (selection
	      (completing-read
	       "☕ Switch to: " (swivb--buffer-list "" t) nil t)))
    (if (zerop (length selection))
		(switch-to-buffer swivb-text)
	  (let ((virtual (assoc selection swivb--virtual-buffers)))
		(cond ((functionp selection)
			   (funcall selection))
			  ((and virtual
					(not (get-buffer selection)))
			   (funcall
				(if find-file-func
					find-file-func #'find-file) (cdr virtual)))
			  (t (funcall
				  (if switch-buffer-func switch-buffer-func #'switch-to-buffer) selection)))))))


(provide 'swivb)
