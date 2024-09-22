(defun jira-bridge/alist-get-in (alist keys)
  "Recursively get a value from ALIST using KEYS."
  (let ((value alist))
    (dolist (key keys value)
      (setq value (if (listp value)
                      (alist-get key value nil nil #'equal)
                    nil)))))

(provide 'jira-bridge-core)
;;; jira-bridge-core.el ends here

