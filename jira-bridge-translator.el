(require 'dash)

(require 'jira-bridge-core)

(defcustom jira-bridge/paragraph-transformers
  `((paragraph . jira-bridge/paragraph-to-org)
    (heading . jira-bridge/heading-to-org)
    (bulletList . ,(lambda (node) (jira-bridge/list-to-org node "- ")))
    (numberedList . ,(lambda (node) (jira-bridge/list-to-org node "1. ")))
    (listItem . jira-bridge/list-item-to-org)
    (rule . ,(lambda (node) (identity "\n-----\n")))
    (codeBlock . jira-bridge/code-block-to-org)
    (table . jira-bridge/table-to-org)
    (text . jira-bridge/text-to-org))
  "")

(defun jira-bridge/node-to-org-default (node)
  ""
  (let ((paragraph-content (alist-get 'content node)))
         (mapconcat
          (lambda (item)
            (alist-get 'text item ""))
          paragraph-content
          "")))

(defun jira-bridge/node-to-org (node)
  ""
  (funcall (alist-get (intern (alist-get 'type node))
                      jira-bridge/paragraph-transformers
                      #'jira-bridge/node-to-org-default)
           node))

(defun jira-bridge/description-to-org (description)
  "Convert Jira issue DESCRIPTION content to Org-mode syntax."
  (let ((content (alist-get 'content description)))
    (concat
     (mapconcat
      #'jira-bridge/node-to-org content "\n"))))

(defun jira-bridge/list-item-to-org (node &optional prefix)
  "Convert a Jira list item NODE to Org-mode list item."
  (let ((content (alist-get 'content node)))
    (concat (or prefix "- ")
            (mapconcat #'jira-bridge/node-to-org content ""))))

(defun jira-bridge/list-to-org (node prefix)
  "Convert a Jira list NODE to Org-mode list with PREFIX."
  (let ((items (alist-get 'content node)))
    (mapconcat (lambda (item)
                 (jira-bridge/list-item-to-org item prefix))
               items "\n")))

(defun jira-bridge/code-block-to-org (node)
  "Convert a Jira code block NODE to Org-mode code block."
  (let* ((attrs (alist-get 'attrs node))
         (language (or (alist-get 'language attrs) ""))
         (content (alist-get 'content node))
         (code (mapconcat #'jira-bridge/node-to-org content "")))
    (format "#+BEGIN_SRC %s\n%s\n#+END_SRC\n" language code)))

(defun jira-bridge/table-row-to-org (row)
  "Convert a Jira table ROW to Org-mode table row."
  (let ((cells (alist-get 'content row)))
    (concat "| "
            (mapconcat (lambda (cell)
                         (let ((content (alist-get 'content cell)))
                           (mapconcat #'jira-bridge/node-to-org content "")))
                       cells
                       " | ")
            " |")))

(defun jira-bridge/table-to-org (node)
  "Convert a Jira table NODE to Org-mode table."
  (let ((rows (alist-get 'content node)))
    (mapconcat #'jira-bridge/table-row-to-org rows "\n")))

(defun jira-bridge/text-to-org (node)
  "Convert a Jira text NODE to a formatted org text segment.

Currently, only a singe text property will be applied to a text
segment at a time."
  (let ((text (alist-get 'text node))
        ;; We'd need a more advanced algorithm to build org text
        ;; segments with multiple types of formatting applied.
        ;; For now, just apply the first.
        (marks (list (seq-first (alist-get 'marks node)))))
    (--reduce-from (let ((mark-type (alist-get 'type it))
                         (attrs (alist-get 'attrs it)))
                     (pcase mark-type
                       ("strong" (concat "*" acc "*"))
                       ("em" (concat "/" acc "/"))
                       ("underline" (concat "_" acc "_"))
                       ("link" (format "[[%s][%s]]" (alist-get 'href attrs) acc))
                       ("code" (format "~%s~" acc))
                       (_ acc)))
                   text
                   marks)))

(defun jira-bridge/heading-to-org (node)
  "Convert a Jira heading NODE to Org-mode heading."
  (let* ((level (jira-bridge/alist-get-in node '(attrs level)))
         (content (mapconcat #'jira-bridge/node-to-org
                             (alist-get 'content node)
                             "")))
    (concat content "\n")))

(defun jira-bridge/paragraph-to-org (node)
  "Convert a Jira paragraph NODE to Org-mode paragraph."
  (let ((content (mapconcat #'jira-bridge/node-to-org
                            (alist-get 'content node)
                            "")))
    content))

(provide 'jira-bridge-translator)
;;; jira-bridge-translator.el ends here

