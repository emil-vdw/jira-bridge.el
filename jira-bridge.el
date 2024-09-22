;;; jira-bridge.el --- sync jira data to org documents  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Emil van der Westhuizen

;; Author: Emil van der Westhuizen <vdwemil@protonmail.com>
;; Package-Requires: ((emacs "29") (request "0.3.3"))
;; Version: 0.1
;; Homepage: https://www.github.com/emil-vdw/jira-bridge.el
;; Keywords: convenience, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:

;; Requirements:
(require 'request)
(require 'auth-source)
(require 'url-parse)

(defcustom jira-bridge/base-url nil
  "Base URL for the Jira instance.")

(defcustom jira-bridge/jira-status-to-org-status
  ;; Items not in this alist will default to the uppercase Jira status.
  '(("To Do" . "TODO"))
  "A map to convert Jira statuses to org todo statuses.")

(defun jira-bridge/api-url (&optional endpoint)
  "Construct the API URL to use when making requests."
  (concat jira-bridge/base-url
          "rest/api/3/"
          endpoint))

(defun jira-bridge/api-get (endpoint &optional params)
  "Perform a GET request to the Jira API at ENDPOINT."
  (let* ((credentials
          ;; Retrieve credentials from authinfo file.
          (car (auth-source-search
                :host (url-host (url-generic-parse-url jira-bridge/base-url))
                :require '(:user :secret)
                :max 1)))
         (username (plist-get credentials :user))
         (secret (plist-get credentials :secret))
         (token (if (functionp secret) (funcall secret) secret))
         (headers
          `(("Authorization" .
             ,(concat "Basic "
                      (base64-encode-string
                       (concat
                        username ":" token) t)))))
         (response
          (request
            (jira-bridge/api-url endpoint)
            :headers headers
            :parser 'json-read
            :sync t
            :timeout 5
            :params params)))
    
    (if (eq (request-response-status-code response) 200)
        (request-response-data response)
      (error "HTTP GET error %s: %s"
             (request-response-status-code response)
             (request-response-error-thrown response)))))

(defun jira-bridge/extract-issue-key-from-url (url)
  "Extract the issue key from the Jira ticket URL."
  (if (string-match ".*/browse/\\([A-Z]+-[0-9]+\\)" url)
      (match-string 1 url)
    (error "Invalid Jira issue URL")))

(defun jira-bridge/fetch-issue (issue-number)
  "Fetch the data of Jira issue with ISSUE-NUMBER."
  (jira-bridge/api-get (concat "issue/" issue-number) '(("showSubTasks" . "true"))))


(defun jira-bridge/alist-get-in (alist keys)
  "Recursively get a value from ALIST using KEYS."
  (let ((value alist))
    (dolist (key keys value)
      (setq value (if (listp value)
                      (alist-get key value nil nil #'equal)
                    nil)))))

(defun jira-bridge/extract-description-text (description)
  "Extract plain text from the description field."
  (let ((content (alist-get 'content description)))
    (mapconcat
     (lambda (paragraph)
       (let ((paragraph-content (alist-get 'content paragraph)))
         (mapconcat
          (lambda (item)
            (alist-get 'text item ""))
          paragraph-content
          "")))
     content
     "\n")))


(defun jira-bridge/pull-issue (issue-key &optional depth)
  "Create an Org Mode item from Jira issue data."
  (let* ((depth (or depth 1))
         (issue-data (jira-bridge/fetch-issue issue-key))
         (key (alist-get 'key issue-data))
         (fields (alist-get 'fields issue-data))
         (summary (alist-get 'summary fields))
         (raw-description (alist-get 'description fields))
         (description (jira-bridge/extract-description-text raw-description))
         (status (jira-bridge/alist-get-in fields '(status name)))
         (priority (jira-bridge/alist-get-in fields '(priority name)))
         (assignee (jira-bridge/alist-get-in fields '(assignee displayName)))
         (issue-type (jira-bridge/alist-get-in fields '(issuetype name)))
         (project (jira-bridge/alist-get-in fields '(project name)))
         (created (alist-get 'created fields))
         (updated (alist-get 'updated fields))
         (url (alist-get 'self issue-data))
         ;; Map Jira priority to Org priority.
         (org-priority (cond ((string= priority "Highest") ?A)
                             ((string= priority "High") ?B)
                             ((string= priority "Medium") ?C)
                             ((string= priority "Low") ?D)
                             ((string= priority "Lowest") ?E)
                             (t ?C))))
    ;; Format the Org item.
    (message "status: %s" status)
    (concat
     ;; Indent child issues.
     (apply #'concat (-repeat depth "*"))
     " "
     (alist-get status jira-bridge/jira-status-to-org-status
                (upcase status)
                nil
                #'equal)
     " "
     (format "[#%c] %s\n" org-priority summary)

     ;; Construct the task's properties from the Jira data.
     ":PROPERTIES:\n"
     (format ":ISSUE_URL: %s\n" "temp")
     (format ":ISSUE_NUMBER: %s\n" key)
     (format ":ISSUE_TYPE: %s\n" issue-type)
     (format ":STATUS: %s\n" status)
     (format ":ASSIGNEE: %s\n" (or assignee "Unassigned"))
     (format ":PROJECT: %s\n" project)
     ":END:\n\n"
     description

     ;; If the issue is an Epic, meaning it has subtasks, fetch and include all
     ;; subtasks as child org tasks.
     (when (equal issue-type "Epic")
       (let* ((child-issues-response
               (jira-bridge/api-get "search"
                                    `(("jql" . ,(concat "parent=" key))
                                      ("fields" . "key"))))
              ;; Vector of child issue objecs.
              (child-issue-data (alist-get 'issues child-issues-response))
              (child-issue-keys (--map (alist-get 'key it) child-issue-data)))
         (apply 'concat (--map (concat "\n\n"
                                       (jira-bridge/pull-issue it (+ depth 1)))
                               child-issue-keys)))))))


(provide 'jira-bridge)
;;; jira-bridge.el ends here

