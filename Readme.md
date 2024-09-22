# jira-bridge.el

Sync Jira data to Org documents in Emacs.

## Introduction

`jira-bridge.el` is an Emacs package that allows you to fetch Jira issues and integrate them into your Org-mode documents. With this package, you can pull Jira issue data directly into your Org files, keeping your task management synchronized with your Jira projects.

## Features

- Fetch Jira issues and their details directly into Org-mode.
- Supports pulling subtasks and child issues recursively.
- Customizable mapping between Jira statuses and Org TODO keywords.
- Integration with `auth-source` for secure credential storage.
- Easy to use within Org capture templates.

## Installation

### Prerequisites

- **Emacs 29** or higher.
- The following Emacs packages:
  - `request` version 0.3.3 or higher.

### Using `use-package` with Elpaca

First, ensure you have Elpaca set up in your Emacs configuration. Then add the following to your Emacs configuration file:

```elisp
(use-package jira-bridge
  :ensure (:type git :host github :repo "emil-vdw/jira-bridge.el")
  :config
  ;; Set your Jira base URL.
  (jira-bridge/base-url "https://your-jira-instance.atlassian.net/")
  ;; Customize Jira status to org status conversions.
  (jira-bridge/jira-status-to-org-status '(("To Do" . "TODO")
                                           ("In Progress" . "BUSY"))))
```

## Configuration

### Setting the Jira Base URL

You need to set the base URL for your Jira instance:

```elisp
(setq jira-bridge/base-url "https://your-jira-instance.atlassian.net/")
```

### Setting Up Credentials with `auth-source`

`jira-bridge.el` uses Emacs's `auth-source` library to securely retrieve your Jira credentials. You need to store your Jira username and an API token in your `authinfo` file or another supported `auth-source` backend.

#### Example `.authinfo` Entry

```
machine your-jira-instance.atlassian.net login your-email@example.com password your-api-token
```

Replace:

- `your-jira-instance.atlassian.net` with your Jira instance URL.
- `your-email@example.com` with your Jira account email.
- `your-api-token` with your Jira API token.

> **Note:** For Atlassian Cloud instances, you need to use an API token instead of your password. You can generate an API token from your Atlassian account settings.

### Customizing Status Mapping

You can customize how Jira statuses map to Org TODO keywords by modifying `jira-bridge/jira-status-to-org-status`:

```elisp
(setq jira-bridge/jira-status-to-org-status
      '(("To Do" . "TODO")
        ("In Progress" . "DOING")
        ("Done" . "DONE")
        ;; Add more mappings as needed
        ))
```

If a Jira status cannot be found in the map above, the todo status is assumed to be the upper-case version of the Jira status, eg. `In Progress` -> `IN PROGRESS`.

## Usage

### Pulling a Jira Issue into Org-mode

You can use the `jira-bridge/pull-issue` function to fetch a Jira issue and format it as an Org-mode entry:

```elisp
(jira-bridge/pull-issue "ISSUE-123")
```

This will return a string that you can insert into an Org buffer.

### Using in an Org Capture Template

Integrate `jira-bridge.el` into your Org capture templates to quickly create tasks from Jira issues.

#### Example Capture Template

```elisp
(setq org-capture-templates
      '(("j" "Jira Issue"
         entry
         (file+headline "~/org/jira.org" "Inbox")
         "%(jira-bridge/pull-issue (jira-bridge/extract-issue-key-from-url (read-string \"Jira Issue URL: \")))"
         :empty-lines 1)))
```

When you invoke the capture template, it will prompt you for the Jira issue URL, fetch the issue details, and create an Org entry in your specified file and heading.

## License

`jira-bridge.el` is licensed under the GNU General Public License v3.0. See the [LICENSE](LICENSE) file for details.

## Acknowledgements

- [request.el](https://github.com/tkf/emacs-request) by tkf.
