(require 'notmuch)
(require 'org)

(defun notmuch-snooze-query (time query)
  "Snoozes messages matching QUERY until TIME.

Snoozing a message removes the inbox tag and adds a snoozed tag
along with a tag encoding when the message should be re-added to
the inbox."
  (let ((time-hm (format-time-string "%Y%m%d-%H%M" time)))
    (notmuch-tag query
                 (list "-inbox" "+snoozed" (concat "+snoozed/until-"
                                                   time-hm)))))

(defun notmuch-unsnooze-helper (get-tag-func tag-func)
  "Internal function to unsnooze the current message.

Expects a function GET-TAG-FUNC that takes no arguments and
returns a list of tags for the currently selected message, and
a function TAG-FUNC taking a list of tag changes as an argument
and updates the current message's tags accordingly. The list
of tag changes is in the same format as `notmuch-tag'."

  (let ((was-snoozed nil))
  (mapc
   (lambda (tag)
     (when (string-match-p "^snoozed\\(/until-[0-9]\\{8\\}-[0-9]\\{4\\}\\)?$" tag)
       (funcall tag-func (list (concat "-" tag)))
       (setq was-snoozed t)))
   (funcall get-tag-func))
  (when was-snoozed
    (funcall tag-func (list (concat "+inbox"))))))

;;;###autoload
(defun notmuch-unsnooze ()
  "Unsnooze currently selected message if snoozed; do nothing otherwise."
  (interactive)
  (cond
      ((eq major-mode 'notmuch-show-mode)
       (notmuch-unsnooze-helper #'notmuch-show-get-tags
                                #'notmuch-show-tag))
      ((eq major-mode 'notmuch-tree-mode)
       (notmuch-unsnooze-helper #'notmuch-tree-get-tags
                                #'notmuch-tree-tag))
      ((eq major-mode 'notmuch-search-mode)
       (notmuch-unsnooze-helper #'notmuch-search-get-tags
                                #'notmuch-search-tag)))
  (notmuch-refresh-this-buffer))

;;;###autoload
(defun notmuch-snooze ()
  "Query for a time and snooze the currently selected message until that time."
  (interactive)
  (let* ((time
          (let ((org-read-date-prefer-future 'time))
            (org-read-date nil t)))
         (time-str (format-time-string "%c" time))
          ;;(time (call-interactively #'notmuch-snooze-read-date))
          (query
           (cond
            ((eq major-mode 'notmuch-tree-mode) (notmuch-tree-get-message-id))
            ((eq major-mode 'notmuch-show-mode) (notmuch-show-get-message-id))
            ((eq major-mode 'notmuch-search-mode)
             (car (notmuch-search-find-stable-query))))))
    (when query
      (notmuch-snooze-query time query)
      (message "Snoozed until: %s" time-str)
      (notmuch-refresh-this-buffer))))

(provide 'notmuch-snooze)
