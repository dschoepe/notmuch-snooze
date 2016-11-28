(require 'notmuch)

(defun notmuch-snooze-query (time query)
  "Snoozes messages matching QUERY until TIME.

Snoozing a message removes the inbox tag and adds a snoozed tag
along with a tag encoding when the message should be re-added to
the inbox."
  (let ((time-hm (format-time-string "%d%m%Y-%H%M" time)))
    (notmuch-tag query
                 (list "-inbox" "+snoozed" (concat "+snoozed"
                                                   time-hm)))))

;;;###autoload
(defun notmuch-snooze ()
  "Query for a time and snooze the currently selected message until that time."
  (interactive)
  (let* ((time (org-read-date nil t))
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
