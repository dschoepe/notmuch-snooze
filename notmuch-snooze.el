(require 'notmuch)

(defun notmuch-snooze (time query)
  "Snoozes messages matching QUERY until TIME.
Displays the output of snooze-mail with newlines removed in the minibuffer.
Snoozing means removing the inbox tag of messages matching the query
and scheduling re-adding the inbox tag at TIME.

TIME can be any time accepted by the perl library Date::Manip."
    (let ((output
	   (with-temp-buffer
	     (with-output-to-string
	       (call-process "snooze-mail" nil standard-output nil time query)))))
      (notmuch-tag query '("-inbox"))
      (message (replace-in-string output "\n" "" output))))

(defun notmuch-tree-snooze (time)
  "Snooze message until TIME.
See `notmuch-snooze' for details on the format of TIME."
  (interactive "MSnooze until: ")
  (notmuch-snooze time (notmuch-tree-get-message-id))
  (notmuch-tree-next-matching-message))

(defun notmuch-show-snooze (time)
  "Snooze message until TIME.
See `notmuch-snooze' for details on the format of TIME."
  (interactive "MSnooze until: ")
  (notmuch-snooze time (notmuch-show-get-message-id))
  (notmuch-show-next-matching-message))

(defun notmuch-search-snooze (time)
  "Snooze message until TIME.
See `notmuch-snooze' for details on the format of TIME."
  (interactive "MSnooze until: ")
  (notmuch-snooze time (notmuch-search-find-thread-id))
  (notmuch-search-next-thread))

(provide 'notmuch-snooze)
