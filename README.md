# notmuch-snooze

## Description

A small tool to "snooze" emails in the [notmuch](http://notmuchmail.org/) email
client, i.e. remove messages from the inbox and add them again later at a
specified time.

## Installation

Add `(require 'notmuch-snooze)` to your Emacs configuration to load the package
and bind the function `notmuch-snooze` to a key, for example `S`:

    (define-key notmuch-search-mode-map (kbd "S") #'notmuch-snooze)
    (define-key notmuch-show-mode-map (kbd "S") #'notmuch-snooze)
    (define-key notmuch-tree-mode-map (kbd "S") #'notmuch-snooze)

Additionally, the `notmuch-unsnooze` script needs to be called
in regular intervals (for example when fetching mail) to unsnooze
previously snoozed messages. The script requires python bindings
for notmuch to be installed.

## Credits

I rewrote this to store the unsnooze time in a tag; this idea is
taken from a [discussion](http://notmuch.198994.n3.nabble.com/email-snoozing-in-notmuch-td4032734.html) on the notmuch mailing list.
