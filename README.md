# notmuch-snooze

## Description

A small script to "snooze" emails in the
[notmuch](http://notmuchmail.org/) email client, i.e. remove messages
from the inbox and add them again later after a specified amount of
time.

## Installation

The `snooze-mail` script requires a running
[at](https://packages.qa.debian.org/a/at.html) daemon and the Perl
library
[Date::Manip](http://search.cpan.org/~sbeck/Date-Manip/lib/Date/Manip.pod).

To install it, put the snooze-mail script somewhere in your `PATH`. For Emacs
integration, load the `notmuch-snooze.el` file in your Emacs configuration by
adding its directory to `load-path` and call `(require 'notmuch-snooze)`.

Keys for the various notmuch-modes can be bound as follows:

    (define-key notmuch-search-mode-map (kbd "S") #'notmuch-search-snooze)
    (define-key notmuch-show-mode-map (kbd "S") #'notmuch-show-snooze)
    (define-key notmuch-tree-mode-map (kbd "S") #'notmuch-tree-snooze)
