#!/usr/bin/env sh

if ! mails=$(mu find -n 99 --sortfield=date --reverse --fields 'f s' --skip-dups flag:unread 2> /dev/null | wc -l ); then
    mails=0
fi

echo "$mails"
