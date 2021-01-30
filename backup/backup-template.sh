#!/usr/bin/env bash
# uncomment for debug
#set -x


# gpg --gen-key
# gpg --list-keys

SIGNATURE=FOO
export PASSPHRASE='BAR'
REMOTE_ROOT=/mnt/backup/duplicity/HOST
REMOTE_HOST=HOST
MODE=incremental
if [ "$FULL" = 1 ]; then
  MODE=full
fi

FAIL=
for path in etc root home var/spool ; do
  if duplicity --encrypt-key $SIGNATURE --sign-key $SIGNATURE --ssh-options -oIdentityFile=$HOME/.ssh/id_backup $MODE /$path sftp://$REMOTE_HOST/$REMOTE_ROOT/$path; then
    duplicity --encrypt-key $SIGNATURE --sign-key $SIGNATURE --ssh-options -oIdentityFile=$HOME/.ssh/id_backup remove-older-than 14D --force sftp://$REMOTE_HOST/$REMOTE_ROOT/$path || FAIL=1
  else
    FAIL=1
  fi
done

if [ "$FAIL" = 1 ]; then
  echo "there were failures" >&2
  exit 1
fi

exit 0
