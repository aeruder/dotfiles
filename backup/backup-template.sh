#!/usr/bin/env bash
# uncomment for debug
#set -x


# gpg --gen-key
# gpg --list-keys

# 0 8 * * 1,2,3,4,5,6 chronic /root/backup.sh
# 0 8 * * 7 chronic env FULL=1 /root/backup.sh

# Generate a ssh key (ssh-keygen -f ~/.ssh/id_backup)
# Copy into authorized_keys on remote
# Run env FULL=1 /root/backup.sh once

SIGNATURE=FOO
export PASSPHRASE='BAR'
REMOTE_ROOT=/mnt/backup/duplicity/OUR_HOST
REMOTE_HOST=REMOTE_HOST
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
