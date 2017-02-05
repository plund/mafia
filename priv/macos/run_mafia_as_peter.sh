#!/bin/bash

# -w should make it persistent
#sudo launchctl load -w /Library/LaunchDaemons/se.peterlund.mafia.plist
#sudo launchctl unload -w /Library/LaunchDaemons/se.peterlund.mafia.plist

echo "Started "$(date "+%Y-%m-%d, %H:%M:%S")
echo "whoami: "$(whoami)

SCRIPT_DIR=$(cd $(dirname $0) && pwd)

#echo "dirname "$(dirname $0)
#echo "pwd "$(pwd)

echo "Starting "$SCRIPT_DIR/start

cd $SCRIPT_DIR
su peter -c "bash -l -c \"$SCRIPT_DIR/start -s\""
