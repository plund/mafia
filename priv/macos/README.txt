2017-02-05

I was unable to get mafia to startup automatically at MacOS (Sierra) boot
time on my MacBook Pro laptop.

--------- This may be old info ---------
http://www.grivet-tools.com/blog/2014/launchdaemons-vs-launchagents/

Let me sum up:
- LaunchDaemons are loaded at system boot
- LaunchAgents are loaded when a user logs in
- launchctl is a command line tool to manually load and unload LaunchDaemon or
  LaunchAgent plists.

LaunchDaemons – Run at Boot
LaunchAgents – Run at Login

To make a LaunchDaemon you create a specially formatted plist and do the
following:

 1. It is put it in the folder /Library/LaunchDaemons/
 2. Make sure it is owned by root and in the group wheel
 3. Has the permissions “644”: Owner – read/write, group – read, everyone – read

LaunchDaemons are plists stored in /Library/LaunchDaemons/
(For installed software and our own custom plists)
or in /System/Library/LaunchDaemons/ (For OS X native processes only).
