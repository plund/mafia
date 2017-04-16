Mafia
=====

A Mafia Game Tracker bot written in Erlang

Requirements
------------
Requires an erlang installation and rebar3.

The bin/start and bin/stop scripts are adaptions for MacOS, but erlang and
rebar3 runs on any OS (also Windows).

Installation
------------
On MacOS easiest done by installing brew and then installing erlang with
'brew install erlang'

Find more information on how to install erlang on www.erlang.org for other
operating systems.

Install 'rebar3'. You find it at http://www.rebar3.org/

Build, configure and start
--------------------------

Check out the start script variable settings, specially the variable $HTTP_IP
and $HTTP_INTERFACE. If ok run it to compile, install and start up in:

    $ bin/start

On Windows remove the "run_erl -daemon $PIPE $LOG_DIR \"exec" part of the start
comand at the bottom of the bin/start file. The remain:

  erl -pa $ERLPATH $PATCHES \
  -h_srv_root $SERVER_ROOT -h_doc_root $DOC_ROOT -h_log_root $LOG_ROOT \
  -repo_dir $REPO_DIR -sname $NODE_NAME -s mafia

should work after replacing variables with constants values (see start script
for guidance).

Connecting to shell
-------------------
Unix/MacOS (when using run_erl):

  to_erl $HOME/running/mafia/pipes/mafia
