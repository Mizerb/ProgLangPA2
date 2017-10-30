#!/bin/bash

erl -noinput -name node1@127.0.0.1 -setcookie election -connect_all true &
erl -noinput -name node2@127.0.0.1 -setcookie election -connect_all true &
erl -noinput -name node3@127.0.0.1 -setcookie election -connect_all true &
erl -noinput -name node4@127.0.0.1 -setcookie election -connect_all true &
erl -noinput -name node5@127.0.0.1 -setcookie election -connect_all true &
erl -name supervisor@127.0.0.1 -setcookie election -connect_all true