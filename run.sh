#!/bin/bash
./rebar compile

ERL_LIBS=deps erl -sname cm -pa ebin/  -setcookie bl -config priv/be-list.config -eval 'application:start(sasl).'
