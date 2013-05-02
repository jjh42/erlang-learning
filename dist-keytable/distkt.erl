% Distributed key-table (in memory).
% Keep a key-table in memory with two erl instances, robust
% to a single failure.

-module(distkt).
-export([start/0, setv/2, getv/1, key_process/0, key_supervisor/0]).

start() ->
	Pid = spawn(?MODULE, key_supervisor, []),
	register(distkt, Pid).

setv(Key, Value) ->
    distkt ! { self(), setv, Key, Value }.

getv(Key) ->
    distkt ! { self(), getv, Key },
    receive
        {Key, invalid_key } ->
            invalid_key;
        {Key, valid, Value} ->
            Value
    end.

key_supervisor() ->
    key_process().

key_process() ->
    Table = ets:new(keytable, [private]),
    key_server(Table).

key_server(Table) ->
    receive
        { Pid, getv, Key} ->
            get_value(Table, Pid, Key);     
        { Pid, setv, Key, Value } ->
            set_key_value(Table, Pid, Key, Value)
    end,
    key_server(Table).

get_value(Table, Pid, Key) ->
    reply_to_lookup(Pid, Key, ets:lookup(Table, Key)).

reply_to_lookup(Pid, Key, []) ->
    Pid ! { Key, invalid_key };
reply_to_lookup(Pid, Key, [{Key, Value}]) ->
    Pid ! { Key, valid, Value }.

set_key_value(Table, _, Key, Value) ->
    ets:insert(Table, {Key, Value}).
