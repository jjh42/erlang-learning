% Distributed key-table (in memory).
% Keep a key-table in memory with multiple erl instances, one
% master (single point of failure) and multiple slaves - robust
% to a failure in the slaves.

-module(distkt).
-export([start_master/0, start_slave/0, 
setv/2, getv/1, key_master/1]).

% Client functions - set and get key values
setv(Key, Value) ->
    distkt ! { self(), setv, Key, Value }.

getv(Key) ->
    distkt ! { self(), getv, Key },
    receive
        {Key, invalid_key } ->
            invalid_key;
        {Key, valid, Value } ->
            Value;
        { error, ErrMsg } ->
            { error, ErrMsg}
    end.

start_master() ->
	Pid = spawn(?MODULE, key_master, [[]]),
	register(distkt, Pid).

key_master([]) ->
    % No slaves are connected
    receive
        { Pid, getv, _} ->
            Pid ! { error, noslaves };
        { Pid, setv, _, _} ->
            Pid ! { error, noslaves };
        { slave_connect, { Node, Process }} ->
            io:format('slave ~s connected~n', [io_lib:write({ Node, Process})]),
            key_master([ { Node, Process } ])   
    end;
key_master(Nodes) ->
    % Some slaves are connected
    receive
        { Pid, getv, _} ->
            Pid ! { error, noslaves };
        { Pid, setv, _, _} ->
            Pid ! { error, noslaves };
        { slave_connect, { Pid, Node }} ->
            io:format('slave ~s connected~n', [io_lib:write({ Pid, Node })]),
            key_master([ Nodes | { Pid, Node } ])   
    end.


start_slave() ->
    master_process() ! { slave_connect, node(), self() }.

master_process() ->
    % Figure out the master process
    { distkt, list_to_atom(string:join(["master", "@", net_adm:localhost()], ""))}.

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
