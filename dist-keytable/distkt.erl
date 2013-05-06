% Distributed key-table (in memory).
% Keep a key-table in memory with multiple erl instances, one
% master (single point of failure) and multiple slaves - robust
% to a failure in the slaves.

-module(distkt).
-export([start_master/0, start_slave/0, 
setv/2, getv/1, key_master/1, slave_key_server/1, list_slaves/0]).

% Client functions - set and get key values
list_slaves() ->
    distkt ! { self(), list_slaves},
    receive
        { list_slaves, SlaveList } ->
            print_slave_list(SlaveList)
    end.

print_slave_list([]) ->
    ok;
print_slave_list([ Slave | Tail ]) ->
    io:format("~s~n", [io_lib:write( Slave )]),
    print_slave_list(Tail).

setv(Key, Value) ->
    distkt ! { self(), setv, Key, Value },
    ok.

getv(Key) ->
    getv_impl(Key, 3).

getv_impl(_Key, 0) ->
    { error, no_nodes_responded };
getv_impl(Key, NTries) ->
    % Failure recovery - try again if a node fails to respond.
    distkt ! { self(), getv, Key },
    receive
        {Key, invalid_key } ->
            invalid_key;
        {Key, valid, Value } ->
            Value;
        { error, ErrMsg } ->
            { error, ErrMsg}
    after 100 ->
        io:format("Warning: node never responded.~n"),
        getv_impl(Key, NTries - 1)
    end.

start_master() ->
	Pid = spawn(?MODULE, key_master, [[]]),
	register(distkt, Pid).

key_master([]) ->
    % No slaves are connected
    receive
        { Pid, getv, _} ->
            Pid ! { error, noslaves },
            key_master([]);
        { Pid, setv, _, _} ->
            Pid ! { error, noslaves },
            key_master([]);
        { slave_connect, Pid } ->
            io:format("slave ~s connected~n", [io_lib:write(Pid)]),
            key_master([ Pid ]);
        { Pid, list_slaves } ->
            Pid ! { list_slaves, [] },
            key_master([])
    end;
key_master(Nodes) ->
    % Some slaves are connected
    receive
        { ReturnPid, getv, Key} ->
            % Randomly select a node to answer the query
            choice(Nodes) ! { ReturnPid, getv, Key },
            key_master(Nodes);
        { _Pid, setv, Key, Value} ->
            % Set the key on all the nodes.
            set_slave_key_value(Nodes, Key, Value),
            key_master(Nodes);
        { slave_connect, Pid } ->
            io:format('slave ~s connected~n', [io_lib:write(Pid)]),
            key_master([ Pid | Nodes ]);
        { Pid, list_slaves } ->
            Pid ! { list_slaves, Nodes },
            key_master(Nodes)
    end.

set_slave_key_value([], _Key, _Value) ->
    ok;
set_slave_key_value([ Node | Tail], Key, Value) ->
    Node ! { setv, Key, Value },
    set_slave_key_value( Tail, Key, Value).


% Slave routines
start_slave() ->
    master_process() ! { slave_connect, self() },
    Table = ets:new(keytable, [private]),
    slave_key_server(Table).

master_process() ->
    % Figure out the master process
    { distkt, list_to_atom(string:join(["master", "@", net_adm:localhost()], ""))}.

slave_key_server(Table) ->
    receive
        { ReturnPid, getv, Key} ->
            slave_get_value(Table, ReturnPid, Key);     
        { setv, Key, Value } ->
            slave_set_key_value(Table, Key, Value)
    end,
    slave_key_server(Table).

slave_get_value(Table, ReturnPid, Key) ->
    reply_to_lookup(ReturnPid, Key, ets:lookup(Table, Key)).

reply_to_lookup(ReturnPid, Key, []) ->
    ReturnPid ! { Key, invalid_key };
reply_to_lookup(ReturnPid, Key, [{Key, Value}]) ->
    ReturnPid ! { Key, valid, Value }.

slave_set_key_value(Table, Key, Value) ->
    ets:insert(Table, {Key, Value}).


% Utility routines
choice([]) ->
    { error, empty_list };
choice(List) ->
    lists:nth(random:uniform(length(List)), List).
