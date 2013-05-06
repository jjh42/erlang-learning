% Distributed key-table (in memory).
% Keep a key-table in memory with multiple erl instances, one
% master (single point of failure) and multiple slaves - robust
% to a failure in the slaves.
%
% The master process mediates key retrieval by handing on key requests
% to a randomly chosen slave. The client retries is a slave fails to respond
% in a timely manner.
%
% To handle recovery, the master process monitors slaves to detect their termination.
% When a new slave comes online it assigns another slave to replicate itself (while
% still handling client requests). The new slave does not respond to queries until
% replication has been achieved.
%
% There are still some cases I don't handle well such as:
%  - If a slave being replicated from dies.
%  - If a node stay alive but stops responding properly.
%  - Any missed message will result in permanent inconsistency.


-module(distkt).
-export([start_master/0, start_slave/0, 
setv/2, getv/1, key_master/1, slave_key_server/1, list_slaves/0,
    replication_process/3]).

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
    getv_impl(Key, 5).

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

slave_connected(Nodes, Pid) ->
    % Called when a new slave connects.
    erlang:monitor(process, Pid),
    io:format("slave ~s connected~n", [io_lib:write(Pid)]),
    [ Pid | Nodes].

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
            Pid ! { replication_complete },
            key_master(slave_connected([], Pid));
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
            choice(Nodes) ! { replicate, Pid },
            key_master(slave_connected(Nodes, Pid));
        { Pid, list_slaves } ->
            Pid ! { list_slaves, Nodes },
            key_master(Nodes);
        {'DOWN', _Ref, process, DownPid, Reason} ->
            key_master(down_slave(Nodes, DownPid, Reason))
    end.

down_slave(Nodes, DownPid, Reason) ->
    io:format("slaves ~s down for reason ~s~n", [io_lib:write(DownPid),
                        io_lib:write(Reason)]),
    lists:delete(DownPid, Nodes).


set_slave_key_value([], _Key, _Value) ->
    ok;
set_slave_key_value([ Node | Tail], Key, Value) ->
    Node ! { setv, Key, Value },
    set_slave_key_value( Tail, Key, Value).


% Slave routines
start_slave() ->
    master_process() ! { slave_connect, self() },
    Table = ets:new(keytable, [protected]),
    slave_key_server_replication(Table).

master_process() ->
    % Figure out the master process
    { distkt, list_to_atom(string:join(["master", "@", net_adm:localhost()], ""))}.

slave_key_server_replication(Table) ->
    % At the beginning we need to get a replica of someone else's table before
    % we respond to queries (in order to minimize inconsistencies).
    receive
        { _, getv, _} ->
            % Drop getv and let some other slave handle it
            slave_key_server_replication(Table);     
        { setv, Key, Value } ->
            slave_set_key_value(Table, Key, Value),
            slave_key_server_replication(Table);
        { replication_complete } ->
            % We can go into normal operation.
            slave_key_server(Table)
    end.
    
slave_key_server(Table) ->
    receive
        { ReturnPid, getv, Key} ->
            slave_get_value(Table, ReturnPid, Key);     
        { setv, Key, Value } ->
            slave_set_key_value(Table, Key, Value);
        { replicate, SlavePid } ->
            % Handle replicating to a new slave
            handle_replication_request( Table, SlavePid)
    end,
    slave_key_server(Table).


handle_replication_request(Table, SlavePid ) ->
    First = ets:first(Table),
    spawn(?MODULE, replication_process, [SlavePid, Table, First]).

replication_process(SlavePid, _Table, '$end_of_table') ->
    SlavePid ! { replication_complete };
replication_process(SlavePid, Table, Key) ->
    [{Key, Value}] = ets:lookup(Table, Key),
    SlavePid ! { setv, Key, Value },
    replication_process(SlavePid, Table, ets:next(Table, Key)).

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
