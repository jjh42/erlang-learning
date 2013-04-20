% Store key/values in a process. Exercise 5.1
%
% I store the DB as a list of (Key, Values) tuple.
% For learning I don't use built-ins for searching the list
% etc. (obviously a better solution is to use a dictionary.

- module(my_db).
- export([start/0, stop/0, write/2, delete/1, read/1, match/1, init/0]).

start() ->
	Pid = spawn(my_db, init, []),
	register(my_db, Pid),
	Pid.

init() ->
	loop([]).

loop(Db) ->
	receive
		{read, Pid, Key} -> 
			do_read(Db, Pid, Key),
			loop(Db);
		{match, Pid, Value} ->
			do_match(Db, Pid, Value),
			loop(Db);
		{write, Key, Value} ->
			loop(do_write(Db, Key, Value));
		{delete, Key} ->
			loop(do_delete(Db, Key));
		stop -> ok
	end.

stop() ->
	my_db ! stop.

read(Key) ->
	my_db ! {read, self(), Key},
	receive
		{read_result, Key, Value} ->
			Value;
		{ error, invalid_key } ->
			{ error, invalid_key }
	end.

write(Key, Value) ->
	my_db ! {write, Key, Value},
	ok.

match(Value) ->
	my_db ! {match, self(), Value},
	receive
		{match_result, Value, Keys} ->
			Keys
	end.

delete(Key) ->
	my_db ! {delete, Key}.

% Implementation
do_read([], Pid, _) ->
	Pid ! {error, invalid_key};
do_read([{Key, Value} | _], Pid, Key) ->
	Pid ! { read_result, Key, Value};
do_read([_ | Tail], Pid, Key) ->
	do_read(Tail, Pid, Key).


do_match(Db, Pid, Value) ->
	Pid ! {match_result, Value, find_matching_keys(Db, Value)}.

find_matching_keys([], _) ->
	[];
find_matching_keys([{Key, Value} | Tail], Value) ->
	[ Key | find_matching_keys(Tail, Value)];
find_matching_keys([_ | Tail], Value) ->
	find_matching_keys(Tail, Value).


do_write(Db, Key, Value) ->
	[ {Key, Value} | do_delete(Db, Key) ].


do_delete([], _) ->
	[];
do_delete([{Key, _} | Tail], Key) ->
	Tail;
do_delete([{NonKey, Value} | Tail], Key) ->
	[{NonKey, Value} | do_delete(Tail, Key)].
