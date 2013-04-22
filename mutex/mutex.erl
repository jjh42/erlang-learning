- module(mutex).
- export([start/0, lock/0, unlock/0, init/0]).

start() ->	Pid = spawn(?MODULE, init, []),
			register(mutex, Pid),
			Pid.

lock() ->
	mutex ! {lock, self()},
	receive
		go ->
			ok
	end.

unlock() ->
	mutex ! {unlock, self()},
	ok.

init() ->
	unlocked().

unlocked() ->
	receive
		{lock, Pid} ->
			Pid ! go,
			locked(Pid)
	end.

locked(Pid) ->
	receive
		{ unlock, Pid} ->
			unlocked()
	end.
