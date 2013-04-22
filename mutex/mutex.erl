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
	process_flag(trap_exit, true),
	unlocked().

unlocked() ->
	receive
		{lock, Pid} ->
			locked(Pid)
	end.

locked(Pid) ->
	% Link up in case the process crashes
	link(Pid),
	Pid ! go,
	receive
		{unlock, Pid} ->
			unlocked();
		{'EXIT', Pid, _} ->
			unlocked()
	end.
