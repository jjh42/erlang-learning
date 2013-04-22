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
		after
			1000 -> timeout
	end.

unlock() ->
	mutex ! {unlock, self()},
	ok.

init() ->
	process_flag(trap_exit, true),
	unlocked().

unlocked() ->
	receive
		% Ignore any unlock messages
		{unlock, _} ->
			unlocked();
		{lock, Pid} ->
			locked(Pid, 0)
	end.

locked(Pid, 0) ->
	% Link up in case the process crashes
	link(Pid),
	Pid ! go,
	receive
		{unlock, Pid} ->
			unlink(Pid),
			unlocked();
		{'EXIT', Pid, _} ->
			unlink(Pid),
			unlocked();
		{lock, Pid} ->
			locked(Pid, 1)
	end;
locked(Pid, N) ->
	% We're already linked
	Pid ! go,
	receive
		{unlock, Pid} ->
			locked(Pid, N - 1);
		{'EXIT', Pid, _} ->
			locked(Pid, N - 1);
		{lock, Pid} ->
			locked(Pid, N + 1)
	end.
