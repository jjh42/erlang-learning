- module(echo).
- export([start/0, echo_proc_loop/0, print/1, stop/0]).

start() ->
	EchoPid = spawn(?MODULE, echo_proc_loop, []),
	register(echo, EchoPid).

stop() ->
	echo ! stop.

print(Msg) ->
	echo ! {self(), Msg}.

do_echo(Msg) ->
	% In the process, print the messages.
	io:format("~w~n", [Msg]).

echo_proc_loop() ->
	receive
		{_, Msg} -> do_echo(Msg),
						echo_proc_loop();
		stop -> true
	end.
