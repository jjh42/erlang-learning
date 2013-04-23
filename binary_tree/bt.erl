% Simple attempt a binary tree using records and macros

- module(bt).
- export([sum/1]).

- record(bnode, { left = none, right = none, value = 0 }).


sum(none) ->
	0;
sum(#bnode{ left = Left, right = Right, value = Value}) ->
	Value + sum(Left) + sum(Right).
