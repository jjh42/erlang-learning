-module(zip).
-export([zip/2]).


zip([], _) ->
    [];
zip(_, []) ->
    [];
zip([LH|LT], [RH|RT]) ->
    [{LH, RH} | zip(LT, RT)].
