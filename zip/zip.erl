-module(zip).
-export([zip/2, zipWith/3, zipWithAlt/3]).


zip([], _) ->
    [];
zip(_, []) ->
    [];
zip([LH|LT], [RH|RT]) ->
    [{LH, RH} | zip(LT, RT)].


zipWith(F, Left, Right) ->
    Z = zip(Left, Right),
    [ F(A, B) || {A, B} <- Z].


zipWithAlt(_F, _, []) ->
    [];
zipWithAlt(_F, [], _) ->
    [];    
zipWithAlt(F, [LH|LT], [RH|RT]) ->
    [F(LH, RH) | zipWithAlt(F, LT, RT)].
