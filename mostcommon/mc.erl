% Find the most common words in the word-list

- module(mc).
- export([mc/0]).

mc() ->
    {ok, IoDev} = file:open('mostcommon.words', 'read'),
    LetterFreq = letter_freqs(IoDev, dict:new()),
    % We now know the letter freq's as a letter:value mapping.
    % Extract into tuple list and sort to find letter rankings.
    ListLetterFreq = dict:to_list(LetterFreq),
    SortedLetterFreq = lists:sort(fun({LA,VA}, {LB,VB}) -> {VA, LA} >= {VB, LB} end,
        ListLetterFreq),
    print_sorted_freqs(SortedLetterFreq).

print_sorted_freqs([]) ->
    ok;
print_sorted_freqs([{Letter, N} | Tail]) ->
    io:format("~c ~b~n", [Letter, N]),
    print_sorted_freqs(Tail).

% Read through each line of the file
% and update the frequency count for each character in the line.
letter_freqs(IoDev, LetterFreq) ->
    case file:read_line(IoDev) of
        {ok, Line} ->
            letter_freqs(IoDev, deal_with_chars(Line, LetterFreq));
        eof ->
            LetterFreq
    end.

deal_with_chars([], LetterFreq) ->
    LetterFreq;
deal_with_chars([C|Tail], LetterFreq) ->
    case C of
        10 ->
            NewFreqs = LetterFreq;
        O ->
           NewFreqs = dict:update_counter(O, 1, LetterFreq)
    end,
    deal_with_chars(Tail, NewFreqs).

