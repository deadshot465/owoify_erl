-module(words).
-include("../shared/word.hrl").

-export([replace/4]).

-spec replace(#word{inner_word::maybe_improper_list(binary() | maybe_improper_list(any(), binary() | []) | char(), binary() | []), replaced_words::sets:set(_)}, re:mp(), unicode:charlist(), boolean()) -> #word{inner_word::maybe_improper_list(binary() | maybe_improper_list(any(), binary() | []) | char(), binary() | []), replaced_words::sets:set(_)}.
replace(Word = #word{inner_word = InnerWord, replaced_words = ExistingReplacedWords}, SearchValue, ReplaceValue, ReplaceReplacedWords) ->
    ContainsReplacedWords = search_value_contains_replaced_words(Word, SearchValue, ReplaceValue),
    if ReplaceReplacedWords =:= false andalso ContainsReplacedWords =:= true ->
        Word;
       true ->
        MatchResult = re:run(InnerWord, SearchValue, [global, {capture, all, binary}]),
        ReplacingWord = case MatchResult of
            {match, _} -> re:replace(InnerWord, SearchValue, ReplaceValue, [global, {return, binary}]);
            _ -> InnerWord
        end,
        ReplacedWords = case MatchResult of
            {match, Collection} ->
                lists:map(fun(Captured) ->
                    Group0 = lists:nth(1, Captured),
                    string:replace(Group0, Group0, ReplaceValue)
                end, Collection);
            _ -> []
        end,
        if ReplacingWord =/= InnerWord ->
            NewReplacedWords = lists:foldl(fun(X, Acc) -> sets:add_element(X, Acc) end, ExistingReplacedWords, ReplacedWords),
            #word{inner_word = ReplacingWord, replaced_words = NewReplacedWords};
           true -> Word
        end
    end.

-spec search_value_contains_replaced_words(#word{inner_word::maybe_improper_list(binary() | maybe_improper_list(any(), binary() | []) | char(), binary() | []), replaced_words::sets:set(_)}, re:mp(), unicode:charlist()) -> boolean().
search_value_contains_replaced_words(#word{replaced_words = ReplacedWords}, SearchValue, ReplaceValue) ->
    Lst = sets:to_list(ReplacedWords),
    Duplicated = lists:any(fun(S) ->
        MatchResult = re:run(S, SearchValue, [global, {capture, all, binary}]),
        case MatchResult of
            {match, [Captured]} ->
                Group0 = lists:nth(1, Captured),
                string:replace(S, Group0, ReplaceValue) =:= S;
            nomatch -> false
        end
     end, Lst),
    Duplicated.