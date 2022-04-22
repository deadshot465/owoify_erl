-module(words).
-include("../shared/word.hrl").

-export([replace/4, replace_with_func_single/4, replace_with_func_multiple/4]).

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

-spec replace_with_func_single(#word{inner_word::maybe_improper_list(binary() | maybe_improper_list(any(), binary() | []) | char(), binary() | []), replaced_words::sets:set(_)}, re:mp(), fun(() -> any()), boolean()) -> #word{inner_word::maybe_improper_list(binary() | maybe_improper_list(any(), binary() | []) | char(), binary() | []), replaced_words::sets:set(_)}.
replace_with_func_single(Word = #word{inner_word = InnerWord, replaced_words = ExistingReplacedWords}, SearchValue, Func, ReplaceReplacedWords) ->
    ReplaceValue = Func(),
    ContainsReplacedWords = search_value_contains_replaced_words(Word, SearchValue, ReplaceValue),
    if ReplaceReplacedWords =:= false andalso ContainsReplacedWords =:= true ->
        Word;
       true ->
        MatchResult = re:run(InnerWord, SearchValue, [global, {capture, all, binary}]),
        ReplacingWord = case MatchResult of
            {match, [[Captured]]} ->
                Group0 = lists:nth(1, Captured),
                string:replace(Group0, Group0, ReplaceValue);
            _ -> InnerWord
        end,
        ReplacedWords = case MatchResult of
            {match, Collection} ->
                lists:map(fun(Captured) ->
                    Group = lists:nth(1, Captured),
                    string:replace(Group, Group, ReplaceValue)
                end, Collection);
            _ -> []
        end,
        if ReplacingWord =/= InnerWord ->
            NewReplacedWords = lists:foldl(fun(X, Acc) -> sets:add_element(X, Acc) end, ExistingReplacedWords, ReplacedWords),
            #word{inner_word = ReplacingWord, replaced_words = NewReplacedWords};
           true -> Word
        end
    end.

replace_with_func_multiple(Word = #word{inner_word = InnerWord, replaced_words = ExistingReplacedWords}, SearchValue, Func, ReplaceReplacedWords) ->
    MatchResult = re:run(InnerWord, SearchValue, [global, {capture, all, binary}]),
    case MatchResult of
        nomatch -> Word;
        {match, [Captures]} ->
            Capture1 = lists:nth(1, lists:nth(2, Captures)),
            Capture2 = lists:nth(1, lists:nth(3, Captures)),
            ReplaceValue = Func(Capture1, Capture2),
            ContainsReplacedWords = search_value_contains_replaced_words(Word, SearchValue, ReplaceValue),
            if ReplaceReplacedWords =:= false andalso ContainsReplacedWords =:= true ->
                Word;
               true ->
                Capture0 = lists:nth(1, lists:nth(1, Captures)),
                ReplacingWord = string:replace(InnerWord, Capture0, ReplaceValue),
                ReplacedWords = case MatchResult of
                    {match, Collection} ->
                        lists:map(fun(Captured) ->
                            Group = lists:nth(1, Captured),
                            string:replace(Group, Group, ReplaceValue)
                        end, Collection);
                    _ -> []
                end,
                if ReplacingWord =/= InnerWord ->
                    NewReplacedWords = lists:foldl(fun(X, Acc) -> sets:add_element(X, Acc) end, ExistingReplacedWords, ReplacedWords),
                    #word{inner_word = ReplacingWord, replaced_words = NewReplacedWords};
                   true -> Word
                end
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