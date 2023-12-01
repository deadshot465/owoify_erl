%% @private

-module(owoify_erl_tests).
-include_lib("eunit/include/eunit.hrl").

source() -> "This is the string to owo! Kinda cute isn't it?".

pokemon_name_list_path() -> "assets/pokemons.txt".

war_and_peace_path() -> "assets/war_and_peace_chapter01-20.txt".

owo_level_test() ->
    Result = owoify_erl:owoify(source()),
    ?assert(string:length(Result) > 0).

uwu_level_test() ->
    Result = owoify_erl:owoify(source(), uwu),
    ?assert(string:length(Result) > 0).

uvu_level_test() ->
    Result = owoify_erl:owoify(source(), uvu),
    ?assert(string:length(Result) > 0).

not_equal_to_source_test() ->
    Result = owoify_erl:owoify(source()),
    ?assertNotEqual(Result, source()).

incorrect_owoness_test() ->
    ?assertException(error, incorrect_owoness, owoify_erl:owoify(source(), abc)).

owo_not_equal_to_uwu_test() ->
    OwoResult = owoify_erl:owoify(source()),
    UwuResult = owoify_erl:owoify(source(), uwu),
    ?assertNotEqual(OwoResult, UwuResult).

owo_not_equal_to_uvu_test() ->
    OwoResult = owoify_erl:owoify(source()),
    UvuResult = owoify_erl:owoify(source(), uvu),
    ?assertNotEqual(OwoResult, UvuResult).

uwu_not_equal_to_uvu_test() ->
    UwuResult = owoify_erl:owoify(source(), uwu),
    UvuResult = owoify_erl:owoify(source(), uvu),
    ?assertNotEqual(UwuResult, UvuResult).

pokemon_names_test() ->
    {ok, Binary} = file:read_file(pokemon_name_list_path()),
    Strings = string:tokens(binary_to_list(Binary), "\r\n"),
    lists:foreach(
        fun(X) ->
            OwoName = owoify_erl:owoify(X),
            UwuName = owoify_erl:owoify(X, uwu),
            UvuName = owoify_erl:owoify(X, uvu),
            ?assert(string:length(OwoName) > 0),
            ?assert(string:length(UwuName) > 0),
            ?assert(string:length(UvuName) > 0)
        end,
        Strings
    ).

long_texts_test_() ->
    {timeout, 600, fun() ->
        long_texts()
    end}.

long_texts() ->
    {ok, Binary} = file:read_file(war_and_peace_path()),
    OwoText = owoify_erl:owoify(binary_to_list(Binary)),
    UwuText = owoify_erl:owoify(binary_to_list(Binary), uwu),
    UvuText = owoify_erl:owoify(binary_to_list(Binary), uvu),
    ?assert(string:length(OwoText) > 0),
    ?assert(string:length(UwuText) > 0),
    ?assert(string:length(UvuText) > 0).
