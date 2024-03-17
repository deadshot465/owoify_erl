%% @author Tetsuki Syu <tetsuki.syu1315@gmail.com> [https://github.com/deadshot465]
%% @doc Turning your worst nightmare into a Rebar3 package. <a href="https://codepen.io/newbeetf2/pen/yLLaNPZ">https://codepen.io/newbeetf2/pen/yLLaNPZ</a>
%% @reference See <a href="https://github.com/mohan-cao/owoify-js">mohan-cao's owoify-js</a>
%% for more information.

-module(owoify_erl).
-include("shared/word.hrl").
-type owoify_level() :: owo | uwu | uvu.

-export([owoify/1, owoify/2, uwuify/1, uvuify/1]).

%% @doc The main entry point of <em>owoify_erl</em>.
%% This function will use the default owoness <em>owo</em> to owoify the source string.
%% @param Source The source string to owoify.
%% @returns The owoified string in Unicode form.
-spec owoify(string()) -> unicode:charlist().
owoify(Source) ->
    owoify(Source, owo).

%% @doc The main entry point of <em>owoify_erl</em>.
%% This function will use the <em>uwu</em> owoness to owoify the source string.
%% @param Source The source string to owoify.
%% @returns The owoified string in Unicode form.
-spec uwuify(string()) -> unicode:charlist().
uwuify(Source) ->
    owoify(Source, uwu).

%% @doc The main entry point of <em>owoify_erl</em>.
%% This function will use the <em>uvu</em> owoness to owoify the source string.
%% @param Source The source string to owoify.
%% @returns The owoified string in Unicode form.
-spec uvuify(string()) -> unicode:charlist().
uvuify(Source) ->
    owoify(Source, uvu).

%% @doc The main entry point of <em>owoify_erl</em>.
%% This function will require the user to explicitly pass the desired owoness to owoify the source string.
%% @param Source The source string to owoify.
%% @param Level The owoness of the result string.
%% @returns The owoified string in Unicode form.
-spec owoify(string(), owoify_level()) -> unicode:charlist().
owoify(Source, Level) ->
    {match, WordMatches} = re:run(Source, word_regex(), [global, {capture, all, binary}]),
    Words = lists:map(
        fun(W) ->
            Match = lists:nth(1, W),
            #word{inner_word = Match, replaced_words = sets:new()}
        end,
        WordMatches
    ),
    SpaceMatches =
        case re:run(Source, space_regex(), [global, {capture, all, binary}]) of
            {match, Matches} -> Matches;
            _ -> []
        end,
    Spaces = lists:map(
        fun(S) ->
            Match = lists:nth(1, S),
            #word{inner_word = Match, replaced_words = sets:new()}
        end,
        SpaceMatches
    ),
    Presets =
        owoify_presets:specific_word_mapping_list() ++
            (case Level of
                owo ->
                    owoify_presets:owo_mapping_list();
                uwu ->
                    owoify_presets:uwu_mapping_list() ++ owoify_presets:owo_mapping_list();
                uvu ->
                    owoify_presets:uvu_mapping_list() ++ owoify_presets:uwu_mapping_list() ++
                        owoify_presets:owo_mapping_list();
                _ ->
                    error(incorrect_owoness)
            end),
    Applied = lists:map(
        fun(W) ->
            lists:foldl(fun(F, Inner) -> F(Inner) end, W, Presets)
        end,
        Words
    ),
    Strings = lists:map(fun words:to_string/1, owoify_utility:interleave_list(Applied, Spaces)),
    string:trim(lists:join("", Strings)).

%% @private
-spec word_regex() -> re:mp().
word_regex() ->
    {ok, Mp} = re:compile("[^\s]+", [unicode]),
    Mp.

%% @private
-spec space_regex() -> re:mp().
space_regex() ->
    {ok, Mp} = re:compile("\s+", [unicode]),
    Mp.
