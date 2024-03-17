%% @private

-module(owoify_utility).

-export([interleave_list/2]).

-spec interleave_list([any()], [any()]) -> [any()].
interleave_list(A, B) ->
    lists:reverse(interleave([], A, B, 0)).

-spec interleave([any()], [any()], [any()], non_neg_integer()) -> [any()].
interleave(Result, Lst, Other, Round) ->
    case math:fmod(Round, 2) of
        +0.0 ->
            LstEmpty = length(Lst) =:= 0,
            if
                LstEmpty =:= true ->
                    OtherEmpty = length(Other),
                    if
                        OtherEmpty =:= false -> Other ++ Result;
                        true -> Result
                    end;
                true ->
                    [Head | Tail] = Lst,
                    interleave([Head | Result], Tail, Other, Round + 1)
            end;
        _ ->
            OtherEmpty = length(Other) =:= 0,
            if
                OtherEmpty ->
                    Result;
                true ->
                    [Head | Tail] = Other,
                    interleave([Head | Result], Lst, Tail, Round + 1)
            end
    end.
