%% @private

-module(owoify_mappings).

-export([map_brackets_to_star_trails/1, map_consonant_r_to_consonant_w/1, map_dead_to_ded/1, map_ew_to_uwu/1, map_fi_to_fwi/1]).
-export([map_fuc_to_fwuc/1, map_haha_to_hehe_xd/1, map_hey_to_hay/1, map_l_or_r_o_to_wo/1, map_le_to_wal/1, map_ll_to_ww/1]).
-export([map_ly_to_wy/1, map_me_to_mwe/1, map_mom_to_mwom/1, map_n_vowel_t_to_nd/1, map_n_vowel_to_ny/1, map_nr_to_nw/1]).
-export([map_o_to_owo/1, map_ol_to_owl/1, map_old_to_owld/1, map_ove_to_uv/1, map_over_to_owor/1, map_period_comma_exclamation_semicolon_to_kaomojis/1]).
-export([map_ple_to_pwe/1, map_poi_to_pwoi/1, map_r_or_l_to_w/1, map_read_to_wead/1, map_ry_to_wwy/1, map_specific_consonants_le_to_letter_and_wal/1]).
-export([map_specific_consonants_o_to_letter_and_wo/1, map_th_to_f/1, map_that_to_dat/1, map_the_to_teh/1, map_time_to_tim/1]).
-export([map_v_or_w_le_to_wal/1, map_ve_to_we/1, map_ver_to_wer/1, map_vowel_or_r_except_o_l_to_wl/1, map_worse_to_wose/1]).
-export([map_you_to_u/1]).

-spec o_to_owo() -> {'re_pattern', _, _, _, _}.
o_to_owo() ->
    {ok, Mp} = re:compile("o", [unicode]),
    Mp.

-spec ew_to_uwu() -> {'re_pattern', _, _, _, _}.
ew_to_uwu() ->
    {ok, Mp} = re:compile("ew", [unicode]),
    Mp.

-spec hey_to_hay() -> {'re_pattern', _, _, _, _}.
hey_to_hay() ->
    {ok, Mp} = re:compile("([Hh])ey", [unicode]),
    Mp.

-spec dead_to_ded_upper() -> {'re_pattern', _, _, _, _}.
dead_to_ded_upper() ->
    {ok, Mp} = re:compile("Dead", [unicode]),
    Mp.

-spec dead_to_ded_lower() -> {'re_pattern', _, _, _, _}.
dead_to_ded_lower() ->
    {ok, Mp} = re:compile("dead", [unicode]),
    Mp.

-spec n_vowel_t_to_nd() -> {'re_pattern', _, _, _, _}.
n_vowel_t_to_nd() ->
    {ok, Mp} = re:compile("n[aeiou]*t", [unicode]),
    Mp.

-spec read_to_wead_upper() -> {'re_pattern', _, _, _, _}.
read_to_wead_upper() ->
    {ok, Mp} = re:compile("Read", [unicode]),
    Mp.

-spec read_to_wead_lower() -> {'re_pattern', _, _, _, _}.
read_to_wead_lower() ->
    {ok, Mp} = re:compile("read", [unicode]),
    Mp.
-spec brackets_to_startrails_fore() -> {'re_pattern', _, _, _, _}.
brackets_to_startrails_fore() ->
    {ok, Mp} = re:compile("[({<]", [unicode]),
    Mp.

-spec brackets_to_startrails_rear() -> {'re_pattern', _, _, _, _}.
brackets_to_startrails_rear() ->
    {ok, Mp} = re:compile("[)}>]", [unicode]),
    Mp.

-spec period_comma_exclamation_semicolon_to_kaomojis_first() -> {'re_pattern', _, _, _, _}.
period_comma_exclamation_semicolon_to_kaomojis_first() ->
    {ok, Mp} = re:compile("[.,](?![0-9])", [unicode]),
    Mp.

-spec period_comma_exclamation_semicolon_to_kaomojis_second() -> {'re_pattern', _, _, _, _}.
period_comma_exclamation_semicolon_to_kaomojis_second() ->
    {ok, Mp} = re:compile("[!;]+", [unicode]),
    Mp.

-spec that_to_dat_upper() -> {'re_pattern', _, _, _, _}.
that_to_dat_upper() ->
    {ok, Mp} = re:compile("That", [unicode]),
    Mp.

-spec that_to_dat_lower() -> {'re_pattern', _, _, _, _}.
that_to_dat_lower() ->
    {ok, Mp} = re:compile("that", [unicode]),
    Mp.

-spec th_to_f_upper() -> {'re_pattern', _, _, _, _}.
th_to_f_upper() ->
    {ok, Mp} = re:compile("TH(?!E)", [unicode]),
    Mp.

-spec th_to_f_lower() -> {'re_pattern', _, _, _, _}.
th_to_f_lower() ->
    {ok, Mp} = re:compile("[Tt]h(?![Ee])", [unicode]),
    Mp.

-spec le_to_wal() -> {'re_pattern', _, _, _, _}.
le_to_wal() ->
    {ok, Mp} = re:compile("le$", [unicode]),
    Mp.

-spec ve_to_we_upper() -> {'re_pattern', _, _, _, _}.
ve_to_we_upper() ->
    {ok, Mp} = re:compile("Ve", [unicode]),
    Mp.

-spec ve_to_we_lower() -> {'re_pattern', _, _, _, _}.
ve_to_we_lower() ->
    {ok, Mp} = re:compile("ve", [unicode]),
    Mp.

-spec ry_to_wwy() -> {'re_pattern', _, _, _, _}.
ry_to_wwy() ->
    {ok, Mp} = re:compile("ry", [unicode]),
    Mp.

-spec rorl_to_w_upper() -> {'re_pattern', _, _, _, _}.
rorl_to_w_upper() ->
    {ok, Mp} = re:compile("(?:R|L)", [unicode]),
    Mp.

-spec rorl_to_w_lower() -> {'re_pattern', _, _, _, _}.
rorl_to_w_lower() ->
    {ok, Mp} = re:compile("(?:r|l)", [unicode]),
    Mp.

-spec ll_to_ww() -> {'re_pattern', _, _, _, _}.
ll_to_ww() ->
    {ok, Mp} = re:compile("ll", [unicode]),
    Mp.

-spec vowel_or_r_except_o_l_to_wl_upper() -> {'re_pattern', _, _, _, _}.
vowel_or_r_except_o_l_to_wl_upper() ->
    {ok, Mp} = re:compile("[AEIUR]([lL])$", [unicode]),
    Mp.

-spec vowel_or_r_except_o_l_to_wl_lower() -> {'re_pattern', _, _, _, _}.
vowel_or_r_except_o_l_to_wl_lower() ->
    {ok, Mp} = re:compile("[aeiur]l$", [unicode]),
    Mp.

-spec old_to_owld_upper() -> {'re_pattern', _, _, _, _}.
old_to_owld_upper() ->
    {ok, Mp} = re:compile("OLD", [unicode]),
    Mp.

-spec old_to_owld_lower() -> {'re_pattern', _, _, _, _}.
old_to_owld_lower() ->
    {ok, Mp} = re:compile("([Oo])ld", [unicode]),
    Mp.

-spec ol_to_owl_upper() -> {'re_pattern', _, _, _, _}.
ol_to_owl_upper() ->
    {ok, Mp} = re:compile("OL", [unicode]),
    Mp.

-spec ol_to_owl_lower() -> {'re_pattern', _, _, _, _}.
ol_to_owl_lower() ->
    {ok, Mp} = re:compile("([Oo])l", [unicode]),
    Mp.

-spec lorr_o_to_wo_upper() -> {'re_pattern', _, _, _, _}.
lorr_o_to_wo_upper() ->
    {ok, Mp} = re:compile("[LR]([oO])", [unicode]),
    Mp.

-spec lorr_o_to_wo_lower() -> {'re_pattern', _, _, _, _}.
lorr_o_to_wo_lower() ->
    {ok, Mp} = re:compile("[lr]o", [unicode]),
    Mp.

-spec specific_consonants_o_to_letter_and_wo_upper() -> {'re_pattern', _, _, _, _}.
specific_consonants_o_to_letter_and_wo_upper() ->
    {ok, Mp} = re:compile("([BCDFGHJKMNPQSTXYZ])([oO])", [unicode]),
    Mp.
-spec specific_consonants_o_to_letter_and_wo_lower() -> {'re_pattern', _, _, _, _}.
specific_consonants_o_to_letter_and_wo_lower() ->
    {ok, Mp} = re:compile("([bcdfghjkmnpqstxyz])o", [unicode]),
    Mp.

-spec vorw_le_to_wal() -> {'re_pattern', _, _, _, _}.
vorw_le_to_wal() ->
    {ok, Mp} = re:compile("[vw]le", [unicode]),
    Mp.

-spec fi_to_fwi_upper() -> {'re_pattern', _, _, _, _}.
fi_to_fwi_upper() ->
    {ok, Mp} = re:compile("FI", [unicode]),
    Mp.

-spec fi_to_fwi_lower() -> {'re_pattern', _, _, _, _}.
fi_to_fwi_lower() ->
    {ok, Mp} = re:compile("([Ff])i", [unicode]),
    Mp.

-spec ver_to_wer() -> {'re_pattern', _, _, _, _}.
ver_to_wer() ->
    {ok, Mp} = re:compile("([Vv])er", [unicode]),
    Mp.

-spec poi_to_pwoi() -> {'re_pattern', _, _, _, _}.
poi_to_pwoi() ->
    {ok, Mp} = re:compile("([Pp])oi", [unicode]),
    Mp.

-spec specific_consonants_le_to_letter_and_wal() -> {'re_pattern', _, _, _, _}.
specific_consonants_le_to_letter_and_wal() ->
    {ok, Mp} = re:compile("([DdFfGgHhJjPpQqRrSsTtXxYyZz])le$", [unicode]),
    Mp.

-spec consonant_r_to_consonant_w() -> {'re_pattern', _, _, _, _}.
consonant_r_to_consonant_w() ->
    {ok, Mp} = re:compile("([BbCcDdFfGgKkPpQqSsTtWwXxZz])r", [unicode]),
    Mp.

-spec ly_to_wy_upper() -> {'re_pattern', _, _, _, _}.
ly_to_wy_upper() ->
    {ok, Mp} = re:compile("Ly", [unicode]),
    Mp.

-spec ly_to_wy_lower() -> {'re_pattern', _, _, _, _}.
ly_to_wy_lower() ->
    {ok, Mp} = re:compile("ly", [unicode]),
    Mp.

-spec ple_to_pwe() -> {'re_pattern', _, _, _, _}.
ple_to_pwe() ->
    {ok, Mp} = re:compile("([Pp])le", [unicode]),
    Mp.

-spec nr_to_nw_upper() -> {'re_pattern', _, _, _, _}.
nr_to_nw_upper() ->
    {ok, Mp} = re:compile("NR", [unicode]),
    Mp.

-spec nr_to_nw_lower() -> {'re_pattern', _, _, _, _}.
nr_to_nw_lower() ->
    {ok, Mp} = re:compile("nr", [unicode]),
    Mp.

-spec fuc_to_fwuc() -> {'re_pattern', _, _, _, _}.
fuc_to_fwuc() ->
    {ok, Mp} = re:compile("([Ff])uc", [unicode]),
    Mp.

-spec mom_to_mwom() -> {'re_pattern', _, _, _, _}.
mom_to_mwom() ->
    {ok, Mp} = re:compile("([Mm])om", [unicode]),
    Mp.

-spec me_to_mwe() -> {'re_pattern', _, _, _, _}.
me_to_mwe() ->
    {ok, Mp} = re:compile("([Mm])e", [unicode]),
    Mp.

-spec n_vowel_to_ny_first() -> {'re_pattern', _, _, _, _}.
n_vowel_to_ny_first() ->
    {ok, Mp} = re:compile("n([aeiou])", [unicode]),
    Mp.

-spec n_vowel_to_ny_second() -> {'re_pattern', _, _, _, _}.
n_vowel_to_ny_second() ->
    {ok, Mp} = re:compile("N([aeiou])", [unicode]),
    Mp.

-spec n_vowel_to_ny_third() -> {'re_pattern', _, _, _, _}.
n_vowel_to_ny_third() ->
    {ok, Mp} = re:compile("N([AEIOU])", [unicode]),
    Mp.

-spec ove_to_uv_upper() -> {'re_pattern', _, _, _, _}.
ove_to_uv_upper() ->
    {ok, Mp} = re:compile("OVE", [unicode]),
    Mp.

-spec ove_to_uv_lower() -> {'re_pattern', _, _, _, _}.
ove_to_uv_lower() ->
    {ok, Mp} = re:compile("ove", [unicode]),
    Mp.

-spec haha_to_hehe_xd() -> {'re_pattern', _, _, _, _}.
haha_to_hehe_xd() ->
    {ok, Mp} = re:compile("\\b(ha|hah|heh|hehe)+\\b", [unicode]),
    Mp.

-spec the_to_teh() -> {'re_pattern', _, _, _, _}.
the_to_teh() ->
    {ok, Mp} = re:compile("\\b([Tt])he\\b", [unicode]),
    Mp.

-spec you_to_u_upper() -> {'re_pattern', _, _, _, _}.
you_to_u_upper() ->
    {ok, Mp} = re:compile("\\bYou\\b", [unicode]),
    Mp.

-spec you_to_u_lower() -> {'re_pattern', _, _, _, _}.
you_to_u_lower() ->
    {ok, Mp} = re:compile("\\byou\\b", [unicode]),
    Mp.

-spec time_to_tim() -> {'re_pattern', _, _, _, _}.
time_to_tim() ->
    {ok, Mp} = re:compile("\\b([Tt])ime\\b", [unicode]),
    Mp.

-spec over_to_owor() -> {'re_pattern', _, _, _, _}.
over_to_owor() ->
    {ok, Mp} = re:compile("([Oo])ver", [unicode]),
    Mp.

-spec worse_to_wose() -> {'re_pattern', _, _, _, _}.
worse_to_wose() ->
    {ok, Mp} = re:compile("([Ww])orse", [unicode]),
    Mp.

faces() ->
    [
        <<"(・`ω´・)"/utf8>>,
        <<";;w;;"/utf8>>,
        <<"owo"/utf8>>,
        <<"UwU"/utf8>>,
        <<">w<"/utf8>>,
        <<"^w^"/utf8>>,
        <<"(* ^ ω ^)"/utf8>>,
        <<"(⌒ω⌒)"/utf8>>,
        <<"ヽ(*・ω・)ﾉ"/utf8>>,
        <<"(o´∀`o)"/utf8>>,
        <<"(o･ω･o)"/utf8>>,
        <<"＼(＾▽＾)／"/utf8>>,
        <<"(*^ω^)"/utf8>>,
        <<"(◕‿◕✿)"/utf8>>,
        <<"(◕ᴥ◕)"/utf8>>,
        <<"ʕ•ᴥ•ʔ"/utf8>>,
        <<"ʕ￫ᴥ￩ʔ"/utf8>>,
        <<"(*^.^*)"/utf8>>,
        <<"(｡♥‿♥｡)"/utf8>>,
        <<"OwO"/utf8>>,
        <<"uwu"/utf8>>,
        <<"uvu"/utf8>>,
        <<"UvU"/utf8>>,
        <<"(*￣з￣)"/utf8>>,
        <<"(つ✧ω✧)つ"/utf8>>,
        <<"(/ =ω=)/"/utf8>>,
        <<"(╯°□°）╯︵ ┻━┻"/utf8>>,
        <<"┬─┬ ノ( ゜-゜ノ)"/utf8>>,
        <<"¯\\_(ツ)_/¯"/utf8>>
    ].

map_o_to_owo(Input) ->
    Replacement = case rand:uniform(2) of
        2 -> <<"owo"/utf8>>;
        _ -> <<"o"/utf8>>
    end,
    words:replace(Input, o_to_owo(), Replacement, false).

map_ew_to_uwu(Input) ->
    words:replace(Input, ew_to_uwu(), <<"uwu"/utf8>>, false).

map_hey_to_hay(Input) ->
    words:replace(Input, hey_to_hay(), <<"\\1ay"/utf8>>, false).

map_dead_to_ded(Input) ->
    FirstResult = words:replace(Input, dead_to_ded_upper(), <<"Ded"/utf8>>, false),
    words:replace(FirstResult, dead_to_ded_lower(), <<"ded"/utf8>>, false).

map_n_vowel_t_to_nd(Input) ->
    words:replace(Input, n_vowel_t_to_nd(), <<"nd"/utf8>>, false).

map_read_to_wead(Input) ->
    FirstResult = words:replace(Input, read_to_wead_upper(), <<"Wead"/utf8>>, false),
    words:replace(FirstResult, read_to_wead_lower(), <<"wead"/utf8>>, false).

map_brackets_to_star_trails(Input) ->
    FirstResult = words:replace(Input, brackets_to_startrails_fore(), <<"｡･:*:･ﾟ★,｡･:*:･ﾟ☆"/utf8>>, false),
    words:replace(FirstResult, brackets_to_startrails_rear(), <<"☆ﾟ･:*:･｡,★ﾟ･:*:･｡"/utf8>>, false).

map_period_comma_exclamation_semicolon_to_kaomojis(Input) ->
    Func = fun() ->
        Length = length(faces()),
        N = rand:uniform(Length),
        lists:nth(N, faces())
    end,
    FirstResult = words:replace_with_func_single(Input, period_comma_exclamation_semicolon_to_kaomojis_first(), Func, false),
    words:replace_with_func_single(FirstResult, period_comma_exclamation_semicolon_to_kaomojis_second(), Func, false).

map_that_to_dat(Input) ->
    FirstResult = words:replace(Input, that_to_dat_lower(), <<"dat"/utf8>>, false),
    words:replace(FirstResult, that_to_dat_upper(), <<"Dat"/utf8>>, false).

map_th_to_f(Input) ->
    FirstResult = words:replace(Input, th_to_f_lower(), <<"f"/utf8>>, false),
    words:replace(FirstResult, th_to_f_upper(), <<"F"/utf8>>, false).

map_le_to_wal(Input) ->
    words:replace(Input, le_to_wal(), <<"wal"/utf8>>, false).

map_ve_to_we(Input) ->
    FirstResult = words:replace(Input, ve_to_we_lower(), <<"we"/utf8>>, false),
    words:replace(FirstResult, ve_to_we_upper(), <<"We"/utf8>>, false).

map_ry_to_wwy(Input) ->
    words:replace(Input, ry_to_wwy(), <<"wwy"/utf8>>, false).

map_r_or_l_to_w(Input) ->
    FirstResult = words:replace(Input, rorl_to_w_lower(), <<"w"/utf8>>, false),
    words:replace(FirstResult, rorl_to_w_upper(), <<"W"/utf8>>, false).

map_ll_to_ww(Input) ->
    words:replace(Input, ll_to_ww(), <<"ww"/utf8>>, false).

map_vowel_or_r_except_o_l_to_wl(Input) ->
    FirstResult = words:replace(Input, vowel_or_r_except_o_l_to_wl_lower(), <<"wl"/utf8>>, false),
    words:replace(FirstResult, vowel_or_r_except_o_l_to_wl_upper(), <<"W\\1"/utf8>>, false).

map_old_to_owld(Input) ->
    FirstResult = words:replace(Input, old_to_owld_lower(), <<"\\1wld"/utf8>>, false),
    words:replace(FirstResult, old_to_owld_upper(), <<"OWLD"/utf8>>, false).

map_ol_to_owl(Input) ->
    FirstResult = words:replace(Input, ol_to_owl_lower(), <<"\\1wl"/utf8>>, false),
    words:replace(FirstResult, ol_to_owl_upper(), <<"OWL"/utf8>>, false).

map_l_or_r_o_to_wo(Input) ->
    FirstResult = words:replace(Input, lorr_o_to_wo_lower(), <<"wo"/utf8>>, false),
    words:replace(FirstResult, lorr_o_to_wo_upper(), <<"W\\1"/utf8>>, false).

map_specific_consonants_o_to_letter_and_wo(Input) ->
    Func = fun(S1, S2) ->
        UppercaseS2 = string:uppercase(S2),
        if UppercaseS2 =:= S2 ->
            W = <<"W"/utf8>>,
            <<S1/bytes, W/bytes, S2/bytes>>;
           true ->
            W = <<"w"/utf8>>,
            <<S1/bytes, W/bytes, S2/bytes>>
        end
    end,
    FirstResult = words:replace(Input, specific_consonants_o_to_letter_and_wo_lower(), <<"\\1wo"/utf8>>, false),
    words:replace_with_func_multiple(FirstResult, specific_consonants_o_to_letter_and_wo_upper(), Func, false).

map_v_or_w_le_to_wal(Input) ->
    words:replace(Input, vorw_le_to_wal(), <<"wal"/utf8>>, false).

map_fi_to_fwi(Input) ->
    FirstResult = words:replace(Input, fi_to_fwi_lower(), <<"\\1wi"/utf8>>, false),
    words:replace(FirstResult, fi_to_fwi_upper(), <<"FWI"/utf8>>, false).

map_ver_to_wer(Input) ->
    words:replace(Input, ver_to_wer(), <<"wer"/utf8>>, false).

map_poi_to_pwoi(Input) ->
    words:replace(Input, poi_to_pwoi(), <<"\\1woi"/utf8>>, false).

map_specific_consonants_le_to_letter_and_wal(Input) ->
    words:replace(Input, specific_consonants_le_to_letter_and_wal(), <<"\\1wal"/utf8>>, false).

map_consonant_r_to_consonant_w(Input) ->
    words:replace(Input, consonant_r_to_consonant_w(), <<"\\1w"/utf8>>, false).

map_ly_to_wy(Input) ->
    FirstResult = words:replace(Input, ly_to_wy_lower(), <<"wy"/utf8>>, false),
    words:replace(FirstResult, ly_to_wy_upper(), <<"Wy"/utf8>>, false).

map_ple_to_pwe(Input) ->
    words:replace(Input, ple_to_pwe(), <<"\\1we"/utf8>>, false).

map_nr_to_nw(Input) ->
    FirstResult = words:replace(Input, nr_to_nw_lower(), <<"nw"/utf8>>, false),
    words:replace(FirstResult, nr_to_nw_upper(), <<"NW"/utf8>>, false).

map_fuc_to_fwuc(Input) ->
    words:replace(Input, fuc_to_fwuc(), <<"\\1wuc"/utf8>>, false).

map_mom_to_mwom(Input) ->
    words:replace(Input, mom_to_mwom(), <<"\\1wom"/utf8>>, false).

map_me_to_mwe(Input) ->
    words:replace(Input, me_to_mwe(), <<"\\1we"/utf8>>, false).

map_n_vowel_to_ny(Input) ->
    FirstResult = words:replace(Input, n_vowel_to_ny_first(), <<"ny\\1"/utf8>>, false),
    SecondResult = words:replace(FirstResult, n_vowel_to_ny_second(), <<"Ny\\1"/utf8>>, false),
    words:replace(SecondResult, n_vowel_to_ny_third(), <<"NY\\1"/utf8>>, false).

map_ove_to_uv(Input) ->
    FirstResult = words:replace(Input, ove_to_uv_lower(), <<"uv"/utf8>>, false),
    words:replace(FirstResult, ove_to_uv_upper(), <<"UV"/utf8>>, false).

map_haha_to_hehe_xd(Input) ->
    words:replace(Input, haha_to_hehe_xd(), <<"hehe xD"/utf8>>, false).

map_the_to_teh(Input) ->
    words:replace(Input, the_to_teh(), <<"\\1eh"/utf8>>, false).

map_you_to_u(Input) ->
    FirstResult = words:replace(Input, you_to_u_upper(), <<"U"/utf8>>, false),
    words:replace(FirstResult, you_to_u_lower(), <<"u"/utf8>>, false).

map_time_to_tim(Input) ->
    words:replace(Input, time_to_tim(), <<"\\1im"/utf8>>, false).

map_over_to_owor(Input) ->
    words:replace(Input, over_to_owor(), <<"\\1wor"/utf8>>, false).

map_worse_to_wose(Input) ->
    words:replace(Input, worse_to_wose(), <<"\\1ose"/utf8>>, false).