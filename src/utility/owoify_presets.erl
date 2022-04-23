%% @private

-module(owoify_presets).

-export([specific_word_mapping_list/0, uvu_mapping_list/0, uwu_mapping_list/0, owo_mapping_list/0]).

-spec specific_word_mapping_list() -> [fun((_) -> any()), ...].
specific_word_mapping_list() ->
    [
        fun owoify_mappings:map_fuc_to_fwuc/1,
        fun owoify_mappings:map_mom_to_mwom/1,
        fun owoify_mappings:map_time_to_tim/1,
        fun owoify_mappings:map_me_to_mwe/1,
        fun owoify_mappings:map_n_vowel_to_ny/1,
        fun owoify_mappings:map_over_to_owor/1,
        fun owoify_mappings:map_ove_to_uv/1,
        fun owoify_mappings:map_haha_to_hehe_xd/1,
        fun owoify_mappings:map_the_to_teh/1,
        fun owoify_mappings:map_you_to_u/1,
        fun owoify_mappings:map_read_to_wead/1,
        fun owoify_mappings:map_worse_to_wose/1
    ].

-spec uvu_mapping_list() -> [fun((_) -> any()), ...].
uvu_mapping_list() ->
    [
        fun owoify_mappings:map_o_to_owo/1,
        fun owoify_mappings:map_ew_to_uwu/1,
        fun owoify_mappings:map_hey_to_hay/1,
        fun owoify_mappings:map_dead_to_ded/1,
        fun owoify_mappings:map_n_vowel_t_to_nd/1
    ].

-spec uwu_mapping_list() -> [fun((_) -> any()), ...].
uwu_mapping_list() ->
    [
        fun owoify_mappings:map_brackets_to_star_trails/1,
        fun owoify_mappings:map_period_comma_exclamation_semicolon_to_kaomojis/1,
        fun owoify_mappings:map_that_to_dat/1,
        fun owoify_mappings:map_th_to_f/1,
        fun owoify_mappings:map_le_to_wal/1,
        fun owoify_mappings:map_ve_to_we/1,
        fun owoify_mappings:map_ry_to_wwy/1,
        fun owoify_mappings:map_r_or_l_to_w/1
    ].

-spec owo_mapping_list() -> [fun((_) -> any()), ...].
owo_mapping_list() ->
    [
        fun owoify_mappings:map_ll_to_ww/1,
        fun owoify_mappings:map_vowel_or_r_except_o_l_to_wl/1,
        fun owoify_mappings:map_old_to_owld/1,
        fun owoify_mappings:map_ol_to_owl/1,
        fun owoify_mappings:map_l_or_r_o_to_wo/1,
        fun owoify_mappings:map_specific_consonants_o_to_letter_and_wo/1,
        fun owoify_mappings:map_v_or_w_le_to_wal/1,
        fun owoify_mappings:map_fi_to_fwi/1,
        fun owoify_mappings:map_ver_to_wer/1,
        fun owoify_mappings:map_poi_to_pwoi/1,
        fun owoify_mappings:map_specific_consonants_le_to_letter_and_wal/1,
        fun owoify_mappings:map_consonant_r_to_consonant_w/1,
        fun owoify_mappings:map_ly_to_wy/1,
        fun owoify_mappings:map_ple_to_pwe/1,
        fun owoify_mappings:map_nr_to_nw/1
    ].