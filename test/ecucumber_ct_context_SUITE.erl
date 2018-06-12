%% Copyright (c) 2018, Jabberbees SAS

%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.

%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%% @author Emmanuel Boutin <emmanuel.boutin@jabberbees.com>

-module(ecucumber_ct_context_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("assert.hrl").

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

all() -> [
    is_defined_returns_false,
    is_defined_returns_true,

    set_value_sets_value,
    set_value_overrides_previous_value,

    add_value_sets_value,
    add_value_appends_value,

    delete_value_removes_value,
    delete_value_does_nothing
].

%%region is_defined

is_defined_returns_false(_) ->
    C = [],
    ?assertEqual(false, ecucumber_ct_context:is_defined(items, C)),
    ok.

is_defined_returns_true(_) ->
    C = ecucumber_ct_context:add_value(items, 42, []),
    ?assertEqual(true, ecucumber_ct_context:is_defined(items, C)),
    ok.

%%endregion

%%region set_value

set_value_sets_value(_) ->
    C = ecucumber_ct_context:set_value(items, 42, []),
    ?assertEqual(42, ecucumber_ct_context:get_value(items, C, [])),
    ok.

set_value_overrides_previous_value(_) ->
    C1 = ecucumber_ct_context:set_value(items, 42, []),
    C2 = ecucumber_ct_context:set_value(items, 314, C1),
    ?assertEqual(314, ecucumber_ct_context:get_value(items, C2, 0)),
    ok.

%%endregion

%%region add_value

add_value_sets_value(_) ->
    C = ecucumber_ct_context:add_value(items, 42, []),
    ?assertEqual([42], ecucumber_ct_context:get_value(items, C, [])),
    ok.

add_value_appends_value(_) ->
    C1 = ecucumber_ct_context:add_value(items, 42, []),
    C2 = ecucumber_ct_context:add_value(items, 314, C1),
    ?assertEqual([42, 314], ecucumber_ct_context:get_value(items, C2, [])),
    ok.

%%endregion

%%region delete_value

delete_value_removes_value(_) ->
    C1 = ecucumber_ct_context:set_value(items, 42, []),
    C2 = ecucumber_ct_context:delete_value(items, C1),
    ?assertEqual(false, ecucumber_ct_context:is_defined(items, C2)),
    ok.

delete_value_does_nothing(_) ->
    C = ecucumber_ct_context:set_value(stuff, 42, []),
    ?assertEqual(C, ecucumber_ct_context:delete_value(items, C)),
    ok.

%%endregion
