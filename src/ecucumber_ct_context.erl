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

-module(ecucumber_ct_context).

-export([
    enter_feature/2,
    leave_feature/1,

    enter_scenario/3,
    leave_scenario/1,
    
    execute_step_def/5,

    is_defined/2,
    get_value/3,
    set_value/3,
    add_value/3,
    delete_value/2,
    delete_values/2
]).

enter_feature(_FeatureName, Context) ->
    Context.

leave_feature(Context) ->
    Context.

enter_scenario(_ScenarioName, Tags, Context) ->
    [{ecucumber_tags, Tags} | Context].

leave_scenario(Context) ->
    case proplists:get_value(ecucumber_failed, Context, false) of
    true -> ct:fail(missing_step_definitions), Context;
    false -> Context
    end.

execute_step_def(Line, GWT, StepParts, Mods, Context) ->
    ct:pal(info,
        "[~p] ~s ~s",
        [Line, egherkin_lib:format_gwt(GWT), egherkin_lib:format_step_parts(StepParts)]
    ),
    Context1 = set_value(ecucumber_line, Line, Context),
    execute_step_def(GWT, StepParts, Mods, Context1).

execute_step_def(GWT, StepParts, [], Context) ->
    ct:pal(error,
        "failed to find step definition!~n"
        "please add following implementation in one of your step definitions modules:~n"
        "~n"
        "step_def(~s, ~p, Context) ->~n"
        "   Context."
        "~n",
        [GWT, StepParts]
    ),
    [{ecucumber_failed, true} | Context];
execute_step_def(GWT, StepParts, [Mod | Mods], Context) ->
    case Mod:step_def(GWT, StepParts, Context) of
    nomatch ->
        execute_step_def(GWT, StepParts, Mods, Context);
    NewContext ->
        ct:pal(info, "done"),
        NewContext
    end.

is_defined(Key, Context) ->
    proplists:is_defined(Key, Context).

get_value(Key, Context, Default) ->
    proplists:get_value(Key, Context, Default).

set_value(Key, Value, Context) ->
    Context2 = lists:keydelete(Key, 1, Context),
    [{Key, Value} | Context2].

add_value(Key, Value, Context) ->
    case lists:keytake(Key, 1, Context) of
    {value, {_, List}, Context2} -> [{Key, List ++ [Value]} | Context2];
    false -> [{Key, [Value]} | Context]
    end.

delete_value(Key, Context) ->
    lists:keydelete(Key, 1, Context).

delete_values(Keys, Context) ->
    lists:foldl(fun(Key, Acc) ->
        lists:keydelete(Key, 1, Acc)
    end, Context, Keys).
