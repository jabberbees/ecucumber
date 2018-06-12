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

-ifndef(ASSERT).
-define(ASSERT, 1).

-define(debugVal(Expr),
	begin
		ct:pal(
			"module: ~s (line ~p)~n"
			"expression: ~s~n"
			"value: ~p~n",
			[?MODULE, ?LINE, (??Expr), Expr])
	end).

-define(assert(Expr),
	begin
	((fun (__X) ->
	    case (Expr) of
		__X -> ok;
		__V ->
            ct:fail(
                "assertion failed: assert~n"
                "module:     ~s (line ~p)~n"
                "expression: ~s~n"
                "expected:   ~p~n"
                "actual:     ~p~n",
                [?MODULE, ?LINE, (??Expr), __X, __V])
	    end
	  end)(true))
	end).

-define(assertEqual(Expected, Expr),
	begin
	((fun (__X) ->
	    case (Expr) of
		__X -> ok;
		__V ->
            ct:fail(
                "assertion failed: assertEqual~n"
                "module:     ~s (line ~p)~n"
                "expression: ~s~n"
                "expected:   ~p~n"
                "actual:     ~p~n",
                [?MODULE, ?LINE, (??Expr), __X, __V])
	    end
	  end)(Expected))
	end).

-define(assertMatch(Guard, Expr),
	begin
	((fun () ->
	    case (Expr) of
		Guard -> ok;
		__V ->
            ct:fail(
                "assertion failed: assertMatch~n"
                "module:     ~s (line ~p)~n"
                "expression: ~s~n"
                "pattern:    ~s~n"
                "actual:     ~p~n",
                [?MODULE, ?LINE, (??Expr), (??Guard), __V])
	    end
	  end)())
	end).

-define(assertLength(Expected, Expr),
	begin
	((fun (__X) ->
	    case length(Expr) of
		__X -> ok;
		__V ->
            ct:fail(
                "assertion failed: assertLength~n"
                "module:     ~s (line ~p)~n"
                "expression: ~s~n"
                "expected:   ~p~n"
                "actual:     ~p~n",
                [?MODULE, ?LINE, (??Expr), __X, __V])
	    end
	  end)(Expected))
	end).

-define(assertKeyValue(Expected, Key, PropList),
	begin
	((fun (__X) ->
	    case proplists:get_value(Key, PropList) of
		__X -> ok;
		__V ->
            ct:fail(
                "assertion failed: assertKeyValue~n"
                "module:     ~s (line ~p)~n"
                "key:        ~s~n"
                "expected:   ~p~n"
                "actual:     ~p~n",
                [?MODULE, ?LINE, Key, __X, __V])
	    end
	  end)(Expected))
	end).

-define(assertKeyExists(Key, PropList),
	begin
	((fun (__X) ->
	    case proplists:is_defined(Key, PropList) of
		__X -> ok;
		__V ->
            ct:fail(
                "assertion failed: assertKeyExists~n"
                "module:     ~s (line ~p)~n"
                "key:        ~s~n"
                "exists:     ~s~n",
                [?MODULE, ?LINE, Key, __V])
	    end
	  end)(true))
	end).

-endif.
