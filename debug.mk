define debug-ct.erl
	ModuleSource = fun(Module) ->
		ModuleInfo = Module:module_info(),
		case lists:keyfind(compile, 1, ModuleInfo) of 
		{compile, Compile} ->
			case lists:keyfind(source, 1, Compile) of 
			{source, Source} ->
				Source;
			_ -> 
				unavailable
			end;
		_ -> 
			unavailable
		end
	end,
	BreakOn = fun(Flags) ->
		i:iaa(Flags)
	end,
	GetIaa = fun() ->
		case int:auto_attach() of
		false -> [];
		{Flags, _} -> Flags
		end
	end,
	AddBreakpoint = fun
		(Module) when is_atom(Module) ->
			io:format("Adding breakpoint: modude ~s~n", [Module]),
			i:im(),
			Src = ModuleSource(Module),
			case i:ii(Src) of
			{module, _} -> 
				BreakOn(GetIaa() ++ [init]),
				ok;
			error -> error
			end;
		({Module, LineOrLines}) when is_atom(Module) ->
			io:format("Adding breakpoint: modude ~s, lines ~p~n", [Module, LineOrLines]),
			i:im(),
			Src = ModuleSource(Module),
			case i:ii(Src) of
			{module, _} ->
				case LineOrLines of
				Line when is_integer(Line) -> i:ib(Module, Line);
				Lines when is_list(Lines) -> [i:ib(Module, Line) || Line <- Lines]
				end,
				BreakOn(GetIaa() ++ [break]),
				ok;
			error ->
				error
			end
	end,
	Breakpoints = [ecucumber_ct_context],
	[AddBreakpoint(Breakpoint) || Breakpoint <- Breakpoints],

	CTOpts = [
		{auto_compile, false},
		{dir, "test"},
		{logdir, "logs"},
		{suite, ecucumber_ct_context_SUITE},
		{testcase, add_value_appends_value}
	],
	case ct:run_test(CTOpts) of
	{Ok, Failed, {UserSkipped, AutoSkipped}} ->
		io:format("~p ok, ~p failed, ~p user skipped, ~p auto skipped~n", [Ok, Failed, UserSkipped, AutoSkipped]);
	{error, Reason} ->
		io:format("error: ~p~n", [Reason])
	end,
	halt()
endef

CT_ERL_OPTS = -noinput \
	-pa $(CURDIR)/ebin $(DEPS_DIR)/*/ebin $(APPS_DIR)/*/ebin $(TEST_DIR) \
	$(CT_EXTRA) \
	$(CT_OPTS)

debug-ct: test-build
	$(gen_verbose) $(call erlang,$(call debug-ct.erl,['$(t)']),$(CT_ERL_OPTS))
