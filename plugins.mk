feature_verbose_0 = @echo " FEATURE " $(filter %.feature,$(?F));
feature_verbose = $(feature_verbose_$(V))

define compile_features
	$(verbose) mkdir -p ebin/ $(TEST_DIR)
	$(feature_verbose) $(call erlang,$(call compile_features.erl,$(1)))
endef

define compile_features.erl
	[begin
		case ecucumber_ct:generate_source(F,
			[{output_src_dir, "$(call core_native_path,$(TEST_DIR))"}]) of
		{ok, Filename} ->
			io:format("          -> ~s~n", [Filename]);
		{error, Reason} ->
			io:format("          error: ~p~n", [Reason]);
		{failed, Line, Reason} ->
			io:format("          error: ~p, line ~p~n", [Reason, Line])
		end
	end || F <- string:tokens("$(1)", " ")],
	halt().
endef

ifneq ($(wildcard features/),)
ecucumber-features: $(sort $(call core_find,features/,*.feature))
	$(if $(strip $?),$(call compile_features,$?))

ebin/$(PROJECT).app:: test-deps ecucumber-features
endif
