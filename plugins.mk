feature_verbose_0 = @echo " FEATURE " $(filter %.feature,$(?F));
feature_verbose = $(feature_verbose_$(V))

# Core targets.

define compile_feature
	$(verbose) mkdir -p ebin/ $(TEST_DIR)
	$(feature_verbose) $(call erlang,$(call compile_feature.erl,$(1)))
endef

define compile_feature.erl
	[begin
		ecucumber_ct:generate_source(F,
			[{output_src_dir, "$(call core_native_path,$(TEST_DIR))"}])
	end || F <- string:tokens("$(1)", " ")],
	halt().
endef

ifneq ($(wildcard features/),)
ebin/$(PROJECT).app:: $(sort $(call core_find,features/,*.feature))
	$(if $(strip $?),$(call compile_feature,$?))
endif
