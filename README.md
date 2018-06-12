# ecucumber

ecucumber is an open source port of Cucumber for Erlang.

Define automatic tests in Gherkin and execute them using Common Test.

## Goals

ecucumber aims at providing a simple and productive BDD experience combining the best parts of Cucumber and Erlang.

Testing a feature using ecucumber should be as easy as writing a Common Test test suite.

ecucumber is *clean* and *well tested* Erlang code.

## How to build and use

ecucumber uses erlang.mk as build tool so you only need a recent version of *make*:

	git clone https://github.com/jabberbees/ecucumber
	cd ecucumber
	make

To run tests:

	make ct

To use: add ecucumber as a test dependency in your erlang.mk based Makefile.

    TEST_DEPS = ecucumber

    dep_ecucumber = git https://github.com/jabberbees/ecucumber.git

## Writing tests

### Rules
Write your tests using the [Gherkin syntax](https://docs.cucumber.io/gherkin/reference/) and place them in the features/ directory.

Gherkin feature file names must be lowercase ending with .feature file extension.

ecucumber only support english Gherkin keywords for the moment. This is a limitation of egherkin, the Gherkin parser used by ecucumber.

### Step definitions
Testing code must be placed in step definitions Erlang modules.

Step definitions modules must implement the following callbacks:

    * setup(Context) -> Context

    Initialization code, called before each scenario or once at the very beginning depending on where the activation tag was placed.
    
    * cleanup(Context) -> Context
    
    Cleanup code, called after each scenario or once at the very end depending on where the activation tag was placed.

    * step_def(GWT, StepParts, Context) -> Context | nomatch

    Where:

        GWT = given_keyword | when_keyword | then_keyword
        StepParts = [StepPart]
        StepPart = binary() | DocString | DataTable
        DocString = {docstring, [Line :: binary()]}
        DataTable = {datatable,
            [RowName :: binary()],
            [{RowName :: binary(), Value :: binary()]}

    Use GWT and StepParts to match a step pattern.
    This is where you place your test code that gets called by ecucumber during execution.
    Either return a Context (modified or unchanged) when the step was handled or nomatch when no match was found.
    When nomatch is returned, the execution proceeds with following step definitions modules, otherwise the execution proceeds to the next step in the scenario with the new Context.

### Binding step definitions with tests
Step definitions are associated with tests using @mod:<module> Gherkin tags.

Example: the following tag will activate evaluation of step definitions in the maths_step_defs Erlang module

    @mod:maths_step_defs

### Step definitions evaluation

All step definitions activated by a @mod:<module> Gherkin tag are evaluated (by calling the step_def/3 callback) until one step definitions module returns a new context instead of nomatch.

The order of evaluation is the following:
    * Scenario tags in order of declaration
    * Feature tags in order of declaration

Example:

    @mod:common
    Feature: Addition

    @mod:addition @mod:calculator
    Scenario: Add two numbers
        Given I have entered 50 into the calculator
        And I have entered 70 into the calculator
        When I press add
        Then the result should be 120 on the screen

Order of evaluation: addition, calculator, common

## BDD workflow with ecucumber

### Write tests

Example: features/addition.feature

    Feature: Addition

    Scenario: Add two numbers
        Given I have entered 50 into the calculator
        And I have entered 70 into the calculator
        When I press add
        Then the result should be 120 on the screen

### Execute tests
    make ct

Tests fail for lack of step definitions but Common Test logs provide code templates to ease step definitions implementation.

### Provide test definitions modules

Example: test/maths_step_defs.erl

    -module(maths_step_defs).

    -export([setup/1, cleanup/1, step_def/3]).

    setup(Context) -> Context.

    cleanup(Context) -> Context.

    step_def(given_keyword, [<<"I">>,<<"have">>,<<"entered">>,Number,<<"into">>,
                            <<"the">>,<<"calculator">>], Context) ->
        Value = binary_to_integer(Number),
        ecucumber_ct_context:add_value(numbers, Value, Context);
    step_def(when_keyword, [<<"I">>,<<"press">>,<<"add">>], Context) ->
        Numbers = ecucumber_ct_context:get_value(numbers, Context, []),
        Result = lists:foldl(fun(N, A) -> A+N end, 0, Numbers),
        ecucumber_ct_context:set_value(result, Result, Context);
    step_def(then_keyword, [<<"the">>,<<"result">>,<<"should">>,<<"be">>,
                            Expected,<<"on">>,<<"the">>,<<"screen">>], Context) ->
        Actual = ecucumber_ct_context:get_value(result, Context, undefined),
        Actual = binary_to_integer(Expected),
        Context;
    step_def(_GWT, _Pattern, _Context) ->
        nomatch.

### Provide the test to code

Example: src/maths.erl

    -module(maths).

    -export([add/1]).

    add(Numbers) ->
        lists:foldl(fun(N, A) -> A+N end, 0, Numbers).

### Bind step definitions

Add @mod:<module> Gherkin tags the Feature: or relevant Scenario: keywords to activate your step definitions modules.
 
Example: features/addition.feature

    **@mod:maths_steps_defs**
    Feature: Addition

    Scenario: Add two numbers
        Given I have entered 50 into the calculator
        And I have entered 70 into the calculator
        When I press add
        Then the result should be 120 on the screen

### Run tests
    make ct

### Iterate
    * Add tests
    * Add step definitions
    * Test, implement, modify, refactor until all tests pass

## ecucumber and Git

Add the following to your .gitignore file:

    test/feature_*.erl

This will allow Git to ignore Common Test suites generated by ecucumber.

## Next steps
    * Adding support for rebar
    * Adding support for eunit

Please contribute!

## Compatibility

ecucumber was developed and tested with **Erlang/OTP R16B03-1** on Windows 10.
Please report any compatibility issue you encounter.
