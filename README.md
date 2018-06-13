# ecucumber

ecucumber is an open source port of [Cucumber](https://cucumber.io/) for Erlang.

Define automatic tests in Gherkin and execute them using Common Test.

## Goals

ecucumber aims at providing a *simple* and *productive* BDD experience combining the best parts of Cucumber and Erlang.

Testing a feature using ecucumber should be as *easy* as writing a Common Test test suite.

ecucumber is *clean* and *well tested* Erlang code.

## How to use

ecucumber only supports [erlang.mk](https://erlang.mk/) as a build tool for the moment.

You only need to add ecucumber as a test dependency in your Makefile, before including erlang.mk.

    TEST_DEPS = ecucumber
    DEP_PLUGINS = ecucumber
    dep_ecucumber = git https://github.com/jabberbees/ecucumber.git

## Tutorial: the BDD workflow with ecucumber

### Create a new Erlang project

Create an empty Erlang project

Add erlang.mk to your project.

Check [here](https://erlang.mk/guide/getting_started.html) if need you help to do this.

### Add the following Makefile

    PROJECT = test

    TEST_DEPS = ecucumber

    DEP_PLUGINS = ecucumber

    dep_ecucumber = git https://github.com/jabberbees/ecucumber.git

    include erlang.mk

### Add the first test

File: features/addition.feature

    Feature: Addition

    Scenario: Add two numbers
        Given I have entered 50 into the calculator
        And I have entered 70 into the calculator
        When I press add
        Then the result should be 120 on the screen

Please look at the reference guide below if you need more information on how to write tests.

Run make once to have ecucumber detect the new test.

    make

FYI you need to run make once each time you add new Gherkin tests.

### Execute tests
At this stage your test is executable even though you have not provided any Erlang code.

    make ct

The test fails but what did you expect? ;-)

You need to provide the Erlang test code.

ecucumber helps you by providing code templates for the missing step definitions.

    Common Test starting
    ...
    TEST INFO: 1 test(s), 1 case(s) in 1 suite(s)
    ...
    failed to find step definition!
    please add following implementation:

    step_def(given_keyword, [<<"I">>,<<"have">>,<<"entered">>,<<"50">>,<<"into">>,
                            <<"the">>,<<"calculator">>], Context) ->
        Context.

    step_def(given_keyword, [<<"I">>,<<"have">>,<<"entered">>,<<"70">>,<<"into">>,
                            <<"the">>,<<"calculator">>], Context) ->
        Context.

    step_def(when_keyword, [<<"I">>,<<"press">>,<<"add">>], Context) ->
        Context.

    step_def(then_keyword, [<<"the">>,<<"result">>,<<"should">>,<<"be">>,
                            <<"120">>,<<"on">>,<<"the">>,<<"screen">>], Context) ->
        Context.
    ...
    Testing testing.test.feature_addition_SUITE: *** FAILED test case 1 of 1 ***
    Testing testing.test.feature_addition_SUITE: TEST COMPLETE, 0 ok, 1 failed of 1 test cases

You just need to copy and paste these step definitions into a step definitions Erlang module.

### Provide test definitions modules
Here is the complete step definitions module for the above test.

File: test/maths_step_defs.erl

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
        Result = maths:add(Numbers),
        ecucumber_ct_context:set_value(result, Result, Context);
    step_def(then_keyword, [<<"the">>,<<"result">>,<<"should">>,<<"be">>,
                            Expected,<<"on">>,<<"the">>,<<"screen">>], Context) ->
        Actual = ecucumber_ct_context:get_value(result, Context, undefined),
        Actual = binary_to_integer(Expected),
        Context;
    step_def(_GWT, _Pattern, _Context) ->
        nomatch.

Notice that we use Erlang pattern matching to bind some parts to variables.

Notice also that one of the step definitions is calling maths:add/1 which we still have not provided. This is the actual business code that we are writing tests for.

### Bind step definitions
If you run the test again, you will get the same errors (missing step definitions) because we have not told ecucumber how to find our new step definitions.

For this, you need to add @mod:\<module\> Gherkin tags above the *Feature:* or relevant *Scenario:* keywords to activate your step definitions modules.
 
File to modify: features/addition.feature

    @mod:maths_steps_defs
    Feature: Addition

    Scenario: Add two numbers
        Given I have entered 50 into the calculator
        And I have entered 70 into the calculator
        When I press add
        Then the result should be 120 on the screen

### Provide the test to code
If you run the test now, you will get an *undef* error because one of the step definitions is calling maths:add/1 which we still have not provided.

    Testing testing.test.feature_addition_SUITE: Starting test, 1 test cases
    - - - - - - - - - - - - - - - - - - - - - - - - - -
    maths:add failed
    Reason: undef
    - - - - - - - - - - - - - - - - - - - - - - - - - -
    Testing testing.test.feature_addition_SUITE: *** FAILED test case 1 of 1 ***
    Testing testing.test.feature_addition_SUITE: TEST COMPLETE, 0 ok, 1 failed of 1 test cases

maths is the actual business code that we are writing tests for.

Here it is.

File: src/maths.erl

    -module(maths).

    -export([add/1]).

    add(Numbers) ->
        lists:foldl(fun(N, A) -> A+N end, 0, Numbers).

### Run tests
    make ct

And voilà! Your test passed!

    TEST INFO: 1 test(s), 1 case(s) in 1 suite(s)

    Testing testing.test.feature_addition_SUITE: Starting test, 1 test cases
    Testing testing.test.feature_addition_SUITE: TEST COMPLETE, 1 ok, 0 failed of 1 test cases

### Iterate
And now the fun starts:
- Add more tests
- Add more step definitions
- Add more business logic
- Test, implement, modify, refactor until all tests pass

## Reference guide: writing tests

### Rules
Write your tests using the [Gherkin syntax](https://docs.cucumber.io/gherkin/reference/).

ecucumber will automatically process Gherkin files placed in the features/ sub-directory of your project with the .feature extension.

ecucumber will generate a Common Test Erlang test module for each .feature file so lowercase filenames are highly recommended. 

ecucumber only supports english Gherkin keywords for the moment. This is a limitation of egherkin, the Gherkin parser used by ecucumber.

### Step definitions
Testing code must be placed in step definitions Erlang modules.

Step definitions modules must implement the following callbacks:

    setup(Context) -> Context

Initialization code, called before each scenario or once at the very beginning depending on where the activation tag was placed.
    
    cleanup(Context) -> Context
    
Cleanup code, called after each scenario or once at the very end depending on where the activation tag was placed.

    step_def(GWT, StepParts, Context) -> Context | nomatch

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
Step definitions are associated with tests using *@mod:\<module\>* Gherkin tags.

Example: the following tag will activate evaluation of step definitions in the maths_step_defs Erlang module

    @mod:maths_step_defs

### Step definitions evaluation

All step definitions activated by a *@mod:\<module\>* Gherkin tag are evaluated by calling the *\<module\>:step_def/3* callback until one step definitions module returns a new context instead of nomatch.

The order of evaluation is the following:
 - Scenario tags in order of declaration
 - Feature tags in order of
   declaration

Example:

    @mod:common @mod:extras
    Feature: Addition

    @mod:addition @mod:calculator
    Scenario: Add two numbers
        Given I have entered 50 into the calculator
        And I have entered 70 into the calculator
        When I press add
        Then the result should be 120 on the screen

Each step of the above scenario will trigger evaluation of the following step definitions modules, in this order:
- addition
- calculator
- common
- extras

## ecucumber and Git

ecucumber generates Common Test suites in the test/ directory.

You should have Git ignore these files. 

Add the following to your .gitignore file:

    test/feature_*.erl

## Next steps
    * Adding support for rebar
    * Adding support for eunit

Please contribute!

## How to contribute
You can contribute by using ecucumber.

You can contribute by providing feedback through tickets.

You can contribute by submitting code and then it's business as usual:
- fork the project’s repository on GitHub by clicking on the Fork button.
- clone your forked repository locally
- create your local branch based on master
- modify/extend the code and provide tests (untested changes will not be merged)
- run all tests (commits with failing tests will not be merged)

    make ct

- add your changes, commit and push
- submit the pull request using the GitHub interface with an explanatory message

## Compatibility

ecucumber was developed and tested with **Erlang/OTP R16B03-1** on Windows 10.

The code only uses basic Erlang syntax and standard modules. It should work on all platforms and Erlang versions.

Please report any compatibility issue you encounter by opening a ticket.
