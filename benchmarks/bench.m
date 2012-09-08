% vim: ts=4 sw=4 et

:- module bench.

:- interface.

:- import_module int.
:- import_module io.
:- import_module list.
:- import_module string.

:- typeclass group(G) where [
        func grp_string(G) = string
    ].

:- type config_data(G)
    --->    config_data(
                cd_compilers                    :: list(compiler),
                cd_mem_limit                    :: int,
                cd_base_mcflags                 :: string,
                cd_test_groups                  :: list(test_group(G)),
                cd_programs                     :: list(program(G))
    ).

:- type compiler
    --->    compiler(
                c_name                          :: string,
                c_path                          :: string
            ).

:- type test_group(G)
    --->    test_group(
                tg_group                        :: G,
                tg_grades                       :: list(grade_spec),
                tg_rtsopts                      :: list(rtopts_spec),
                tg_gc_initial_heap_size         :: list(int),
                tg_gc_markers                   :: list(int)
    ).

:- type grade_spec
    --->    grade(
                g_name      :: string,
                g_str       :: string
            ).

:- type rtopts_spec
    --->    rtopts(
                rto_name    :: string,
                rto_str     :: string
            ).

:- type program(G)
    --->    program(
                p_name          :: string,
                p_dir           :: string,
                p_binary        :: string,
                p_args          :: string,
                p_extra_args    :: func(G) = string
            ).

:- pred bench(config_data(G)::in, io::di, io::uo) is det
    <= group(G).

    % Do not provide any extra args and accept all grades.
    %
:- func no_args(T) = string.

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module cord.
:- import_module float.
:- import_module getopt.
:- import_module maybe.
:- import_module require.

:- import_module checkpoint.
:- import_module result.
:- import_module foreign.

:- type option
    --->    samples
    ;       help
    ;       rebuild
    ;       print_config
    ;       output
    ;       checkpoint_file.

bench(ConfigData, !IO) :-
    io.command_line_arguments(Args0, !IO),
    OptionOps = option_ops_multi(short, long, defaults),
    getopt.process_options(OptionOps, Args0, Args, Result),
    (
        Args = [],
        (
            Result = ok(Options),
            lookup_bool_option(Options, help, Help),
            (
                Help = yes,
                usage(!IO)
            ;
                Help = no,
                run(Options, ConfigData, !IO)
            )
        ;
            Result = error(Error),
            io.write_string(Error, !IO),
            usage(!IO),
            set_exit_status(1, !IO)
        )
    ;
        Args = [_ | _],
        io.write_string("Extra arguments on command line.\n", !IO),
        usage(!IO),
        set_exit_status(1, !IO)
    ).

:- pred short(char::in, option::out) is semidet.

short('h', help).
short('n', samples).
short('r', rebuild).
short('c', print_config).
short('o', output).

:- pred long(string::in, option::out) is semidet.

long("help",            help).
long("samples",         samples).
long("rebuild",         rebuild).
long("print-config",    print_config).
long("checkpoint-file", checkpoint_file).

:- pred defaults(option::out, option_data::out) is multi.

defaults(help,              bool(no)).
defaults(samples,           int(20)).
defaults(rebuild,           bool(no)).
defaults(print_config,      bool(no)).
defaults(output,            string("results.txt")).
defaults(checkpoint_file,   string("checkpoint")).

:- pred usage(io::di, io::uo) is det.

usage(!IO) :-
    write_string("./bench <-h | --help>\n", !IO),
    write_string("./bench <-c | --print-config>\n", !IO),
    write_string("./bench [-r -n N -o results.txt]\n", !IO),
    write_string("\t[--checkpoint-file <checkpoint>]\n", !IO).

:- pred run(option_table(option)::in, config_data(G)::in, io::di, io::uo)
    is det <= group(G).

run(Options, ConfigData, !IO) :-
    lookup_bool_option(Options, print_config, PrintConfig),
    lookup_bool_option(Options, rebuild, Rebuild),
    lookup_int_option(Options, samples, Samples),
    lookup_string_option(Options, output, Output),
    lookup_string_option(Options, checkpoint_file, CheckpointFile),

    configure(Samples, ConfigData, Config),

    (
        PrintConfig = yes,
        print_config(Config, Result),
        write_string(append_list(list(Result)), !IO)
    ;
        PrintConfig = no
    ),
    (
        Rebuild = yes,
        rebuild(Config, !IO)
    ;
        Rebuild = no
    ),
    ( Samples > 0 ->
        run_tests(Output, CheckpointFile, Config, _Results, !IO)
    ;
        true
    ).

% Rebuild binaries.
%------------------------------------------------------------------------%

    % A build describes a single compilation of a test,
    %
:- type build
    --->    build(
                    % The place where the executable will be copied to
                    % once built.
                b_target    :: string,

                    % The directory containing the sources.
                b_dir       :: string,

                    % The test of the Mmake.params file used to
                    % configure the build.
                b_params    :: string,

                    % The path to mmc (not including mmc).
                b_mercury_path  :: string,

                    % The binary that mmake will generate.
                b_binary    :: string
            ).

    % Rebuild all the tests.
    %
:- pred rebuild(config(G)::in, io::di, io::uo) is det.

rebuild(config(_, ConfigData, BuildConfigs, _), !IO) :-
    getenv("PATH", OrigPath, !IO),
    write_string("Rebuilding\n", !IO),
    flush_output(!IO),
    Programs = ConfigData ^ cd_programs,
    Builds = condense(map((func(P) =
            map(make_build(P), BuildConfigs)),
        Programs)),
    foldl(rebuild_prog(OrigPath), Builds, !IO),
    write_string("Done rebuilding\n", !IO),
    flush_output(!IO).

    % For this program and this build configuration, create a build
    % object that describes how to build it.
    %
:- func make_build(program(G), build_config) = build.

make_build(Program, build_config(Group, Compiler, Grade, MCFlags)) =
        build(Target, Dir, Params, Path, Binary) :-
    Target = get_target(Group, Compiler, Grade, Program),
    Path = format("%s/bin", [s(Compiler ^ c_path)]),
    Dir = Program ^ p_dir,
    Params = format("GRADE = %s\nMCFLAGS = %s\n",
        [s(GradeStr), s(MCFlags)]),
    GradeStr = Grade ^ g_str,
    Binary = Program ^ p_binary.

    % Generate a target name.
    %
:- func get_target(string, compiler, grade_spec, program(G)) = string.

get_target(Group, Compiler, Grade, Program) = Target :-
    Target = format("%s_%s_%s_%s", [s(Group), s(ProgName), s(CompilerName), 
        s(GradeName)]),
    CompilerName = Compiler ^ c_name,
    ProgName = Program ^ p_name,
    GradeName = Grade ^ g_name.

    % Build the build described,
    %
:- pred rebuild_prog(string::in, build::in, io::di, io::uo) is det.

rebuild_prog(OrigPath, Build, !IO) :-
    Build = build(Target, Dir, Params, Path, Binary),
    format("adding %s to path\n", [s(Path)], !IO),
    setenv("PATH", Path ++ ":" ++ OrigPath, !IO),
    system("rm -rf tempdir", !IO),
    system(format("cp -a %s tempdir", [s(Dir)]), !IO),
    cd("tempdir", !IO),
    write_file("Mmake.params", Params, !IO),
    system("mmake depend", !IO),
    system("mmake all", !IO),
    cd("..", !IO),
    rename("tempdir/" ++ Binary, Target, Res, !IO),
    check_error($module, $pred, Res, !IO).

% Run tests.
%------------------------------------------------------------------------%

    % A test describes the data needed to execute a single test.
    %
:- type test
    --->    test(
                    % The name of the test,
                    % this is generated from the target name and the
                    % runtime options.
                t_name      :: string,

                    % The command to run the test.
                t_cmd       :: string,

                    % The name of the eventlog file (for threadscope) if
                    % there is one.
                t_eventlog  :: string,

                    % Environments to set.
                t_envs      :: envs_spec
            ).

    % A set of environment variable settings.
    %
:- type envs_spec
    --->    envs_spec(
                evs_name    :: string,
                evs_pairs   :: list(env_spec)
            ).

:- type env_spec
    --->    env_spec(
                es_name     :: string,
                es_value    :: string
            ).

    % Generate all the tests.
    %
    % Generate all the above test objects for all the programs.
    %
:- pred make_tests(config(G)::in, list(test)::out) is det
    <= group(G).

make_tests(config(_, ConfigData, _, Configs), Tests) :-
    Programs = ConfigData ^ cd_programs,
    Tests = condense(map((func(P) =
            map(make_test(P), Configs)
        ),
        Programs)).

    % Generate a single test.
    %
:- func make_test(program(G), test_config(G)) = test
    <= group(G).

make_test(Program, Config) =
        test(Name, Cmd, Eventlog, Envs) :-
    Config = test_config(Group, Compiler, Grade, Envs),
    Target = get_target(grp_string(Group), Compiler, Grade, Program),
    EnvOptsName = Envs ^ evs_name,
    Name = format("%s_%s", [s(Target), s(EnvOptsName)]),
    ProgArgs = Program ^ p_args,
    GetArgs = Program ^ p_extra_args,
    TestProgArgs = GetArgs(Group),
    Cmd = format("./%s %s %s", [s(Target), s(TestProgArgs), s(ProgArgs)]),
    Eventlog = format("%s.eventlog", [s(Target)]).

:- type tests_state
    --->    tests_state(
                ts_tests_not_ran    :: list(test),
                ts_results_so_far   :: cord(result.result)
            ).

    % Run all the tests,
    %
    % A log is written to the stream given in the second argument,
    % Results are also returned.
    %
:- pred run_tests(string::in, string::in, config(G)::in,
    cord(result.result)::out, io::di, io::uo) is det <= group(G).

run_tests(OutputFile, CheckpointFile, Config, Results, !IO) :-
    checkpoint.open_log(CheckpointFile, MaybeCheckpoint, !IO),
    (
        (
            MaybeCheckpoint = open_new_log(ChkLog),
            make_tests(Config, Tests),
            Results0 = cord.empty,
            format("Running %d tests...\n", [i(length(Tests))], !IO),
            flush_output(!IO)
        ;
            MaybeCheckpoint = open_existing_log(ChkLog, State),
            State = tests_state(Tests, Results0),
            format("Resuming benchmarks using checkpoint, %d tests to finish\n",
                [i(length(Tests))], !IO)
        ),

        open_output(OutputFile, MaybeStream, !IO),
        (
            MaybeStream = ok(Stream),

            ( not is_empty(Results0) ->
                print_results(Results0, ResultsCord0),
                ResultsStr = append_list(list(ResultsCord0)),
                write_string(Stream, ResultsStr, !IO),
                flush_output(Stream, !IO)
            ;
                true
            ),

            run_tests_loop(Config, Stream, Tests, ChkLog, Results0, Results,
                !IO),
            close_output(Stream, !IO),
            close_and_delete_log(ChkLog, !IO),
            write_string("Finished running tests\n", !IO),
            flush_output(!IO)
        ;
            MaybeStream = error(Error),
            error(format("%s: %s", [s(OutputFile), s(error_message(Error))]))
        )
    ;
        MaybeCheckpoint = open_error(Error),
        error(format("Checkpoint error: %s\n", [s(Error)]))
    ;
        MaybeCheckpoint = open_io_error(Error),
        error(format("%s: %s",
            [s(CheckpointFile), s(error_message(Error))]))
    ).

:- pred run_tests_loop(config(C)::in, output_stream::in, list(test)::in,
    checkpoint_log(tests_state)::in, 
    cord(result.result)::in, cord(result.result)::out, io::di, io::uo)
    is det.

run_tests_loop(_, _, [], _, !Results, !IO).
run_tests_loop(Config, Stream, [Test | Tests], ChkLog, !Results, !IO) :-
    run_test(Config, Stream, Test, ResultsTest, !IO),
    !:Results = !.Results ++ ResultsTest,
    checkpoint(tests_state(Tests, !.Results), ChkLog, !IO),
    run_tests_loop(Config, Stream, Tests, ChkLog, !Results, !IO).

    % run_test(Samples, Stream, Test, Results, !IO)
    %
    % Run a single test. Samples gives the number of times to repeat the
    % test, a item is Results is generated for each sample.
    %
:- pred run_test(config(G)::in, output_stream::in, test::in,
    cord(result.result)::out, io::di, io::uo) is det.

run_test(Config, Stream, Test, Results, !IO) :-
    Samples = Config ^ c_samples,
    Test = test(Name, Cmd, Eventlog, Environment),
    EvPairs = Environment ^ evs_pairs,
    foldl(setenv, EvPairs, !IO),
    format("Running %s\n", [s(Name)], !IO),
    run_test_samples(Name, Samples, Cmd, Eventlog, Times, !IO),
    print_results(Results, Cord),
    ResultsStr = append_list(list(Cord)),
    write_string(ResultsStr, !IO),
    write_string(Stream, ResultsStr, !IO),
    flush_output(Stream, !IO),
    Results = cord.from_list(map((func(Time) = result(Name, Time)), Times)).

:- pred setenv(env_spec::in, io::di, io::uo) is det.

setenv(env_spec(Name, Value), !IO) :-
    setenv(Name, Value, !IO).

    % This is the loop that run_tests uses.
    %
:- pred run_test_samples(string::in, int::in, string::in, string::in,
    list(float)::out, io::di, io::uo) is det.

run_test_samples(Name, Samples, Cmd, Eventlog, Times, !IO) :-
    ( Samples > 0 ->
        run_test_samples(Name, Samples - 1, Cmd, Eventlog, Times0, !IO),
        promise_equivalent_solutions [Time, !:IO] (
            time((pred(IO0::di, IO::uo) is det :-
                    system(Cmd, IO0, IO)
                ), Time, !IO)
        ),
        Times = [Time | Times0],
        rename(Eventlog, format("%s_%d.eventlog", [s(Name), i(Samples)]),
            _Res, !IO)
        % Ignore the error as there may not be an eventlog.
        % check_error($module, $pred, Res, !IO)
    ;
        Times = []
    ).

% Configuration
%------------------------------------------------------------------------%

    % The complete configuration.
    %
:- type config(G)
    --->    config(
                c_samples       :: int,
                c_data          :: config_data(G),
                c_builds        :: list(build_config),
                c_tests         :: list(test_config(G))
            ).

    % A build_config describes one way of building a program.
    % It is not program specific.
    %
:- type build_config
    --->    build_config(
                bc_group        :: string,
                bc_compiler     :: compiler,
                bc_grade        :: grade_spec,
                bc_mcflags      :: string
            ).

    % A test_config descirbes one way of running a test.
    %
:- type test_config(Group)
    --->    test_config(
                tc_group        :: Group,
                tc_compiler     :: compiler,
                tc_grade        :: grade_spec,

                    % Runtime/environment options.
                tc_envs         :: envs_spec
            ).

    % Do configuration,
    %
    % This generates the build and test data structures.
    %
:- pred configure(int::in, config_data(G)::in, config(G)::out) is det
    <= group(G).

configure(Samples, ConfigData, config(Samples, ConfigData, Builds, Tests)) :-
    Groups = ConfigData ^ cd_test_groups,
    BaseMCFlags = ConfigData ^ cd_base_mcflags,
    Compilers = ConfigData ^ cd_compilers,
    map2(configure_group(BaseMCFlags, Compilers), 
        Groups, GroupBuilds, GroupTests),
    Builds = condense(GroupBuilds),
    Tests = condense(GroupTests).

:- pred configure_group(string::in, list(compiler)::in, test_group(G)::in,
    list(build_config)::out, list(test_config(G))::out) is det
    <= group(G).

configure_group(BaseMCFlags, Compilers, TestGroup, Builds, Tests) :-
    TestGroup = test_group(Group, Grades, Rtsopts, GCInitialHeapSizes,
        GCMarkerss),
    config_builds(grp_string(Group), Compilers, Grades, BaseMCFlags, Builds),
    config_tests(Group, Compilers, Grades, GCMarkerss, GCInitialHeapSizes,
        Rtsopts, Tests).

:- pred config_builds(string::in, list(compiler)::in, list(grade_spec)::in,
    string::in, list(build_config)::out) is det.

config_builds(Group, Compilers, Grades, MCFlags, Configs) :-
    Configs = condense(map((func(C) = 
            map((func(G) =
                build_config(Group, C, G, MCFlags)
            ),
            Grades)),
        Compilers)).

:- pred config_tests(G::in, list(compiler)::in, list(grade_spec)::in,
    list(int)::in, list(int)::in, list(rtopts_spec)::in,
    list(test_config(G))::out) is det.

config_tests(Group, Compilers, Grades, GCMarkerss, GCInitialHeapSizes, RTOpts,
        Configs) :-
    Environments = condense(map((func(RTO) =
        condense(map((func(GCMarkers) =
            map((func(GCInitialHeapSize) = Environment :-
                Name = format("%s_gc-m%d_gc-sz%d",
                    [s(RTO ^ rto_name), i(GCMarkers),
                        i(GCInitialHeapSize / (1024*1024))]),
                Environment = envs_spec(Name,
                    [
                     env_spec("MERCURY_OPTIONS", RTO ^ rto_str),
                     env_spec("GC_MARKERS",      from_int(GCMarkers)),
                     env_spec("GC_INITIAL_HEAP_SIZE",
                                                 from_int(GCInitialHeapSize))
                    ])
                ),
                GCInitialHeapSizes)
            ),
            GCMarkerss))
        ),
        RTOpts)),
    Configs = condense(map((func(C) = 
        condense(map((func(G) =
            map((func(Ev) =
                    test_config(Group, C, G, Ev)
                ),
                Environments)
            ),
            Grades))
        ),
        Compilers)).

:- pred print_config(config(G)::in, cord(string)::out) is det
    <= group(G).

print_config(config(Samples, ConfigData, Builds, Tests), Result) :-
    map(print_build, Builds, BuildsRes),
    map(print_test, Tests, TestsRes),
    Programs = ConfigData ^ cd_programs,
    map(print_program, Programs, ProgsRes),
    LN = singleton("\n"),
    Result =
        singleton("Configuration:\n") ++
        LN ++
        singleton(format("\tSamples: %d\n", [i(Samples)])) ++
        LN ++
        singleton("\tBuilds:\n") ++
        cord_list_to_cord(map(compose(indent(2), line), BuildsRes)) ++
        LN ++
        singleton("\tTests:\n") ++
        cord_list_to_cord(map(compose(indent(2), line), TestsRes)) ++
        LN ++
        singleton("\tPrograms:\n") ++
        cord_list_to_cord(map(compose(indent(2), line), ProgsRes)).

:- pred print_build(build_config::in, cord(string)::out) is det.

print_build(build_config(Group, Compiler, Grade, _), Result) :-
    Result = singleton(
        format("%s, Compiler: %s Grade: %s",
            [s(Group), s(Compiler ^ c_name), s(Grade ^ g_name)])).

:- pred print_test(test_config(G)::in, cord(string)::out) is det
    <= group(G).

print_test(test_config(Group, Compiler, Grade, Env), Result) :-
    Result = singleton(
        format("%s, Compiler %s Grade: %s Env: %s",
            [s(grp_string(Group)), s(Compiler ^ c_name), s(Grade ^ g_name),
             s(Env ^ evs_name)])).

:- func indent(int, cord(string)) = cord(string).

indent(N, S0) =
    ( N < 0 ->
        unexpected($module, $pred, "N < 0")
    ; N = 0 ->
        S0
    ;
        indent(N - 1, singleton("\t") ++ S0)
    ).

:- func line(cord(string)) = cord(string).

line(S) = S ++ singleton("\n").

:- func compose(func(B) = C, func(A) = B, A) = C.

compose(F, G, X) = F(G(X)).

% Programs.
%------------------------------------------------------------------------%

:- pred print_program(program(G)::in, cord(string)::out) is det.

print_program(Program, singleton(Name)) :-
    Name = Program ^ p_name.

no_args(_) = "".

