% vim: ts=4 sw=4 et

:- module bench.

:- interface.

:- import_module int.
:- import_module io.
:- import_module list.
:- import_module string.

:- type config_data
    --->    config_data(
                cd_mercury_path                 :: string,
                cd_mem_limit                    :: int,
                cd_base_mcflags                 :: string,
                cd_test_groups                  :: list(test_group),
                cd_programs                     :: list(program)
    ).

:- type test_group
    --->    test_group(
                tg_group                        :: string,
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

:- type program
    --->    program(
                p_name          :: string,
                p_dir           :: string,
                p_binary        :: string,
                p_args          :: string,
                p_extra_args    :: func(string) = string
            ).

:- pred bench(config_data::in, io::di, io::uo) is det.

    % Do not provide any extra args and accept all grades.
    %
:- func no_args(string) = string.

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module cord.
:- import_module float.
:- import_module getopt.
:- import_module maybe.
:- import_module require.

:- import_module result.

:- type option
    --->    samples
    ;       help
    ;       rebuild
    ;       print_config
    ;       output.

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

:- pred defaults(option::out, option_data::out) is multi.

defaults(help,          bool(no)).
defaults(samples,       int(20)).
defaults(rebuild,       bool(no)).
defaults(print_config,  bool(no)).
defaults(output,        string("results.txt")).

:- pred usage(io::di, io::uo) is det.

usage(!IO) :-
    write_string("./bench <-h | --help>\n", !IO),
    write_string("./bench <-c | --print-config>\n", !IO),
    write_string("./bench [-r -n N -o results.txt] \n", !IO).

:- pred run(option_table(option)::in, config_data::in, io::di, io::uo) is det.

run(Options, ConfigData, !IO) :-
    lookup_bool_option(Options, print_config, PrintConfig),
    lookup_bool_option(Options, rebuild, Rebuild),
    lookup_int_option(Options, samples, Samples),
    lookup_string_option(Options, output, Output),

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
        open_output(Output, MaybeStream, !IO),
        (
            MaybeStream = ok(Stream),
            run_tests(Config, Stream, _Results, !IO),
            close_output(Stream, !IO)
            %print_results(Results, ResultsCord),
            %write_string(append_list(list(ResultsCord)), !IO)
        ;
            MaybeStream = error(Error),
            error(format("%s: %s", [s(Output), s(error_message(Error))]))
        )
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

                    % The binary that mmake will generate.
                b_binary    :: string
            ).

    % Rebuild all the tests.
    %
:- pred rebuild(config::in, io::di, io::uo) is det.

rebuild(config(_, ConfigData, BuildConfigs, _), !IO) :-
    getenv("PATH", PATH0, !IO),
    MercuryPath = ConfigData ^ cd_mercury_path,
    PATH = MercuryPath ++ ":" ++ PATH0,
    setenv("PATH", PATH, !IO),
    write_string("Rebuilding\n", !IO),
    flush_output(!IO),
    Programs = ConfigData ^ cd_programs,
    Builds = condense(map((func(P) =
            map(make_build(P), BuildConfigs)),
        Programs)),
    foldl(rebuild_prog, Builds, !IO),
    write_string("Done rebuilding\n", !IO),
    flush_output(!IO).

    % For this program and this build configuration, create a build
    % object that describes how to build it.
    %
:- func make_build(program, build_config) = build.

make_build(Program, build_config(Group, Grade, MCFlags)) =
        build(Target, Dir, Params, Binary) :-
    Target = get_target(Group, Grade, Program),
    Dir = Program ^ p_dir,
    Params = format("GRADE = %s\nMCFLAGS = %s\n",
        [s(GradeStr), s(MCFlags)]),
    GradeStr = Grade ^ g_str,
    Binary = Program ^ p_binary.

    % Generate a target name.
    %
:- func get_target(string, grade_spec, program) = string.

get_target(Group, Grade, Program) =
        format("%s_%s_%s", [s(Group), s(ProgName), s(GradeName)]) :-
    ProgName = Program ^ p_name,
    GradeName = Grade ^ g_name.

    % Build the build described,
    %
:- pred rebuild_prog(build::in, io::di, io::uo) is det.

rebuild_prog(Build, !IO) :-
    Build = build(Target, Dir, Params, Binary),
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
:- pred make_tests(config::in, list(test)::out) is det.

make_tests(config(_, ConfigData, _, Configs), Tests) :-
    Programs = ConfigData ^ cd_programs,
    Tests = condense(map((func(P) =
            map(make_test(P), Configs)
        ),
        Programs)).

    % Generate a single test.
    %
:- func make_test(program, test_config) = test.

make_test(Program, Config) =
        test(Name, Cmd, Eventlog, Envs) :-
    Config = test_config(Group, Grade, Envs),
    Target = get_target(Group, Grade, Program),
    EnvOptsName = Envs ^ evs_name,
    Name = format("%s_%s", [s(Target), s(EnvOptsName)]),
    ProgArgs = Program ^ p_args,
    GetArgs = Program ^ p_extra_args,
    TestProgArgs = GetArgs(Group),
    Cmd = format("./%s %s %s", [s(Target), s(TestProgArgs), s(ProgArgs)]),
    Eventlog = format("%s.eventlog", [s(Target)]).

    % Run all the tests,
    %
    % A log is written to the stream given in the second argument,
    % Results are also returned.
    %
:- pred run_tests(config::in, output_stream::in, cord(result.result)::out,
    io::di, io::uo) is det.

run_tests(Config, Stream, Results, !IO) :-
    write_string("Running tests...\n", !IO),
    flush_output(!IO),
    make_tests(Config, Tests),
    list.map_foldl(run_test(Config, Stream), Tests, Results0, !IO),
    Results = cord_list_to_cord(Results0),
    write_string("Finished running tests\n", !IO),
    flush_output(!IO).

    % run_test(Samples, Stream, Test, Results, !IO)
    %
    % Run a single test. Samples gives the number of times to repeat the
    % test, a item is Results is generated for each sample.
    %
:- pred run_test(config::in, output_stream::in, test::in,
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
:- type config
    --->    config(
                c_samples       :: int,
                c_data          :: config_data,
                c_builds        :: list(build_config),
                c_tests         :: list(test_config)
            ).

    % A build_config describes one way of building a program.
    % It is not program specific.
    %
:- type build_config
    --->    build_config(
                bc_group        :: string,
                bc_grade        :: grade_spec,
                bc_mcflags      :: string
            ).

    % A test_config descirbes one way of running a test.
    %
:- type test_config
    --->    test_config(
                tc_group        :: string,
                tc_grade        :: grade_spec,

                    % Runtime/environment options.
                tc_envs         :: envs_spec
            ).

    % Do configuration,
    %
    % This generates the build and test data structures.
    %
:- pred configure(int::in, config_data::in, config::out) is det.

configure(Samples, ConfigData, config(Samples, ConfigData, Builds, Tests)) :-
    Groups = ConfigData ^ cd_test_groups,
    MCFlags = ConfigData ^ cd_base_mcflags,
    map2(configure_group(MCFlags), Groups, GroupBuilds, GroupTests),
    Builds = condense(GroupBuilds),
    Tests = condense(GroupTests).

:- pred configure_group(string::in, test_group::in,
    list(build_config)::out, list(test_config)::out) is det.

configure_group(MCFlags, Group, Builds, Tests) :-
    Group = test_group(Name, Grades, Rtsopts, GCInitialHeapSizes, GCMarkerss),
    config_builds(Name, Grades, MCFlags, Builds),
    config_tests(Name, Grades, GCMarkerss, GCInitialHeapSizes,
        Rtsopts, Tests).

:- pred config_builds(string::in, list(grade_spec)::in, string::in,
    list(build_config)::out) is det.

config_builds(Group, Grades, MCFlags, Configs) :-
    Configs = map((func(G) =
            build_config(Group, G, MCFlags)
        ),
        Grades).

:- pred config_tests(string::in, list(grade_spec)::in,
    list(int)::in, list(int)::in, list(rtopts_spec)::in,
    list(test_config)::out) is det.

config_tests(Group, Grades, GCMarkerss, GCInitialHeapSizes, RTOpts,
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
    Configs = condense(map((func(G) =
        map((func(Ev) =
                test_config(Group, G, Ev)
            ),
            Environments)
        ),
        Grades)).

:- pred print_config(config::in, cord(string)::out) is det.

print_config(config(Samples, ConfigData, Builds, Tests), Result) :-
    map(print_build, Builds, BuildsRes),
    map(print_test, Tests, TestsRes),
    MercuryPath = ConfigData ^ cd_mercury_path,
    Programs = ConfigData ^ cd_programs,
    map(print_program, Programs, ProgsRes),
    LN = singleton("\n"),
    Result =
        singleton("Configuration:\n") ++
        LN ++
        singleton(format("\tSamples: %d\n", [i(Samples)])) ++
        singleton(format("\tPath: %s\n", [s(MercuryPath)])) ++
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

print_build(build_config(Group, Grade, _), Result) :-
    Result = singleton(
        format("%s, Grade: %s",
            [s(Group), s(Grade ^ g_name)])).

:- pred print_test(test_config::in, cord(string)::out) is det.

print_test(test_config(Group, Grade, Env), Result) :-
    Result = singleton(
        format("%s, Grade: %s Env: %s",
            [s(Group), s(Grade ^ g_name), s(Env ^ evs_name)])).

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

:- pred print_program(program::in, cord(string)::out) is det.

print_program(Program, singleton(Name)) :-
    Name = Program ^ p_name.

no_args(_) = "".

% Foreign code.
%------------------------------------------------------------------------%

:- pred system(string::in, io::di, io::uo) is det.

system(Cmd, !IO) :-
    call_system(Cmd, Res, !IO),
    ( Res = ok(0) ->
        true
    ;
        unexpected($module, $pred, "Command failed " ++ Cmd)
    ).

:- pred write_file(string::in, string::in, io::di, io::uo) is det.

write_file(Name, Contents, !IO) :-
    open_output(Name, Result, !IO),
    (
        Result = ok(Stream),
        write_string(Stream, Contents, !IO),
        close_output(Stream, !IO)
    ;
        Result = error(Error),
        unexpected($module, $pred, Name ++ ": " ++ error_message(Error))
    ).

:- pred cd(string::in, io::di, io::uo) is det.

cd(Dir, !IO) :-
    cd(Dir, Return, Errno, !IO),
    ( Return = 0 ->
        true
    ;
        decode_errno(Errno, Message),
        unexpected($module, $pred, "chdir: " ++ Message)
    ).

:- pred decode_errno(int::in, string::out) is det.

:- pragma foreign_proc("C",
    decode_errno(Errno::in, Message::out),
    [will_not_call_mercury, promise_pure],
    "
        int length;
        char *buff;

        buff = strerror(Errno);

        length = strlen(buff);
        Message = GC_MALLOC(length + 1);
        strcpy(Message, buff);
    ").

:- pred cd(string::in, int::out, int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    cd(Dir::in, Return::out, Errno::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
    "
        Return = chdir(Dir);
        Errno = errno;
        IO = IO0;
    ").

:- pred rename(string::in, string::in, maybe_error::out, io::di, io::uo)
    is det.

rename(Src, Dest, Res, !IO) :-
    rename_file(Src, Dest, Res0, !IO),
    (
        Res0 = ok,
        Res = ok
    ;
        Res0 = error(Error),
        Res = error(error_message(Error))
    ).

:- pred getenv(string::in, string::out, io::di, io::uo) is det.

getenv(Key, Value, !IO) :-
    getenv(Key, Res, Value, !IO),
    (
        Res = yes
    ;
        Res = no,
        unexpected($pred, $module, "getenv returned NULL for " ++ Key)
    ).

:- pred getenv(string::in, bool::out, string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    getenv(Key::in, Res::out, Value::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
    "
        Value = getenv(Key);
        if (Value == NULL)
            Res = MR_FALSE;
        else
            Res = MR_TRUE;
        IO = IO0;
    ").

:- pred setenv(string::in, string::in, io::di, io::uo) is det.

setenv(Key, Value, !IO) :-
    setenv(Key, Value, Res, Errno, !IO),
    ( Res = 0 ->
        true
    ;
        decode_errno(Errno, Message),
        unexpected($module, $pred, "setenv: " ++ Message)
    ).

:- pred setenv(string::in, string::in, int::out, int::out, io::di, io::uo)
    is det.

:- pragma foreign_proc("C",
    setenv(Key::in, Value::in, Res::out, Errno::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
    "
        Res = setenv(Key, Value, 1);
        Errno = errno;
        IO = IO0;
    ").

:- pred time(pred(io, io), float, io, io).
:- mode time(pred(di, uo) is det, out, di, uo) is cc_multi.

time(P, Time, !IO) :-
    gettimeofday(Start, !IO),
    P(!IO),
    gettimeofday(End, !IO),
    Time = End - Start.

:- pred gettimeofday(float::out, io::di, io::uo) is cc_multi.

gettimeofday(Time, !IO) :-
    gettimeofday(Secs, USecs, Return, !IO),
    ( Return = 0 ->
        Time = float(Secs) + float(USecs)/1000000.0
    ;
        error("gettimeofday failed")
    ).

:- pred gettimeofday(int::out, int::out, int::out, io::di, io::uo) is cc_multi.

:- pragma foreign_proc("C",
    gettimeofday(Secs::out, USecs::out, Return::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
    "
        struct timeval t;
        Return = gettimeofday(&t, NULL);
        Secs = t.tv_sec;
        USecs = t.tv_usec;
        IO = IO0;
    ").

:- pred check_error(string::in, string::in, maybe_error::in,
    io::di, io::uo) is det.

check_error(_, _, ok, !IO).
check_error(Module, Pred, error(Error), !IO) :-
    unexpected(Module, Pred, Error).

