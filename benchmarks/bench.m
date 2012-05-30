% vim: ts=4 sw=4 et

:- module bench.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module cord.
:- import_module float.
:- import_module getopt.
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.

:- type option
    --->    samples
    ;       help
    ;       rebuild
    ;       print_config
    ;       output.

main(!IO) :-
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
                run(Options, !IO)
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
    write_string("./batch <-h | --help>\n", !IO),
    write_string("./batch <-c | --print-config>\n", !IO),
    write_string("./batch [-r -n N -o results.txt] \n", !IO).

:- pred run(option_table(option)::in, io::di, io::uo) is det.

run(Options, !IO) :-
    lookup_bool_option(Options, print_config, PrintConfig),
    lookup_bool_option(Options, rebuild, Rebuild),
    lookup_int_option(Options, samples, Samples),
    lookup_string_option(Options, output, Output),

    configure(Samples, Config),

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

rebuild(config(_, BuildConfigs, _), !IO) :-
    getenv("PATH", PATH0, !IO),
    PATH = mercury_path ++ ":" ++ PATH0,
    setenv("PATH", PATH, !IO),
    write_string("Rebuilding\n", !IO),
    flush_output(!IO),
    Builds = condense(map((func(P) =
            map(make_build(P), BuildConfigs)),
        programs)),
    foldl(rebuild_prog, Builds, !IO),
    write_string("Done rebuilding\n", !IO),
    flush_output(!IO).

    % For this program and this build configuration, create a build
    % object that describes how to build it.
    %
:- func make_build(program, build_config) = build.

make_build(Program, build_config(CorT, Grade)) =
        build(Target, Dir, Params, Binary) :-
    Target = get_target(CorT, Grade, Program),
    Dir = Program ^ p_dir,
    Params = format("GRADE = %s\n", [s(GradeStr)]),
    GradeStr = Grade ^ g_str,
    Binary = Program ^ p_binary.

    % Generate a target name.
    %
:- func get_target(control_or_test, grade_spec, program) = string.

get_target(CorT, Grade, Program) =
        format("%s_%s_%s", [s(CorTStr), s(ProgName), s(GradeName)]) :-
    c_or_t_string(CorT, CorTStr),
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
    rename("tempdir/" ++ Binary, Target, !IO).

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

                    % The options for the runtime system.
                t_rtsopts   :: string
            ).

    % Generate all the tests.
    %
    % Generate all the above test objects for all the programs.
    %
:- pred make_tests(config::in, list(test)::out) is det.

make_tests(config(_, _, Configs), Tests) :-
    Tests = condense(map((func(P) =
            map(make_test(P), Configs)
        ),
        programs)).

    % Generate a single test.
    %
:- func make_test(program, test_config) = test.

make_test(Program, Config) =
        test(Name, Cmd, RTSOptsStr) :-
    Config = test_config(CorT, Grade, RTSOpts),
    RTSOpts = rtopts(RTSOptsName, RTSOptsStr),
    Target = get_target(CorT, Grade, Program),
    Name = format("%s_%s", [s(Target), s(RTSOptsName)]), 
    ProgArgs = Program ^ p_args,
    Cmd = format("./%s %s", [s(Target), s(ProgArgs)]).

    % Run all the tests,
    %
    % A log is written to the stream given in the second argument,
    % Results are also returned.
    %
:- pred run_tests(config::in, output_stream::in, cord(bench.result)::out,
    io::di, io::uo) is det.

run_tests(Config, Stream, Results, !IO) :-
    write_string("Running tests...\n", !IO),
    flush_output(!IO),
    make_tests(Config, Tests),
    Samples = Config ^ c_samples,
    list.map_foldl(run_test(Samples, Stream), Tests, Results0, !IO),
    Results = cord_list_to_cord(Results0),
    write_string("Finished running tests\n", !IO),
    flush_output(!IO).

    % run_test(Samples, Stream, Test, Results, !IO)
    % 
    % Run a single test. Samples gives the number of times to repeat the
    % test, a item is Results is generated for each sample.
    %
:- pred run_test(int::in, output_stream::in, test::in,
    cord(bench.result)::out, io::di, io::uo) is det.

run_test(Samples, Stream, Test, Results, !IO) :-
    Test = test(Name, Cmd, RTSOpts),
    setenv("MERCURY_OPTIONS", RTSOpts, !IO),
    setenv("GC_MARKERS", string(gc_markers), !IO),
    setenv("GC_INITIAL_HEAP_SIZE", string(gc_initial_heap_size), !IO),
    format("Running %s\n", [s(Name)], !IO),
    run_test_samples(Samples, Cmd, Times, !IO),
    print_results(Results, Cord),
    ResultsStr = append_list(list(Cord)),
    write_string(ResultsStr, !IO),
    write_string(Stream, ResultsStr, !IO),
    flush_output(Stream, !IO),
    Results = cord.from_list(map((func(Time) = result(Name, Time)), Times)).

    % This is the loop that run_tests uses.
    %
:- pred run_test_samples(int::in, string::in, list(float)::out, io::di, io::uo)
    is det. 

run_test_samples(Samples, Cmd, Times, !IO) :-
    ( Samples > 0 ->
        run_test_samples(Samples - 1, Cmd, Times0, !IO),
        promise_equivalent_solutions [Time, !:IO] (
            time((pred(IO0::di, IO::uo) is det :-
                    system(Cmd, IO0, IO)
                ), Time, !IO)
        ),
        Times = [Time | Times0]
    ;
        Times = []
    ).

% Results 
%------------------------------------------------------------------------%

    % A result is the name of the test (same as t_name above),
    % and how long that test took.
    %
:- type result
    --->    result(
                r_name          :: string,
                r_time          :: float 
            ).

:- pred print_results(cord(bench.result)::in, cord(string)::out) is det.

print_results(Results, Cord) :-
    Items = map(print_result, Results),
    Cord = foldl((++), Items, init).

:- func print_result(bench.result) = cord(string).

print_result(result(Name, Time)) =
    singleton(format("%s %.2f\n", [s(Name), f(Time)])).

% Configuration
%------------------------------------------------------------------------%

    % The complete configuration.
    %
:- type config
    --->    config(
                c_samples       :: int,
                c_buildsi       :: list(build_config),
                c_tests         :: list(test_config)
            ).

    % A build_config describes one way of building a program.
    % It is not program specific.
    %
:- type build_config
    --->    build_config(
                bc_c_or_t       :: control_or_test,
                bc_grade        :: grade_spec
            ).

    % A test_config descirbes one way of running a test.
    %
:- type test_config
    --->    test_config(
                tc_c_or_t       :: control_or_test,
                tc_grade        :: grade_spec,

                    % Runtime options.
                tc_rtopts       :: rtopts_spec
            ).

    % Do configuration,
    %
    % This generates the build and test data structures.
    %
:- pred configure(int::in, config::out) is det.

configure(Samples, config(Samples, Builds, Tests)) :-
    config_builds(control, control_group_grades, BuildsC),
    config_builds(test, test_group_grades, BuildsT),
    Builds = BuildsC ++ BuildsT,
    config_tests(control, control_group_grades, control_group_rtopts, TestsC),
    config_tests(test, test_group_grades, test_group_rtopts, TestsT),
    Tests = TestsC ++ TestsT.

:- pred config_builds(control_or_test::in, list(grade_spec)::in,
    list(build_config)::out) is det.

config_builds(CorT, Grades, Configs) :-
    Configs = map((func(G) =
            build_config(CorT, G)
        ),
        Grades).

:- pred config_tests(control_or_test::in, list(grade_spec)::in,
    list(rtopts_spec)::in, list(test_config)::out) is det.

config_tests(CorT, Grades, RTOpts, Configs) :-
    Configs = condense(map((func(G) =
        map((func(RTO) =
                test_config(CorT, G, RTO)
            ),
            RTOpts)
        ),
        Grades)).

:- pred print_config(config::in, cord(string)::out) is det.

print_config(config(Samples, Builds, Tests), Result) :-
    map(print_build, Builds, BuildsRes),
    map(print_test, Tests, TestsRes),
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
        cord_list_to_cord(map(compose(indent(2), line), TestsRes)).

:- pred print_build(build_config::in, cord(string)::out) is det.

print_build(build_config(CorT, Grade), Result) :-
    c_or_t_string(CorT, CorTStr),
    Result = singleton(
        format("%s, Grade: %s",
            [s(CorTStr), s(Grade ^ g_name)])).

:- pred print_test(test_config::in, cord(string)::out) is det.

print_test(test_config(CorT, Grade, RTOpts), Result) :-
    c_or_t_string(CorT, CorTStr),
    Result = singleton(
        format("%s, Grade: %s RTOpts: %s",
            [s(CorTStr), s(Grade ^ g_name), s(RTOpts ^ rto_name)])).

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

% Data.
%------------------------------------------------------------------------%

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
                p_name      :: string,
                p_dir       :: string,
                p_binary    :: string,
                p_args      :: string
            ).

    % The path to use.
:- func mercury_path = string.

mercury_path =
    "/srv/scratch/dev/old_2009.install/bin".

    % The GC's initial heap size, in bytes.  (512MB)
    %
:- func gc_initial_heap_size = int.

gc_initial_heap_size = 512*1024*1024.

    % Use only one marker thread for GC regardless of grade/test.
    %
:- func gc_markers = int.

gc_markers=1.

    % A limit to the amount of memory available for a process, in kilobytes.
    % XXX This doesn't work, I don't know how to setrlimit in python.
:- func mem_limit = int.

mem_limit = 1024*1024.

:- func mercury_engines = list(int).

mercury_engines = 1..4.

    % Values for --max-contexts-per-thread
    %
:- func num_max_contexts_per_thread = list(int).

num_max_contexts_per_thread =
    map((func(X) = pow(2,X)), 1..8).

:- func base_mcflags = string.

base_mcflags="-O2 --intermodule-optimization".

:- func control_group_grades = list(grade_spec).

control_group_grades=[asmfast,
                      asmfastpar].

:- func control_group_rtopts = list(rtopts_spec).

control_group_rtopts=[rtopts("P1-lc2", "-P 1")].

:- func test_group_grades = list(grade_spec).

test_group_grades = [asmfastpar].

:- func asmfastpar = grade_spec.

asmfastpar = grade("asmfast.par", "asm_fast.gc.par.stseg").

:- func asmfast = grade_spec.
asmfast = grade("asmfast", "asm_fast.gc.stseg").

    % Test group options.
    %
:- func test_group_rtopts = list(rtopts_spec).

test_group_rtopts =
    condense(map(
        (func(P) =
            map(
                (func(C) = rtopts(format("P%d-c%d", [i(P), i(C)]),
                    format("-P %d --max-contexts-per-thread %d", [i(P), i(C)]))),
                num_max_contexts_per_thread)),
        mercury_engines)).

    % The programs to test.
:- func programs = list(program).

programs = [
    %program("mandelbrot_indep", "mandelbrot", "mandelbrot",
    %    "-x 600 -y 600"),
    program("mandelbrot_indep_left", "mandelbrot", "mandelbrot",
        "-l -x 600 -y 600")
    ].

%------------------------------------------------------------------------%

:- type control_or_test
    --->    control
    ;       test.

:- pred c_or_t_string(control_or_test, string).
:- mode c_or_t_string(in, out) is det.

c_or_t_string(control,  "control").
c_or_t_string(test,     "test").

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

:- pred rename(string::in, string::in, io::di, io::uo) is det.

rename(Src, Dest, !IO) :-
    rename_file(Src, Dest, Res, !IO),
    (
        Res = ok
    ;
        Res = error(Error),
        unexpected($module, $pred, error_message(Error))
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

