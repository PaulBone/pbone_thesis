
:- module bench_nolimit.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module float.
:- import_module int.
:- import_module list.
:- import_module string.
:- import_module require.

:- import_module bench.

main(!IO) :-
    bench(config, !IO).

:- func config = config_data.

config = Data :-
    Data = config_data(
        [compiler("2009_nolimit", 
            "/srv/scratch/dev/old_2009_nolimit.install/bin")],
        mem_limit,
        base_mcflags,
        Groups,
        programs
    ),
    Groups = [
        test_group("control",
            control_group_grades,
            control_group_rtopts,
            [gc_initial_heap_size],
            [1]),
        test_group("test",
            test_group_grades,
            test_group_rtopts,
            [gc_initial_heap_size],
            [1])
        ].

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

% XXX This was unsed in testing so far.
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
    program("mandelbrot_indep_left", "mandelbrot", "mandelbrot",
        "-l -x 600 -y 600",
        mandelbrot_args)
    ].

:- func mandelbrot_args(string) = string.

mandelbrot_args(Group) = Args :-
    ( Group = "test" ->
        Args = ""
    ; Group = "control" ->
        Args = "-s"
    ;
        unexpected($module, $pred, "Unknown group")
    ).

