
:- module bench_gc_extra.

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
        "/usr/local/mercury/bin",
        mem_limit,
        base_mcflags,
        Groups,
        programs
    ),
    Groups = [
        test_group("control2",
            control_group_grades,
            control_group_rtopts,
            gc_initial_heap_size,
            gc_markers)
        ].

    % The GC's initial heap size, in bytes. 
    %
:- func gc_initial_heap_size = list(int).

gc_initial_heap_size = map((func(X) = X*1024*1024), L) :-
    L = [1, 16, 32,
         64, 128, 256, 384, 512].

    % Use only one marker thread for GC regardless of grade/test.
    %
:- func gc_markers = list(int).

gc_markers=1..4.

    % A limit to the amount of memory available for a process, in kilobytes.
    % XXX This doesn't work, I don't know how to setrlimit in python.
:- func mem_limit = int.

mem_limit = 1024*1024.

:- func mercury_engines = list(int).

mercury_engines = 1..4.

:- func base_mcflags = string.

base_mcflags="-O2 --intermodule-optimization --par-loop-control".

:- func control_group_grades = list(grade_spec).

control_group_grades=[asmfast,
                      asmfastpar].

:- func control_group_rtopts = list(rtopts_spec).

control_group_rtopts=[rtopts("P1", "-P 1")].

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
    map((func(P) = rtopts(format("P%d", [i(P)]),
            format("-P %d", [i(P)]))),
        mercury_engines).

    % The programs to test.
:- func programs = list(program).

programs = [
    program("mandelbrot_heap", "mandelbrot_heap", "mandelbrot",
        "-x 600 -y 600",
        mandelbrot_args),
    program("raytracer", "raytracer", "main",
        " < raytracer/snowgoon_1200.gml",
        no_args)
    ].

:- func mandelbrot_args(string) = string.

mandelbrot_args(Group) = Args :-
    ( Group = "test" ->
        Args = ""
    ; Group = "control2" ->
        Args = "-s"
    ;
        unexpected($module, $pred, "Unknown group")
    ).

