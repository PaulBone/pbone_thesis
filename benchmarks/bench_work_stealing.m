:- module bench_work_stealing.

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

:- type group
    --->    control
    ;       test.

:- func config = config_data(group).

config = Data :-
    Data = config_data(
        Compilers,
        mem_limit,
        base_mcflags,
        Groups,
        programs
    ),
    Compilers = [
        compiler("orig-ws",     BaseDir ++ "/old_2011-04.install"),
        compiler("revised-ws",  BaseDir ++ "/old_2011-06.install")
        %compiler("current",     BaseDir ++ "/par_rts.install")
    ],
    BaseDir = "/srv/scratch/dev",
    Groups = [
        test_group(control,
            control_group_grades,
            control_group_rtopts,
            [gc_initial_heap_size],
            [1]),
        test_group(test,
            test_group_grades,
            test_group_rtopts,
            [gc_initial_heap_size],
            [1])
    ].

    % The GC's initial heap size, in bytes.  (512MB)
    %
:- func gc_initial_heap_size = int.

gc_initial_heap_size = 512*1024*1024.

    % A limit to the amount of memory available for a process, in kilobytes.
    % XXX This doesn't work, I don't know how to setrlimit in python.
:- func mem_limit = int.

mem_limit = 1024*1024.

:- func mercury_engines = list(int).

mercury_engines = 1..4.

    % Values for --max-contexts-per-thread
    %
:- func num_max_contexts_per_thread = list(int).

num_max_contexts_per_thread = [32].

% XXX: this was unused in my testing so far.
:- func base_mcflags = string.

base_mcflags="-O2 --intermodule-optimization".

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
    condense(map(
        (func(P) =
            map(
                (func(C) = rtopts(format("P%d-c%d", [i(P), i(C)]),
                    format("-P %d --max-contexts-per-thread %d", [i(P), i(C/P)]))),
                num_max_contexts_per_thread)),
        mercury_engines)).

    % The programs to test.
:- func programs = list(program(group)).

programs = [
    program("mandelbrot_indep", "mandelbrot", "mandelbrot",
        "-x 600 -y 600",
        mandelbrot_args),
    program("mandelbrot_indep_left", "mandelbrot", "mandelbrot",
        "-l -x 600 -y 600",
        mandelbrot_args),
    program("fibs_43_gc40", "fibs", "fibs",
        "-d 43 43", fibs_args),
    program("fibs_43_gc30", "fibs", "fibs",
        "-d 30 43", fibs_args),
    program("fibs_43_gc20", "fibs", "fibs",
        "-d 20 43", fibs_args),
    program("fibs_43_gc10", "fibs", "fibs",
        "-d 10 43", fibs_args),
    program("fibs_43_gc0", "fibs", "fibs",
        "43", fibs_args)
    ].

:- func mandelbrot_args(group) = string.

mandelbrot_args(test) = "".
mandelbrot_args(control) = "-s".

:- func fibs_args(group) = string.

fibs_args(test) = "--parallel".
fibs_args(control) = "--no-parallel".

