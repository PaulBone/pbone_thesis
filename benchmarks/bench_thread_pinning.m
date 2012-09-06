:- module bench_thread_pinning.

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
    --->    group(par, pinning).

:- type par
    --->    seq
    ;       par.

:- type pinning
    --->    no_pinning 
    ;       pinning.

:- func group_string(group) = string.

group_string(group(Par, Pinning)) =
    format("%s-%s", [s(string(Par)), s(string(Pinning))]).

:- instance group(group) where [
        func(grp_string/1) is group_string
    ].

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
        compiler("cpuaffinity", BaseDir ++ "/current.install-nohwloc"),
        compiler("hwloc",       BaseDir ++ "/current.install-hwloc")
    ],
    BaseDir = "/srv/scratch/dev",
    Groups = map((func(G) =
        test_group(G,
            test_group_grades,
            test_group_rtopts(G),
            [gc_initial_heap_size],
            [1])),
        [ group(seq, no_pinning),
          group(par, no_pinning),
          group(par, pinning) ]).

    % The GC's initial heap size, in bytes.  (512MB)
    %
:- func gc_initial_heap_size = int.

gc_initial_heap_size = 512*1024*1024.

    % A limit to the amount of memory available for a process, in kilobytes.
    % XXX This doesn't work, I don't know how to setrlimit in python.
:- func mem_limit = int.

mem_limit = 1024*1024.

:- func mercury_engines(group) = list(int).

mercury_engines(group(seq, _)) = [1].
mercury_engines(group(par, _)) = 1..8.

    % Values for --max-contexts-per-thread
    %
:- func num_max_contexts_per_thread = list(int).

num_max_contexts_per_thread = [1024].

% XXX: this was unused in my testing so far.
:- func base_mcflags = string.

base_mcflags="-O2 --intermodule-optimization".

:- func test_group_grades = list(grade_spec).

test_group_grades = [asmfastpar].

:- func asmfastpar = grade_spec.

asmfastpar = grade("asmfast.par", "asm_fast.gc.par.stseg").

:- func asmfast = grade_spec.
asmfast = grade("asmfast", "asm_fast.gc.stseg").

    % Test group options.
    %
:- func test_group_rtopts(group) = list(rtopts_spec).

test_group_rtopts(Group) = Opts :-
    C = 1024,
    Opts = map((func(P) = rtopts(format("P%d-c%d", [i(P), i(C)]),
            format("-P %d --max-contexts-per-thread %d %s",
                [i(P), i(C/P), s(pinning_rtsopts(Group))]))),
        mercury_engines(Group)).

:- func pinning_rtsopts(group) = string.

pinning_rtsopts(group(_, no_pinning)) = "".
pinning_rtsopts(group(_, pinning)) = "--thread-pinning".

    % The programs to test.
:- func programs = list(program(group)).

programs = [
    program("raytracer", "raytracer", "main",
        " < raytracer/snowgoon_1200.gml",
        no_args),
    program("mandelbrot_indep", "mandelbrot", "mandelbrot",
        "-x 600 -y 600",
        mandelbrot_args),
    program("mandelbrot_indep_left", "mandelbrot", "mandelbrot",
        "-l -x 600 -y 600",
        mandelbrot_args),
    program("fibs_43_gc10", "fibs", "fibs",
        "-d 10 43", fibs_args),
    program("fibs_43_gc0", "fibs", "fibs",
        "43", fibs_args)
    ].

:- func mandelbrot_args(group) = string.

mandelbrot_args(group(seq, _)) = "-s".
mandelbrot_args(group(par, _)) = "".

:- func fibs_args(group) = string.

fibs_args(group(seq, _)) = "".
fibs_args(group(par, _)) = "--parallel".

