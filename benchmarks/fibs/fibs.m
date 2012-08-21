
:- module fibs.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module getopt.
:- import_module list.
:- import_module string.

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    OptionOps = option_ops_multi(short_options, long_options, default_options),
    getopt.process_options(OptionOps, Args, NonOptionsArgs, GetoptResult),
    (
        GetoptResult = ok(OptionTable),
        getopt.lookup_bool_option(OptionTable, help, Help),
        (
            Help = yes,
            usage(!IO)
        ;
            Help = no,
            ( handle_options(OptionTable, NonOptionsArgs, N, UsePar, UseGC) ->
                run(N, UsePar, UseGC, !IO)
            ;
                io.write_string(stderr_stream, "Error handling options.\n",
                    !IO),
                io.set_exit_status(1, !IO)
            )
        )
    ;
        GetoptResult = error(Error), 
        io.format(stderr_stream, "Invalid command line arguments: %s\n",
            [s(Error)], !IO),
        io.set_exit_status(1, !IO)
    ).

%---------------------------------------------------------------------------%

:- type option
    --->    help
    ;       parallel
    ;       granularity_control.

:- pred short_options(char::in, option::out) is semidet.

short_options('h', help).
short_options('?', help).
short_options('p', parallel).
short_options('d', granularity_control).

:- pred long_options(string::in, option::out) is semidet.

long_options("help",                help).
long_options("parallel",            parallel).
long_options("gc-depth",            granularity_control).

:- pred default_options(option::out, option_data::out) is multi.

default_options(help,                   bool(no)).
default_options(parallel,               bool(no)).
default_options(granularity_control,    int(0)).

:- pred handle_options(option_table(option)::in, list(string)::in,
    int::out, use_par::out, granularity_control::out) is semidet.

handle_options(Options, Args, N, UsePar, GranularityControl) :-
    Args = [NArgStr],
    string.to_int(NArgStr, N),
    N >= 0,

    lookup_bool_option(Options, parallel, Parallel),
    (
        Parallel = yes,
        UsePar = use_par
    ;
        Parallel = no,
        UsePar = use_seq
    ),

    lookup_int_option(Options, granularity_control, Depth),
    ( Depth = 0 ->
        GranularityControl = no_granularity_control
    ;
        GranularityControl = use_granularity_control(Depth),
        Depth > 0
    ).

:- pred usage(io::di, io::uo) is det.

usage(!IO) :-
    Str = "fibs -h\n" ++
          "fibs [options] N\n" ++
          "\toptions may be one or more of:\n" ++
          "\t\t-p --parallel Use parallelism\n" ++
          "\t\t-d --gc-depth Use granularity control with specified\n" ++
          "\t\t              depth\n",
    io.write_string(Str, !IO).

%---------------------------------------------------------------------------%

:- type use_par
    --->    use_par
    ;       use_seq.

:- type granularity_control
    --->    use_granularity_control(int)
    ;       no_granularity_control.

:- pred run(int::in, use_par::in, granularity_control::in, io::di, io::uo)
    is det.

run(Num, Par, GC, !IO) :-
    (
        Par = use_par,
        (
            GC = use_granularity_control(Depth),
            F = fibs_par_gc(Num, Depth)
        ;
            GC = no_granularity_control,
            F = fibs_par(Num)
        )
    ;
        Par = use_seq,
        (
            GC = use_granularity_control(Depth),
            F = fibs_seq_gc(Num, Depth)
        ;
            GC = no_granularity_control,
            F = fibs_seq(Num)
        )
    ),
    io.format("fibs(%d) = %d\n", [i(Num), i(F)], !IO).

%---------------------------------------------------------------------------%

:- func fibs_par_gc(int, int) = int.

fibs_par_gc(N, G) = F :-
    (
        N < 2
    ->
        F = 1
    ;
        ( G > 0 ->
            (
                F1 = fibs_par_gc(N-1, G-1)
            &
                F2 = fibs_par_gc(N-2, G-1)
            )
        ;
            F1 = fibs_seq(N-1),
            F2 = fibs_seq(N-2)
        ),
        F = F1 + F2
    ).

:- func fibs_par(int) = int.

fibs_par(N) = F :-
    (
        N < 2
    ->
        F = 1
    ;
        (
            F1 = fibs_par(N-1)
        &
            F2 = fibs_par(N-2)
        ),
        F = F1 + F2
    ).

:- func fibs_seq_gc(int, int) = int.

fibs_seq_gc(N, G) = F :-
    (
        N < 2
    ->
        F = 1
    ;
        ( G > 0 ->
            F1 = fibs_seq_gc(N-1, G-1),
            F2 = fibs_seq_gc(N-2, G-1)
        ;
            F1 = fibs_seq(N-1),
            F2 = fibs_seq(N-2)
        ),
        F = F1 + F2
    ).

:- func fibs_seq(int) = int.

fibs_seq(N) = F :-
    (
        N < 2
    ->
        F = 1
    ;
        F = fibs_seq(N-1) + fibs_seq(N-2)
    ).

