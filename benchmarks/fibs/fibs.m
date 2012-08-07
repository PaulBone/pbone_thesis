
:- module fibs.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    (
        Args = [NumStr, GranStr],
        string.to_int(NumStr, Num),
        string.to_int(GranStr, Gran)
    ->
        F = fibs(Num, Gran),
        io.format("fibs(%d) = %d\n", [i(Num), i(F)], !IO)
    ;
        io.write_string(stderr_stream, "Invalid command line arguments\n",
            !IO),
        io.set_exit_status(1, !IO)
    ).

:- func fibs(int, int) = int.

fibs(N, G) = F :-
    (
        N < 2
    ->
        F = 1
    ;
        ( G > 0 ->
            (
                F1 = fibs(N-1, G-1)
            &
                F2 = fibs(N-2, G-1)
            )
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

