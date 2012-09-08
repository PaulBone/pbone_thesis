%------------------------------------------------------------------------%
%------------------------------------------------------------------------%

:- module checkpoint.

:- interface.

:- import_module io.
:- import_module string.

%------------------------------------------------------------------------%

:- type checkpoint_log(S).

:- type open_result(S)
    --->    open_new_log(checkpoint_log(S))
    ;       open_existing_log(checkpoint_log(S), S)
    ;       open_error(string)
    ;       open_io_error(io.error).

:- pred open_log(string::in, open_result(S)::out,
    io::di, io::uo) is det.

:- pred close_and_delete_log(checkpoint_log(S)::in, io::di, io::uo) is det.

:- pred checkpoint(S::in, checkpoint_log(S)::in, io::di, io::uo) is det.

%------------------------------------------------------------------------%
%------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module maybe.
:- import_module require.

:- import_module foreign.

%------------------------------------------------------------------------%

:- type checkpoint_log(S)
    --->    checkpoint_log(
                ci_stream           :: io.output_stream,
                ci_filename         :: string
            ).

%------------------------------------------------------------------------%

open_log(Name, Result, !IO) :-
    try_read_existing_file(Name, TRResult, !IO),
    (
        TRResult = tr_success(State),

        OldLogName = Name ++ ".old",
        rename(Name, OldLogName, MaybeRenameError, !IO),
        check_error($module, $pred, MaybeRenameError, !IO),
        open_output(Name, MaybeStream, !IO),
        (
            MaybeStream = ok(Stream),
            Log = checkpoint_log(Stream, Name),
            % Write into the new log before we remove the old one.
            % header(Stream, Config, !IO),
            checkpoint(State, Log, !IO),
            remove_file(OldLogName, RemoveResult, !IO),
            (
                RemoveResult = ok,
                Result = open_existing_log(Log, State)
            ;
                RemoveResult = error(Error),
                Result = open_io_error(Error)
            )
        ;
            MaybeStream = error(Error),
            Result = open_io_error(Error)
        )
    ;
        ( TRResult = tr_io_error(_)
        ; TRResult = tr_empty
        ),
        % The error could be that the file doesn't exist.  so try to create
        % a new log.
        open_output(Name, MaybeStream, !IO),
        (
            MaybeStream = ok(Stream),
            Log = checkpoint_log(Stream, Name),
            % header(Stream, Config, !IO),
            Result = open_new_log(Log)
        ;
            MaybeStream = error(WriteError),
            (
                TRResult = tr_io_error(ReadError),
                % Return the original error.
                Result = open_io_error(ReadError)
            ;
                TRResult = tr_empty,
                Result = open_io_error(WriteError)
            )
        )
    ;
        TRResult = tr_error(Error),
        Result = open_error(Error)
    ).

:- type try_read_result(S)
    --->    tr_success(
                trs_state       :: S
            )
    ;       tr_empty
    ;       tr_error(string)
    ;       tr_io_error(io.error).

:- pred try_read_existing_file(string::in, try_read_result(S)::out,
    io::di, io::uo) is det.

try_read_existing_file(Name, Result, !IO) :-
    open_input(Name, MaybeStream, !IO),
    (
        MaybeStream = ok(Stream),
        read_log_for_state(Stream, MaybeState, !IO),
        (
            MaybeState = yes(State),
            Result = tr_success(State)
        ;
            MaybeState = no,
            Result = tr_empty
        )
    ;
        MaybeStream = error(Error),
        Result = tr_io_error(Error)
    ).

:- pred read_log_for_state(input_stream::in,
    maybe(S)::out, io::di, io::uo) is det.

read_log_for_state(Stream, MaybeState, !IO) :-
    read(Stream, MaybeRead, !IO),
    (
        MaybeRead = ok(State0),
        read_log_for_latest_state(Stream, State0, State, !IO),
        MaybeState = yes(State)
    ;
        ( MaybeRead = eof
        ; MaybeRead = error(_, _)
        ),
        MaybeState = no
    ).

:- pred read_log_for_latest_state(input_stream::in, S::in, S::out,
    io::di, io::uo) is det.

read_log_for_latest_state(Stream, !State, !IO) :-
    read(Stream, MaybeRead, !IO),
    (
        MaybeRead = ok(!:State),
        read_log_for_latest_state(Stream, !State, !IO)
    ;
        ( MaybeRead = eof
        ; MaybeRead = error(_, _)
        )
    ).

%------------------------------------------------------------------------%

close_and_delete_log(Log, !IO) :-
    io.close_output(Log ^ ci_stream, !IO),
    remove_file(Log ^ ci_filename, RmResult, !IO),
    (
        RmResult = ok
    ;
        RmResult = error(Error),
        unexpected($module, $pred,
            format("Could not remove old checkpoint file: %s",
            [s(error_message(Error))]))
    ).

%------------------------------------------------------------------------%

checkpoint(State, Log, !IO) :-
    Stream = Log ^ ci_stream,
    io.write(Stream, State, !IO),
    io.write_string(Stream, ".\n", !IO),
    io.flush_output(Stream, !IO).

%------------------------------------------------------------------------%

:- pred header(output_stream::in, C::in, io::di, io::uo) is det.

header(Stream, Config, !IO) :-
    io.write(Stream, Config, !IO),
    io.write_string(Stream, ".\n", !IO),
    io.flush_output(Stream, !IO).

%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
