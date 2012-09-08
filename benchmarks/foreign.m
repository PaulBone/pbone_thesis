%------------------------------------------------------------------------%
%------------------------------------------------------------------------%

:- module foreign.

:- interface.

:- import_module io.
:- import_module string.
:- import_module maybe.
:- import_module float.

%------------------------------------------------------------------------%

:- pred system(string::in, io::di, io::uo) is det.

:- pred write_file(string::in, string::in, io::di, io::uo) is det.

:- pred cd(string::in, io::di, io::uo) is det.

:- pred rename(string::in, string::in, maybe_error::out, io::di, io::uo)
    is det.

:- pred getenv(string::in, string::out, io::di, io::uo) is det.

:- pred setenv(string::in, string::in, io::di, io::uo) is det.

:- pred time(pred(io, io), float, io, io).
:- mode time(pred(di, uo) is det, out, di, uo) is cc_multi.

:- pred check_error(string::in, string::in, maybe_error::in,
    io::di, io::uo) is det.

%------------------------------------------------------------------------%
%------------------------------------------------------------------------%

:- implementation.

:- import_module bool.

:- import_module require.

%------------------------------------------------------------------------%

system(Cmd, !IO) :-
    call_system(Cmd, Res, !IO),
    ( Res = ok(0) ->
        true
    ;
        unexpected($module, $pred, "Command failed " ++ Cmd)
    ).

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

rename(Src, Dest, Res, !IO) :-
    rename_file(Src, Dest, Res0, !IO),
    (
        Res0 = ok,
        Res = ok
    ;
        Res0 = error(Error),
        Res = error(error_message(Error))
    ).

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

check_error(_, _, ok, !IO).
check_error(Module, Pred, error(Error), !IO) :-
    unexpected(Module, Pred, Error).

