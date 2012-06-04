%
% A program to check my thesis for bad habbits,
%
% Bad habbits checked for:
%   Contractions, such as "it's"
%

:- module check.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char.
:- import_module cord.
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.

:- type prose_error == string.

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    list.foldl2(check_file, Args, cord.empty, Errors, !IO),
    cord.foldl_pred(print_error, Errors, !IO),
    ( not is_empty(Errors) ->
        io.set_exit_status(1, !IO)
    ;
        true
    ).
    
:- pred check_file(string::in, cord(prose_error)::in, cord(prose_error)::out,
    io::di, io::uo) is det.

check_file(Name, !Errors, !IO) :-
    io.open_input(Name, Result, !IO),
    (
        Result = ok(Stream),
        check_lines(Name, Stream, 1, cord.empty, FileErrors, !IO),
        !:Errors = !.Errors ++ FileErrors,
        io.close_input(Stream, !IO)
    ;
        Result = error(IOError),
        error(format("Couldn't open file %s: %s", [s(Name),
            s(error_message(IOError))]))
    ).

:- pred check_lines(string::in, input_stream::in, int::in,
    cord(prose_error)::in, cord(prose_error)::out, io::di, io::uo) is det.

check_lines(Name, Stream, LineNo, !Errors, !IO) :-
    io.read_line_as_string(Stream, Result, !IO),
    (
        Result = ok(Line),
        check_line(Name, Line, LineNo, !Errors),
        check_lines(Name, Stream, LineNo + 1, !Errors, !IO)
    ;
        Result = eof
    ;
        Result = error(IOError),
        error(format("%s:%s", [s(Name), s(error_message(IOError))]))
    ).

:- pred check_line(string::in, string::in, int::in,
    cord(prose_error)::in, cord(prose_error)::out) is det.

check_line(Name, Line, LineNo, !Errors) :-
    Words = words_separator(separator, Line),
    foldl(check_contractions(Words, Name, LineNo), contractions, !Errors),
    foldl(check_hypenations(Words, Name, LineNo), invalid_hypenations,
        !Errors).

:- pred check_contractions(list(string)::in, string::in, int::in, 
    string::in, cord(prose_error)::in, cord(prose_error)::out) is det.

check_contractions(Words, Name, LineNo, Contraction, !Errors) :-
    check_bad_words(Words, Name, LineNo, Contraction, 
        format("Contraction \"%s\" found.",
            [s(Contraction)]),
        !Errors).

:- pred check_hypenations(list(string)::in, string::in, int::in,
    string::in, cord(prose_error)::in, cord(prose_error)::out) is det.

check_hypenations(Words, Name, LineNo, Hypenation, !Errors) :-
    check_bad_words(Words, Name, LineNo, Hypenation,
        format("Hypenation is inconsistent \"%s\".",
            [s(Hypenation)]),
        !Errors).

:- pred check_bad_words(list(string)::in, string::in, int::in,
    string::in, string::in, cord(prose_error)::in, cord(prose_error)::out)
    is det.

check_bad_words(Words, Name, LineNo, BadWord, Message, !Errors) :-
    BadWordLower = to_lower(BadWord) `with_type` string,
    (
        some [WordLower, Word] (
            member(Word, Words),
            WordLower = to_lower(Word),
            WordLower = BadWordLower
        )
    ->
        record_error(Name, LineNo, Message, !Errors)
    ;
        true
    ).

:- pred record_error(string::in, int::in, string::in,
    cord(prose_error)::in, cord(prose_error)::out) is det.

record_error(Name, LineNo, Message, !Errors) :-
    Error = format("%s:%d: %s", [s(Name), i(LineNo), s(Message)]),
    !:Errors = snoc(!.Errors, Error).

:- pred print_error(prose_error::in, io::di, io::uo) is det.

print_error(Error, !IO) :-
    io.format("%s\n", [s(Error)], !IO).

:- func contractions = list(string).

contractions = [
    "can't",
    "won't",
    "isn't",
    "it's",
    "don't",
    "shouldn't",
    "doesn't",
    "couldn't",
    "masn't",
    "won't",
    "didn't",
    "hasn't",
    "havn't",
    "I'm",
    "I've",
    "I'd",
    "we're",
    "we've",
    "we'd",
    "it've",
    "it'd",
    "they're",
    "they've",
    "they'd" ].

:- func invalid_hypenations = list(string).

invalid_hypenations = [
    "call-site",
    "type-class",
    "straight-forward"
    ].

:- pred separator(char::in) is semidet.

separator(C) :- is_whitespace(C).
separator('{').
separator('}').
separator('[').
separator(']').
separator('(').
separator(')').
separator(',').
separator('.').
separator(';').
separator(':').

