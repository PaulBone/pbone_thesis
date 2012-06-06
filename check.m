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

:- import_module bool.
:- import_module char.
:- import_module cord.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module parsing_utils.
:- import_module require.
:- import_module string.
:- import_module unit.

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
        read_file_as_string(Stream, Result2, !IO),
        (
            Result2 = ok(Contents),
            check_file_string(Name, Contents, FileErrors),
            !:Errors = !.Errors ++ FileErrors
        ;
            Result2 = error(_, IOError),
            error(format("Couldn't read file %s: %s", [s(Name),
                s(error_message(IOError))]))
        ),
        io.close_input(Stream, !IO)
    ;
        Result = error(IOError),
        error(format("Couldn't open file %s: %s", [s(Name),
            s(error_message(IOError))]))
    ).

:- pred check_file_string(string::in, string::in, cord(prose_error)::out)
    is det.

check_file_string(Name, Contents, Errors) :-
    promise_equivalent_solutions [Result] (
        parse(Contents, parse_texfile(Name), Result)
    ),
    (
        Result = ok(Errors)
    ;
        Result = error(MaybeMessage, Line, Col),
        make_parse_error(Name, MaybeMessage, Line, Col, Error),
        Errors = singleton(Error)
    ).

:- pred parse_texfile(string::in, src::in, cord(prose_error)::out,
    ps::in, ps::out) is semidet.

parse_texfile(File, Src, Errors, !PS) :-
    Lines = src_to_line_numbers(Src),
    parse_tex(Lines, File, Src, Errors, !PS),
    eof(Src, _, !PS).

:- pred parse_tex(line_numbers::in, string::in, src::in, cord(prose_error)::out,
    ps::in, ps::out) is semidet.

parse_tex(Lines, File, Src, Errors, !PS) :-
    whitespace(Src, _, !PS),
    (
        promise_equivalent_solutions [PS0] (
            ( parse_comment(Src, _, !.PS, PS0)
            ; char(punctuation, Src, _, !.PS, PS0)
            ; char(symbol, Src, _, !.PS, PS0)
            )
        )
    ->
        !:PS = PS0,
        parse_tex(Lines, File, Src, Errors, !PS)
    ;
        ( parse_macro(Lines, File, Src, Errors0, !.PS, PS0) ->
            Errors1 = Errors0,
            PS1 = PS0
        ; parse_and_check_word(Lines, File, Src, Errors0, !.PS, PS0) ->
            Errors1 = Errors0,
            PS1 = PS0
        ; parse_group(Lines, File, Src, Errors0, !.PS, PS0) ->
            Errors1 = Errors0,
            PS1 = PS0
        ;
            false
        )
    ->
        ErrorsA = Errors1,
        !:PS = PS1,
        parse_tex(Lines, File, Src, ErrorsB, !PS),
        Errors = ErrorsA ++ ErrorsB
    ;
        Errors = cord.init
    ).

:- pred parse_tex_arg(line_numbers::in, string::in, src::in,
    cord(prose_error)::out, ps::in, ps::out) is semidet.

parse_tex_arg(Lines, File, Src, Errors, !PS) :-
    whitespace(Src, _, !PS),
    (
        promise_equivalent_solutions [PS0] (
            ( parse_comment(Src, _, !.PS, PS0)
            ; char(punctuation, Src, _, !.PS, PS0)
            )
        )
    ->
        !:PS = PS0,
        parse_tex_arg(Lines, File, Src, Errors, !PS)
    ;
        ( parse_macro(Lines, File, Src, Errors0, !.PS, PS0) ->
            Errors1 = Errors0,
            PS1 = PS0
        ; parse_and_check_word(Lines, File, Src, Errors0, !.PS, PS0) ->
            Errors1 = Errors0,
            PS1 = PS0
        ;
            false
        )
    ->
        ErrorsA = Errors1,
        !:PS = PS1,
        parse_tex_arg(Lines, File, Src, ErrorsB, !PS),
        Errors = ErrorsA ++ ErrorsB
    ;
        Errors = cord.init
    ).

:- pred parse_and_check_word(line_numbers::in, string::in, src::in,
    cord(prose_error)::out, ps::in, ps::out) is semidet.

parse_and_check_word(Lines, File, Src, Errors, !PS) :-
    current_offset(Src, Offset, !PS),
    parse_word(Src, WordChars, !PS),
    Word = string.from_char_list(WordChars),
    offset_to_line_number_and_position(Lines, Offset, Line, _),
    check_word(File, Word, Line, cord.init, Errors).

:- pred parse_word(src::in, list(char)::out, ps::in, ps::out) is semidet.

parse_word(Src, Word, !PS) :-
    next_char(Src, C, !PS),
    not not_word_char(C),
    (
        parse_word(Src, Word0, !.PS, PS0)
    ->
        Word = [C | Word0],
        !:PS = PS0
    ;
        % It's okay if the rest of the word isn't a word.
        Word = [C]
    ).

:- pred parse_macro(line_numbers::in, string::in, src::in,
    cord(prose_error)::out, ps::in, ps::out) is semidet.

parse_macro(Lines, File, Src, Errors, !PS) :-
    next_char(Src, '\\', !PS),
    (
        % Short macros are things like escapes spacing and \" for umlouts.
        next_char(Src, C, !.PS, PS0),
        macro_char_short(C)
    ->
        !:PS = PS0,
        Errors = cord.init
    ;
        identifier(macro_chars, macro_chars, Src, Ident, !.PS, PS0)
    ->
        !:PS = PS0,
        ( member(Ident, special_macros) ->
            CheckArgs = no
        ;
            CheckArgs = yes
        ),
        zero_or_more(parse_macro_arg(Lines, File, CheckArgs), Src, ArgErrors,
            !PS),
        Errors = cord_list_to_cord(ArgErrors)
    ;
        whitespace(Src, _, !PS),
        Errors = cord.init
    ).

:- pred parse_group(line_numbers::in, string::in, src::in,
    cord(prose_error)::out, ps::in, ps::out) is semidet.

parse_group(Lines, File, Src, Errors, !PS) :-
    check.brackets('{', '}', parse_tex(Lines, File),
        Src, Errors, !PS).

:- pred parse_macro_arg(line_numbers::in, string::in, bool::in, src::in,
    cord(prose_error)::out, ps::in, ps::out) is semidet.

parse_macro_arg(Lines, File, CheckArgs, Src, Errors, !PS) :-
    (
        check.brackets('{', '}', parse_tex_arg(Lines, File), 
            Src, Errors0, !.PS, PS0)
    ->
        !:PS = PS0,
        Errors1 = Errors0
    ;
        check.brackets('[', ']', parse_tex_arg(Lines, File),
            Src, Errors0, !.PS, PS0)
    ->
        !:PS = PS0,
        Errors1 = Errors0
    ;
        false
    ),
    (
        CheckArgs = yes,
        Errors = Errors1
    ;
        CheckArgs = no,
        Errors = cord.init
    ).

:- pred parse_comment(src::in, unit::out, ps::in, ps::out) is semidet.

parse_comment(Src, unit, !PS) :-
    next_char(Src, '%', !PS),
    skip_to_eol(Src, _, !PS).

:- pred brackets(char, char, parser(T), src, T, ps, ps).
:- mode brackets(in, in, in(parser), in, out, in, out) is semidet.

brackets(L, R, P, Src, Out, !PS) :-
    next_char(Src, L, !PS),
    P(Src, Out, !PS),
    ( next_char(Src, R, !.PS, PS0) ->
        !:PS = PS0
    ;
        fail_with_message("Couldn't match right bracket", Src, unit, !PS)
    ).

:- pred char(pred(char), src, unit, ps, ps).
:- mode char(pred(in) is semidet, in, out, in, out) is semidet.

char(P, Src, unit, !PS) :-
    next_char(Src, Char, !PS),
    P(Char).

:- func macro_chars = string.

macro_chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ*".

% Checking code.
%------------------------------------------------------------------------%

:- pred check_word(string::in, string::in, int::in,
    cord(prose_error)::in, cord(prose_error)::out) is det.

check_word(Name, Word, LineNo, !Errors) :-
    foldl(check_contractions(Word, Name, LineNo), contractions, !Errors),
    foldl(check_hypenations(Word, Name, LineNo), invalid_hypenations,
        !Errors),
    foldl(check_capitalizations(Word, Name, LineNo), acronyms, !Errors).

:- pred check_capitalizations(string::in, string::in, int::in,
    string::in, cord(prose_error)::in, cord(prose_error)::out) is det.

check_capitalizations(Word, Name, LineNo, Acronym, !Errors) :-
    (
        stri_compare(Word, Acronym, 0),
        Word \= Acronym
    ->
        Message = format("%s is an incorrect capitalization of %s.",
            [s(Word), s(Acronym)]),
        record_error(Name, LineNo, Message, !Errors)
    ;
        true
    ).

:- pred check_contractions(string::in, string::in, int::in, 
    string::in, cord(prose_error)::in, cord(prose_error)::out) is det.

check_contractions(Word, Name, LineNo, Contraction, !Errors) :-
    check_bad_words(Word, Name, LineNo, Contraction,
        "Contraction \"%s\" found.", !Errors).

:- pred check_hypenations(string::in, string::in, int::in,
    string::in, cord(prose_error)::in, cord(prose_error)::out) is det.

check_hypenations(Word, Name, LineNo, Hypenation, !Errors) :-
    check_bad_words(Word, Name, LineNo, Hypenation,
        "Hypenation is inconsistent \"%s\".", !Errors).

:- pred check_bad_words(string::in, string::in, int::in,
    string::in, string::in, cord(prose_error)::in, cord(prose_error)::out)
    is det.

check_bad_words(Word, Name, LineNo, BadWord, Message, !Errors) :-
    (
        stri_compare(BadWord, Word, 0)
    ->
        record_error(Name, LineNo, format(Message, [s(Word)]), !Errors)
    ;
        true
    ).

:- pred stri_compare(string::in, string::in, int::in) is semidet.

stri_compare(StrA, StrB, Index) :-
    (
        index(StrA, Index, CharA),
        index(StrB, Index, CharB)
    ->
        (
            CharA = CharB
        ;
            to_lower(CharA) = (to_lower(CharB) `with_type` char)
        ),
        stri_compare(StrA, StrB, Index+1)
    ;
        not index(StrA, Index, _),
        not index(StrB, Index, _)
    ).

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
    "low-level",
    "straight-forward"
    ].

:- func acronyms = list(string).

% These must be capitalized here to work.
acronyms = [
    "POSIX",
    "CPU",
    "GHC",
    "GCC"
    ].

:- pred punctuation(char::in) is semidet.

punctuation('.').
punctuation(',').
punctuation(':').
punctuation(';').
punctuation('!').
punctuation('?').
punctuation('(').
punctuation(')').

:- pred quote(char::in) is semidet.

quote('`').
quote('\'').

:- pred symbol(char::in) is semidet.

symbol(C) :- quote(C).
symbol('-').
symbol('[').
symbol(']').
symbol('&').

:- pred not_word_char(char::in) is semidet.

not_word_char(C) :- is_whitespace(C).
not_word_char(C) :- punctuation(C).
not_word_char('{').
not_word_char('}').
not_word_char('[').
not_word_char(']').
not_word_char('(').
not_word_char(')').
not_word_char('\\').
not_word_char('%').

:- pred macro_char_short(char::in) is semidet.

macro_char_short('\\').
macro_char_short('[').
macro_char_short(']').
macro_char_short('&').
macro_char_short('%').
macro_char_short('$').
macro_char_short(',').
macro_char_short('.').
macro_char_short(':').
macro_char_short(';').
macro_char_short('_').
macro_char_short('-').

:- func special_macros = list(string).

special_macros = [
        "citep",
        "citet"
    ].

% Errors.
%------------------------------------------------------------------------%

:- type prose_error == string.

:- pred record_error(string::in, int::in, string::in,
    cord(prose_error)::in, cord(prose_error)::out) is det.

record_error(File, LineNo, Message, !Errors) :-
    make_error(File, Message, LineNo, no, Error),
    !:Errors = snoc(!.Errors, Error).

:- pred make_parse_error(string::in, maybe(string)::in, int::in, int::in,
    prose_error::out) is det.

make_parse_error(File, no, Line, Col, Error) :-
    make_error(File, "Parse error", Line, yes(Col), Error). 
make_parse_error(File, yes(Message0), Line, Col, Error) :-
    Message = format("Parse error: %s", [s(Message0)]),
    make_error(File, Message, Line, yes(Col), Error).

:- pred make_error(string::in, string::in, int::in, maybe(int)::in,
    prose_error::out) is det.

make_error(File, Message, Line, no, Error) :-
    format("%s:%d: %s", [s(File), i(Line), s(Message)], Error).
make_error(File, Message, Line, yes(Col), Error) :-
    format("%s:%d: col %d, %s", [s(File), i(Line), i(Col), s(Message)], Error).

:- pred print_error(prose_error::in, io::di, io::uo) is det.

print_error(Error, !IO) :-
    io.format("%s\n", [s(Error)], !IO).

