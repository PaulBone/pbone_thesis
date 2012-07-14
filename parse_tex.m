%----------------------------------------------------------------------------%
% TeX Parsing code.
%----------------------------------------------------------------------------%

:- module parse_tex.

:- interface.

:- import_module cord.
:- import_module int.
:- import_module parsing_utils.
:- import_module string.

%----------------------------------------------------------------------------%

:- type locn
    --->    locn(
                l_file      :: string,
                l_line      :: int,
                l_pos       :: int
            ).

:- type token
    --->    word(
                tw_word     :: string,
                tw_locn     :: locn
            ).

%----------------------------------------------------------------------------%

:- pred parse_texfile(string::in, src::in, cord(token)::out,
    ps::in, ps::out) is semidet.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module list.
:- import_module unit.

parse_texfile(File, Src, Tokens, !PS) :-
    Lines = src_to_line_numbers(Src),
    parse_tex(Lines, File, Src, Tokens, !PS),
    eof(Src, _, !PS).

:- pred parse_tex(line_numbers::in, string::in, src::in, cord(token)::out,
    ps::in, ps::out) is semidet.

parse_tex(Lines, File, Src, Tokens, !PS) :-
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
        parse_tex(Lines, File, Src, Tokens, !PS)
    ;
        ( parse_macro(Lines, File, Src, Tokens0, !.PS, PS0) ->
            Tokens1 = Tokens0,
            PS1 = PS0
        ; parse_word(Lines, File, Src, Token, !.PS, PS0) ->
            Tokens1 = cord.singleton(Token),
            PS1 = PS0
        ; parse_group(Lines, File, Src, Tokens0, !.PS, PS0) ->
            Tokens1 = Tokens0,
            PS1 = PS0
        ;
            false
        )
    ->
        TokensA = Tokens1,
        !:PS = PS1,
        parse_tex(Lines, File, Src, TokensB, !PS),
        Tokens = TokensA ++ TokensB
    ;
        Tokens = cord.init
    ).

:- pred parse_tex_arg(line_numbers::in, string::in, src::in,
    cord(token)::out, ps::in, ps::out) is semidet.

parse_tex_arg(Lines, File, Src, Tokens, !PS) :-
    whitespace(Src, _, !PS),
    (
        promise_equivalent_solutions [PS0] (
            ( parse_comment(Src, _, !.PS, PS0)
            ; char(punctuation, Src, _, !.PS, PS0)
            )
        )
    ->
        !:PS = PS0,
        parse_tex_arg(Lines, File, Src, Tokens, !PS)
    ;
        ( parse_macro(Lines, File, Src, Tokens0, !.PS, PS0) ->
            Tokens1 = Tokens0,
            PS1 = PS0
        ; parse_word(Lines, File, Src, Token, !.PS, PS0) ->
            Tokens1 = singleton(Token),
            PS1 = PS0
        ;
            false
        )
    ->
        TokensA = Tokens1,
        !:PS = PS1,
        parse_tex_arg(Lines, File, Src, TokensB, !PS),
        Tokens = TokensA ++ TokensB
    ;
        Tokens = cord.init
    ).

:- pred parse_word(line_numbers::in, string::in, src::in,
    token::out, ps::in, ps::out) is semidet.

parse_word(Lines, File, Src, Token, !PS) :-
    current_offset(Src, Offset, !PS),
    parse_word2(Src, WordChars, !PS),
    Word = string.from_char_list(WordChars),
    offset_to_line_number_and_position(Lines, Offset, Line, Pos),
    Locn = locn(File, Line, Pos),
    Token = word(Word, Locn).

:- pred parse_word2(src::in, list(char)::out, ps::in, ps::out) is semidet.

parse_word2(Src, Word, !PS) :-
    next_char(Src, C, !PS),
    not not_word_char(C),
    (
        parse_word2(Src, Word0, !.PS, PS0)
    ->
        Word = [C | Word0],
        !:PS = PS0
    ;
        % It's okay if the rest of the word isn't a word.
        Word = [C]
    ).

:- pred parse_macro(line_numbers::in, string::in, src::in,
    cord(token)::out, ps::in, ps::out) is semidet.

parse_macro(Lines, File, Src, Tokens, !PS) :-
    next_char(Src, '\\', !PS),
    (
        % Short macros are things like escapes spacing and \" for umlouts.
        next_char(Src, C, !.PS, PS0),
        macro_char_short(C)
    ->
        !:PS = PS0,
        Tokens = cord.init
    ;
        identifier(macro_chars, macro_chars, Src, Ident, !.PS, PS0)
    ->
        !:PS = PS0,
        ( member(Ident, special_macros) ->
            CheckArgs = no
        ;
            CheckArgs = yes
        ),
        zero_or_more(parse_macro_arg(Lines, File, CheckArgs), Src, ArgTokens,
            !PS),
        Tokens = cord_list_to_cord(ArgTokens)
    ;
        whitespace(Src, _, !PS),
        Tokens = cord.init
    ).

:- pred parse_group(line_numbers::in, string::in, src::in,
    cord(token)::out, ps::in, ps::out) is semidet.

parse_group(Lines, File, Src, Tokens, !PS) :-
    parse_tex.brackets('{', '}', parse_tex(Lines, File),
        Src, Tokens, !PS).

:- pred parse_macro_arg(line_numbers::in, string::in, bool::in, src::in,
    cord(token)::out, ps::in, ps::out) is semidet.

parse_macro_arg(Lines, File, CheckArgs, Src, Tokens, !PS) :-
    (
        parse_tex.brackets('{', '}', parse_tex_arg(Lines, File), 
            Src, Tokens0, !.PS, PS0)
    ->
        !:PS = PS0,
        Tokens1 = Tokens0
    ;
        parse_tex.brackets('[', ']', parse_tex_arg(Lines, File),
            Src, Tokens0, !.PS, PS0)
    ->
        !:PS = PS0,
        Tokens1 = Tokens0
    ;
        false
    ),
    (
        CheckArgs = yes,
        Tokens = Tokens1
    ;
        CheckArgs = no,
        Tokens = cord.init
    ).

:- pred parse_comment(src::in, unit::out, ps::in, ps::out) is semidet.

parse_comment(Src, unit, !PS) :-
    next_char(Src, '%', !PS),
    skip_to_eol(Src, _, !PS).

%----------------------------------------------------------------------------%

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

%----------------------------------------------------------------------------%

:- func macro_chars = string.

macro_chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ*".

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
        "cite",
        "citep",
        "citet",
        "ref",
        "label"
    ].

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%
