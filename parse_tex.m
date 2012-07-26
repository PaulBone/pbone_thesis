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
            )
    ;       punct(
                tp_punct    :: string,
                tp_locn     :: locn
            )
    ;       macro(
                tm_name     :: string,
                tm_args     :: cord(macro_arg),
                tm_locn     :: locn
            )
    ;       environment(
                te_name     :: string,
                te_args     :: cord(macro_arg),
                te_tokens   :: cord(token),
                te_locn     :: locn
            ).

:- type word
    --->    word(
                fw_word     :: string,
                fw_locn     :: locn
            ).

:- type macro_arg
    --->    macro_arg(
                ma_tokens   :: cord(token),
                ma_locn     :: locn
            ).

%----------------------------------------------------------------------------%

:- pred token_locn(token::in, locn::out) is det.

:- func words(cord(token)) = cord(word).

:- pred parse_texfile(string::in, src::in, cord(token)::out,
    ps::in, ps::out) is semidet.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module list.
:- import_module require.
:- import_module unit.

:- import_module util.
:- import_module tex.

token_locn(word(_, Locn), Locn).
token_locn(punct(_, Locn), Locn).
token_locn(macro(_, _, Locn), Locn).
token_locn(environment(_, _, _, Locn), Locn).

%----------------------------------------------------------------------------%

words(Tokens) =
    cord_concat(map(token_words, Tokens)).

:- func token_words(token) = cord(word).

token_words(word(Word, Locn)) = cord.singleton(word(Word, Locn)).
token_words(punct(_, _)) = cord.empty.
token_words(macro(Ident, Args, _)) = Words :-
    conservative_macro_info(Ident, MacroIsRef, _),
    (
        ( MacroIsRef = macro_is_reference
        ; MacroIsRef = macro_is_anchor
        ),
        Words = cord.empty
    ;
        MacroIsRef = macro_is_not_reference,
        Words = cord_concat(map(macro_arg_words, Args))
    ).
token_words(environment(Ident, _, Tokens, _)) = Words :-
    ( not environment_does_not_contain_prose(Ident) ->
        Words = cord_concat(map(token_words, Tokens))
    ;
        Words = cord.empty
    ).

:- func macro_arg_words(macro_arg) = cord(word).

macro_arg_words(macro_arg(Tokens, _)) = words(Tokens).

%----------------------------------------------------------------------------%

parse_texfile(File, Src, Tokens, !PS) :-
    Lines = src_to_line_numbers(Src),
    parse_tex(Lines, File, Src, Tokens0, !PS),
    eof(Src, _, !PS),
    detect_environments(Tokens0, Tokens).

:- pred parse_tex(line_numbers::in, string::in, src::in, cord(token)::out,
    ps::in, ps::out) is semidet.

parse_tex(Lines, File, Src, Tokens, !PS) :-
    whitespace(Src, _, !PS),
    (
        promise_equivalent_solutions [PS0] (
            ( parse_comment(Src, _, !.PS, PS0)
            ; char(symbol, Src, _, !.PS, PS0)
            )
        )
    ->
        !:PS = PS0,
        parse_tex(Lines, File, Src, Tokens, !PS)
    ;
        ( parse_punct(Lines, File, Src, Token, !.PS, PS0) ->
            Tokens1 = cord.singleton(Token),
            PS1 = PS0
        ; parse_macro(Lines, File, Src, Token, !.PS, PS0) ->
            Tokens1 = cord.singleton(Token),
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
        parse_comment(Src, _, !.PS, PS0)
    ->
        !:PS = PS0,
        parse_tex_arg(Lines, File, Src, Tokens, !PS)
    ;
        ( parse_punct(Lines, File, Src, Token, !.PS, PS0) ->
            Tokens1 = cord.singleton(Token),
            PS1 = PS0
        ; parse_macro(Lines, File, Src, Token, !.PS, PS0) ->
            Tokens1 = cord.singleton(Token),
            PS1 = PS0
        ; parse_word(Lines, File, Src, Token, !.PS, PS0) ->
            Tokens1 = cord.singleton(Token),
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
    offset_to_locn(Lines, File, Src, Locn, !.PS),
    parse_word2(Src, WordChars, !PS),
    Word = string.from_char_list(WordChars),
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
    token::out, ps::in, ps::out) is semidet.

parse_macro(Lines, File, Src, Token, !PS) :-
    offset_to_locn(Lines, File, Src, Locn, !.PS),
    next_char(Src, '\\', !PS),
    (
        % Short macros are things like escapes spacing and \" for umlouts.
        next_char(Src, C, !.PS, PS0),
        macro_char_short(C)
    ->
        !:PS = PS0,
        Token = macro(char_to_string(C), cord.empty, Locn)
    ;
        identifier(macro_chars, macro_chars, Src, Ident, !.PS, PS0)
    ->
        !:PS = PS0,
        zero_or_more(parse_macro_arg(Lines, File), Src, ArgTokens, !PS),
        Token = macro(Ident, cord.from_list(ArgTokens), Locn)
    ;
        whitespace(Src, _, !PS),
        Token = macro(" ", cord.empty, Locn)
    ).

:- pred parse_group(line_numbers::in, string::in, src::in,
    cord(token)::out, ps::in, ps::out) is semidet.

parse_group(Lines, File, Src, Tokens, !PS) :-
    parse_tex.brackets('{', '}', parse_tex(Lines, File),
        Src, Tokens, !PS).

:- pred parse_punct(line_numbers::in, string::in, src::in,
    token::out, ps::in, ps::out) is semidet.

parse_punct(Lines, File, Src, Token, !PS) :-
    offset_to_locn(Lines, File, Src, Locn, !.PS),
    one_or_more(char(punctuation), Src, Chars, !PS),
    Token = punct(string.from_char_list(Chars), Locn).

:- pred parse_macro_arg(line_numbers::in, string::in, src::in,
    macro_arg::out, ps::in, ps::out) is semidet.

parse_macro_arg(Lines, File, Src, Arg, !PS) :-
    offset_to_locn(Lines, File, Src, Locn, !.PS),
    (
        parse_tex.brackets('{', '}', parse_tex_arg(Lines, File),
            Src, Tokens, !.PS, PS0)
    ->
        !:PS = PS0,
        Arg = macro_arg(Tokens, Locn)
    ;
        parse_tex.brackets('[', ']', parse_tex_arg(Lines, File),
            Src, Tokens, !.PS, PS0)
    ->
        !:PS = PS0,
        Arg = macro_arg(Tokens, Locn)
    ;
        false
    ).

:- pred parse_comment(src::in, unit::out, ps::in, ps::out) is semidet.

parse_comment(Src, unit, !PS) :-
    next_char(Src, '%', !PS),
    skip_to_eol(Src, _, !PS).

:- pred offset_to_locn(line_numbers::in, string::in, src::in,
    locn::out, ps::in) is det.

offset_to_locn(Lines, File, Src, Locn, PS) :-
    current_offset(Src, Offset, PS, _),
    offset_to_line_number_and_position(Lines, Offset, Line, Pos),
    Locn = locn(File, Line, Pos).

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

:- pred char(pred(char), src, char, ps, ps).
:- mode char(pred(in) is semidet, in, out, in, out) is semidet.

char(P, Src, Char, !PS) :-
    next_char(Src, Char, !PS),
    P(Char).

%----------------------------------------------------------------------------%

:- pred detect_environments(cord(token)::in, cord(token)::out) is det.

detect_environments(Tokens, TokenEnvironments) :-
    ( parse_to_environments(cord.init, TokenEnvironmentsPrime, Tokens, _) ->
        TokenEnvironments = TokenEnvironmentsPrime
    ;
        unexpected($module, $pred, "Couldn't parse into environment tree.")
    ).

:- pred parse_to_environments(cord(token)::in, cord(token)::out,
    cord(token)::in, cord(token)::out) is semidet.

parse_to_environments(!Envs, !Tokens) :-
    ( parse_to_environment(Env, !Tokens) ->
        !:Envs = snoc(!.Envs, Env),
        parse_to_environments(!Envs, !Tokens)
    ;
        true
    ).

:- pred parse_to_environment(token::out, cord(token)::in, cord(token)::out)
    is semidet.

parse_to_environment(Env, !Tokens) :-
    next_token(Token0, !Tokens),
    make_nested_environments(Token0, Token),
    require_complete_switch [Token]
    (
        ( Token = word(_, _)
        ; Token = punct(_, _)
        ; Token = environment(_, _, _, _)
        ),
        Env = Token
    ;
        Token = macro(Name, Args0, Locn),
        Name \= "end",
        (
            Name = "begin",
            cord.head_tail(Args0, EnvNameArg, Args),
            arg_to_string(EnvNameArg, EnvName) 
        ->
            parse_until_end_environment(EnvName, cord.empty, Contents,
                !Tokens),
            next_token(Token, !Tokens),
            token_is_end_env(EnvName, Token),
            Env = environment(EnvName, Args, Contents, Locn) 
        ;
            Env = Token
        )
    ).

:- pred make_nested_environments(token::in, token::out) is det.

make_nested_environments(word(Word, Locn), word(Word, Locn)).
make_nested_environments(punct(Punct, Locn), punct(Punct, Locn)).
make_nested_environments(macro(Ident, Args0, Locn),
        macro(Ident, Args, Locn)) :-
    cord.map_pred(macro_arg_make_environments, Args0, Args).
make_nested_environments(environment(_, _, _, _), _) :-
    unexpected($module, $pred,
        "Environment conversion is bottom up, this shouldn't happen").

:- pred macro_arg_make_environments(macro_arg::in, macro_arg::out) is det.

macro_arg_make_environments(macro_arg(Tokens0, Locn),
        macro_arg(Tokens, Locn)) :-
   detect_environments(Tokens0, Tokens). 

:- pred parse_until_end_environment(string::in,
    cord(token)::in, cord(token)::out,
    cord(token)::in, cord(token)::out) is det.

parse_until_end_environment(EnvName, !Contents, !Tokens) :-
    ( next_token(Token0, !Tokens) ->
        make_nested_environments(Token0, Token), 
        ( token_is_end_env(EnvName, Token) ->
            true
        ;
            !:Contents = snoc(!.Contents, Token),
            parse_until_end_environment(EnvName, !Contents, !Tokens)
        )
    ;
        true
    ).

:- pred next_token(T::out, cord(T)::in, cord(T)::out)
    is semidet.

next_token(Token, !Tokens) :-
    cord.head_tail(!.Tokens, Token, !:Tokens). 

:- pred token_is_end_env(string::in, token::in) is semidet.

token_is_end_env(Name, Token) :-
    Token = macro("end", EndArgs, _),
    cord.head_tail(EndArgs, Arg, _),
    Arg = macro_arg(Tokens, _),
    cord.head_tail(Tokens, word(Name, _), _).

:- pred arg_to_string(macro_arg::in, string::out) is semidet.

arg_to_string(macro_arg(Tokens, _), String) :-
    cord.head_tail(Tokens, word(String, _), _).

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

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%
