%----------------------------------------------------------------------------%
% TeX Parsing code.
%----------------------------------------------------------------------------%

:- module parse_tex.

:- interface.

:- import_module cord.
:- import_module int.
:- import_module parsing_utils.
:- import_module pretty_printer.
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
                tm_args     :: cord(macro_arg(token)),
                tm_locn     :: locn
            )
    ;       environment(
                te_name     :: string,
                te_args     :: cord(macro_arg(token)),
                te_tokens   :: cord(token),
                te_locn     :: locn
            ).

:- type word
    --->    word(
                fw_word     :: string,
                fw_locn     :: locn
            ).

:- type macro_or_environment
    --->    macro(
                m_name      :: string,
                m_args      :: cord(macro_arg(macro_or_environment)),
                m_locn      :: locn
            )
    ;       environment(
                e_name      :: string,
                e_args      :: cord(macro_arg(macro_or_environment)),
                e_contents  :: cord(macro_or_environment),
                e_locn      :: locn
            ).

:- type macro_arg(T)
    --->    macro_arg(
                ma_contents :: cord(T),
                ma_locn     :: locn
            ).

%----------------------------------------------------------------------------%

:- pred token_locn(token::in, locn::out) is det.

:- func words(cord(token)) = cord(word).

:- func macros(cord(token)) = cord(macro_or_environment).

:- pred parse_texfile(string::in, src::in, cord(token)::out,
    ps::in, ps::out) is semidet.

%----------------------------------------------------------------------------%

:- func pretty_tokens(cord(token)) = doc.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module list.
:- import_module maybe.
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
token_words(macro(Ident, Args, Locn)) = Words :-
    conservative_macro_info(Ident, _, _, ContainsProse),
    (
        ContainsProse = does_not_contain_prose,
        Words = cord.empty
    ;
        ContainsProse = replace_with_noun,
        Words = cord.singleton(word("Noun", Locn))
    ;
        ContainsProse = contains_prose,
        Words = cord_concat(map(macro_arg_words, Args))
    ).
token_words(environment(Ident, _, Tokens, _)) = Words :-
    ( not environment_does_not_contain_prose(Ident) ->
        Words = cord_concat(map(token_words, Tokens))
    ;
        Words = cord.empty
    ).

:- func macro_arg_words(macro_arg(token)) = cord(word).

macro_arg_words(macro_arg(Tokens, _)) = words(Tokens).

%----------------------------------------------------------------------------%

macros(Tokens) = cord_concat(map(token_macros, Tokens)).

:- func token_macros(token) = cord(macro_or_environment).

token_macros(word(_, _)) = cord.empty.
token_macros(punct(_, _)) = cord.empty.
token_macros(macro(Ident, Args0, Locn)) =
        cord.singleton(macro(Ident, Args, Locn)) :-
    Args = map(macro_arg_tokens_to_macro_arg_macros, Args0).
token_macros(environment(Ident, Args0, Tokens, Locn)) =
        cord.singleton(environment(Ident, Args, Macros, Locn)) :-
    Args = map(macro_arg_tokens_to_macro_arg_macros, Args0),
    Macros = macros(Tokens).

:- func macro_arg_tokens_to_macro_arg_macros(macro_arg(token)) = 
    macro_arg(macro_or_environment).

macro_arg_tokens_to_macro_arg_macros(macro_arg(Tokens, Locn)) =
        macro_arg(Macros, Locn) :-
    Macros = macros(Tokens).

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
        parse_comment(Src, _, !PS)
    ->
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
    WordChars = [FirstChar | _],
    not not_first_word_char(FirstChar),
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
    ( next_char(Src, '$', !PS) ->
        ( next_char(Src, '$', !PS) ->
            Token = macro("$$", cord.empty, Locn)
        ;
            Token = macro("$", cord.empty, Locn)
        )
    ; char(symbol, Src, C, !PS) ->
        Token = macro(char_to_string(C), cord.empty, Locn)
    ;
        next_char(Src, '\\', !PS),
        (
            % Short macros are things like escapes spacing and \" for umlouts.
            next_char(Src, C, !PS),
            macro_char_short(C)
        ->
            Token = macro(char_to_string(C), cord.empty, Locn)
        ;
            identifier(macro_chars, macro_chars, Src, Ident, !PS)
        ->
            zero_or_more(parse_macro_arg(Lines, File), Src, ArgTokens, !PS),
            (
                Ident = "begin",
                ArgTokens = [macro_arg(Tokens, _)],
                [ArgToken] = list(Tokens),
                word("verbatim", _) = ArgToken
            ->
                % Skip over stuff inside a verbatim environment.
                skip_to_end_of_verbatim(Src, !PS),
                Token = environment("verbatim", empty, empty, Locn)
            ;
                Token = macro(Ident, cord.from_list(ArgTokens), Locn)
            )
        ;
            whitespace(Src, _, !PS),
            Token = macro(" ", cord.empty, Locn)
        )
    ).

:- pred skip_to_end_of_verbatim(src::in, ps::in, ps::out) is semidet.

skip_to_end_of_verbatim(Src, !PS) :-
    (
        keyword("", "\\end{verbatim}", Src, _, !PS)
    ->
        true
    ;
        next_char(Src, _, !PS),
        skip_to_end_of_verbatim(Src, !PS)
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
    ( one_or_more(char(punctuation), Src, CharsP, !PS) ->
        Chars = CharsP
    ;
        n(3, char(unify('-')), Src, Chars, !PS)
    ),
    Token = punct(string.from_char_list(Chars), Locn).

:- pred parse_macro_arg(line_numbers::in, string::in, src::in,
    macro_arg(token)::out, ps::in, ps::out) is semidet.

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

:- pred n(int, pred(src, T, ps, ps), src, list(T), ps, ps).
:- mode n(in, pred(in, out, in, out) is semidet, in, out, in, out) is semidet.

n(N, P, Src, L, !PS) :-
    ( N = 0 ->
        L = []
    ;
        P(Src, X, !PS),
        n(N-1, P, Src, Xs, !PS),
        L = [X | Xs]
    ).

%----------------------------------------------------------------------------%

:- pred detect_environments(cord(token)::in, cord(token)::out) is det.

detect_environments(Tokens, Environments) :-
    parse_to_environments(no, cord.init, Environments, Tokens, _).

:- pred parse_to_environments(maybe(string)::in, 
    cord(token)::in, cord(token)::out,
    cord(token)::in, cord(token)::out) is det.

parse_to_environments(MaybeCurEnv, !Envs, !Tokens) :-
    ( next_token(Token, !.Tokens, NextTokens) ->
        ( ends_cur_env(MaybeCurEnv, Token) ->
            true
        ;
            ( is_start_environment(Token, _, _, _) ->
                ( parse_to_environment(EnvPrime, !Tokens) ->
                    Env = EnvPrime
                ;
                    !:Tokens = NextTokens,
                    parse_token_to_environments(Token, Env)
                )
            ;
                !:Tokens = NextTokens,
                parse_token_to_environments(Token, Env)
            ),
            !:Envs = snoc(!.Envs, Env),
            parse_to_environments(MaybeCurEnv, !Envs, !Tokens)
        )
    ;
        true
    ).

:- pred parse_to_environment(token::out,
    cord(token)::in, cord(token)::out) is semidet.

parse_to_environment(Env, !Tokens) :-
    next_token(StartToken, !Tokens),
    is_start_environment(StartToken, Name, Args, Locn),
    parse_to_environments(yes(Name), cord.init, Contents, !Tokens),
    next_token(EndToken, !Tokens),
    is_end_environment(EndToken, Name),
    Env = environment(Name, Args, Contents, Locn).

:- pred parse_token_to_environments(token::in, token::out) is det.

parse_token_to_environments(T@word(_, _), T).
parse_token_to_environments(T@punct(_, _), T).
parse_token_to_environments(T@macro(_, _, _), T).
parse_token_to_environments(environment(Name, Args, Tokens0, Locn),
        environment(Name, Args, Tokens, Locn)) :-
    detect_environments(Tokens0, Tokens).

:- pred is_start_environment(token::in, string::out,
    cord(macro_arg(token))::out, locn::out) is semidet.

is_start_environment(macro(MName, Args0, Locn), Name, Args, Locn) :-
    ( MName = "begin" ->
        head_tail(Args0, Arg, Args),
        arg_to_string(Arg, Name)
    ;
        ( MName = "$"
        ; MName = "$$"
        ),
        Name = MName,
        Args = Args0
    ).

:- pred ends_cur_env(maybe(string)::in, token::in) is semidet.

ends_cur_env(yes(Name), Token) :-
    is_end_environment(Token, Name).

:- pred is_end_environment(token::in, string::in) is semidet.

is_end_environment(macro(MName, Args, _), Name) :-
    (
        MName = "end",
        head_tail(Args, Arg, _),
        arg_to_string(Arg, Name)
    ;
        ( MName = "$"
        ; MName = "$$"
        ),
        Name = MName
    ).

:- pred next_token(T::out, cord(T)::in, cord(T)::out)
    is semidet.

next_token(Token, !Tokens) :-
    cord.head_tail(!.Tokens, Token, !:Tokens). 

:- pred token_is_end_env(string::in, token::in) is semidet.

token_is_end_env(Name, Token) :-
    ( Token = macro("end", EndArgs, _) ->
        cord.head_tail(EndArgs, Arg, _),
        Arg = macro_arg(Tokens, _),
        cord.head_tail(Tokens, word(Name, _), _)
    ;
        Token = macro(Name, _, _),
        ( Name = "$"
        ; Name = "$$"
        )
    ).

:- pred arg_to_string(macro_arg(token)::in, string::out) is semidet.

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
symbol('[').
symbol(']').
symbol('&').
symbol('~').
symbol('-').

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
not_word_char('$').
not_word_char('~').

:- pred not_first_word_char(char::in) is semidet.

not_first_word_char(C) :- not_word_char(C).
not_first_word_char('-').

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
macro_char_short('~').

%----------------------------------------------------------------------------%
% Pretty printing.

pretty_tokens(Tokens) = Doc :-
    Docs0 = map(pretty_token, Tokens), 
    Doc = docs(intersperse(docs([str(" "), nl]), list(Docs0))).

:- func pretty_token(token) = doc.

pretty_token(word(Word, _)) = str(Word).
pretty_token(punct(Punct, _)) = str(Punct).
pretty_token(macro(Name, Args, _)) = Doc :-
    ArgsDocs = pretty_macro_args(Args),
    Docs = [str("\\"), str(Name), indent(ArgsDocs)], 
    Doc = group(Docs).
pretty_token(environment(Name, Args, Tokens, _)) = Doc :-
    ArgsDocs = pretty_macro_args(Args),
    TokenDocs = [pretty_tokens(Tokens)],
    Docs = [hard_nl, 
        str("\\begin{"), str(Name), str("}"), nl, indent(ArgsDocs), hard_nl,
        indent(TokenDocs), hard_nl,
        str("\\end{"), str(Name), str("}"), hard_nl],
    Doc = group(Docs).

:- func pretty_macro_args(cord(macro_arg(token))) = docs.

pretty_macro_args(Args) = Docs :-
    ArgsDocs0 = map(pretty_macro_arg, list(Args)),
    Docs = intersperse(nl, ArgsDocs0).

:- func pretty_macro_arg(macro_arg(token)) = doc.

pretty_macro_arg(macro_arg(Tokens, _)) = Doc :-
    TokenDocs = pretty_tokens(Tokens),
    Doc = docs([str("{"), indent([TokenDocs]), str("}")]).

:- func intersperse(doc, docs) = docs.

intersperse(_, []) = [].
intersperse(Delim, [X | Xs0]) = Result :-
    Xs = intersperse(Delim, Xs0),
    (
        Xs = [],
        Result = [X | Xs]
    ;
        Xs = [_ | _],
        Result = [X, Delim | Xs]
    ).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%
