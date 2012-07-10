%----------------------------------------------------------------------------%
%
% A program to check my thesis for bad habbits,
%
% Bad habbits checked for:
%   Contractions, such as "it's",
%   Bad capitalisations, "cpu",
%   Bad Hypenations "most-likely",
%   Bad ngrams "speeds up"
%
%----------------------------------------------------------------------------%

:- module check.

:- interface.

:- import_module io.

%----------------------------------------------------------------------------%

:- pred main(io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module array.
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

:- import_module parse_tex.

%----------------------------------------------------------------------------%

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
        Result = ok(Tokens),
        check_tokens(Tokens, Errors)
    ;
        Result = error(MaybeMessage, Line, Col),
        make_parse_error(locn(Name, Line, Col), MaybeMessage, Error),
        Errors = singleton(Error)
    ).

% Checking code.
%------------------------------------------------------------------------%

:- pred check_tokens(cord(token)::in, cord(prose_error)::out) is det.

check_tokens(Tokens, Errors) :-
    WordErrors = cord_concat(cord.map(check_word, Tokens)),
    check_ngrams(Tokens, NGramErrors),
    Errors = WordErrors ++ NGramErrors.

:- func check_word(token) = cord(prose_error).

check_word(word(Word, Locn)) = Errors :-
    list.map(check_contractions(Word, Locn), contractions,
        ContractionErrorMaybeList),
    list.map(check_hypenations(Word, Locn), invalid_hypenations,
        HypenationErrorMaybeList),
    list.map(check_capitalizations(Word, Locn), acronyms, 
        CapitalisationErrorMaybeList),
    Errors = cord_from_maybe_list(ContractionErrorMaybeList) ++
        cord_from_maybe_list(HypenationErrorMaybeList) ++
        cord_from_maybe_list(CapitalisationErrorMaybeList).

:- pred check_capitalizations(string::in, locn::in,
    string::in, maybe(prose_error)::out) is det.

check_capitalizations(Word, Locn, Acronym, MaybeError) :-
    (
        stri_compare(Word, Acronym, 0),
        Word \= Acronym
    ->
        Message = format("%s is an incorrect capitalization of %s.",
            [s(Word), s(Acronym)]),
        make_error(Locn, Message, Error),
        MaybeError = yes(Error)
    ;
        MaybeError = no
    ).

:- pred check_contractions(string::in, locn::in,
    string::in, maybe(prose_error)::out) is det.

check_contractions(Word, Locn, Contraction, MaybeError) :-
    check_bad_words(Word, Locn, Contraction,
        "Contraction \"%s\" found.", MaybeError).

:- pred check_hypenations(string::in, locn::in,
    string::in, maybe(prose_error)::out) is det.

check_hypenations(Word, Locn, Hypenation, MaybeError) :-
    check_bad_words(Word, Locn, Hypenation,
        "Hypenation is inconsistent \"%s\".", MaybeError).

:- pred check_bad_words(string::in, locn::in,
    string::in, string::in, maybe(prose_error)::out) is det.

check_bad_words(Word, Locn, BadWord, Message, MaybeError) :-
    (
        stri_compare(BadWord, Word, 0)
    ->
        make_error(Locn, format(Message, [s(Word)]), Error),
        MaybeError = yes(Error)
    ;
        MaybeError = no
    ).

:- pred check_ngrams(cord(token)::in, cord(prose_error)::out) is det.

check_ngrams(Tokens, Errors) :-
    TokensArray = array(list(Tokens)),
    array.bounds(TokensArray, Start, End),
    check_ngrams_pos(TokensArray, Start, End, cord.init, Errors).

:- pred check_ngrams_pos(array(token)::in, int::in, int::in,
    cord(prose_error)::in, cord(prose_error)::out) is det.

check_ngrams_pos(Tokens, Pos, End, !Errors) :-
    ( Pos =< End ->
        list.foldl(check_ngram(Tokens, Pos, End), bad_ngrams,
            !Errors),
        check_ngrams_pos(Tokens, Pos+1, End, !Errors)
    ;
        true
    ).

:- pred check_ngram(array(token)::in, int::in, int::in, list(string)::in,
    cord(prose_error)::in, cord(prose_error)::out) is det. 

check_ngram(Tokens, Pos, End, NGram, !Errors) :-
    compare_ngram(Tokens, Pos, End, NGram, Match),
    (
        Match = yes,
        array.lookup(Tokens, Pos, Token),
        Token = word(_, Locn),
        record_error(Locn, format("Bad ngram: %s", [s(string(NGram))]),
            !Errors)
    ;
        Match = no 
    ).

:- pred compare_ngram(array(token)::in, int::in, int::in, list(string)::in,
    bool::out) is det.

compare_ngram(_, _, _, [], yes).
compare_ngram(Tokens, Pos, End, [H | T], Match) :-
    ( Pos > End ->
        Match = no
    ;
        array.lookup(Tokens, Pos, Token),
        Token = word(Word, _),
        ( stri_compare(Word, H, 0) ->
            compare_ngram(Tokens, Pos + 1, End, T, Match)
        ;
            Match = no
        )
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

% Data
%------------------------------------------------------------------------%

:- func contractions = list(string).

contractions = [
    "can't",
    "won't",
    "isn't",
    "it's",
    "arn't",
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
    "they'd",
    "heapsize"
    ].

:- func invalid_hypenations = list(string).

invalid_hypenations = [
    "call-site",
    "type-class",
    "low-level",
    "straight-forward",
    "most-likely",
    "work-stealing",
    "heap-size",
    "one-another"
    ].

:- func acronyms = list(string).

% These must be capitalized here to work.
acronyms = [
    "POSIX",
    "CPU",
    "GHC",
    "GCC",
    "GC"
    ].

:- func bad_ngrams = list(list(string)).

bad_ngrams = [
        ["speeds", "up"]
    ].

% Errors.
%------------------------------------------------------------------------%

:- type prose_error
    --->    prose_error(
                pe_locn         :: locn,
                pe_message      :: string
            ).

:- pred record_error(locn::in, string::in,
    cord(prose_error)::in, cord(prose_error)::out) is det.

record_error(Locn, Message, !Errors) :-
    !:Errors = snoc(!.Errors, prose_error(Locn, Message)).

:- pred make_parse_error(locn::in, maybe(string)::in,
    prose_error::out) is det.

make_parse_error(Locn, no, Error) :-
    Error = prose_error(Locn, "Parse error").
make_parse_error(Locn, yes(Message0), Error) :-
    Message = format("Parse error: %s", [s(Message0)]),
    Error = prose_error(Locn, Message).

:- pred make_error(locn::in, string::in, prose_error::out) is det.

make_error(Locn, Message, prose_error(Locn, Message)).

:- pred print_error(prose_error::in, io::di, io::uo) is det.

print_error(prose_error(Locn, Message), !IO) :-
    Locn = locn(File, Line, Col),
    io.format("%s:%d: col %d, %s\n", [s(File), i(Line), i(Col), s(Message)],
        !IO).

% Utils
%------------------------------------------------------------------------%

:- func cord_concat(cord(cord(T))) = cord(T).

cord_concat(Cords) = cord_list_to_cord(list(Cords)).

:- func cord_from_maybe_list(list(maybe(T))) = cord(T).

cord_from_maybe_list([]) = cord.empty.
cord_from_maybe_list([H | T]) = Cord :-
    Cord0 = cord_from_maybe_list(T),
    (
        H = no,
        Cord = Cord0
    ;
        H = yes(Item),
        Cord = cons(Item, Cord0)
    ).

