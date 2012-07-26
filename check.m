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
:- import_module util.
:- import_module tex.

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
    Words = words(Tokens),
    WordErrors = cord_concat(cord.map(check_word, Words)),
    check_ngrams(Tokens, NGramErrors),
    Errors = WordErrors ++ NGramErrors.

:- func check_word(word) = cord(prose_error).

check_word(word(Word, Locn)) = Errors :-
    list.map(check_contractions(Word, Locn), contractions,
        ContractionErrorMaybeList),
    list.map(check_hypenations(Word, Locn), invalid_hypenations,
        HypenationErrorMaybeList),
    list.map(check_macro_words(Word, Locn), macro_words,
        MacroWordsErrorMaybeList),
    list.map(check_capitalizations(Word, Locn), acronyms ++ names,
        CapitalisationErrorMaybeList),
    Errors = cord_from_maybe_list(ContractionErrorMaybeList) ++
        cord_from_maybe_list(HypenationErrorMaybeList) ++
        cord_from_maybe_list(MacroWordsErrorMaybeList) ++
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

:- pred check_macro_words(string::in, locn::in,
    string::in, maybe(prose_error)::out) is det.

check_macro_words(Word, Locn, MacroWord, MaybeError) :-
    check_bad_words(Word, Locn, MacroWord,
        "This word should be typed with a macro \"%s\".", MaybeError).

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
    tokens_to_consecutive_words(Tokens, Words),
    cord.map_pred(check_ngrams_words, Words, Errorss),
    Errors = cord_concat(Errorss).

:- pred check_ngrams_words(cord(word)::in, cord(prose_error)::out) is det.

check_ngrams_words(Words, Errors) :-
    WordsArray = array(list(Words)),
    array.bounds(WordsArray, Start, End),
    check_ngrams_pos(WordsArray, Start, End, cord.init, Errors).

:- pred check_ngrams_pos(array(word)::in, int::in, int::in,
    cord(prose_error)::in, cord(prose_error)::out) is det.

check_ngrams_pos(Words, Pos, End, !Errors) :-
    ( Pos =< End ->
        list.foldl(check_ngram(Words, Pos, End), bad_ngrams,
            !Errors),
        check_double(Words, Pos, End, !Errors),
        check_ngrams_pos(Words, Pos+1, End, !Errors)
    ;
        true
    ).

:- pred check_ngram(array(word)::in, int::in, int::in, list(string)::in,
    cord(prose_error)::in, cord(prose_error)::out) is det.

check_ngram(Words, Pos, End, NGram, !Errors) :-
    compare_ngram(Words, Pos, End, NGram, Match),
    (
        Match = yes,
        array.lookup(Words, Pos, Word),
        Word = word(_, Locn),
        record_error(Locn, format("Bad ngram: %s", [s(string(NGram))]),
            !Errors)
    ;
        Match = no
    ).

:- pred check_double(array(word)::in, int::in, int::in,
    cord(prose_error)::in, cord(prose_error)::out) is det.

check_double(Words, Pos, End, !Errors) :-
    ( Pos = End ->
        true
    ;
        array.lookup(Words, Pos, WordA),
        array.lookup(Words, Pos+1, WordB),
        WordA = word(WordStrA, Locn),
        WordB = word(WordStrB, _),
        (
            WordStrA \= "~",
            stri_compare(WordStrA, WordStrB, 0)
        ->
            record_error(Locn, format("Double word: %s", [s(WordStrA)]),
                !Errors)
        ;
            true
        )
    ).

:- pred compare_ngram(array(word)::in, int::in, int::in, list(string)::in,
    bool::out) is det.

compare_ngram(_, _, _, [], yes).
compare_ngram(Words, Pos, End, [H | T], Match) :-
    ( Pos > End ->
        Match = no
    ;
        array.lookup(Words, Pos, Word),
        (
            Word = word(WordStr, _),
            stri_compare(WordStr, H, 0)
        ->
            compare_ngram(Words, Pos + 1, End, T, Match)
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
    "one-another",
    "no-longer"
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

% Proper nouns that should begin with a capital letter.
:- func names = list(string).

names = [
    "Mercury"
    ].

% Words that should be written with macros.
:- func macro_words = list(string).

macro_words = [
    "ThreadScope"
    ].

:- func bad_ngrams = list(list(string)).

bad_ngrams = [
        ["time", "speeds", "up"],
        ["can", "not"],
        ["round", "robin"]
    ].

% Convert the document into different data structures.
%----------------------------------------------------------------------------%

    % Build a cord of cords containing consecutive words.
    %
    % Words are consecutive when not seperated by a macro unless the macro
    % is something like \emph or \code.
    %
:- pred tokens_to_consecutive_words(cord(token)::in, cord(cord(word))::out)
    is det.

tokens_to_consecutive_words(Tokens, Words) :-
    list.foldl2(token_to_consecutive_words, list(Tokens), cord.empty, LastGroup,
        cord.empty, Groups),
    Words = snoc(Groups, LastGroup).

:- pred token_to_consecutive_words(token::in,
    cord(word)::in, cord(word)::out,
    cord(cord(word))::in, cord(cord(word))::out) is det.

token_to_consecutive_words(word(Word, Locn), !LastGroup, !Groups) :- 
    !:LastGroup = snoc(!.LastGroup, word(Word, Locn)).
token_to_consecutive_words(punct(_, _), !LastGroup, !Groups) :-
    end_group(!LastGroup, !Groups).
token_to_consecutive_words(macro(Name, Args, Locn), !LastGroup, !Groups) :-
    ( length(Name, 1) ->
        % Short macros usually just stand for a letter, or sometimes a line
        % break.
        ( Name = "\\" ->
            % Linebreak, this is whitespace and is ignored.
            true
        ; Name = " " ->
            % A tex whitespace hint, this is also ignored.
            true
        ;
            % Some other short charicter, usually punctuation.
            end_group(!LastGroup, !Groups)
        )
    ;
        conservative_macro_info(Name, IsRef, BreaksFlow),
        (
            IsRef = macro_is_reference,
            % Reference macros don't get checked, just pretend that they're a
            % proper noun.
            !:LastGroup = snoc(!.LastGroup, word("RefNoun", Locn))
        ;
            IsRef = macro_is_anchor
        ;
            IsRef = macro_is_not_reference,
            % The macro breaks the flow of text.
            foldl2(macro_arg_to_consecutive_words(BreaksFlow),
                list(Args), !LastGroup, !Groups)
        )
    ).
token_to_consecutive_words(environment(Name, _Args, Tokens, _), !LastGroup,
        !Groups) :-
    ( not environment_does_not_contain_prose(Name) ->
        % Note that we don't check the environments args, I don't know of
        % any that have prose to check.
        list.foldl2(token_to_consecutive_words, list(Tokens),
            !LastGroup, !Groups)
    ;
        true
    ).

:- pred macro_arg_to_consecutive_words(macro_breaks_flow::in, macro_arg::in,
    cord(word)::in, cord(word)::out,
    cord(cord(word))::in, cord(cord(word))::out) is det.

macro_arg_to_consecutive_words(BreaksFlow, macro_arg(Tokens, _),
        !LastGroup, !Groups) :-
    (
        BreaksFlow = macro_breaks_flow,
        end_group(!LastGroup, !Groups)
    ;
        BreaksFlow = macro_does_not_break_flow
    ),
    list.foldl2(token_to_consecutive_words, list(Tokens),
        !LastGroup, !Groups),
    (
        BreaksFlow = macro_breaks_flow,
        end_group(!LastGroup, !Groups)
    ;
        BreaksFlow = macro_does_not_break_flow
    ).

:- pred end_group(cord(T)::in, cord(T)::out,
    cord(cord(T))::in, cord(cord(T))::out) is det.

end_group(!LastGroup, !Groups) :-
    !:Groups = snoc(!.Groups, !.LastGroup),
    !:LastGroup = cord.empty.

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

