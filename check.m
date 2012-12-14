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
:- import_module counter.
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
        %trace [io(!IO)] (
        %    write_doc(pretty_tokens(Tokens), !IO)
        %),
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
    Macros = macros(Tokens),
    MacroErrors = cord_concat(cord.map(check_macro, Macros)),
    Errors = WordErrors ++ NGramErrors ++ MacroErrors.

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

% Checking of ngrams
%------------------------------------------------------------------------%

:- pred check_ngrams(cord(token)::in, cord(prose_error)::out) is det.

check_ngrams(Tokens, Errors) :-
    tokens_to_consecutive_words(Tokens, Words, counter.init(0), _),
    cord.map_pred(check_ngrams_words, Words, NgramErrorss),
    NgramErrors = cord_concat(NgramErrorss),
    tokens_to_references(Tokens, References),
    cord.map_pred(check_reference, References, ReferenceErrorss),
    ReferenceErrors = cord_concat(ReferenceErrorss),
    Errors = NgramErrors ++ ReferenceErrors.

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

% Checking of macros
%------------------------------------------------------------------------%

:- func check_macro(macro_or_environment) = cord(prose_error).

check_macro(macro(Name, Args, Locn)) = Errors :-
    some [!Errors] (
        !:Errors = empty,
        ( Name = "cite" ->
            record_error(Locn,
                "Bad citation, one of the natbib macros.",
                !Errors)
        ;
            true
        ),
        ArgErrors = cord_concat(cord.map(check_macro_arg, Args)),
        Errors = !.Errors ++ ArgErrors
    ).
check_macro(environment(_Name, Args, Macros, _Locn)) =
        ArgErrors ++ MacrosErrors :-
    ArgErrors = cord_concat(cord.map(check_macro_arg, Args)),
    MacrosErrors = cord_concat(cord.map(check_macro, Macros)).

:- func check_macro_arg(macro_arg(macro_or_environment)) = cord(prose_error).

check_macro_arg(macro_arg(Contents, _Locn)) =
    cord_concat(cord.map(check_macro, Contents)).

% Checking of references.
%------------------------------------------------------------------------%

:- pred check_reference(reference::in, cord(prose_error)::out) is det. 

check_reference(reference(MaybeWordPart, _Label, MaybePrefix, Type, SawNBSP, Locn),
        Errors) :-
    some [!Errors] (
        !:Errors = empty,
        (
            MaybeWordPart = no,
            record_error(Locn,
                "Reference without word before it",
                !Errors)
        ;
            MaybeWordPart = yes(WordPart),
            (
                word_part_matches_type(WordPart, Type)
            ->
                true
            ;
                record_error(Locn,
                    "Word part of reference does not match reference type",
                    !Errors)
            ),
            (
                SawNBSP = saw_nbsp
            ;
                SawNBSP = did_not_see_nbsp,
                record_error(Locn,
                    "Missing NBSP between word and reference",
                    !Errors)
            )
        ),
        (
            MaybePrefix = yes(Prefix),
            Type = rt_unknown
        ->
            record_error(Locn,
                format("Unknown reference prefix %s", [s(Prefix)]),
                !Errors)
        ;
            true
        ),
        Errors = !.Errors
    ).

:- pred word_part_matches_type(string, reference_type).
:- mode word_part_matches_type(in, in) is semidet.

word_part_matches_type(_, rt_unknown).
word_part_matches_type("page", rt_page_of(_Type)).
word_part_matches_type("Page", rt_page_of(_Type)).
word_part_matches_type("Chapter", rt_chapter).
word_part_matches_type("Chapters", rt_chapter).
word_part_matches_type("Section", rt_section).
word_part_matches_type("Sections", rt_section).
word_part_matches_type("Figure", rt_figure).
word_part_matches_type("Figures", rt_figure).
word_part_matches_type("Table", rt_table).
word_part_matches_type("Tables", rt_table).
word_part_matches_type("Algorithm", rt_algorithm).
word_part_matches_type("Algorithms", rt_algorithm).
word_part_matches_type("Equation", rt_equation).
word_part_matches_type("Equaiions", rt_equation).
word_part_matches_type("and", Type) :-
    Type \= rt_page_of(_).

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
    "let's",
    %"lets",
    "heapsize",
    "callgraph"
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
    "no-longer",
    "re-order",
    "re-ordering",
    "re-ordered",
    "shut-down",
    "call-graph",
    "worth-while",
    "dead-lock",
    "dead-locks"
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
    "Mercury",
    "Forth",
    "Prolog",
    "Haskell",
    "Fortran",
    "Erlang"
    ].

% Words that should be written with macros.
:- func macro_words = list(string).

macro_words = [
    "ThreadScope"
    ].

:- func bad_ngrams = list(list(string)).

bad_ngrams = [
        % Bad phrase
        ["time", "speeds", "up"],

        % These should be one word.
        ["can", "not"],
        ["round", "robin"],
        ["worth", "while"],
        ["left", "most"],
        ["right", "most"],
        ["dead", "lock"],
        ["dead", "locks"],
        ["miss", "estimate"],
        ["miss", "estimated"],
        ["mis", "estimate"],
        ["mis", "estimated"],
        ["miss", "calculate"],
        ["miss", "calculated"],
        ["mis", "calculate"],
        ["mis", "calculated"],
        ["non", "deterministic"],

        % Hypenate this.
        ["if", "then", "else"],
        ["if", "then", "elses"],
        ["if", "then", "else's"]
       
        %["collection", "time"],
        %["mutation", "time"]
    ].

% Convert the document into different data structures.
%----------------------------------------------------------------------------%

    % Build a cord of cords containing consecutive words.
    %
    % Words are consecutive when not seperated by a macro unless the macro
    % is something like \emph or \code.
    %
:- pred tokens_to_consecutive_words(cord(token)::in, cord(cord(word))::out,
    counter::in, counter::out) is det.

tokens_to_consecutive_words(Tokens, Words, !IDs) :-
    list.foldl3(token_to_consecutive_words, list(Tokens), cord.empty, LastGroup,
        cord.empty, Groups, !IDs),
    Words = snoc(Groups, LastGroup).

:- pred token_to_consecutive_words(token::in,
    cord(word)::in, cord(word)::out,
    cord(cord(word))::in, cord(cord(word))::out,
    counter::in, counter::out) is det.

token_to_consecutive_words(word(Word, Locn), !LastGroup, !Groups, !IDs) :-
    !:LastGroup = snoc(!.LastGroup, word(Word, Locn)).
token_to_consecutive_words(punct(_, _), !LastGroup, !Groups, !IDs) :-
    end_group(!LastGroup, !Groups).
token_to_consecutive_words(macro(Name, Args, Locn), !LastGroup, !Groups,
        !IDs) :-
    ( length(Name, 1) ->
        % Short macros usually just stand for a letter, or sometimes a line
        % break.
        ( Name = "\\" ->
            % Linebreak, this is whitespace and is ignored.
            true
        ;
            ( Name = " "
            ; Name = "~"
            )
        ->
            % A tex whitespace hint, this is also ignored.
            true
        ;
            % Some other short charicter, usually punctuation.
            end_group(!LastGroup, !Groups)
        )
    ;
        conservative_macro_info(Name, IsRef, BreaksFlow, ContainsProse),
        (
            IsRef = macro_is_reference,
            % Reference macros don't get checked, just pretend that they're a
            % proper noun.
            insert_noun("RefNoun", Locn, !LastGroup, !IDs)
        ;
            ( IsRef = macro_is_anchor
            ; IsRef = macro_is_not_reference
            ),
            (
                ContainsProse = contains_prose,
                % The macro breaks the flow of text.
                foldl3(macro_arg_to_consecutive_words(BreaksFlow),
                    list(Args), !LastGroup, !Groups, !IDs)
            ;
                ContainsProse = replace_with_noun,
                maybe_end_group(BreaksFlow, !LastGroup, !Groups),
                insert_noun("Noun", Locn, !LastGroup, !IDs),
                maybe_end_group(BreaksFlow, !LastGroup, !Groups)
            ;
                ContainsProse = does_not_contain_prose,
                maybe_end_group(BreaksFlow, !LastGroup, !Groups)
            )
        )
    ).
token_to_consecutive_words(environment(Name, _Args, Tokens, _), !LastGroup,
        !Groups, !IDs) :-
    ( environment_breaks_flow(Name) ->
        BreaksFlow = macro_breaks_flow,
        end_group(!LastGroup, !Groups)
    ;
        BreaksFlow = macro_does_not_break_flow
    ),
    ( not environment_does_not_contain_prose(Name) ->
        % Note that we don't check the environments args, I don't know of
        % any that have prose to check.
        list.foldl3(token_to_consecutive_words, list(Tokens),
            !LastGroup, !Groups, !IDs)
    ;
        true
    ),
    maybe_end_group(BreaksFlow, !LastGroup, !Groups).

:- pred insert_noun(string::in, locn::in, cord(word)::in, cord(word)::out,
    counter::in, counter::out) is det.

insert_noun(Name, Locn, !LastGroup, !IDs) :-
    allocate(Num, !IDs),
    !:LastGroup = snoc(!.LastGroup,
        word(format("%s_%d", [s(Name), i(Num)]), Locn)).

:- pred macro_arg_to_consecutive_words(macro_breaks_flow::in,
    macro_arg(token)::in, cord(word)::in, cord(word)::out,
    cord(cord(word))::in, cord(cord(word))::out, counter::in, counter::out)
    is det.

macro_arg_to_consecutive_words(BreaksFlow, macro_arg(Tokens, _),
        !LastGroup, !Groups, !IDs) :-
    (
        BreaksFlow = macro_breaks_flow,
        end_group(!LastGroup, !Groups)
    ;
        BreaksFlow = macro_does_not_break_flow
    ),
    list.foldl3(token_to_consecutive_words, list(Tokens),
        !LastGroup, !Groups, !IDs),
    (
        BreaksFlow = macro_breaks_flow,
        end_group(!LastGroup, !Groups)
    ;
        BreaksFlow = macro_does_not_break_flow
    ).

:- pred maybe_end_group(macro_breaks_flow::in,
    cord(T)::in, cord(T)::out, cord(cord(T))::in, cord(cord(T))::out) is det.

maybe_end_group(macro_breaks_flow, !LastGroup, !Groups) :-
    end_group(!LastGroup, !Groups).
maybe_end_group(macro_does_not_break_flow, !LastGroup, !Groups).

:- pred end_group(cord(T)::in, cord(T)::out,
    cord(cord(T))::in, cord(cord(T))::out) is det.

end_group(!LastGroup, !Groups) :-
    !:Groups = snoc(!.Groups, !.LastGroup),
    !:LastGroup = cord.empty.

%------------------------------------------------------------------------%

:- type reference
    --->    reference(
                r_word_part     :: maybe(string),
                r_label         :: string,
                r_label_prefix  :: maybe(string),
                r_type          :: reference_type,
                r_nbsp          :: saw_nbsp,
                r_locn          :: locn
            ).

:- type reference_type
    --->    rt_unknown
    ;       rt_chapter
    ;       rt_section
    ;       rt_figure
    ;       rt_table
    ;       rt_algorithm
    ;       rt_equation
    ;       rt_page_of(reference_type).

:- type saw_nbsp
    --->    saw_nbsp
    ;       did_not_see_nbsp.

:- pred tokens_to_references(cord(token)::in, cord(reference)::out) is det.

tokens_to_references(Tokens, References) :-
    tokens_to_references(Tokens, no, did_not_see_nbsp, empty, References).

:- pred tokens_to_references(cord(token)::in, maybe(string)::in, saw_nbsp::in,
    cord(reference)::in, cord(reference)::out) is det.

tokens_to_references(Tokens0, MaybeWord0, SawNBSP0, !References) :-
    ( cord.head_tail(Tokens0, Token, Tokens) ->
        (
            (
                Token = word(Word, _),
                MaybeWord = yes(Word),
                SeenNBSP = did_not_see_nbsp
            ;
                Token = punct(_, _),
                MaybeWord = no,
                SeenNBSP = did_not_see_nbsp
            ),
            tokens_to_references(Tokens, MaybeWord, SeenNBSP, !References) 
        ;
            Token = macro(Name, Args, Locn),
            ( Name = "~" ->
                SeenNBSP = saw_nbsp,
                MaybeWord = MaybeWord0,
                tokens_to_references(Tokens, MaybeWord, SeenNBSP, !References) 
            ;
                ( Name = "ref"
                ; Name = "pageref"
                )
            ->
                start_reference(Name, Args, Locn, MaybeWord0, SawNBSP0,
                    Tokens, !References)
            ;
                macro_to_references(Name, Args, Tokens, MaybeWord0,
                    SawNBSP0, !References)
            )
        ;
            Token = environment(_Name, _Args, EnvTokens, _Locn),
            environment_to_references(EnvTokens, Tokens, !References)
        )
    ;
        true
    ).

:- pred start_reference(string::in, cord(macro_arg(token))::in, locn::in,
    maybe(string)::in, saw_nbsp::in, cord(token)::in,
    cord(reference)::in, cord(reference)::out) is det.

start_reference(Name, Args, Locn, MaybeWord, SawNBSP, Tokens, !References) :-
    ref_macro_to_reference(Name, Args, Locn, MaybeWord, SawNBSP, Reference),
    !:References = snoc(!.References, Reference),
    % We parse any following references by modeling a state machine, each
    % predicate represents a state.
    continue_reference(Tokens, Reference, did_not_see_nbsp, !References).

:- pred continue_reference(cord(token)::in, reference::in,
    saw_nbsp::in, cord(reference)::in, cord(reference)::out) is det.

continue_reference(Tokens0, FirstReference, SawNBSP0, !References) :-
    ( cord.head_tail(Tokens0, Token, Tokens) ->
        (
            Token = word(Word, _),
            ( Word = "and" ->
                continue_reference(Tokens, FirstReference, did_not_see_nbsp,
                    !References)
            ;
                tokens_to_references(Tokens, yes(Word), did_not_see_nbsp, 
                    !References)
            )
        ;
            Token = punct(Punct, _),
            ( Punct = "," ->
                continue_reference(Tokens, FirstReference, did_not_see_nbsp,
                    !References)
            ;
                tokens_to_references(Tokens, no, did_not_see_nbsp,
                    !References)
            )
        ;
            Token = macro(Name, Args, Locn),
            ( Name = "~" ->
                continue_reference(Tokens, FirstReference, saw_nbsp,
                    !References)
            ;
                ( Name = "ref"
                ; Name = "pageref"
                )
            ->
                ref_macro_to_reference(Name, Args, Locn,
                    FirstReference ^ r_word_part, SawNBSP0, Reference),
                !:References = snoc(!.References, Reference),
                continue_reference(Tokens, FirstReference, did_not_see_nbsp,
                    !References)
            ;
                macro_to_references(Name, Args, Tokens, no,
                    did_not_see_nbsp, !References)
            )
        ;
            Token = environment(_Name, _Args, EnvTokens, _Locn),
            environment_to_references(EnvTokens, Tokens, !References)
        )
    ;
        true
    ).

:- pred ref_macro_to_reference(string::in, cord(macro_arg(token))::in, locn::in,
    maybe(string)::in, saw_nbsp::in, reference::out) is det.

ref_macro_to_reference(Name, Args, Locn, MaybeWord, SawNBSP, Reference) :-
    ( cord.head_tail(Args, FirstArg, _) ->
        FirstArg = macro_arg(ArgTokens, _),
        LabelStrings = pretty_tokens(ArgTokens),
        Label = strip(append_list(list(LabelStrings)))
    ;
        Label = ""
    ),
    (
        LabelParts = split_at_char(':', Label),
        LabelParts = [LabelPrefix | _LabelRest],
        length(LabelParts) > 1
    ->
        MaybeLabelPrefix = yes(LabelPrefix),
        (
            ref_type(LabelPrefix, TypePrime)
        -> 
            Type0 = TypePrime
        ;
            Type0 = rt_unknown
        )
    ;
        MaybeLabelPrefix = no,
        Type0 = rt_unknown
    ),
    ( Name = "pageref" ->
        Type = rt_page_of(Type0)
    ;
        Type = Type0
    ),
    Reference = reference(MaybeWord, Label, MaybeLabelPrefix, Type,
        SawNBSP, Locn).

:- pred macro_to_references(string::in, cord(macro_arg(token))::in,
    cord(token)::in, maybe(string)::in, saw_nbsp::in, 
    cord(reference)::in, cord(reference)::out) is det.

macro_to_references(Name, Args, Tokens, MaybeWord, SeenNBSP, !References) :-
    conservative_macro_info(Name, _, BreaksFlow, ContainsProse),
    (
        ContainsProse = contains_prose,
        ArgsTokens = cord_concat(map((func(A) = A ^ ma_contents),
            Args))
    ;
        ( ContainsProse = replace_with_noun
        ; ContainsProse = does_not_contain_prose
        ),
        ArgsTokens = empty
    ),
    (
        BreaksFlow = macro_breaks_flow,
        tokens_to_references(ArgsTokens, no, did_not_see_nbsp,
            !References),
        tokens_to_references(Tokens, no, did_not_see_nbsp,
            !References)
    ;
        BreaksFlow = macro_does_not_break_flow,
        tokens_to_references(ArgsTokens ++ Tokens, MaybeWord,
            SeenNBSP, !References)
    ).

:- pred environment_to_references(cord(token)::in, cord(token)::in,
    cord(reference)::in, cord(reference)::out) is det.

environment_to_references(EnvTokens, Tokens, !References) :-
    tokens_to_references(EnvTokens, no, did_not_see_nbsp,
        !References),
    tokens_to_references(Tokens, no, did_not_see_nbsp,
        !References).

:- pred ref_type(string, reference_type).
:- mode ref_type(in, out) is semidet.

ref_type("chap", rt_chapter).
ref_type("sec", rt_section).
ref_type("fig", rt_figure).
ref_type("tab", rt_table).
ref_type("alg", rt_algorithm).
ref_type("eqn", rt_equation).

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

