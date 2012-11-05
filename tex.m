
:- module tex.

:- interface.

:- import_module string.

:- type macro_is_ref
    --->    macro_is_reference
    ;       macro_is_anchor
    ;       macro_is_not_reference.

:- inst ref_or_anchor
    --->    macro_is_reference
    ;       macro_is_anchor.

:- type macro_breaks_flow
    --->    macro_breaks_flow
    ;       macro_does_not_break_flow.

:- type contains_prose
    --->    contains_prose
    ;       does_not_contain_prose
    ;       replace_with_noun.

:- pred conservative_macro_info(string::in, macro_is_ref::out,
    macro_breaks_flow::out, contains_prose::out) is det.

:- pred environment_does_not_contain_prose(string::in) is semidet.

:- pred environment_breaks_flow(string::in) is semidet.

:- implementation.

:- pred macro_is_ref_or_anchor(string::in, macro_is_ref::out(ref_or_anchor))
    is semidet.

macro_is_ref_or_anchor("cite",      macro_is_reference).
macro_is_ref_or_anchor("citep",     macro_is_reference).
macro_is_ref_or_anchor("citet",     macro_is_reference).
macro_is_ref_or_anchor("ref",       macro_is_reference).
macro_is_ref_or_anchor("pageref",   macro_is_reference).
macro_is_ref_or_anchor("label",     macro_is_anchor).

:- pred macro_breaks_flow(string::in) is semidet.

macro_breaks_flow("chapter").
macro_breaks_flow("section").
macro_breaks_flow("subsection").
macro_breaks_flow("subsubsection").
macro_breaks_flow("footnote").

:- inst does_not_contain_prose
    ---> does_not_contain_prose
    ;    replace_with_noun.

:- pred macro_does_not_contain_prose(string::in,
    contains_prose::out(does_not_contain_prose)) is semidet.

macro_does_not_contain_prose(Macro, ContainsProse) :-
    (
        (
            Macro = "code",
            ContainsProsePrime = replace_with_noun
        ;
            Macro = "input",
            ContainsProsePrime = does_not_contain_prose
        )
    ->
        ContainsProse = ContainsProsePrime
    ;
        macro_is_ref_or_anchor(Macro, RefOrAnchor),
        (
            RefOrAnchor = macro_is_reference,
            ContainsProse = replace_with_noun
        ;
            RefOrAnchor = macro_is_anchor,
            ContainsProse = does_not_contain_prose
        )
    ).

conservative_macro_info(Ident, IsRef, BreaksFlow, ContainsProse) :-
    ( macro_is_ref_or_anchor(Ident, IsRefPrime) ->
        IsRef = IsRefPrime
    ;
        IsRef = macro_is_not_reference
    ),
    ( macro_breaks_flow(Ident) ->
        BreaksFlow = macro_breaks_flow
    ;
        BreaksFlow = macro_does_not_break_flow
    ),
    ( macro_does_not_contain_prose(Ident, ContainsProsePrime) ->
        ContainsProse = ContainsProsePrime
    ;
        ContainsProse = contains_prose
    ).

environment_does_not_contain_prose("array").
environment_does_not_contain_prose("verbatim").
environment_does_not_contain_prose("algorithmic").
environment_does_not_contain_prose("$$").

environment_breaks_flow("algorithm").
environment_breaks_flow("table").
environment_breaks_flow("figure").

