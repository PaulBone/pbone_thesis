
:- module tex.

:- interface.

:- import_module string.

:- type macro_is_ref
    --->    macro_is_reference
    ;       macro_is_anchor
    ;       macro_is_not_reference.

:- type macro_breaks_flow
    --->    macro_breaks_flow
    ;       macro_does_not_break_flow.

:- pred macro_info(string::in, macro_is_ref::out, macro_breaks_flow::out)
    is semidet.

:- pred conservative_macro_info(string::in, macro_is_ref::out,
    macro_breaks_flow::out) is det.

:- pred environment_does_not_contain_prose(string::in) is semidet.

:- pred environment_breaks_flow(string::in) is semidet.

:- implementation.

macro_info("cite", macro_is_reference, macro_does_not_break_flow).
macro_info("citep", macro_is_reference, macro_does_not_break_flow).
macro_info("citet", macro_is_reference, macro_does_not_break_flow).
macro_info("ref", macro_is_reference, macro_does_not_break_flow).
macro_info("pageref", macro_is_reference, macro_does_not_break_flow).
macro_info("label", macro_is_anchor, macro_does_not_break_flow).
macro_info("chapter", macro_is_not_reference, macro_breaks_flow).
macro_info("section", macro_is_not_reference, macro_breaks_flow).
macro_info("subsection", macro_is_not_reference, macro_breaks_flow).
macro_info("subsubsection", macro_is_not_reference, macro_breaks_flow).

conservative_macro_info(Ident, IsRef, BreaksFlow) :-
    ( macro_info(Ident, IsRefPrime, BreaksFlowPrime) ->
        IsRef = IsRefPrime,
        BreaksFlow = BreaksFlowPrime
    ;
        IsRef = macro_is_not_reference,
        BreaksFlow = macro_does_not_break_flow
    ).

environment_does_not_contain_prose("array").
environment_does_not_contain_prose("verbatim").
environment_does_not_contain_prose("algorithmic").
environment_does_not_contain_prose("$$").

environment_breaks_flow("algorithm").
environment_breaks_flow("table").
environment_breaks_flow("figure").

