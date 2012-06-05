% vim: ts=4 sw=4 et
%------------------------------------------------------------------------%

:- module result.

%------------------------------------------------------------------------%

:- interface.

:- import_module cord.
:- import_module float.
:- import_module maybe.
:- import_module string.

%------------------------------------------------------------------------%

    % A result is the name of the test (same as t_name above),
    % and how long that test took.
    %
:- type result
    --->    result(
                r_name          :: string,
                r_time          :: float 
            ).

%------------------------------------------------------------------------%

:- func get_name(result) = string.

:- func get_time(result) = float.

:- pred print_results(cord(result.result)::in, cord(string)::out) is det.

:- pred parse_result(string::in, maybe(result.result)::out) is det.

%------------------------------------------------------------------------%
%------------------------------------------------------------------------%

:- implementation.

:- import_module list.

%------------------------------------------------------------------------%

get_name(R) = R ^ r_name.

get_time(R) = R ^ r_time.

%------------------------------------------------------------------------%

print_results(Results, Cord) :-
    Items = map(print_result, Results),
    Cord = foldl((++), Items, init).

:- func print_result(result.result) = cord(string).

print_result(result(Name, Time)) =
    singleton(format("%s %.2f\n", [s(Name), f(Time)])).

%------------------------------------------------------------------------%

parse_result(String, MaybeResult) :-
    (
        [Name, Num] = words(String),
        string.to_float(Num, Numf)
    ->
        MaybeResult = yes(result(Name, Numf))
    ;
        MaybeResult = no
    ).

