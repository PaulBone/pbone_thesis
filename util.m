
:- module util.

:- interface.

:- import_module cord.
:- import_module list.
:- import_module maybe.

:- func cord_concat(cord(cord(T))) = cord(T).

:- func cord_from_maybe_list(list(maybe(T))) = cord(T).

:- implementation.

cord_concat(Cords) = cord_list_to_cord(list(Cords)).

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

