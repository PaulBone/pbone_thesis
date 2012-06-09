
:- module complex.

:- interface.

:- import_module float.

:- type complex
    --->    complex(
                c_real          :: float,
                c_imaginary     :: float
            ).

:- func complex + complex = complex.

:- func complex * complex = complex.

:- func magnitude(complex) = float.

:- implementation.

:- import_module math.

C1 + C2 = C :-
    C1 = complex(C1r, C1i),
    C2 = complex(C2r, C2i),
    Cr = C1r + C2r,
    Ci = C1i + C2i,
    C = complex(Cr, Ci).

C1 * C2 = C :-
    C1 = complex(C1r, C1i),
    C2 = complex(C2r, C2i),
    Cr = C1r * C2r - C1i * C2i,
    Ci = C1r * C2i + C2r * C1i,
    C = complex(Cr, Ci).

magnitude(C) = M :-
    R = C ^ c_real,
    I = C ^ c_imaginary,
    M = sqrt(R*R + I*I).

