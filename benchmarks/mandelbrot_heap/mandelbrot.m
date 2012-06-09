:- module mandelbrot.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module cord.
:- import_module float.
:- import_module getopt.
:- import_module int.
:- import_module list.
:- import_module math.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.

:- import_module complex.
:- import_module vec.

main(!IO) :-
    command_line_arguments(Args, !IO),
    OptionOps = option_ops_multi(short_options, long_options, default_options),
    getopt.process_options(OptionOps, Args, NonOptionsArgs, GetoptResult),
    (
        GetoptResult = ok(OptionTable),
        (
            NonOptionsArgs = [],
            getopt.lookup_bool_option(OptionTable, help, Help),
            (
                Help = yes,
                usage(!IO),
                Result = ok
            ;
                Help = no,
                process_options(OptionTable, MaybeOptions),
                (
                    MaybeOptions = ok(Options),
                    real_main(Options, !IO),
                    Result = ok
                ;
                    MaybeOptions = error(Error),
                    Result = error(format("Error processing options: %s\n",
                        [s(Error)]))
                )
            )
        ;
            NonOptionsArgs = [FirstArg | _],
            Result = error(format("Error processing argument: %s\n",
                [s(FirstArg)]))
        )
    ;
        GetoptResult = error(Error),
        Result = error(format("Error processing options: %s\n",
            [s(Error)]))
    ),

    (
        Result = ok
    ;
        Result = error(ErrorMessage),
        write_string(stderr_stream, ErrorMessage, !IO),
        usage(!IO),
        io.set_exit_status(1, !IO)
    ).

:- type option
    --->    help
    ;       dim_x
    ;       dim_y
    ;       dependent_conjunctions
    ;       left_recursion
    ;       sequential.

:- pred short_options(char::in, option::out) is semidet.

short_options('h', help).
short_options('?', help).
short_options('x', dim_x).
short_options('y', dim_y).
short_options('d', dependent_conjunctions).
short_options('l', left_recursion).
short_options('s', sequential).

:- pred long_options(string::in, option::out) is semidet.

long_options("help", help).
long_options("dependent-conjunctions", dependent_conjunctions).
long_options("left-recursion", left_recursion).
long_options("sequential", sequential).

:- pred default_options(option::out, option_data::out) is multi.

default_options(help,                   bool(no)).
default_options(dim_x,                  maybe_int(no)).
default_options(dim_y,                  maybe_int(no)).
default_options(dependent_conjunctions, bool(no)).
default_options(left_recursion,         bool(no)).
default_options(sequential,             bool(no)).

:- type options
    --->    options(
                opts_dim_x              :: int,
                opts_dim_y              :: int,
                opts_use_dep_conjs      :: use_dependent_conjunctions,
                opts_sequential         :: sequential
            ).

:- type use_dependent_conjunctions
    --->    use_dependent_conjunctions
    ;       use_independent_conjunctions
    ;       use_left_independent_conjunctions.

:- type sequential
    --->    sequential_execution
    ;       parallel_execution.

:- pred process_options(option_table(option)::in, maybe_error(options)::out)
    is det.

process_options(Table, MaybeOptions) :-
    getopt.lookup_bool_option(Table, dependent_conjunctions, DepConjsBool),
    getopt.lookup_bool_option(Table, left_recursion, LeftRecursion),
    (
        DepConjsBool = yes,
        (
            LeftRecursion = yes,
            error("Left recursion and dependent conjunctions not supported")
        ;
            LeftRecursion = no,
            DepConjs = use_dependent_conjunctions
        )
    ;
        DepConjsBool = no,
        (
            LeftRecursion = yes,
            DepConjs = use_left_independent_conjunctions
        ;
            LeftRecursion = no,
            DepConjs = use_independent_conjunctions
        )
    ),

    getopt.lookup_bool_option(Table, sequential, SequentialBool),
    (
        SequentialBool = yes,
        Sequential = sequential_execution
    ;
        SequentialBool = no,
        Sequential = parallel_execution
    ),

    getopt.lookup_maybe_int_option(Table, dim_x, MaybeX),
    getopt.lookup_maybe_int_option(Table, dim_y, MaybeY),
    (
        (
            MaybeX = yes(DimX),
            MaybeY = yes(DimY)
        ;
            MaybeX = no,
            MaybeY = no,
            dimension(DimX, DimY)
        ),
        MaybeOptions = ok(options(DimX, DimY, DepConjs, Sequential))
    ;
        (
            MaybeX = yes(_),
            MaybeY = no
        ;
            MaybeX = no,
            MaybeY = yes(_)
        ),
        MaybeOptions = error("Specify both of -x and -y or neither of them")
    ).

:- pred usage(io::di, io::uo) is det.

usage(!IO) :-
    io.progname("mandelbrot", ProgName, !IO),
    format("Usage: %s <opts>\n", [s(ProgName)], !IO),
    write_string("<opts> may be one or more of:\n", !IO),
    write_string("\t-x X -y Y\n", !IO),
    write_string("\t\tThe dimensions of the image, specify neither or both", !IO),
    write_string("\t-d --dependent-conjunctions\n", !IO),
    write_string("\t\tUse an accumulator to represent the rows rendered so far", !IO).

:- pred real_main(options::in, io::di, io::uo) is det.

real_main(Options, !IO) :-
    Options = options(DimX, DimY, _, _),
    viewport(StartX, StartY, Length, Height),
    StepX = Length / float(DimX),
    StepY = Height / float(DimY),
    draw_image(Options, vecf(StartX, StartY), StepX, StepY, veci(DimX, DimY),
        Image),
    write_ppm(Image, !IO).

:- pred draw_image(options::in, vecf::in, float::in, float::in,
    veci::in, image::out) is det.

draw_image(Options, Start, StepX, StepY, Dim,
        image(Dim, Rows)) :-
    draw_rows(Options, Start, StepX, StepY, Dim, Rows).

:- pred draw_rows(options::in, vecf::in, float::in, float::in,
    veci::in, cord(colour)::out) is det.

draw_rows(Options, Start, StepX, StepY, Dim, Rows) :-
    Start = vecf(StartX, StartY),
    Dim = veci(DimX, DimY),
    pos_list(StartY, StepY, DimY, Ys),
    pos_list(StartX, StepX, DimX, Xs),
    DepConjs = Options ^ opts_use_dep_conjs,
    Seq = Options ^ opts_sequential,
    (
        DepConjs = use_dependent_conjunctions,
        draw_rows_dep(Seq, Xs, Ys, Rows)
    ;
        DepConjs = use_independent_conjunctions,
        draw_rows_indep(Seq, Xs, Ys, Rows)
    ;
        DepConjs = use_left_independent_conjunctions,
        draw_rows_indep_left(Seq, Xs, Ys, Rows)
    ).

:- pred draw_rows_dep(sequential::in, list(float)::in, list(float)::in,
    cord(colour)::out) is det.

draw_rows_dep(parallel_execution, Xs, Ys, Rows) :-
    map_foldl(draw_row(Xs), append_row, Ys, empty, Rows).
draw_rows_dep(sequential_execution, Xs, Ys, Rows) :-
    map_foldl_seq(draw_row(Xs), append_row, Ys, empty, Rows).

:- pred draw_rows_indep(sequential::in, list(float)::in, list(float)::in,
    cord(colour)::out) is det.

draw_rows_indep(Seq, Xs, Ys, Rows) :-
    (
        Seq = sequential_execution,
        my_map_seq(draw_row(Xs), Ys, RowList)
    ;
        Seq = parallel_execution,
        my_map(draw_row(Xs), Ys, RowList)
    ),
    foldl(append_row, RowList, empty, Rows).

:- pred draw_rows_indep_left(sequential::in, list(float)::in, list(float)::in,
    cord(colour)::out) is det.

draw_rows_indep_left(Seq, Xs, Ys, Rows) :-
    (
        Seq = sequential_execution,
        my_mapl_seq(draw_row(Xs), Ys, RowList)
    ;
        Seq = parallel_execution,
        my_mapl(draw_row(Xs), Ys, RowList)
    ),
    foldl(append_row, RowList, empty, Rows).

:- pred append_row(cord(X)::in, cord(X)::in, cord(X)::out) is det.

append_row(Row, !Rows) :-
    !:Rows = !.Rows ++ Row.

:- pred draw_row(list(float)::in, float::in, cord(colour)::out) is det.

draw_row(Xs, Y, Row) :-
    draw_row_2(Xs, Y, empty, Row).

:- pred draw_row_2(list(float)::in, float::in,
    cord(colour)::in, cord(colour)::out) is det.

draw_row_2([], _, !Row).
draw_row_2([X | Xs], Y, !Row) :-
    Pos = vecf(X, Y),
    calc_pixel(Pos, Colour),
    !:Row = snoc(!.Row, Colour),
    draw_row_2(Xs, Y, !Row).

:- pred calc_pixel(vecf::in, colour::out) is det.

calc_pixel(Pos, Colour) :-
    max_iters(MaxIters),
    Pos = vecf(X, Y),
    escape(complex(0.0, 0.0), complex(X, Y), MaxIters, 0, Iters),
    ( Iters > 0 ->
        colour_gradient(blue, yellow,
            sqrt(float(Iters)) / sqrt(float(MaxIters)),
            Colour)
    ;
        Colour = black
    ).

:- pred colour_gradient(colour::in, colour::in, float::in, colour::out) is det.

colour_gradient(Start, End, R, colour(Red, Green, Blue)) :-
    component_gradient(Start ^ red, End ^ red, R, Red),
    component_gradient(Start ^ green, End ^ green, R, Green),
    component_gradient(Start ^ blue, End ^ blue, R, Blue).

:- pred component_gradient(int::in, int::in, float::in, int::out) is det.

component_gradient(Start, End, V, Result) :-
    StartF = float(Start),
    EndF = float(End),
    Result = round_to_int(StartF + V * (EndF - StartF)).

:- pred escape(complex::in, complex::in, int::in,
    int::in, int::out) is det.

escape(N, C, MaxIters, !Iters) :-
    ( MaxIters > 0 ->
        N2 = N * N,
        R = N2 + C,
        (
            magnitude(R) > 2.0
        ->
            true
        ;
            !:Iters = !.Iters + 1,
            escape(R, C, MaxIters - 1, !Iters)
        )
    ;
        !:Iters = -1
    ).

%----------------------------------------------------------------------------%

:- pred pos_list(float::in, float::in, int::in, list(float)::out) is det.

pos_list(Cur, Step, Num, List) :-
    ( Num > 0 ->
        pos_list(Cur + Step, Step, Num - 1, Tail),
        List = [Cur | Tail]
    ;
        List = []
    ).

:- pred map_foldl(pred(X, Y), pred(Y, A, A), list(X), A, A).
:- mode map_foldl(pred(in, out) is det, pred(in, in, out) is det,
    in, in, out) is det.

map_foldl(_, _, [], !Acc).
map_foldl(M, F, [X | Xs], !Acc) :-
    (
        M(X, Y),
        F(Y, !Acc)
    &
        map_foldl(M, F, Xs, !Acc)
    ).

:- pred map_foldl_seq(pred(X, Y), pred(Y, A, A), list(X), A, A).
:- mode map_foldl_seq(pred(in, out) is det, pred(in, in, out) is det,
    in, in, out) is det.

map_foldl_seq(_, _, [], !Acc).
map_foldl_seq(M, F, [X | Xs], !Acc) :-
    (
        M(X, Y),
        F(Y, !Acc)
    ,
        map_foldl_seq(M, F, Xs, !Acc)
    ).


:- pred my_map(pred(X, Y), list(X), list(Y)).
:- mode my_map(pred(in, out) is det, in, out) is det.

my_map(_, [], []).
my_map(M, [X | Xs], [Y | Ys]) :-
    M(X, Y) &
    my_map(M, Xs, Ys).

:- pred my_map_seq(pred(X, Y), list(X), list(Y)).
:- mode my_map_seq(pred(in, out) is det, in, out) is det.

my_map_seq(_, [], []).
my_map_seq(M, [X | Xs], [Y | Ys]) :-
    M(X, Y) ,
    my_map_seq(M, Xs, Ys).

:- pred my_mapl(pred(X, Y), list(X), list(Y)).
:- mode my_mapl(pred(in, out) is det, in, out) is det.

my_mapl(_, [], []).
my_mapl(M, [X | Xs], [Y | Ys]) :-
    my_mapl(M, Xs, Ys) &
    M(X, Y).

:- pred my_mapl_seq(pred(X, Y), list(X), list(Y)).
:- mode my_mapl_seq(pred(in, out) is det, in, out) is det.

my_mapl_seq(_, [], []).
my_mapl_seq(M, [X | Xs], [Y | Ys]) :-
    my_mapl_seq(M, Xs, Ys) ,
    M(X, Y).

%----------------------------------------------------------------------------%

:- type image
    --->    image(
                dim             :: veci,
                pixels          :: cord(colour)
            ).

:- type colour
    --->    colour(
                red             :: int,
                green           :: int,
                blue            :: int
            ).

:- pred write_ppm(image::in, io::di, io::uo) is det.

write_ppm(image(Dim, Rows), !IO) :-
    io.open_binary_output(filename, Result, !IO),
    (
        Result = ok(Stream),
        Dim = veci(Width, Height),
        string.format("P6 %d %d 255\n", [i(Width), i(Height)], String),
        string.foldl(write_1byte_char(Stream), String, !IO),
        foldl(write_colour(Stream), list(Rows), !IO),
        io.close_binary_output(Stream, !IO)
    ;
        Result = error(Error),
        error(format("%s: %s", [s(filename), s(error_message(Error))]))
    ).

:- pred write_colour(binary_output_stream::in, colour::in,
    io::di, io::uo) is det.

write_colour(Stream, colour(R, G, B), !IO) :-
    write_byte(Stream, R, !IO),
    write_byte(Stream, G, !IO),
    write_byte(Stream, B, !IO).

:- pred write_1byte_char(binary_output_stream::in, char::in,
    io::di, io::uo) is det.

write_1byte_char(Stream, Char, !IO) :-
    write_byte(Stream, char.to_int(Char), !IO).

%----------------------------------------------------------------------------%

:- pred dimension(int::out, int::out) is det.

dimension(1024, 1024).

    % viewport(X, Y, Length, Height),
    %
:- pred viewport(float::out, float::out, float::out, float::out) is det.

viewport(-0.75, -0.75, 0.5, 0.5).

:- pred max_iters(int::out) is det.

max_iters(5000).

:- func filename = string.

filename = "mandelbrot.ppm".

:- func blue = colour.

blue = colour(0, 0, 255).

:- func yellow = colour.

yellow = colour(255, 255, 0).

:- func black = colour.

black = colour(0, 0, 0).

