% vim: ts=4 sw=4 et
%------------------------------------------------------------------------%

:- module report.

%------------------------------------------------------------------------%

:- interface.

:- import_module io.

%------------------------------------------------------------------------%

:- pred main(io::di, io::uo) is det.

%------------------------------------------------------------------------%
%------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module cord.
:- import_module float.
:- import_module getopt.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module string.

:- import_module result.

%------------------------------------------------------------------------%

:- type option
    --->    drop
    ;       relative.

main(!IO) :-
    io.command_line_arguments(Args0, !IO),
    OptionOps = option_ops_multi(short, long, defaults),
    getopt.process_options(OptionOps, Args0, Args, Result),
    (
        Result = ok(Options),

        map_foldl(read_file, Args, ResultsList, !IO),
        Results = cord_list_to_cord(ResultsList),
        generate_report(Options, Results, Report),
        print_report(Report, ReportStrs),
        write_string(append_list(list(ReportStrs)), !IO)
    ;
        Result = error(Error),
        io.write_string(Error, !IO),
        set_exit_status(1, !IO)
    ).

:- pred short(char::in, option::out) is semidet.

short('d', drop).
short('r', relative).

:- pred long(string::in, option::out) is semidet.

long("drop",            drop).
long("relative",        relative).

:- pred defaults(option::out, option_data::out) is multi.

defaults(drop,          bool(no)).
defaults(relative,      string("")).

%------------------------------------------------------------------------%

:- pred read_file(string::in, cord(result.result)::out,
    io::di, io::uo) is det.

read_file(Filename, Results, !IO) :-
    open_input(Filename, MaybeStream, !IO),
    (
        MaybeStream = ok(Stream),
        read_results(Filename, Stream, cord.init, Results, !IO),
        close_input(Stream, !IO)
    ;
        MaybeStream = error(Error),
        error(format("%s: %s", [s(Filename), s(error_message(Error))]))
    ).

:- pred read_results(string::in, input_stream::in,
    cord(result.result)::in, cord(result.result)::out,
    io::di, io::uo) is det.

read_results(Filename, Stream, !Results, !IO) :-
    read_line_as_string(Stream, MaybeLine, !IO),
    (
        MaybeLine = ok(Line),
        parse_result(Line, MaybeResult),
        (
            MaybeResult = yes(Result),
            !:Results = snoc(!.Results, Result),
            read_results(Filename, Stream, !Results, !IO)
        ;
            MaybeResult = no,
            error(format("Failed to parse result from '%s'", [s(Line)]))
        )
    ;
        MaybeLine = eof
    ;
        MaybeLine = error(Error),
        error(format("%s: %s", [s(Filename), s(error_message(Error))]))
    ).

%------------------------------------------------------------------------%

:- type report
    --->    report(
                r_groups            :: map(string, report_group),
                r_relative_scores   :: maybe(map(string, float))
            ).

:- type report_group
    --->    report_group(
                rg_name         :: string,
                rg_samples      :: int,
                rg_mean         :: float,
                rg_stdev        :: maybe(float),
                rg_entries      :: list(float)
            ).

:- pred generate_report(option_table(option)::in,
    cord(result.result)::in, report::out) is det.

generate_report(Options, ResultsCord, report(GroupsMap, MaybeRelScores)) :-
    lookup_bool_option(Options, drop, DropBool),
    (
        DropBool = yes,
        Drop = drop_first_and_last
    ;
        DropBool = no,
        Drop = keep_first_and_last
    ),
    Results = list(ResultsCord),
    GroupNames = remove_adjacent_dups(sort(map(get_name, Results))),
    map(generate_report_group(Drop, Results), GroupNames, Groups),
    foldl(add_group_to_map, Groups, map.init, GroupsMap),
    lookup_string_option(Options, relative, Relative),
    ( Relative = "" ->
        MaybeRelScores = no
    ;
        ( map.search(GroupsMap, Relative, RelGroup) ->
            RelScore = RelGroup ^ rg_mean,
            RelScoresMap = foldl((func(G, M) =
                    map.det_insert(M, G ^ rg_name, RelScore / (G ^ rg_mean))
                ), Groups, map.init),
            MaybeRelScores = yes(RelScoresMap)
        ;
            error("Group not found for computing relative scores")
        ) 
    ).

:- pred add_group_to_map(report_group::in,
    map(string, report_group)::in, map(string, report_group)::out) is det.

add_group_to_map(G, !M) :-
    map.det_insert(G ^ rg_name, G, !M).

:- type drop
    --->    drop_first_and_last
    ;       keep_first_and_last.

:- pred generate_report_group(drop::in, list(result.result)::in, string::in,
    report_group::out) is det.

generate_report_group(Drop, Results0, Name, Group) :-
    filter((pred(R::in) is semidet :-
            Name = get_name(R)
        ), Results0, Results),
    Entries0 = list.map(get_time, Results),
    (
        Drop = drop_first_and_last,
        sort(Entries0, Entries1),
        (
            drop_first_and_last(Entries1, Entries2)
        ->
            Entries = Entries2
        ;
            error("Too few results to drop first and last results")
        )
    ;
        Drop = keep_first_and_last,
        Entries = Entries0
    ),
    Samples = length(Entries),
    % This cannot divide by zero.
    Mean = sum(Entries) / float(Samples),
    ( Samples >= 2 ->
        Variances = map((func(X) = pow(X - Mean, 2)), Entries),
        Stdev = sqrt(sum(Variances)/(float(Samples) - 1.0)),
        MaybeStdev = yes(Stdev)
    ;
        % Sample Stdev is undefined if there is only one sample.
        MaybeStdev = no
    ),
    Group = report_group(Name, Samples, Mean, MaybeStdev, Entries).

:- pred drop_first_and_last(list(T)::in, list(T)::out) is semidet.

drop_first_and_last(!Xs) :-
    L = length(!.Xs),
    drop(1, !Xs),
    take(L - 2, !Xs).

:- func sum(list(float)) = float.

sum(Xs) = foldl((+), Xs, 0.0).

:- pred print_report(report::in, cord(string)::out) is det.

print_report(report(GroupsMap, RelScores), Report) :-
    map(print_report_group(GroupsMap, RelScores), keys(GroupsMap),
        GroupReports),
    Report0 = join(singleton("\n"), GroupReports),
    Report = snoc(Report0, "\n").

:- pred print_report_group(map(string, report_group)::in,
    maybe(map(string, float))::in, string::in, cord(string)::out) is det.

print_report_group(Groups, MaybeRelScores, Name, Report) :-
    map.lookup(Groups, Name, Group),
    Group = report_group(_, Samples, Mean, MaybeStdev, _Entries),
    (
        MaybeStdev = yes(Stdev),
        StdevStr = format("%4.2f", [f(Stdev)])
    ;
        MaybeStdev = no,
        StdevStr = "-"
    ),
    (
        MaybeRelScores = yes(RelScores),
        map.lookup(RelScores, Name, RelScore),
        Report = singleton(format(
            "%s:\t%d samples,\tmean: %4.1f (%2.2f) \tStdev: %s",
            [s(Name), i(Samples), f(Mean), f(RelScore), s(StdevStr)]))
    ;
        MaybeRelScores = no,
        Report = singleton(format(
            "%s:\t%d samples,\tmean: %4.1f\tStdev: %s",
            [s(Name), i(Samples), f(Mean), s(StdevStr)]))
    ).

%------------------------------------------------------------------------%

:- func join(cord(T), list(cord(T))) = cord(T).

join(_, []) = empty.
join(Sep, [H | T]) = R :-
    (
        T = [],
        R = H
    ;
        T = [_ | _],
        R = H ++ Sep ++ join(Sep, T)
    ).

:- func sqrt(float) = float.

sqrt(Input) = Output :-
    sqrt(Input, Output).

:- pred sqrt(float::in, float::out) is det.

:- pragma foreign_decl("C",
    "
#include <math.h>
    ").

:- pragma foreign_proc("C",
    sqrt(Input::in, Output::out),
    [will_not_call_mercury, thread_safe, promise_pure, will_not_throw_exception],
    "
#ifdef MR_USE_SINGLE_PREC_FLOAT
        Output = sqrtf(Input);
#else
        Output = sqrt(Input);
#endif
    ").

