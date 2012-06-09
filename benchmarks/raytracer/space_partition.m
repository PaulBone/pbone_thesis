%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

% This module defines the stuff for interpreting GML programs.

:- module space_partition.

:- interface.

:- import_module eval.
:- import_module list.
:- import_module pair.
:- import_module renderer.
:- import_module trans.
:- import_module vector.

:- func create_scene(object) = scene.

:- type scene
    --->    scene(
                space_tree,
                list(object)    % objects which can't be partitioned
                                % (e.g. planes)
            ).

:- type invalid_transformation
    --->    invalid_transformation(transformation).

:- type invalid_object
    --->    invalid_object(object).

:- type normal == vector.

:- type surface_area == real.

:- type space_tree
    --->    space_tree(
                bounding_box,
                surface_area,
                list(space_tree_node)
            ).

:- type space_tree_node
    --->    node(
                space_tree
            )
    ;       leaf(
                space_tree_object
            ).

:- type space_tree_object
    --->    space_tree_object(
                bounding_box,
                surface_area,
                object
            ).

:- pred traverse_space_tree(space_tree::in, point::in, vector::in,
    intersection_result::out) is det.

:- type bounding_box == pair(point).

:- func find_object_bounding_box(object) = bounding_box.
:- func transform_bounding_box(bounding_box, trans) = bounding_box.

:- implementation.

:- import_module bool.
:- import_module exception.
:- import_module float.
:- import_module int.
:- import_module io.
:- import_module math.
:- import_module maybe.
:- import_module tree.

create_scene(Obj) = scene(Tree, Others) :-
    split_partitionable_objects(Obj, Partitionable, Others),
    Tree = build_space_tree(Partitionable).

:- pred split_partitionable_objects(object::in, list(object)::out,
    list(object)::out) is det.

split_partitionable_objects(union(Object1, Object2),
        Partitionable, NonPartitionable) :-
    split_partitionable_objects(Object1,
        Partitionable1, NonPartitionable1),
    split_partitionable_objects(Object2,
        Partitionable2, NonPartitionable2),
    list.append(Partitionable1, Partitionable2, Partitionable),
    list.append(NonPartitionable1, NonPartitionable2, NonPartitionable).

split_partitionable_objects(transform(Object, Trans),
        Partitionable, NonPartitionable) :-
    split_partitionable_objects(Object, Partitionable0, NonPartitionable0),
    TransformObject = (func(Object1) = transform(Object1, Trans)),
    Partitionable = list.map(TransformObject, Partitionable0),
    NonPartitionable = list.map(TransformObject, NonPartitionable0).

split_partitionable_objects(basic_object(Id, Obj, Light),
        Partitionable, NonPartitionable) :-
    ( Obj = plane(_) ->
        Partitionable = [],
        NonPartitionable = [basic_object(Id, Obj, Light)]
    ;
        NonPartitionable = [],
        Partitionable = [basic_object(Id, Obj, Light)]
    ).

split_partitionable_objects(Obj, Partitionable, NonPartitionable) :-
    Obj = intersect(Obj1, Obj2),
    (
        ( object_contains_plane(Obj1) = yes
        ; object_contains_plane(Obj2) = yes
        )
    ->
        Partitionable = [],
        NonPartitionable = [Obj]
    ;
        NonPartitionable = [],
        Partitionable = [Obj]
    ).

split_partitionable_objects(Obj, Partitionable, NonPartitionable) :-
    Obj = difference(Obj1, Obj2),
    (
        ( object_contains_plane(Obj1) = yes
        ; object_contains_plane(Obj2) = yes
        )
    ->
        Partitionable = [],
        NonPartitionable = [Obj]
    ;
        NonPartitionable = [],
        Partitionable = [Obj]
    ).

:- func object_contains_plane(object) = bool.

object_contains_plane(basic_object(_, Obj, _)) =
    ( Obj = plane(_) -> yes ; no ).
object_contains_plane(transform(Obj, _)) =
    object_contains_plane(Obj).
object_contains_plane(union(Obj1, Obj2)) = Result :-
    bool.and(object_contains_plane(Obj1), object_contains_plane(Obj2), Result).
object_contains_plane(intersect(Obj1, Obj2)) = Result :-
    bool.and(object_contains_plane(Obj1), object_contains_plane(Obj2), Result).
object_contains_plane(difference(Obj1, Obj2)) = Result :-
    bool.and(object_contains_plane(Obj1), object_contains_plane(Obj2), Result).

%-----------------------------------------------------------------------------%

:- func find_bounding_box(list(object)) = bounding_box.

find_bounding_box([]) = O - O :-
    O = point(0.0, 0.0, 0.0).
find_bounding_box([Obj | Objs]) =
    max_box(find_object_bounding_box(Obj), find_bounding_box(Objs)).

:- func max_box(bounding_box, bounding_box) = bounding_box.

max_box(P1 - P2, P3 - P4) = min_point(P1, P3) - max_point(P2, P4).

:- func min_point(point, point) = point.

min_point(point(X1, Y1, Z1), point(X2, Y2, Z2)) =
    point(min(X1, X2), min(Y1, Y2), min(Z1, Z2)).

:- func max_point(point, point) = point.

max_point(point(X1, Y1, Z1), point(X2, Y2, Z2)) =
    point(max(X1, X2), max(Y1, Y2), max(Z1, Z2)).

find_object_bounding_box(basic_object(_Id, Obj, _List)) =
    find_basic_object_bounding_box(Obj).
find_object_bounding_box(union(Obj1, Obj2)) =
    max_box(find_object_bounding_box(Obj1), find_object_bounding_box(Obj2)).
find_object_bounding_box(transform(Obj, Transformation)) =
        transform_bounding_box(find_object_bounding_box(Obj), Trans) :-
    Trans = maybe_transformation_to_trans(yes(Transformation)).
find_object_bounding_box(intersect(Obj1, Obj2)) =
    max_box(find_object_bounding_box(Obj1), find_object_bounding_box(Obj2)).
find_object_bounding_box(difference(Obj1, Obj2)) =
    max_box(find_object_bounding_box(Obj1), find_object_bounding_box(Obj2)).

:- func find_basic_object_bounding_box(basic_object) = bounding_box.

find_basic_object_bounding_box(sphere(_)) =
    point(-1.0, -1.0, -1.0) - point(1.0, 1.0, 1.0).
find_basic_object_bounding_box(cube(_)) =
    point(0.0, 0.0, 0.0) - point(1.0, 1.0, 1.0).
find_basic_object_bounding_box(cylinder(_)) =
    point(-1.0, 0.0, -1.0) - point(1.0, 1.0, 1.0).
find_basic_object_bounding_box(cone(Surface)) =
    % Just an approximation.
    find_basic_object_bounding_box(cylinder(Surface)).
find_basic_object_bounding_box(plane(Obj)) = _ :-
    throw(invalid_object(basic_object(0, plane(Obj), []))).

transform_bounding_box(Point1 - Point2, Trans) = MinPoint - MaxPoint :-
    Point1 = point(X1, Y1, Z1),
    Point2 = point(X2, Y2, Z2),
    Point3 = point(X2, Y1, Z1),
    Point4 = point(X1, Y2, Z1),
    Point5 = point(X1, Y1, Z2),
    Point6 = point(X2, Y2, Z1),
    Point7 = point(X1, Y2, Z2),
    Point8 = point(X2, Y1, Z2),

    TPoint1 = point_to_world_space(Trans, Point1),
    TPoint2 = point_to_world_space(Trans, Point2),
    TPoint3 = point_to_world_space(Trans, Point3),
    TPoint4 = point_to_world_space(Trans, Point4),
    TPoint5 = point_to_world_space(Trans, Point5),
    TPoint6 = point_to_world_space(Trans, Point6),
    TPoint7 = point_to_world_space(Trans, Point7),
    TPoint8 = point_to_world_space(Trans, Point8),

    MinPoint = min_point(TPoint1,
            min_point(TPoint2,
            min_point(TPoint3,
            min_point(TPoint4,
            min_point(TPoint5,
            min_point(TPoint6,
            min_point(TPoint7,
            TPoint8))))))),

    MaxPoint = max_point(TPoint1,
            max_point(TPoint2,
            max_point(TPoint3,
            max_point(TPoint4,
            max_point(TPoint5,
            max_point(TPoint6,
            max_point(TPoint7,
            TPoint8))))))).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % We currently use binary trees, but other arities may/may not be better.
    %
:- func build_space_tree(list(object)) = space_tree.

build_space_tree(Objects) = Tree :-
    Max = float.max,
    Min = float.min,
    MinBound = point(Max, Max, Max),
    MaxBound = point(Min, Min, Min),
    BoundingBox = MinBound - MaxBound,
    SurfaceArea = 0.0,
    Tree0 = space_tree(BoundingBox, SurfaceArea, []),

    SpaceObjects = list.map(
        (func(Object) = space_tree_object(BBox, ObjSurfaceArea, Object) :-
            BBox = find_object_bounding_box(Object),
            ObjSurfaceArea = bounding_box_surface_area(BBox)
        ),
        Objects),

    % We might want to shuffle the input objects to
    % avoid pathological inputs from the modeller.
    list.foldl(space_tree_insert, SpaceObjects, Tree0, Tree).

:- pred write_space_tree(space_tree::in, io::di, io::uo) is det.

write_space_tree(space_tree(Box, Area, Nodes), !IO) :-
    io.write_string("tree(\n", !IO),
    io.write_string("bounding box: ", !IO),
    io.write(Box, !IO),
    io.nl(!IO),
    io.write_string("area: ", !IO),
    io.write_float(Area, !IO),
    io.nl(!IO),
    io.write_string("[", !IO),
    io.write_list(Nodes, "\n", write_space_node, !IO),
    io.write_string("]\n", !IO).

:- pred write_space_node(space_tree_node::in, io::di, io::uo) is det.

write_space_node(node(Tree), !IO) :-
    io.write_string("sub-tree:\n", !IO),
    write_space_tree(Tree, !IO),
    io.nl(!IO).

write_space_node(leaf(Leaf), !IO) :-
    io.write_string("leaf:\n", !IO),
    io.write(Leaf, !IO),
    io.nl(!IO).

:- pred space_tree_insert(space_tree_object::in,
    space_tree::in, space_tree::out) is det.

space_tree_insert(Object, space_tree(OldBox, _, Subtrees0), Tree) :-
    Object = space_tree_object(ObjBox, _, _),
    NewBox = max_box(OldBox, ObjBox),
    NewSurfaceArea = bounding_box_surface_area(NewBox),
    (
        Subtrees0 = [],
        Subtrees = [leaf(Object)]
    ;
        Subtrees0 = [Leaf],
        Subtrees = [leaf(Object), Leaf]
    ;
        Subtrees0 = [_, _ | _],
        SurfaceAreaChange = real_max,
        select_subtree(ObjBox, Subtrees0, 1, SurfaceAreaChange - 0,
            SelectedSubtree),
        subtree_insert(Subtrees0, SelectedSubtree, Object, Subtrees)

    ),
    Tree = space_tree(NewBox, NewSurfaceArea, Subtrees).

:- type subtree_result
    --->    subtree_result(
                bounding_box,
                surface_area,   % new surface area
                int             % index in list of parent nodes
            ).

:- pred select_subtree(bounding_box::in, list(space_tree_node)::in,
    int::in, pair(surface_area, int)::in, int::out) is det.

select_subtree(_, [], _, _ - Selected, Selected).
select_subtree(ObjBox, [Subtree | Subtrees], Index,
        SurfaceAreaChange0 - Selected0, Selected) :-
    (
        Subtree = node(space_tree(OldBox, OldSurfaceArea, _))
    ;
        Subtree = leaf(space_tree_object(OldBox, OldSurfaceArea, _))
    ),
    NewBox = max_box(OldBox, ObjBox),
    NewSurfaceArea = bounding_box_surface_area(NewBox),
    SurfaceAreaChange = NewSurfaceArea - OldSurfaceArea,
    ( SurfaceAreaChange < SurfaceAreaChange0 ->
        select_subtree(ObjBox, Subtrees, Index + 1,
            SurfaceAreaChange - Index, Selected)
    ;
        select_subtree(ObjBox, Subtrees, Index + 1,
            SurfaceAreaChange0 - Selected0, Selected)
    ).

:- pred subtree_insert(list(space_tree_node)::in, int::in,
    space_tree_object::in, list(space_tree_node)::out) is det.

subtree_insert([], _, Object, [leaf(Object)]).
subtree_insert([Tree0 | Trees0], Index, Object, Trees) :-
    ( Index = 1 ->
        (
            Tree0 = leaf(LeafObject),
            LeafObject = space_tree_object(OldBox, _, _),
            Object = space_tree_object(ObjBox, _, _),
            NewBox = max_box(OldBox, ObjBox),
            NewSurfaceArea = bounding_box_surface_area(NewBox),
            Tree = node(space_tree(NewBox, NewSurfaceArea,
                [leaf(Object), leaf(LeafObject)]))
        ;
            Tree0 = node(Subtree0),
            space_tree_insert(Object, Subtree0, Subtree),
            Tree = node(Subtree)
        ),
        Trees = [Tree | Trees0]
    ;
        subtree_insert(Trees0, Index - 1, Object, Trees1),
        Trees = [Tree0 | Trees1]
    ).

    % Compute the surface area of the bounding box.
    %
:- func bounding_box_surface_area(bounding_box) = float.

bounding_box_surface_area(point(X1, Y1, Z1) - point(X2, Y2, Z2)) =
        (L + M) * N + L * M :-
    L = X1 - X2,
    M = Y1 - Y2,
    N = Z1 - Z2.

traverse_space_tree(space_tree(BoundingBox, _, Nodes),
        RayOrigin, RayDirection, Intersections) :-
    ( intersect_bounding_box(BoundingBox, RayOrigin, RayDirection) ->
        traverse_space_tree_nodes(Nodes, RayOrigin, RayDirection,
            empty, Intersections)
    ;
        Intersections = empty
    ).

:- pred intersect_bounding_box(bounding_box::in,
        point::in, vector::in) is semidet.

intersect_bounding_box(BoundingBox, RayOrigin, RayDirection) :-
    RayOrigin = point(XRay, YRay, ZRay),
    RayDirection = point(XDir, YDir, ZDir),
    BoundingBox = point(XMin, YMin, ZMin) - point(XMax, YMax, ZMax),
    intersect_bounding_box_2(XRay, YRay, ZRay, XDir, YDir, ZDir,
        XMin, YMin, ZMin, XMax, YMax, ZMax).

:- pred intersect_bounding_box_2(real::in, real::in, real::in,
    real::in, real::in, real::in, real::in, real::in, real::in,
    real::in, real::in, real::in) is semidet.

:- pragma foreign_proc("C",
    intersect_bounding_box_2(XRay::in, YRay::in, ZRay::in,
        XDir::in, YDir::in, ZDir::in, XMin::in, YMin::in, ZMin::in,
        XMax::in, YMax::in, ZMax::in),
    [promise_pure, will_not_call_mercury],
"{
    double minB[3], maxB[3];    /*box */
    double origin[3], dir[3];   /*ray */
    double coord[3];        /* hit point (not used -stayl)*/
    MR_bool succeeded;

    minB[0] = XMin;
    minB[1] = YMin;
    minB[2] = ZMin;
    maxB[0] = XMax;
    maxB[1] = YMax;
    maxB[2] = ZMax;
    origin[0] = XRay;
    origin[1] = YRay;
    origin[2] = ZRay;
    dir[0] = XDir;
    dir[1] = YDir;
    dir[2] = ZDir;
    succeeded = HitBoundingBox(minB, maxB, origin, dir, coord);
    /*fprintf(stderr, succeeded ? ""1\\n"" : ""0\\n""); */
    SUCCESS_INDICATOR = succeeded;
}").

:- pragma foreign_decl("C", "
/*
Fast Ray-Box Intersection
by Andrew Woo
from ""Graphics Gems"", Academic Press, 1990
*/

#define NUMDIM  3
#define RIGHT   0
#define LEFT    1
#define MIDDLE  2

MR_bool HitBoundingBox(double minB[3],double maxB[3], double origin[3],
            double dir[3], double coord[3]);
").

:- pragma foreign_code("C", "
MR_bool HitBoundingBox(double minB[3],double maxB[3], double origin[3],
    double dir[3], double coord[3])
{
    MR_bool inside = MR_TRUE;
    char    quadrant[NUMDIM];
    int     i;
    int     whichPlane;
    double  maxT[NUMDIM];
    double  candidatePlane[NUMDIM];

    /* Find candidate planes; this loop can be avoided if
    rays cast all from the eye(assume perpsective view) */
    for (i=0; i<NUMDIM; i++) {
        if (origin[i] < minB[i]) {
            quadrant[i] = LEFT;
            candidatePlane[i] = minB[i];
            inside = MR_FALSE;
        } else if (origin[i] > maxB[i]) {
            quadrant[i] = RIGHT;
            candidatePlane[i] = maxB[i];
            inside = MR_FALSE;
        } else  {
            quadrant[i] = MIDDLE;
        }
    }

    /* Ray origin inside bounding box */
    if (inside)  {
        coord = origin;
        return (MR_TRUE);
    }

    /* Calculate T distances to candidate planes */
    for (i = 0; i < NUMDIM; i++) {
        if (quadrant[i] != MIDDLE && dir[i] != 0.0) {
            maxT[i] = (candidatePlane[i]-origin[i]) / dir[i];
        } else {
            maxT[i] = -1.0;
        }
    }

    /* Get largest of the maxT's for final choice of intersection */
    whichPlane = 0;
    for (i = 1; i < NUMDIM; i++) {
        if (maxT[whichPlane] < maxT[i]) {
            whichPlane = i;
        }
    }

    /* Check final candidate actually inside box */
    if (maxT[whichPlane] < 0.) {
        return (MR_FALSE);
    }
    for (i = 0; i < NUMDIM; i++) {
        if (whichPlane != i) {
            coord[i] = origin[i] + maxT[whichPlane] *dir[i];
            if (coord[i] < minB[i] || coord[i] > maxB[i])
                return (MR_FALSE);
        } else {
            coord[i] = candidatePlane[i];
        }
    }
    return (MR_TRUE);               /* ray hits box */
}").

:- pred traverse_space_tree_nodes(list(space_tree_node)::in, point::in,
    vector::in, intersection_result::in, intersection_result::out) is det.

traverse_space_tree_nodes([], _, _, Results, Results).
traverse_space_tree_nodes([Node | Nodes], RayOrigin,
        RayDirection, Results0, Results) :-
    (
        Node = leaf(SpaceObject),
        SpaceObject = space_tree_object(_, _, Object),
        MaybeTransformation = no,
        find_object_intersection(Object, MaybeTransformation,
            RayOrigin, RayDirection, Result)
    ;
        Node = node(Tree),
        traverse_space_tree(Tree, RayOrigin, RayDirection, Result)
    ),
    Results1 = make_tree(Results0, Result),
    traverse_space_tree_nodes(Nodes, RayOrigin, RayDirection,
        Results1, Results).

%-----------------------------------------------------------------------------%
