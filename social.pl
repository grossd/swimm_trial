:- module(social,
          [ load_graph/1,
            pagerank/1
          ]).
:- use_module(graphdb).
:- use_module(library(csv)).
:- use_module(library(solution_sequences)).
:- use_module(library(apply)).

load_graph(Base) :-
    clear_graph,
    new_type(artist),
    new_type(edge),
    decl_prop(name, shared),
    format(string(NodesFile), 'graphs/~w.nodes', [Base]),
    forall(offset(1, csv_read_file_row(NodesFile, row(_Id, Name, NewID), [])),
           (   new_node(NewID, [artist]),
               set_prop(NewID, name, Name)
           )),
    format(string(EdgesFile), 'graphs/~w.edges', [Base]),
    forall(csv_read_file_row(EdgesFile, row(From, To), []),
           (   atomic_list_concat([From,To], '.', EdgeId),
               new_edge(EdgeId, [edge], From, To)
           )).

pagerank(Iter) :-
    findall(Node, node(Node), Nodes),
    maplist(init_pr, Nodes),
    iter(Iter).

init_pr(Node) :-
    set_prop(Node, heat, 1).

pr(Node, PR) :-
    node(Node),
    prop(Node, heat, PR).

iter(Iter) :-
    Iter > 0,
    !,
    findall(Node-PR, pr(Node, PR), Pairs),
    maplist(dist, Pairs),
    length(Pairs, Count),
    Remain is (1-0.85)/Count,
    maplist(reset(Remain), Pairs),
    Iter1 is Iter-1,
    iter(Iter1).
iter(_).

dist(Node-PR) :-
    findall(To, edge(_, edge, Node, To), Tos),
    (   Tos == []
    ->  true
    ;   length(Tos, L),
        Add is 0.85*PR/L,
        maplist(add_pr(Add), Tos)
    ).

reset(Remain, Node-PR) :-
    Sub is Remain-PR,
    add_pr(Sub, Node).

add_pr(Add, Node) :-
    (   prop(Node, heat, PR0)
    ->  true
    ;   PR0 = 1
    ),
    PR1 is PR0+Add,
    set_prop(Node, heat, PR1).
