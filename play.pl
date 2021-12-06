:- set_prolog_flag(toplevel_mode, recursive).

:- use_module(graphdb).

user:portray(Buckets) :-
    compound(Buckets),
    compound_name_arity(Buckets, [], Arity),
    format('<~D buckets>', [Arity]).
