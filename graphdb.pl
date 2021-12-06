/*  Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2020, VU University Amsterdam
			 CWI, Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(graphdb,
          [ new_type/1,                 % ++Name
            types_mask/2,               % ?Types, ?Mask

            new_node/2,                 % ++Id, ++Types
            new_edge/4,                 % ++Id, ++Types, ++From, ++To

            node/1,                     % ?Id
            edge/1,                     % ?Id
            edge/2,                     % ?Id,?Type
            edge/4,                     % ?Id,?Type,?From,?To

            decl_prop/2,                % +Prop, +Type

            set_prop/3,                 % ++Id, ++Key, +Value
            prop/3,                     % ++Id, ++Key, ?Value

            clear_graph/0
          ]).
:- use_module(library(error)).
:- use_module(library(lists)).

:- dynamic
    db_type/2,                             % Name, Id
    db_node/2,                             % Name, TypeMask
    db_edge/4,                             % Name, TypeMask, From, To
    db_prop/2,                             % Name, Meta,
    db_prop/3.                             % Id, Name, Value.

:- dynamic
    cache_type_mask/2.                     % ?Types, ?Mask

%!  clear_graph
%
%   Clear all graph data.

clear_graph :-
    propagate(clear_graph),
    retractall(db_type(_,_)),
    retractall(db_node(_,_)),
    retractall(db_edge(_,_,_,_)),
    retractall(db_prop(_,_)),
    retractall(db_type(_,_)),
    retractall(db_prop(_,_,_)),
    retractall(cache_type_mask(_,_)),
    b_setval(graphdb_properties, []()),
    nb_setval(graphdb_next_id, 1).


		 /*******************************
		 *            TYPES		*
		 *******************************/

%!  new_type(++Name) is det.
%
%   Declare  the  existence  of  a  new   type,  allocating  an  integer
%   identifier.

new_type(Name) :-
    between(1, infinite, Id),
    (   \+ db_type(_, Id)
    ->  !,
        assertz(db_type(Name, Id))
    ).

%!  types_mask(?Types:list(atom), ?Mask:integer) is det.
%
%   Convert between a list of types and a bitmask.

types_mask(Types, Mask) :-
    cache_type_mask(Types, Mask),
    !.
types_mask(Types, Mask) :-
    types_mask_int(Types, Mask),
    asserta(cache_type_mask(Types, Mask)).

types_mask_int(Types, Mask) :-
    is_list(Types),
    !,
    types_to_mask(Types, 0, Mask).
types_mask_int(Types, Mask) :-
    mask_to_types(Mask, Types).

types_to_mask([], Mask, Mask).
types_to_mask([H|T], Mask0, Mask) :-
    (   db_type(H, I)
    ->  Mask1 is Mask0 \/ (1<<I),
        types_to_mask(T, Mask1, Mask)
    ;   existence_error(type, H)
    ).

mask_to_types(0, []) :-
    !.
mask_to_types(Mask, [H|T]) :-
    I is lsb(Mask),
    (   db_type(H, I)
    ->  Mask1 is Mask /\ \(1<<I),
        mask_to_types(Mask1, T)
    ;   existence_error(type_id, I)
    ).

		 /*******************************
		 *            NODES		*
		 *******************************/

%!  new_node(++Id, ++Types:list(atom)) is det.
%
%   Create a new node of the indicated types.

new_node(Id, Types) :-
    types_mask(Types, Mask),
    assertz(db_node(Id, Mask)).

%!  node(?Id) is nondet.
%
%   True when Id is a node.

node(Id) :-
    db_node(Id, _).


		 /*******************************
		 *            EDGES		*
		 *******************************/

%!  new_edge(++Id, ++Types, ++From, ++To) is det.

new_edge(Id, Types, From, To) :-
    types_mask(Types, Mask),
    assertz(db_edge(Id, Mask, From, To)).

%!  edge(?Id) is nondet.
%!  edge(?Id, ?Type) is nondet.
%!  edge(?Id, ?Type, ?From, ?To) is nondet.
%
%   True when Id is an edge

edge(Id) :-
    db_edge(Id, _, _, _).

edge(Id, Type) :-
    edge(Id, Type, _, _).

edge(Id, Type, From, To) :-
    (   atom(Type)
    ->  db_type(Type, I),
        Mask is 1<<I,
        db_edge(Id, TypeMask, From, To),
        TypeMask /\ Mask =\= 0
    ;   db_edge(Id, TypeMask, From, To),
        types_mask(Types, TypeMask),
        member(Type, Types)
    ).


		 /*******************************
		 *           PROPERTIES		*
		 *******************************/

%!  decl_prop(+Prop, ++Meta) is det.
%
%   Make assertions about a property.  For now open ended, but the
%   following are specified:
%
%     - shared
%       Property is gloobal and not backtrackable.

decl_prop(Prop, Meta) :-
    must_be(atom, Prop),
    must_be(ground, Meta),
    (   db_prop(Prop, Meta)
    ->  true
    ;   assert(db_prop(Prop, Meta))
    ).

		 /*******************************
		 *   BACKTRACKABLE ATTRIBUTES	*
		 *******************************/

%!  set_prop(++Id, ++Key, +Value) is det.
%
%   Associate Key-Value with Id, to be undone on backtracking.

set_prop(Id, Key, Value) :-
    db_prop(Key, shared),
    !,
    retractall(db_prop(Id, Key, _)),
    assertz(db_prop(Id, Key, Value)).
set_prop(Id, Key, Value) :-
    id_int(Id, Int),
    b_getval(graphdb_properties, Array),
    (   compound_name_arity(Array, _, Size),
        Int =< Size,
        arg(Int, Array, Dict)
    ->  (   var(Dict)
        ->  put_dict(Key, #{}, Value, Dict),
            propagate(property(new, Id, Key, Value))
        ;   get_dict(Key, Dict, Value0)
        ->  (   Value0 == Value
            ->  true                       % no change
            ;   b_set_dict(Key, Dict, Value),
                propagate(property(update(Value0), Id, Key, Value))
            )
        ;   put_dict(Key, Dict, Value, Dict2),
            setarg(Int, Array, Dict2),
            propagate(property(new, Id, Key, Value))
        )
    ;   resize_array(Array, Int, Array2),
        arg(Int, Array2, Dict),
        put_dict(Key, #{}, Value, Dict),
        b_setval(graphdb_properties, Array2),
        propagate(property(new, Id, Key, Value))
    ).

resize_array(Array0, Size, Array) :-
    NewSize is 2<<msb(Size),
    compound_name_arity(Array, [], NewSize),
    compound_name_arity(Array0, _, OldSize),
    copy_args(1, OldSize, Array0, Array).

copy_args(I, End, From, To) :-
    I =< End,
    !,
    arg(I, From, Arg),
    arg(I, To, Arg),
    I2 is I+1,
    copy_args(I2, End, From, To).
copy_args(_, _, _, _).


%!  prop(++Id, ++Key, ?Value) is semidet.
%!  prop(++Id,  ?Key, ?Value) is nondet.
%
%   True when Key-Value is associated with Id.

prop(Id, Key, Value) :-
    nonvar(Key),
    !,
    (   db_prop(Key, shared)
    ->  db_prop(Id, Key, Value)
    ;   nonvar(Id)
    ->  bprop(Id, Key, Value)
    ;   (node(Id);edge(Id)),
        bprop(Id, Key, Value)
    ).
prop(Id, Key, Value) :-
    db_prop(Id, Key, Value).
prop(Id, Key, Value) :-
    bprop(Id, Key, Value).

bprop(Id, Key, Value) :-
    id_int(Id, Int),
    b_getval(graphdb_properties, Array),
    arg(Int, Array, Dict),
    nonvar(Dict),
    get_dict(Key, Dict, Value).

:- thread_local
    p_id_int/2.

id_int(Id, Int) :-
    p_id_int(Id, Int),
    !.
id_int(Id, Int) :-
    nb_getval(graphdb_next_id, Int),
    I2 is Int+1,
    nb_setval(graphdb_next_id, I2),
    assertz(p_id_int(Id, Int)).


		 /*******************************
		 *            HOOKING		*
		 *******************************/

:- multifile
    hook/1.

propagate(Event) :-
    hook(Event),
    !.
propagate(_).


:- multifile
    user:exception/3.

user:exception(undefined_global_variable, graphdb_properties, retry) :-
    b_setval(graphdb_properties, []()).
user:exception(undefined_global_variable, graphdb_next_id, retry) :-
    nb_setval(graphdb_next_id, 1).
