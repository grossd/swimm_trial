/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
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

:- module(heat,
          [ max_heat/2,                 % -Heat, -Nodes:list
            max_heat_gen/2              % -Node,-Heat
          ]).
:- use_module(library(hashtable)).
:- use_module(library(assoc)).

/** <module> Maintain the heat of a set of nodes

This module maintains a backtracking map that allows provides access
to all nodes from a property value.  It is represented in a two step
data structure:

  - First an assoc maps from property value to a hashtable
  - The hashtable contains all nodes with the given property
    value.
*/

%!  graphdb:hook(+Event) is semidet.
%
%   Trap changes to the heat property to maintain an inverse map ordered
%   by heat value (descending).

:- multifile
    graphdb:hook/1.

graphdb:hook(property(Action, Node, heat, Heat)) :-
    b_getval(heat_map, Map0),
    (   Action = update(Heat0)
    ->  del_heat(Map0, Heat0, Node, Map)
    ;   Map = Map0
    ),
    add_heat(Map, Heat, Node).

del_heat(Map0, Heat, Node, Map) :-
    NegHeat is -Heat,
    get_assoc(NegHeat, Map0, HT),
    ht_del(HT, Node, _),
    (   ht_size(HT, 0)
    ->  del_assoc(NegHeat, Map0, _, Map),
        b_setval(heat_map, Map)
    ;   Map = Map0
    ).

add_heat(Map, Heat, Node) :-
    NegHeat is -Heat,
    (   get_assoc(NegHeat, Map, HT)
    ->  true
    ;   ht_new(HT),
        put_assoc(NegHeat, Map, HT, Map2),
        b_setval(heat_map, Map2)
    ),
    ht_put(HT, Node, true).

%!  max_heat(-Heat, -Nodes:list) is semidet.
%
%   True when Nodes is a list of nodes with the maximum heat Heat. Fails
%   if no node has been assigned a heat.

max_heat(Heat, Nodes) :-
    b_getval(heat_map, Map),
    min_assoc(Map, NegHeat, HT),
    Heat is -NegHeat,
    ht_keys(HT, Nodes).

%!  max_heat_gen(-Node, -Heat) is nondet.
%
%   True when Node has Heat. Enumerates  (on backtrackng) all nodes that
%   have an assigned heat in descending heat order.

max_heat_gen(Node, Heat) :-
    b_getval(heat_map, Map),
    gen_assoc(NegHeat, Map, HT),
    Heat is -NegHeat,
    ht_gen(HT, Node, _).


		 /*******************************
		 *           GLOBAL VAR		*
		 *******************************/

graphdb:hook(clear_graph) :-
    empty_assoc(Map),
    b_setval(heat_map, Map).

user:exception(undefined_global_variable, heat_map, retry) :-
    empty_assoc(Map),
    b_setval(heat_map, Map).

