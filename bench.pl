:- use_module(graphdb).

%!  set_args(+Count, +Subjects, +Domain, +Range)
%
%   Set Count properties on a random subject  with random key and value,
%   all integers.  For example:
%
%       ?- time(set_args(1 000 000, 1000, 10, 50)).

set_args(N, S, D, R) :-
    N > 0,
    !,
    set_arg(S, D, R),
    N2 is N - 1,
    set_args(N2, S, D, R).
set_args(_, _, _, _).

set_arg(S, D, R) :-
    random_between(1, S, N),
    random_between(1, D, K),
    random_between(1, R, V),
    set_prop(N, K, V).


		 /*******************************
		 *              HEAT		*
		 *******************************/
