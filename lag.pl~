/*  File:    lag.pl
    Author:  Carlo,,,
    Created: Nov 27 2012
    Purpose: Linear AGgregation
*/

:- module(lag,
	  [integrate/3
	  ]).

:- [library(aggregate)].

integrate(min, Goal, R) :-
	State = (_, _),
	repeat,
	(   call(Goal, V),
	    arg(1, State, C),
	    ( ( var(C) ; V < C ) -> U = V ; U = C ),
	    nb_setarg(1, State, U),
	    fail
	;   arg(1, State, R)
	).

integrate(count, Goal, R) :-
	State = (0, _),
	repeat,
	(   call(Goal),
	    arg(1, State, C),
	    U is C+1,
	    nb_setarg(1, State, U),
	    fail
	;   arg(1, State, R)
	).

integrate(ave, Goal, Ave) :-
	State = state(0, 0, _),
	repeat,
	(   call(Goal, V),
	    arg(1, State, C), U is C+1, nb_setarg(1, State, U),
	    arg(2, State, S), T is S+V, nb_setarg(2, State, T),
	    fail
	;   arg(1, State, C), arg(2, State, S), Ave is S/C
	).

:- meta_predicate integrate(+, :, ?).
