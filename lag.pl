/*  File         : lag.pl
    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013, Carlo Capelli

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
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
	    ( ( var(C) ; V @< C ) -> U = V ; U = C ),
	    nb_setarg(1, State, U),
	    fail
	;   arg(1, State, R)
	),
	!.

integrate(min_list_associated, Goal, Min-Ws) :-
	State = (_, [], _),
	forall(call(Goal, V, W),	% W stands for witness
	(	arg(1, State, C),	% C is current min
		arg(2, State, CW),	% CW are current min witnesses
		( ( var(C) ; V @< C ) -> U = V, Ws = [W] ; U = C, (C == V -> Ws = [W|CW] ; Ws = CW) ),
		nb_setarg(1, State, U),
		nb_setarg(2, State, Ws)
	)),
	arg(1, State, Min), arg(2, State, Ws).

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
