/*  File:    lifter.lp
    Author:  Carlo Capelli
    Purpose: Just syntax sugar to improve readability, getting rid of
             temporary variables.
    Copyright (C): 2013, Carlo Capelli

    Description:

    Placeholders lifter - hence the name

    Replace each ° occurrence with a (new) var and join.
        ex: p(.q(.°.).r(.s(.°.).).) ~= q(.Q.), s(.S.), p(.Q.r(.S.).)

    Allows to name variables to be reused.
        ex: p(.q(.°X.).r(.X.).) ~= q(.X.), p(.X.r(.X.).)

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

:- module(lifter,
    [op(100, fx, (°)) % prefix to name a variable
    ]).

/* as reported by aBathologist, missing the following declaration could lead to
   an error == trapUndefined(): undefined: lifter:append/3 ==
*/
:- use_module(library(lists), [append/3]).

%%  funq(+P, -J)
%
%	to be able to debug, comment out goal_expansion directive
%	and call funq/2
%
funq(P, J) :-
  findq(P, T, L),
  conj(L, T, J).

findq(P, T, Z) :-
  compound(P),
  P =.. [F|As],
  argsq(As, Bs, M),
  (   firstq(Bs, T, Cs)
  ->  Q =.. [F|Cs],
      append(M, [Q], Z)
  ;   T =.. [F|Bs],
      Z = M
  ).

firstq([A|As], T, [T|As]) :-
  A == ° ; nonvar(A), A = ° T, var(T) .
firstq([A|As], T, [A|Bs]) :-
  firstq(As, T, Bs).

argsq([], [], []).
argsq([A|As], [B|Bs], L) :-
  findq(A, B, C),
  argsq(As, Bs, Cs),
  !, append(C, Cs, L).
argsq([A|As], [A|Bs], L) :-
  argsq(As, Bs, L).

conj([L], T, (L, T)) :- !.
conj([J|Js], T, (J, R)) :- conj(Js, T, R).

:- multifile user:goal_expansion/2.
user:goal_expansion(X, Y) :-
  ( current_prolog_flag(xref, true)
  ; X = (_ , _)
  ; X = (_ ; _)
  ; X = (_ -> _)
  )
  -> !, fail % leave unchanged
  ;  funq(X, Y).
