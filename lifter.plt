/*  File:    lifter.plt
    Author:  Carlo,,,
    Created: Nov 11 2012
    Purpose: test lifter.pl rewrite utility
*/

:- use_module(lifter).

fac(N, F) :-
	N > 1 -> F is fac(° is N-1, °) * N ; F is 1.

delta_fields(FA, FB, FD) :-
	sort(FA, SA),
	sort(FB, SB),
	append(maplist([X,Y]>>(Y = +X), ord_subtract(SA, SB, °), °)
	      ,maplist([X,Y]>>(Y = -X), ord_subtract(SB, SA, °), °)
	      ,FD).

delta_fields_named(FA, FB, FD) :-
    append(maplist([X,Y]>>(Y = +X),
		   ord_subtract(sort(FA, °A),
				sort(FB, °B), °), °)
          ,maplist([X,Y]>>(Y = -X),
		   ord_subtract(B, A, °), °)
          ,FD).

:- begin_tests(lifter).

% double nesting
%
test(1) :-
	fac(3, X), X == 6.

% test lambda interaction
%
test(2) :-
	delta_fields([1,2,3,4], [1,5,2], X),
	X = [+ 3, + 4, - 5].

% simple string processing
%
test(3) :-
	append(`1`, `2`, °) == `12`.

% test named variables
%
test(4) :-
    delta_fields_named([1,2,3,4], [1,5,2], X),
    X = [+ 3, + 4, - 5].

:- end_tests(lifter).
