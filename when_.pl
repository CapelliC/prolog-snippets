/** <module> when_
 *
 *  Answering http://stackoverflow.com/a/36760627/874024 and http://stackoverflow.com/a/36649172/874024
 *  --------
 *
 *  source file /home/carlo/prolog/snippets/when_.pl
 *  created at gio apr 21 06:35:56 2016
 *
 *  @author carlo
 *  @version 0.9.9
 *  @copyright carlo
 *  @license LGPL v2.1
 */

:- module(when_,
	[when_/1
	,op(1199, xfx, (-:-))
	]).

:- meta_predicate when_(0).

user:term_expansion((H -:- B), (H :- maplist(when_, Cs))) :-
	conj(B, Cs).

conj((C,Bs), [C|Cs]) :- !, conj(Bs, Cs).
conj(C, [C]).

%%  when_ is det.
%
%   apply grounding delay (when/2) to all argument' predicates
%
when_(P) :-
	%must_be(list, P),
	strip_module(P,_,Q), Q =.. [_|As],
	%P =.. [_|As],
	or_list(As, Exp), % hurry debugging :-) display(Exp),
	when(Exp, P).

or_list([A], ground(A)) :- !.
or_list([A|As], (ground(A);Exp)) :- or_list(As, Exp).
