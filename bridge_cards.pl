/** <module> bridge_cards
 *
 *  answering http://stackoverflow.com/q/36845448/874024
 *  --------
 *
 *  source file /home/carlo/prolog/snippets/bridge_cards.pl
 *  created at mer apr 27 10:32:29 2016
 *
 *  @author carlo
 *  @version 0.9.9
 *  @copyright carlo
 *  @license LGPL v2.1
 */

:- module(bridge_cards, [bridge_cards/0]).
:- use_module(library(http/html_write)).

%%  bridge_cards is det.
%
%   showcase HTML layout facilities
%
bridge_cards :-
	hands_sorted(Hands),
	layout_table(Hands).

layout_table([S,W,N,E]) :-
	phrase(html([\css,
	 table([
		tr([\empty,		\layout_hand(N),	\empty]),
		tr([\layout_hand(W),	\empty,		\layout_hand(E)]),
		tr([\empty,		\layout_hand(S),	\empty])
	])]), Tokens),
	with_output_to(atom(X), print_html(Tokens)),
	win_html_write(X).

css --> html(style(type='text/css',
	['.size{background-color:lightgrey;}'
	,'.player{color:blue;}'
	,'.value{text-align:right;background-color:lightgreen}'
	])).

empty --> html(td([class=size],[])).

layout_hand(Cards) -->
	{findall(S-Vs, (
		member(S, [♣,♦,♥,♠]),
		findall(V, member(card(S,V),Cards), Vs)
	), SuitesValues)},
	html(td([class=player], table(\layout_suits(SuitesValues)))).

layout_suits([]) --> [].
layout_suits([Suit-Values|SVs]) -->
	html(tr([td(Suit), \layout_values(Values)])),
	layout_suits(SVs).

layout_values([]) --> [].
layout_values([V|Vs]) --> html(td([class=value], \layout_value(V))), layout_values(Vs).

layout_value(V) --> {nth1(V,['A',2,3,4,5,6,7,8,9,'T','J','Q','K'],C)}, html(C).

% build a bridge deck
bridge_deck(Cs) :-
	findall(card(S,V), (member(S,[♥,♦,♣,♠]),between(1,13,V)), Cs).

% shuffle
bridge_hands([S,W,N,E]) :-
	bridge_deck(Cs),
setrand(rand(1,2,3)), % get a known random set, to ease debugging
	random_permutation(Cs, RCs),
	maplist([H]>>length(H,13), [S,W,N,E]),
	append([S,W,N,E], RCs).

hands_sorted(Sorted) :-
	bridge_hands(Hands),
	maplist(sort, Hands, Sorted).

group_hand(Hand, Groups) :-
	findall([A,B,C|D],
	  (append([_,[A,B,C|D],_],Hand), 'same suit and consecutive numbers'([B,C|D],A)), Groups).

'same suit and consecutive numbers'([card(S,Y)|R], card(S,X)) :-
	succ(X,Y),
	'same suit and consecutive numbers'(R, card(S,Y)).
'same suit and consecutive numbers'([], _).
