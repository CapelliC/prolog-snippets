/*  File:    small.pl
    Author:  Carlo,,,
    Created: Jun  4 2014
    Purpose: test Graphviz_emu
*/

:- module(small, [small/0]).

:- if(\+ current_predicate(graph_window/2)).
:-   use_module(pqGraphviz_emu).
:- endif.

small :-
	graph_window(small, []).
small(G) :-
	make_node(G, a, A),
	make_node(G, b, B),
	new_edge(G, A, B).
