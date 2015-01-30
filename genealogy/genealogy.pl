%%  <module> genealogy
%
%   build a genealogical 'tree' on relation parent_child(P, C),
%   using gv_uty direct graphviz pointers model
%   assign  nodes'attributes color,shape depending by female/1,male/1,died/1
%
%	@author: Ing. Carlo Capelli
%	@license: LGPL v2.1
%

:- module(genealogy,
	[genealogy/1
	,genealogy/2
	]).
:- use_module(library(clpfd)).

:- if(\+ current_predicate(graph_window/2)).
:-   use_module(pqGraphviz_emu).
:- endif.

% TBD check availability of relations in calling module
required(_Module, _Predicate).
optional(_Module, _Predicate).

check_requirements(Module) :-
	required(Module, parent_child/2),
	maplist(optional(Module), [female/1,male/1,died/1]).

genealogy(T) :-
	genealogy(user, T).
genealogy(M, T) :-
	check_requirements(M),
	graph_window(generations(M),
	[graph_name(family)
	,window_title(T)
	,node_defaults([style=radial, fillcolor=white, shape=ellipse, label=''])
	,edge_defaults([arrowhead=vee])
	]).

%%	generations(+Module, +G)
%
%	build a generational tree (a DAG, really), that is, persons in stacked layers
%
generations(M, G) :-
	% thanks to CLP(FD), making a correctly layered DAG its a breeze...
	setof(P, A^(M:parent_child(P, A) ; M:parent_child(A, P)), Ps),
	length(Ps, Pn),
	length(Rs, Pn),
	Rs ins 1..100,	% arbitrary, but doesn't accept +inf
	pairs_keys_values(RPs, Rs, Ps),
	maplist(make_rank(M, RPs), RPs),
	label(Rs),
	keysort(RPs, SRPs),
	group_pairs_by_key(SRPs, Ranks),
	% ranks done, now we can place persons at generations
	forall(member(Rank-Pr, Ranks), (
		new_subgraph(G, Rank, Subg),	% generation subgraph
		set_attrs(Subg, [rank:same, rankdir:'LR']),
		shared_children(M, Pr, ParentsPairs, Singles),
		maplist(make_family(M, Subg), ParentsPairs),
		maplist(make_person(M, Subg), Singles),
		maplist(link_parents(M, G), Pr)
	)).

%%	make_family(+Module, +GenerationGraph, +Parents)
%
%	bind strictly with a subgraph
%
make_family(M, GenerationGraph, (X,Y)) :-
	new_subgraph(GenerationGraph, [X,Y], Family),
	make_person(M, Family, X),
	make_person(M, Family, Y).

%%	make_person(+Module, +G, +P)
%
%	create a node for each person, mapping to visual attributes
%
make_person(M, G, P) :-
	make_node(G, P, N),
	( catch(M:female(P),_,fail) -> C = red ; catch(M:male(P),_,fail) -> C = cyan ; C = green ),
	( catch(M:died(P),_,fail) -> set_attrs(N, shape=octagon) ; true ),
	set_attrs(N, [fillcolor=white:C, label=P]).

%%	link_parents(+Module, +G, +P)
%
%	link a person to its parents, if any available
%
link_parents(M, G, P) :-
	(	M:parent_child(X, P)
	->	(	( M:parent_child(Y, P), X \= Y )
		->	two_parents(G, P, X, Y)
		;	one_parent(G, P, X)
		)
	;	true
	).

two_parents(G, P, X, Y) :-
	maplist(find_node(G), [P,X,Y], [Pp,Xp,Yp]),
	sort([X, Y], S),
	(	find_node(G, S, Join)
	->	true
	;	make_node(G, S, [shape=point, width:0.1, height:0.1], Join),
		new_edge(G, Xp, Join),
		new_edge(G, Yp, Join)
	),
	new_edge(G, Join, Pp).

one_parent(G, P, X) :-
	maplist(find_node(G), [P,X], [Pp,Xp]),
	find_node(G, X, Xp),
	new_edge(G, Xp, Pp).

%%	make_rank(+Module, +RankPersons, -RankPersons)
%
%	compute generation based ranking via CLP(FD)
%
make_rank(M, RPs, Rp-P) :-
	findall(G, M:parent_child(P, G), Gs),
	maplist(generated(M, Rp, RPs), Gs).

generated(M, Rp, RPs, G) :-
	member(Rg-G, RPs),
	Rg #= Rp+1,
	(   M:parent_child(Q, G)
	->  memberchk(Rq-Q, RPs),
	    Rq #= Rp
	;   true
	).

%%	shared_children(+Module, +Ps, -Pairs, -NoShared)
%
%	group persons in Ps, pairing two persons sharing all children
%
shared_children(M, Ps, [(P,Q)|Shared], NoShared) :-
	old_fashion_family(M, Ps, P,Q, _, Zs),
	!, shared_children(M, Zs, Shared, NoShared).
shared_children(_, Ps, [], Ps).

%%	old_fashion_family(+Module, +Persons, +P,+Q, -Children, -RestPersons)
%
%	P,Q from Persons share all and only children - a proper, old fashion family
%
old_fashion_family(M, Ps, P,Q, Children, Zs) :-
	% select a couple sharing a child
	select(P, Ps, Rs), M:parent_child(P, A),
	select(Q, Rs, Zs), M:parent_child(Q, A),
	% see if they share all children
	setof(Gen, M:parent_child(P, Gen), Children),
	setof(Gen, M:parent_child(Q, Gen), Children).
