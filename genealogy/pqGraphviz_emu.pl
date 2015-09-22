%% <module> pqGraphviz_emu
%  emulate pqGraphviz shared library interface
%  generating a temporary DOT file, rendered to a PDF
%
%  Carlo Capelli - Brescia 2014
%

:- module(pqGraphviz_emu,
	  [graph_window/2
	  ,new_subgraph/3
	  ,set_attrs/2
	  ,make_node/3
	  ,make_node/4
	  ,find_node/3
	  ,new_edge/3
	  ]).

% a dumb replacement for object pointers,
% as needed by Graphviz library cgraph
:- use_module(allocator).

:- meta_predicate graph_window(1, +).

% partial replacement of pqGraphviz shared library
% main entry point - for Prolog clients
%
graph_window(Worker, Options) :-

	alloc_init,

	empty(graph, G0),
	graph_options(G0,
		      [id   = graph_window
		      ,kind = digraph
		      |Options], G), !,
	alloc_new(G, Gp),

	% Worker will issue graph objects creation on G
	call(Worker, Gp),

	( memberchk(temp = Temp, Options) -> true ; temp = Temp ),
	
	% generate a temporary DOT script
	format(atom(TempDot), '~s.dot', [Temp]),
	open(TempDot, write, Dot),

	graph2dot(Dot, Gp),
	close(Dot),

	alloc_clear,

	% generate viewable
	Ext = svg,
	format(atom(Out), '~s.~s', [Temp, Ext]),
	format(atom(Spec), '-T~s', [Ext]),
	process_create(path(dot), [Spec, '-o', file(Out), file(TempDot)], []).


new_subgraph(ParG, Id, NewGp) :-
	empty(graph, NewG),
	term_to_atom(Id, IdW),
	alloc_new(NewG.put(id, IdW).put(kind, subgraph), NewGp),
	dladd(ParG, subgraphs, NewGp).

set_attrs(A, B) :-
	(   is_list(A)  % unfold A
	->  forall(member(X, A), set_attrs(X, B))
	;   (   is_list(B) % unfold B
	    ->	forall(member(Y, B), set_attrs(A, Y))
	    ;	deco_KV(K, B, V),
		dladd(A, attributes, attr{key:K, value:V})
	    )
	).

make_node(G, Id, Np) :-
	make_node(G, Id, [], Np).
make_node(G, Id, As, Np) :-
	empty(node, N0),
	term_to_atom(Id, IdW),
	N = N0.put(id, IdW),
	alloc_new(N, Np),
	set_attrs(Np, As),
	dladd(G, nodes, Np).

% graphviz search also in contained subgraphs...
find_node(Gp, Id, Np) :-
	term_to_atom(Id, IdW),
	alloc_get(Gp, G),
	(  member(Np, G.nodes),
	   alloc_get(Np, N),
	   N >:< node{id:IdW}
	-> true
	;  member(S, G.subgraphs),
	   find_node(S, Id, Np)
	).


new_edge(G, NS, NT) :-
	empty(edge, E),
	alloc_new(E.put(src, NS).put(dst, NT), Ep),
	dladd(G, edges, Ep).

%%%%%%% LOCAL utilities %%%%%%%

deco_KV(K, K:V, V).
deco_KV(K, K=V, V).

set_opt(D, K:V, D.put(K, V)).
set_opt(D, K=V, D.put(K, V)).
set_opt(D, Str, R) :-
	Str =.. [K, V],
	R = D.put(K, V).

graph_options(G, [], G).
graph_options(G0, [Opt|Options], G) :-
	set_opt(G0, Opt, G1),
	graph_options(G1, Options, G).

empty(graph, graph{id:_
		  ,kind:_
		  ,subgraphs:[]
		  ,nodes:[]
		  ,edges:[]
		  ,attributes:[]
		  }).
empty(node, node{id:_, attributes:[]}).
empty(edge, edge{src:_, dst:_, attributes:[]}).

indent(+FD, File) :-
	indent(FD, File),
	put(File, 0' ).
indent(File, File).

fdline(FD, Spec, Args) :-
	indent(FD, File),
	format(File, Spec, Args),
	nl(File), !.

dladd(P, K, V) :-
	alloc_get(P, D),
	nb_set_dict(K, D, [V|D.K]),
	alloc_set(P, D).

% objects are added to lists in reverse order
%  this takes care of reversing to get the original
%  insertion sequence that could be important as graphviz
%  order objects as declared, when possible
revall(L, P) :-
	reverse(L, R),
	forall(member(A, R), call(P, A)).

:- meta_predicate revall(+,1).

graph2dot(Dot, Gp) :-
	alloc_get(Gp, G),
	fdline(Dot, '~w "~w" {', [G.kind, G.id]),

	output_defaults(+Dot, G, node, node_defaults),
	output_defaults(+Dot, G, edge, edge_defaults),

	attrs2dot(+Dot, G.attributes),
	revall(G.subgraphs, graph2dot(+Dot)),
	revall(G.nodes, node2dot(+Dot)),
	revall(G.edges, edge2dot(+Dot)),
	fdline(Dot, '}', []).

output_defaults(Dot, G, Obj, List) :-
	(   get_dict(List, G, L)
	->  fdline(Dot, '~w [', [Obj]),
	    forall(member(X, L), (
		       deco_KV(K, X, V),
		       attr2dot(+Dot, attr{key:K, value:V}))),
	    fdline(Dot, ']', [])
	;   true
	).

attrs2dot(FD, As) :-
	(   As == []
	->  true
	;   revall(As, attr2dot(+FD))
	).

attr2dot(FD, attr{key:Key, value:Value}) :-
	Spec = '~w = "~w"',
	fdline(FD, Spec, [Key, Value]).

node2dot(FD, Np) :-
	alloc_get(Np, N),
	fdline(FD, '"~w" [', [N.id]),
	attrs2dot(+FD, N.attributes),
	fdline(FD, ']', []).

edge2dot(FD, Ep) :-
	alloc_get(Ep, E),
	alloc_get(E.dst, T),
	alloc_get(E.src, S),
	fdline(FD, '"~w" -> "~w" [', [S.id, T.id]),
	attrs2dot(+FD, E.attributes),
	fdline(FD, ']', []).
