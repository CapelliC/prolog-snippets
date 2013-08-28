/*  Author:        Carlo Capelli
    E-mail:        cc.carlo.cap@gmail.com
    Copyright (C): Carlo Capelli

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(dirtree,
	  [dirtree/2,                % +Root, -Tree
	   dirtree/3,                % +Path, +Curr, -Tree
	   sortree/2,                % +Tree, -Sorted
	   sortree/3,                % +Compare, +Tree, -Sorted
	   compare_by_attr/4,	     % +Attribute, +Item1, +Item2, -Rel
	   extension_embedding_enable/0,
	   extension_to_embed/1,     % +Ext
	   source_target/2,          % +Path, +XML
	   extensions_from_saved/1,  % -Extensions
	   counted_extensions/1,     % -CountedExtensions
	   get_ftp_ls_output/2,
	   get_ftp_ls_output//1,
	   dirzap/1,                 % +Root remove entire subtree from Root
	   capture_attrs/3,	     % previsit visit
	   assign_path/2
	  ]).

:- use_module([library(lambda),
	       library(dcg/basics),
	       library(aggregate),
	       library(xpath)
	      ]).

/** <module> Filesystem listing to XML.

This library provides utilities to load directories content from
filesystem to XML element/3 and some utility. It's also possible load
text of selected files types (by extension).

Of interest could be the parsing of FTP command 'ls', allowing
remote browsing after =process_create= with redirection of standard
output.

*/

:- dynamic extension_to_embed/1.
:- dynamic source_target/2.

%%	dirtree(+Root, ?Xml) is det.
%%	dirtree(+Path, +Item, ?Xml) is det.
%
%	start actual scan (need current path)
%
dirtree(P, T) :-
	dirtree(P, P, T).

dirtree(P, Dir, element(dir, [name = Dir, size = S, path = P], D)) :-
	exists_directory(P),
	!,
	scandir(P, Dir, D),
	aggregate_all(sum(X),
		      (member(element(_, As, _), D),
		       memberchk(size = X, As)
		      ), S).
dirtree(P, File, element(file, A, Q)) :-
	(   catch((exists_file(P), size_file(P, S)), E, (print_message(error, E), S = 0))
	->  A = [name = File, size = S, path = P],
	    embed(P, Q)
	;   A = [name = File, size = 0, path = P]
	).

scandir(P, _Dir, D) :-
	directory_files(P, L),
	findall(X, ( member(E, L), E \== '..', E \== '.',
		     format(atom(Q), '~w/~w', [P, E]),
		     dirtree(Q, E, X)), D).

%%	sortree(+T, -S)
%%	sortree(:P, +T, -S)
%
%	sort XML elements by name
%
sortree(T, S) :-
	sortree(compare_by_attr(name), T, S).

:- meta_predicate sortree(3, +, -).

sortree(C, element(E, As, Cs), element(E, As, Ss)) :-
	predsort(C, Cs, Ts),
	maplist(sortree, Ts, Ss).
sortree(_C, E, E).

compare_by_attr(A, R, X, Y) :-
	xpath(X, /self(@A), Vx),
	xpath(Y, /self(@A), Vy),
	( Vx @< Vy -> R = < ; R = > ).

%%	extension_embedding_enable is det
%
%	call before dirtree to load source text lines (not parsed)
%
extension_embedding_enable :-
	maplist(\E^(  extension_to_embed(E)
		   -> true
		   ;  assert(extension_to_embed(E))), [pl, php, tpl]).

embed(P, XLines) :-
	file_name_extension(_, X, P),
	extension_to_embed(X),
	setup_call_cleanup(open(P, read, H), fetch_lines(H, Lines), close(H)),
	maplist(\Line^XLine^(XLine = element(line, [text = Line], [])), Lines, XLines),
	!.
embed(_, []).

fetch_lines(H, L) :-
	read_line_to_codes(H, Codes),
	( Codes == end_of_file ->
	  L = []
	 ;
	  atom_codes(A, Codes),
	  xml_quote_attribute(A, Q),
	  fetch_lines(H, T),
	  L = [Q|T]
	).

%%	extensions_from_saved(-Result) is det.
%
%	search each DOM branch and extract extension
%
extensions_from_saved(Exts) :-
	source_target(_, XML),
	load_xml_file(XML, DOM),
	extensions_from_DOM(DOM, Exts).

extensions_from_DOM(DOM, Exts) :-
	aggregate_all(set(X), (xpath(DOM, //file(@name), File),
			       file_name_extension(_, X, File)
			      ), Exts).

%%	counted_extensions(Counted) is det.
%
%       get all extensions and count each file
%
counted_extensions(Counted) :-
	source_target(_, XML),
	load_xml_file(XML, DOM),
	setof(X = Count, (findall(Ext, (
				xpath(DOM, //file(@name), File),
				file_name_extension(_, Ext, File)), Exts),
			        aggregate(count, member(X, Exts), Count)
			 ), Counted).

counted_extensions_old(Counted) :-
	source_target(_, XML),
	load_xml_file(XML, DOM),
	extensions_from_DOM(DOM, ExtS),
	maplist(\Ext^Count^(aggregate_all(count,
					  (xpath(DOM, //file(@name), File),
					   file_name_extension(_, Ext, File)
					  ), C), Count = (Ext = C)
			   ), ExtS, Counted).

%%	get_ftp_ls_output(Stdout, Ftped)
%
%	parse output of ftp 'ls ...'
%
get_ftp_ls_output(Stdout, Ftped) :-
	phrase_from_file(get_ftp_ls_output(Ftped), Stdout).

eos([],[]).
get_ftp_ls_output([]) --> eos.
get_ftp_ls_output([E|Es]) -->
	ftp_ls_entry(E),
	get_ftp_ls_output(Es).
ftp_ls_entry(E) -->
	"d",
	entry(dir, E).
ftp_ls_entry(E) -->
	"-",
	entry(file, E).

s --> " ", whites.

permission([R, W, X]) -->
	[R, W, X].

entry(Kind, element(Kind, [name = Name, size = Size, lastmod = T, user = User, group = Group, perm = P, c2 = I1], [])) -->
	permission(UserP),
	permission(GroupP),
	permission(OthersP),
	s,
	integer(I1),
	s,
	string(GroupC),
	s,
	string(UserC),
	s,
	integer(Size),
	timestamp(T),
	string(NameC), "\n",
	{ maplist(atom_codes, P, [UserP, GroupP, OthersP]),
	  maplist(atom_codes, [User, Name, Group], [UserC, NameC, GroupC])
	}.

month(M, I) :-
	atom_codes(A, M),
	downcase_atom(A, D),
	nth1(I, [jan,feb,mar,apr,maj,jun,jul,aug,sep,oct,nov,dec], D)
	-> true
	;  I = 0.

timestamp(date(YYYY,MO,DD,HH,MM,SS,0,-,-)) -->
	(   " ", [M,O,N], " ", [D1,D2], " ", [H1,H2], ":", [M1,M2], " "
	->  {maplist(number_codes, [DD,HH,MM], [[D1,D2], [H1,H2], [M1,M2]]), month([M,O,N], MO), YYYY=2012, SS=0}
	;   " ", [M,O,N], " ", [D1,D2], "  ", [H1,H2,H3,H4], " " ->
	    {maplist(number_codes, [DD,YYYY], [[D1,D2], [H1,H2,H3,H4]]),  month([M,O,N], MO), HH=0, MM=0, SS=0}
	).


%%	dirzap(+X:atom) is det.
%
%	remove directory (content is lost of course)
%
dirzap(X) :-
	dirtree(X, T),
	forall(xpath(T, //file(@path), Path),
	       delete_file(Path)),
	findall(Path, xpath(T, //dir(@path), Path), Dirs),
	reverse(Dirs, RDirs),
	maplist(delete_directory, RDirs).

%%	capture_attrs(+Attr,+Elem,?Values) is det
%
%	get all Attr=Value from attrs
%
capture_attrs(Attr, element(_, Attrs, Tree), [Value|List]) :-
	memberchk(Attr = Value, Attrs),
	maplist(capture_attrs(Attr), Tree, SList),
	flatten(SList, List).

%%	assign_path(+NoPath, -WithPath)
%
%	assign to dirtree path attribute from root
%
assign_path(element(dir, A, NoPath), element(dir, A, WithPath)) :-
	memberchk(path=Path, A),
	maplist(assign_path(Path), NoPath, WithPath).

assign_path(Path, element(T, A, S), element(T, Wp, Sp)) :-
	memberchk(name=N, A),
	append(A, [path=Path], Wp),
	atomic_list_concat([Path, N], /, Next),
	(   T == dir
	->  maplist(assign_path(Next), S, Sp)
	;   Sp = S
	).
