/*  Part of SWI-Prolog

    Author:        Carlo Capelli
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
	[dirtree/2		% +Root, -Tree
	,dirtree/3		% +Path, +Curr, -Tree
	,sortree/2		% +Tree, -Sorted
	,sortree/3		% +Compare, +Tree, -Sorted
	,compare_by_attr/4	% +Attribute, +Item1, +Item2, -Rel
	,extension_embedding/0	%
	,extension_to_embed/1	% +Ext
	,dom_ext_file/3		% +DOM, ?Ext, ?File
	,dom_kind_ext_entry/4	% +DOM, +Kind, ?Ext, ?Entry
	,source_target/2		% +Path, +XML
	,extensions_from_saved/1	% -Extensions
	,extensions_from_DOM/2	% +Dom, -Extensions
	,counted_extensions/1	% -CountedExtensions
	,counted_extensions/2	% +DOM, -Counted
	,get_ftp_ls_output/2	% +Stdout, -Ftped:list
	,get_ftp_ls_output//1	% -Ftped:list
	,dirzap/1		% +Root
	,capture_attrs/3		% +Attr, +Elem, ?Values
	,assign_path/2		% +DOM, -DOM
	,entry_attribute/3	% +DOM, +Attr, ?Value
	,entry_upd_attr/4       % +Old, +Attr, +Val, -New
	,entry_path_name/3	% +DOM, ?Path, ?Name
	,entry_extensions/4	% +DOM, +Tag, +Exts, ?Entry
	]).

:- use_module(library(dcg/basics)).
:- use_module(library(aggregate)).
:- use_module(library(xpath)).

/** <module> Filesystem listing to XML.

This library provides utilities to load directories content from
filesystem to XML element/3 and some utility. It's also possible load
text of selected files types (by extension).

Of interest could be the parsing of FTP command 'ls', allowing
remote browsing after =process_create= with redirection of standard
output.

*/

%% extension_to_embed(?Extension) is det.
%
%  declare which file kind must have its text loaded
%
:- dynamic extension_to_embed/1.

%% source_target(Source, Target) is det.
%
%  cache a correspondence between folder path and save file
%
:- dynamic source_target/2.

%%	dirtree(+Root, ?Xml) is det.
%
%	Start actual scan, store Root path on Root element.
%
%	@arg Root the directory *absolute* path from where to start.
%	@arg Xml the structure read from directory Root.
%
dirtree(P, T) :-
	dirtree(P, P, S),
	S = element(dir, [N, D], X),
	T = element(dir, [N, D, path = P], X).

%%	dirtree(+Path, +Item, ?Xml) is det.
%
%	Start actual scan from provided path.
%
%	@arg Path actual path to scan.
%	@arg Root the directory *absolute* path from where to start.
%	@arg Xml the structure read from directory Root.
%
dirtree(P, Dir, element(dir, [name = Dir, size = S], D)) :-
	exists_directory(P),
	!,
	scandir(P, Dir, D),
	aggregate_all(sum(X),
		      (member(element(_, As, _), D),
		       memberchk(size = X, As)
		      ), S).
dirtree(P, File, element(file, A, Q)) :-
	(   catch((exists_file(P), size_file(P, S)), E, (print_message(error, E), S = 0))
	->  A = [name = File, size = S],
	    embed(P, Q)
	;   A = [name = File, size = 0]
	).

scandir(P, _Dir, D) :-
	directory_files(P, L),
	findall(X, ( member(E, L), E \== '..', E \== '.',
		     format(atom(Q), '~w/~w', [P, E]),
		     dirtree(Q, E, X)), D).

%%	sortree(+T, -S)
%
%	sort XML elements by name
%
sortree(T, S) :-
	sortree(compare_by_attr(name), T, S), !.

:- meta_predicate sortree(3, +, -).

%% sortree(+C, +E, -S) is det.
%
%  performs a predsort of each branch found in element E
%
%  @arg C comparison predicate
%  @arg E input element, to be sorted
%  @arg S sorted element
%
sortree(C, element(E, As, Cs), element(E, As, Ss)) :-
	predsort(C, Cs, Ts),
	!, maplist(sortree, Ts, Ss).
sortree(_C, E, E).

%% compare_by_attr(A, R, X, Y) is det.
%
%  See predsort for a description. Avoid returning = of entries will be deleted.
%
%  @arg A attribute to use for comparison
%  @arg R comparison result
%  @arg X attribute on left node
%  @arg Y attribute on right node
%
compare_by_attr(A, R, X, Y) :-
	xpath(X, /self(@A), Vx),
	xpath(Y, /self(@A), Vy),
	( Vx @< Vy -> R = < ; R = > ).

%%	extension_embedding is det
%
%	call before dirtree to load source text lines (not parsed)
%
extension_embedding :-
	maplist(extension_embedding, [pl, php, tpl]).
extension_embedding(E) :-
	extension_to_embed(E)
	-> true
	;  assert(extension_to_embed(E)).

embed(P, XLines) :-
	file_name_extension(_, X, P),
	extension_to_embed(X),
	setup_call_cleanup(open(P, read, H), fetch_lines(H, Lines), close(H)),
	%maplist(\Line^XLine^(XLine = element(line, [text = Line], [])), Lines, XLines),
	maplist(embed_line, Lines, XLines),
	!.
embed(_, []).

embed_line(Line, XLine) :-
	XLine = element(line, [text = Line], []).

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

%% dom_ext_file(+DOM, ?Ext, ?File) is nondet.
%
%  get the extension from a filename entry
%
%  @arg DOM XML representation of directory
%  @arg Ext filename extension
%  @arg File filename
%
dom_ext_file(DOM, Ext, File) :-
	xpath(DOM, //file(@name), File),
	file_name_extension(_, Ext, File).

%% dom_kind_ext_entry(DOM, Kind, Ext, Entry) is nondet.
%
%  get filename extension of entry
%
%  @arg DOM XML representation of directory
%  @arg Kind attribute to match
%  @arg Ext pathname extension
%  @arg Entry the XML element/3 representation matched
%
dom_kind_ext_entry(DOM, Kind, Ext, Entry) :-
	xpath(DOM, //Kind, Entry),
	xpath(Entry, /self(@name), Name),
	file_name_extension(_, Ext, Name).

%% extensions_from_DOM(DOM, Exts) is det.
%
%  describe extensions_from_DOM
%
%  @arg DOM describe DOM
%  @arg Exts describe Exts
%
extensions_from_DOM(DOM, Exts) :-
	aggregate_all(set(X), (xpath(DOM, //file(@name), File),
			       file_name_extension(_, X, File)
			      ), Exts).

%%	counted_extensions(?Counted) is det.
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

%% counted_extensions(+DOM, -Counted:list) is det.
%
%  aggregate
%
%  @arg DOM XML folders representation
%  @arg Counted a list of Extension = OccurrenceCount
%
counted_extensions(DOM, Counted) :-
	setof(X = Count, (findall(Ext, (
				xpath(DOM, //file(@name), File),
				file_name_extension(_, Ext, File)), Exts),
			        aggregate(count, member(X, Exts), Count)
			 ), Counted).
/*
counted_extensions_old(Counted) :-
	source_target(_, XML),
	load_xml_file(XML, DOM),
	extensions_from_DOM(DOM, ExtS),
	maplist(\Ext^Count^(aggregate_all(count,
					  (xpath(DOM, //file(@name), File),
					   file_name_extension(_, Ext, File)
					  ), C), Count = (Ext = C)
			   ), ExtS, Counted).
*/

%%	get_ftp_ls_output(Stdout, Ftped)
%
%	parse output of ftp command 'ls ...'
%
get_ftp_ls_output(Stdout, Ftped) :-
	phrase_from_file(get_ftp_ls_output(Ftped), Stdout).

%%	get_ftp_ls_output(A,B,Entries) is det.
%
%	parse output of ftp command 'ls ...',
%	capturing all attributes found (and known)
%
%	@arg Entries XML structure from sequential listing of attributed entries
%
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

%% entry_attribute(element(+DOM, +Attr, ?Value) is det.
%
%  get a named value from node attributes
%
%  @arg DOM
%  @arg Attr name of attribute
%  @arg Value value of attribute
%
entry_attribute(element(_,Attrs,_), Attr, Value) :-
	memberchk(Attr=Value, Attrs).

entry_upd_attr(Old, Attr, Val, New) :-
	Old = element(T, As, C),
	New = element(T, Bs, C),
	(   append(L, [Attr=_ |R], As)
	->  append(L, [Attr=Val |R], Bs)
	;   Bs = [Attr=Val |As]
	).

%% entry_path_name(+DOM, ?Path, ?Name) is det.
%
%  get path and name of element
%
%  @arg E DOM structure
%  @arg Path element file path
%  @arg Name element file name
%
entry_path_name(E, Path, Name) :-
	entry_attribute(E, path, Path),
	entry_attribute(E, name, Name).

%%	entry_with_extensions(DOM, Tag, Exts, Entry) is nondet.
%
%	get all entries with filename matching extension
%
entry_extensions(DOM, Tag, Exts, Entry) :-
	Patt =.. [//,Tag],
	xpath(DOM, Patt, Entry),
	entry_attribute(Entry, name, N),
	file_name_extension(_, X, N),
	memberchk(X, Exts).
