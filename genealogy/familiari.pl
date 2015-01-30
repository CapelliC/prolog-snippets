%%	<module> familiari
%
%	display a graph of known family members
%
%	@author: Ing. Carlo Capelli
%	@license: LGPL v2.1
%

:- module(familiari, [familiari/0]).
:- use_module(genealogy).

% interface

familiari :- genealogy(familiari, 'Maria Family').

:- op(100, xfx, gen).
:- op(100, fx, femm).
:- op(100, fx, masc).
:- op(100, fx, fu).

parent_child(P,C) :- P gen C.
female(P) :- femm P.
male(P) :- masc P.
died(P) :- fu P.

% database

laura gen leonardo.
laura gen chicca.
laura gen clara.
laura gen teresa.

giovanni gen teresa.
giovanni gen gabri.
giovanni gen eros.
giovanni gen sandro.
giovanni gen angelo.

caterina gen eros.
caterina gen gabri.

eli gen sandro.
eli gen angelo.

teresa gen carla.
teresa gen giorgio.
teresa gen lia.
teresa gen maria.
teresa gen antonia.
teresa gen cinzia.

battista gen carla.
battista gen giorgio.
battista gen lia.
battista gen maria.
battista gen antonia.
battista gen cinzia.

maria gen stella.
maria gen ivan.
carlo gen stella.
carlo gen ivan.

antonia gen nilo.
antonia gen rais.
antonia gen arno.
antonia gen amira.
angeloa gen nilo.
angeloa gen rais.
angeloa gen arno.
angeloa gen amira.

lia gen loris.
paolo gen loris.

carla gen sara.
carla gen fiore.
carla gen silvia.

giulio gen sara.
giulio gen fiore.
giulio gen silvia.

cinzia gen elisa.
alberto gen elisa.

catina gen battista.
catina gen gianna.
catina gen luigi.
catina gen andrea.

giosuè gen battista.
giosuè gen gianna.
giosuè gen luigi.
giosuè gen andrea.

andrea gen sergio.
andrea gen fabio.
dorvalina gen sergio.
dorvalina gen fabio.

elisa gen michael.
elisa gen dylan.
mauro gen michael.
mauro gen dylan.

gianna gen fabrizio.
gianna gen rudy.
gianna gen willy.
gianna gen laurag.
gianna gen oscar.

giacomo gen fabrizio.
giacomo gen rudy.
giacomo gen willy.
giacomo gen laurag.
giacomo gen oscar.

leonardo gen carlo_s.
leonardo gen miriam.
leonardo gen rosanna.

silvana gen carlo_s.
silvana gen miriam.
silvana gen rosanna.

silvana gen elena.
umberto gen elena.

silvia gen giulia.
silvia gen christian.

nicola gen giulia.
nicola gen christian.

gabri gen manuela.
mario gen manuela.

angelo gen elisabetta.

sandro gen roberta.
sandro gen eli_sandro.

zanina gen roberta.
zanina gen roberta.

clara gen christian_c.
clara gen massimiliano.

bocchio gen christian_c.
bocchio gen massimiliano.

% optional person attributes

femm eli.
femm catina.
femm laura.
femm teresa.
femm stella.
femm maria.
femm amira.
femm antonia.
femm lia.
femm carla.
femm sara.
femm silvia.
femm cinzia.
femm elisa.
femm caterina.
femm caterina.
femm gabri.
femm gianna.
femm chicca.
femm clara.

masc angelo.
masc loris.
masc paolo.
masc giulio.
masc fiore.
masc giorgio.
masc alberto.
masc giosuè.
masc luigi.
masc andrea.
masc sandro.
masc eros.
masc leonardo.
masc giovanni.
masc battista.
masc ivan.
masc carlo.
masc nilo.
masc rais.
masc arno.
masc angeloa.

fu nilo.
fu sara.
fu luigi.
fu andrea.
fu sandro.
fu eros.
fu leonardo.
fu luigi.
fu gabri.
fu giovanni.
fu laura.
fu giosu.
fu catina.
fu eli.
