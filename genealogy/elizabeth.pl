:- module(elizabeth, [elizabeth/0, is_maternal_aunt/2, is_sister/2]).
:- use_module(genealogy).

elizabeth :- genealogy(elizabeth, 'Elizabeth II Family').

female('Elizabeth II').
female('Lady Elizabeth Bowes-Lyon').
female('Princess Mary of Teck').
female('Cecilia Cavendish-Bentinck').
female('Rose Bowes-Lyon').

parent_child('George VI', 'Elizabeth II').
parent_child('Lady Elizabeth Bowes-Lyon','Elizabeth II').

parent_child('George V', 'George VI').
parent_child('Princess Mary of Teck', 'George VI').

parent_child('Cecilia Cavendish-Bentinck','Rose Bowes-Lyon').
parent_child('Cecilia Cavendish-Bentinck','Lady Elizabeth Bowes-Lyon').
parent_child('Claude Bowes-Lyon', 'Lady Elizabeth Bowes-Lyon').
parent_child('Claude Bowes-Lyon', 'Rose Bowes-Lyon').

is_sister(Person, Sister) :-
	parent_child(Parent, Person),
	parent_child(Parent, Sister),
	Sister \= Person,
	female(Sister).

is_maternal_aunt(Person, Aunt) :-
	parent_child(Parent, Person),
	female(Parent),
	parent_child(GranParent, Parent),
	parent_child(GranParent, Aunt), Aunt \= Parent.
