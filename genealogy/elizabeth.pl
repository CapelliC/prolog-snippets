:- module(elizabeth, [elizabeth/0]).
:- use_module(genealogy).

elizabeth :- genealogy(elizabeth, 'Elizabeth II Family').

parent_child('George VI', 					'Elizabeth II').
parent_child('Lady Elizabeth Bowes-Lyon', 	'Elizabeth II').

parent_child('George V', 					'George VI').
parent_child('Princess Mary of Teck', 		'George VI').

parent_child('Cecilia Cavendish-Bentinck',	'Lady Elizabeth Bowes-Lyon').
parent_child('Claude Bowes-Lyon',			'Lady Elizabeth Bowes-Lyon').
