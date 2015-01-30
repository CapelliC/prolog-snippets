/** <module> allocator
*/
:- module(allocator,
	  [alloc_init/0
	  ,alloc_new/2
	  ,alloc_get/2
	  ,alloc_set/2
	  ,alloc_clear/0
	  ]).

alloc_init :-
	( nb_current(allocator, _) -> alloc_clear ; true ),
	nb_setval(allocator, 0).

alloc_new(Obj, Key) :-
	nb_getval(allocator, Free),
	Next is Free + 1,
	nb_setval(allocator, Next),
	atom_number(Key, Free),
	nb_setval(Key, Obj).

alloc_get(Key, Obj) :-
	nb_getval(Key, Obj).
alloc_set(Key, Obj) :-
	nb_setval(Key, Obj).

alloc_clear :-
	nb_getval(allocator, Free),
	forall(between(1, Free, C),
	       (   atom_number(Key, C),
		   nb_delete(Key)
	       )),
	nb_delete(allocator).
