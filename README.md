Some note on components

+ lifter  : descriptive functions - a bit of user friendly Prolog
+ dirtree : a declarative interface to file system (xml at work), and a bit of ftp
+ lag     : linear aggregation - just a sketch by now, showing how efficient aggregation can be performed in Prolog

About lifter, I can't resist...

here is part of a comment from SWI-Prolog mailing list (original list no more available, see http://swi-prolog.996271.n3.nabble.com/Renaming-map-dict-class-type-tp13585p13742.html),

explaining what lifter is.

From the very first lifter' users:

===========

With `lifter`, the clause

    longer(A,B) :-
        length(A,º) > length(B,º).

is expanded to

    longer(A, B) :-
        length(A, C),
        length(B, D),
        C > D.

I like that this approach satisfies the desire for expressions that
"evaluate in place" but keeps the flexibility and potent, multi-directional
functionality of prolog relations.

===========

License LGPL 2.1
Author Capelli Carlo
