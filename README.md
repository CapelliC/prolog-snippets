Some note on components

+ lifter  : descriptive functions - a bit of user friendly Prolog
+ dirtree : a declarative interface to file system (xml at work), and a bit of ftp
+ lag     : linear aggregation - just a sketch by now, showing how efficient aggregation can be performed in Prolog

About lifter, I can't resist...

here is part of a comment from SWI-Prolog mailing list [https://lists.iai.uni-bonn.de/pipermail/swi-prolog/2013/011893.html],
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

License LGPL V2.1
Author Ing. Capelli Carlo
