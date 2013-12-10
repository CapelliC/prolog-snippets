Some Prolog code I wrote, and sometime used

About lifter, I can't resist the temptation to report part of a comment
from SWI-Prolog mailing list [http://lists.iai.uni-bonn.de/pipermail/swi-prolog/2013/011893.html]

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
