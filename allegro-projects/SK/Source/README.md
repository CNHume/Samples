Document
--------
S-K Reduction Engine (Version 1.0.5)
Basic Survival Guide
Chris Hume 29-Mar-92


Purpose
-------
Implement elements of an elementary S-K Reduction Engine.
This code was developed in connection with ASU's CSE 555
"Automata Theory" taught by Prof. Ed Ashcroft, Fall 1991.


Background
----------
The user is assumed familiar with the contents of:

    "A New Implementation Technique for Applicative Languages" by
    D.A. Turner, published 1979, Software-Practice and Experience
    [vol.9, pp.31-49] John Wiley & Sons, Ltd.

wherein a language called SASL is described.

The article gives a concise introduction to Combinatory Logic,
and to the implementation technique known as "Graph Reduction"
(which this SK-Reduction Engine realizes.)

Comprehensive introduction to Combinatory Logic may be found in:

    "Introduction to Combinators and lambda-Calculus"
    J. Roger Hindley and Jonathan P. Seldin, 1986
    [London Mathematical Society: Student Texts, vol. 1]
    Cambridge University Press

Combinatory Logic, and the Lambda-Calculus, provide formal foundation for
the computer scientific sub-heading of "Functional Programming Languages".
The languages Haskell, Miranda, and Standard ML are all examples on this,
quite active, line of research.

The most distinctive feature of functional languages is that they adopt a lazy
evaluation semantics (as realized by a head-first normalization order) instead
of the eager, or applicative evaluation order used in "conventional" languages.

Thus functional languages strongly presuppose that memory is garbage collected.
This supports first class expression of partially reduced (functional) values;
and accounts for their compact equational presentation of recursive functions.


Operating Environment and Source Files
--------------------------------------
The files you will need to demonstrate operation
of the S-K Reduction Engine are as follows:

     SK.TEXT,   which contains the text you are now reading,
    SKA.LISP,   which implements Lambda* Abstraction,
    SKB.LISP,   which implements Beta Reduction,
    SKC.LISP,   is the Laboratory for new, experimental Combinators.
    SKD.LISP,   renders Turner's Test Programs in a dialect of SASL.
    SKT.LISP,   provides various extensions to SASL, in SASL itself.

Once in a (more or less) Common Lisp environment,
one would exit, normally, by evaluating the form:

Lisp> (exit)


Loading the S-K Reduction Engine into the Lisp Environment
----------------------------------------------------------
Compilation is optional in Common Lisp, but will improve
performance of the S-K Reduction Engine.

If SKA.FASL and SKB.FASL are not present, or if you need
to recompile them, enter the following sequence of forms:

Lisp> (compile-file "SKA.LISP")
Lisp> (load "SKA")
Lisp> (compile-file "SKB.LISP")
Lisp> (load "SKB")

Loading "SKA" before compiling "SKB" is not strictly
necessary, but will prevent certain Warning Messages
from being issued.

The SASL demonstration files similarly may be compiled,
prior to loading, although there is little performance
benefit (since SASL is "compiled" under S-K Reduction.)

Alternatively, the source files can be loaded directly.
The S-K Reduction Engine would then be implemented in
Lisp's interpretive mode.

To load the SASL source files directly, for example:

Lisp> (load "SKC.LISP")


Referring to Symbols in the SK Package
--------------------------------------
The "SK" Symbol Package specifies that certain of its
symbols are for EXPORT (e.g., certain interfaces).

This means that these symbols can be accessed by
prepending the SK Package Prefix.

Examples: SK:BETA, SK:DEFC, or SK:LAMBDA*

To avoid having to enter (or see) this prefix,
enter the form:

Lisp> (use-package 'sk)

The Package System will then attempt to unify these
Exported Symbols with the USER Package.

At this point, the S-K Reduction Engine should be
loaded up and ready to go!


Performing Abstraction
----------------------
The "SKA" sub-module augments the Lisp Reader
such that the syntax:

Lisp> [cdr][car](cons car cdr)

will be read as though one had typed:

Lisp> (lambda* cdr (lambda* car (cons car cdr)))

The LAMBDA* Macro will then be evaluated and the value
returned will correspond the the Lambda Abstraction of
the interior (SASL) form.

The code returned in this case would correspond to the
compiled form of a "Reversed CONS" operation.  To make
this operation into a User Defined Combinator, enter:

Lisp> (defc XCONS [cdr][car](cons car cdr))

The current definition of a User Defined Combinator
is obtained via the Symbol Accessor: CDEFINITION.

Note that the symbol in question must be quoted:

Lisp> (cdefinition 'xcons)

Such a definition will not be compiled until the first
time it is "Beta Reduced".  Thereafter, its definition
may reflect the result of self-modifying S-K Reduction.

The Primitive Combinators are currently dispatched
without looking to see if they have a redefinition.

Note that, when called from within the Lisp Evaluation
context, the LAMBDA* macro will accept certain keyword
options.

For example, to disable Turner's S-Abbreviations enter:

Lisp> (lambda* cdr [car](cons car cdr) :optimize nil)

These keywords all have corresponding Parameters,
which hold their current Default Values.  These
Parameters can be set to new values when keyword
passing becomes tedious.

To specify that only the S-Abstractions of Turner should be
applied (but that neither the H nor the E Combinator should
be produced), by default, one would enter the form:

Lisp> (setq *optimize* :standard)

This default mechanism MUST be resorted to where the
Currying Operator, [], is concerned since its syntax
does not provide for keyword passing.


Reductions
----------
One enters into the S-K Reduction Engines evaluation
context via the BETA Macro:

Lisp> (beta (xcons foo bar))

The Beta Reduced value will then be returned: (BAR . FOO),
for example.  If you are messing about with Recursive
Functions the Y-Combinator is apt to produce Circular
(i.e., Infinitely Repeating) Lists.  In order to allow
the Lisp Evaluation environment to PRINT these values,
finitely, the variable *PRINT-CIRCLE* must be bound to
a non-NIL value.

To ensure this, enter the following form:

Lisp> (setq *print-circle* t)

The BETA Macro also accepts various keyword options, so
for example, to disable reduction of CL-Terms interior
to CL-Terms that have been determined to be irreducible:

Lisp> (beta (xcons foo bar) :normal nil)

To disable the reduction of "Interior CL-Terms",
by default, one would enter the form:

Lisp> (setq *normal* nil)

Note that where LAMBDA* is applied as a Combinator,
within the S-K Reduction Engine evaluation context,
options for Abstraction are set via the BETA Macro.

CL-Term syntax doesn't provide for keyword options.


Counting, Tracing, and Warnings
-------------------------------
The LAMBDA* Abstraction Macro and the BETA Reduction
Macro each take the three additional keywords below:

    Keyword     Parameter       Values
    -------     ---------       ------
    :count      *count*         Non-NIL enables Abstraction
                                and Reduction Counting.
    :trace      *trace*         Non-NIL enables Abstraction
                                and Reduction Tracing.
    :warn       *warn*          Non-NIL (the default value)
                                enables Usage Warnings.


Beta Reduction vs. Lisp Evaluation
----------------------------------
Please note that a Beta Reduction must be evaluated
within the BETA Macro (which passes it to SK-EVAL).

Typing something like:

Lisp> ([cdr][car](cons car cdr) foo bar)

will result in Lisp trying to evaluate an expression
containing Primitive Combinators, and will not yield
meaningful results.

The BETA Macro establishes the S-K Reduction context:

Lisp> (beta ([cdr][car](cons car cdr) foo bar))
