# schutz

A someday-to-be semantic editor for multiple, nicely defined
programming languages.

One day, the goal of this is to be an editor capable of doing semantic
browsing and editing on code that need not be complete or compilable.
Examples are finding all actual parameters corresponding to a formal
and nesting/unnesting procedures and other blocks of code.

As a base for semantic editing, this has entailed a many-year project
to allow editing to be done textually, while also maintaining an AST
view of the parsable portions of code.  Syntax directed editing has
turned out to be too ponderous for serious work, so the actual editing
works as text editing.

So far, as of 2021-02-04, Schutz mostly only does text editing with a
very strange and complicated internal representation, which can be
viewed as an abstract syntax tree.

The majority of the implementation is language-independent, subject to
some reasonable assumptions that not all programming languages comply
with, such as lexical scanning and parsing can be done without getting
entangled with nested scope analysis.  A specific language is
configured with a formal language definition in a specialized
language.  Currently, a lexical scanner for each language must be
hand-coded and follow an unusual interface.

Languages currently defined comprise two versions of its language
definition language (a primitive one used only for internal
bootstrapping) and Modula-3.  These are used in Schutz's own
implementation.

Schutz mostly maintains comments as typed, but does automated layout
of syntactic constructs.  If you don't like its layout rules, you can
rewrite them in the language definition.  There are several forms of
conditional layout rules you can use, e.g. conditional line breaks.

By default, Schutz maintains syntactically incorrect code as typed,
for you to correct as you desire, but it displays its suggested error
repairs and will apply them if asked to.  For syntactically incorrect
code, it uses the repaired portion to construct the AST.  The error
repair algorithm is elaborate.

Lexical scanning and parsing are incremental, reexamining only a
region nearby to textual edits that have been made.  Schutz does this
only when asked.  It is hopefully apparent that reparsing after every
keystroke would be a user nightmare.  The planned semantic analysis
system is also partial and incremental.

A great deal of effort has gone into making the internal
representation compact.  Early estimates showed around 5 times pure
text file size.  This is good, compared to most internal tree forms,
which tend to be more like ten times or more.  However, moving to
64-bit machines will have increased this.

To learn more or try it out, start by looking at the embryonic
documentation files in subdirectory ./doc/pdf or ./doc/html of the
repository.

