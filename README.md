# schutz
A someday-to-be semantic editor for multiple, nicely defined programming languages.

One day, the goal of this is to be an editor capable of doing semantic browsing
and editing on code that need not be complete or compilable.  Examples are finding 
all actual parameters corresponding to a formal and nesting/unnesting procedures
and other blocks of code.  

As a base for semantic editing, this has entailed a multi-year project to allow
editing to be done textually, while also maintaining an AST view of the parsable
portions of code.  Syntax directed editing has turned out to be too ponderous for
serious work, so the actual editing works as text editing. 

So far, as of 2017-07-18, Schutz mostly only does text editing with a very strange 
and complicated internal representation, but it can be viewed as an AST too.  

The majority of the implementation is language-independent, subject to some 
reasonable assumptions that not all programming languages comply with, such
as lexical scanning and parsing can be done without getting entangled with 
nested scope analysis.  A specific langauge is configured with a formal 
language definition in a specialized language.  Currently, a lexical scanner
for each language must be hand-coded and follow an unusual interface.  

Languages currently defined comprise two versions of its language definition 
language (a primitive one used only for internal bootstrapping) and Modula-3.
These are used in Schutz's own implementation.  

Schutz mostly maintains comments as typed, but does automated layout of syntactic
constructs.  If you don't like its layout rules, you can rewrite them in the
language definition.  There are several forms of conditional layout rules you 
can use, e.g. conditional line breaks.   

By default, Schutz maintains syntactically incorrect code as typed, for you to 
correct as you desire, but will apply its error corrections if asked to.  Its 
error corrections are used to construct parts of the AST for syntactically incorrect 
code.  The error correction is elaborate. It reparses only when asked.  

A great deal of effort has gone into making the internal representation compact.
Early estimates showed around 5 times pure text file size.  This is good, compared
to most internal tree forms, which tend to be more like ten times or more.  
