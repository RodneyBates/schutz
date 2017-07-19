
## -----------------------------------------------------------------------1- ##
## This file is part of the Schutz semantic editor.                          ##
## Copyright 1988..2017, Rodney M. Bates.                                    ##
## rodney.m.bates@acm.org                                                    ##
## Licensed under the MIT License.                                           ##
## -----------------------------------------------------------------------2- ##

# File tocm3.sh, subdirectory scripts, Schutz Semantic editor. 

# Change things to build with CM3

rm -f ../setCompiler.sh
ln -s ./scripts/setcm3.sh ../setCompiler.sh
echo "Compiler for script commands set to CM3."

./topickle2.sh 
echo "Pickle imports set to 'Pickle2'."
./removecompiled.sh 
echo "Compiled and pickle files removed."
./copynilpickles.sh
echo "NIL pickles copied." 
