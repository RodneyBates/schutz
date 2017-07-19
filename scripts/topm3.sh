
## -----------------------------------------------------------------------1- ##
## This file is part of the Schutz semantic editor.                          ##
## Copyright 1988..2017, Rodney M. Bates.                                    ##
## rodney.m.bates@acm.org                                                    ##
## Licensed under the MIT License.                                           ##
## -----------------------------------------------------------------------2- ##

# File topm3.sh, subdirectory scripts, Schutz Semantic editor. 

# Change things to build with PM3

rm -f ../setCompiler.sh
ln -s ./scripts/setpm3.sh ../setCompiler.sh
echo "Compiler for script commands set to PM3"

./topickle.sh 
echo "Pickle imports set to 'Pickle'."
./removecompiled.sh 
echo "Compiled files removed."
./copynilpickles.sh
echo "NIL pickles copied." 
