
## -----------------------------------------------------------------------1- ##
## This file is part of the Schutz semantic editor.                          ##
## Copyright 1988..2020, Rodney M. Bates.                                    ##
## rodney.m.bates@acm.org                                                    ##
## Licensed under the MIT License.                                           ##
## -----------------------------------------------------------------------2- ##

# File unboot.sh, subdirectory scripts, Schutz Semantic editor. 

source ../setCompiler.sh 

echo "Resetting to manual versions of Ldl0Tok.i3 and Ldl0Child.i3"
cd ../ldl0/src
cp -p Ldl0Tok.i3.manual ../derived/Ldl0Tok.i3
rm ../$TARGET/Ldl0Tok_i.o 
rm ../$TARGET/Ldl0Tok.io 
rm -f ..$TARGET/Ldl0Tok.mo
echo "Ldl0Tok.i3 restored to manual version:"
ls -l ../derived/Ldl0Tok.i3*

cp -p Ldl0Child.i3.manual ../derived/Ldl0Child.i3
echo "Ldl0Child.i3 restored to manual version:"
rm ../$TARGET/Ldl0Child_i.o 
rm ../$TARGET/Ldl0Child.io 
ls -l ../derived/Ldl0Child.i3*

