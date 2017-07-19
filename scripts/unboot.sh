
## -----------------------------------------------------------------------1- ##
## This file is part of the Schutz semantic editor.                          ##
## Copyright 1988..2017, Rodney M. Bates.                                    ##
## rodney.m.bates@acm.org                                                    ##
## Licensed under the MIT License.                                           ##
## -----------------------------------------------------------------------2- ##

# File unboot.sh, subdirectory scripts, Schutz Semantic editor. 

source ../setCompiler.sh 

echo "Resetting to manual versions of Ldl0Tok.i3 and Ldl0Child.i3"
cd ../ldl0/src
cp Ldl0Tok.i3.manual ../derived/Ldl0Tok.i3
# ^It would be nice to use -p here, so the date on the
#   source file reflects its actual creation date, but
#   this undermines the recompilation of the compiler. 
echo "Ldl0Tok.i3 restored to manual version:"
ls -l ../derived/Ldl0Tok.i3*

cp Ldl0Child.i3.manual ../derived/Ldl0Child.i3
echo "Ldl0Child.i3 restored to manual version:"
ls -l ../derived/Ldl0Child.i3*

# Force recompile: (manual versions have very old dates)  
rm -f ../../ldlboot/$TARGET/Ldl0Tok.io
rm -f ../../ldlboot/$TARGET/Ldl0Tok.mo
rm -f ../../ldlboot/$TARGET/Ldl0Child.io
rm -f ../../ldlboot/$TARGET/Ldl0Child.mo

