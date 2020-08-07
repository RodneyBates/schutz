
## -----------------------------------------------------------------------1- ##
## This file is part of the Schutz semantic editor.                          ##
## Copyright 1988..2020, Rodney M. Bates.                                    ##
## rodney.m.bates@acm.org                                                    ##
## Licensed under the MIT License.                                           ##
## -----------------------------------------------------------------------2- ##

# File updateldl1.sh, subdirectory scripts, Schutz Semantic editor. 

# Copy various generated files for language ldl1 to places they will be sought. 

echo "========================================================================"
echo "  Copy various generated files to where they will be sought." 
echo "  Copy generated source files for Ldl1 to ldl1/derived" 
cd ../ldl1/derived
cp -p ../../boot/Ldl1Tok.i3 Ldl1Tok.i3 
rm -f ../$TARGET/Ldl1Tok_i.o # Force recompile.
rm -f ../$TARGET/Ldl1Tok.io # Force recompile.
cp -p ../../boot/Ldl1Child.i3 Ldl1Child.i3 
rm -f ../$TARGET/Ldl1Child_i.o # Force recompile.
rm -f ../$TARGET/Ldl1Child.io # Force recompile.
cp -p ../../boot/Ldl1MakeEst.m3 Ldl1MakeEst.m3 
rm -f ../$TARGET/Ldl1MakeEst_m.o # Force recompile.
rm -f ../$TARGET/Ldl1MakeEst.io # Force recompile.
ls -l Ldl1Tok.i3 Ldl1Child.i3 Ldl1MakeEst.m3  

echo "  Copy generated pickle files for Ldl1 to ldlbatch/derived" 
cd ../../ldlbatch/derived
cp -p ../../boot/Ldl1Sem.pkl Ldl1Sem.pkl
ls -l Ldl1Sem.pkl 

echo "  Copy generated pickle files for Ldl1 to edit/derived" 
cd ../../edit/derived
cp -p ../../boot/Ldl1Sem.pkl Ldl1Sem.pkl
ls -l Ldl1Sem.pkl 

echo "  Copy generated pickle files to resources "
cd ../../resources
cp -p ../boot/Ldl1Sem.pkl Ldl1Sem.pkl
ls -l Ldl1Sem.pkl
cd ../scripts 




