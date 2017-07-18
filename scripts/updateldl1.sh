
## -----------------------------------------------------------------------1- ##
## This file is part of the Schutz semantic editor.                          ##
## Copyright 1988..2017, Rodney M. Bates.                                    ##
## rodney.m.bates@acm.org                                                    ##
## Licensed under the MIT License.                                           ##
## -----------------------------------------------------------------------2- ##

# File updateldl1.sh, subdirectory scripts, Schutz Semantic editor. 

# Copy various generated files for ldl0 to places they will be sought. 

echo "========================================================================"
echo "  Copy various generated files to where they will be sought." 
echo "  Copying generated source files for Ldl1 to ldl1/derived" 
cd ../ldl1/derived
cp -p ../../boot/Ldl1Tok.i3 Ldl1Tok.i3 
cp -p ../../boot/Ldl1Child.i3 Ldl1Child.i3 
cp -p ../../boot/Ldl1MakeEst.m3 Ldl1MakeEst.m3 
ls -l Ldl1Tok.i3 Ldl1Child.i3 Ldl1MakeEst.m3  

echo "  Copying generated pickle files for Ldl1 to ldlbatch/derived" 
cd ../../ldlbatch/derived
cp -p ../../boot/Ldl1Sem.pkl Ldl1Sem.pkl
ls -l Ldl1Sem.pkl 

echo "  Copying generated pickle files for Ldl1 to edit/derived" 
cd ../../edit/derived
cp -p ../../boot/Ldl1Sem.pkl Ldl1Sem.pkl
ls -l Ldl1Sem.pkl 

echo "  Copying Ldl1Sem.pkl to resources "
cd ../../resources
cp -p ../boot/Ldl1Sem.pkl Ldl1Sem.pkl
ls -l Ldl1Sem.pkl
cd ../scripts 




