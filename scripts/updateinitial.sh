
## -----------------------------------------------------------------------1- ##
## This file is part of the Schutz semantic editor.                          ##
## Copyright 1988..2017, Rodney M. Bates.                                    ##
## rodney.m.bates@acm.org                                                    ##
## Licensed under the MIT License.                                           ##
## -----------------------------------------------------------------------2- ##

# File updateinitial.sh, subdirectory scripts, Schutz Semantic editor. 

# Set various *.initial files equal to their prefix files.   

echo "Copying Ldl0 initial files."
cd ../ldl0/src
cp -p ../../boot/Ldl0Child.i3 Ldl0Child.i3.initial
cp -p ../../boot/Ldl0Tok.i3 Ldl0Tok.i3.initial
cp -p ../../boot/Ldl0MakeEst.m3 Ldl0MakeEst.m3.initial
cp -p ../../boot/Ldl0Sem.pkl Ldl0Sem.pkl.initial
ls -l *.initial 

echo "Copying Ldl1 initial files."
cd ../../ldl1/src
cp -p ../../boot/Ldl1Child.i3 Ldl1Child.i3.initial
cp -p ../../boot/Ldl1Tok.i3 Ldl1Tok.i3.initial
cp -p ../../boot/Ldl1MakeEst.m3 Ldl1MakeEst.m3.initial
cp -p ../../boot/Ldl1Sem.pkl Ldl1Sem.pkl.initial
ls -l *.initial 

echo "Copying M3 initial files."
cd ../../m3/src
cp -p ../../boot/M3Child.i3 M3Child.i3.initial
cp -p ../../boot/M3Tok.i3 M3Tok.i3.initial
cp -p ../../boot/M3MakeEst.m3 M3MakeEst.m3.initial
cp -p ../../boot/M3Sem.pkl M3Sem.pkl.initial
ls -l *.initial 

cd ../../scripts
