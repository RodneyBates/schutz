
## -----------------------------------------------------------------------1- ##
## This file is part of the Schutz semantic editor.                          ##
## Copyright 1988..2020, Rodney M. Bates.                                    ##
## rodney.m.bates@acm.org                                                    ##
## Licensed under the MIT License.                                           ##
## -----------------------------------------------------------------------2- ##

# File updateinitial.sh, subdirectory scripts, Schutz Semantic editor. 

# Set various *.initial files equal to their prefix files.
# Ultimately, these files will be regenerated during bootstrapping of
# language addition and used to recompile.  Initial versions are for
# initial compilation of the code that does the generation.  They need
# not be fully up-to-date, but must be sufficient to initiall compile.
# This script copies the current versions to the initial versions, for
# future (re)bootstrapping. 

echo "Copy Ldl0 initial files."
cd ../ldl0/src
cp -p ../../boot/Ldl0Child.i3 Ldl0Child.i3.initial
cp -p ../../boot/Ldl0Tok.i3 Ldl0Tok.i3.initial
cp -p ../../boot/Ldl0MakeEst.m3 Ldl0MakeEst.m3.initial
cp -p ../../boot/Ldl0Sem.pkl Ldl0Sem.pkl.initial
ls -l *.initial 

echo "Copy Ldl1 initial files."
cd ../../ldl1/src
cp -p ../../boot/Ldl1Child.i3 Ldl1Child.i3.initial
cp -p ../../boot/Ldl1Tok.i3 Ldl1Tok.i3.initial
cp -p ../../boot/Ldl1MakeEst.m3 Ldl1MakeEst.m3.initial
cp -p ../../boot/Ldl1Sem.pkl Ldl1Sem.pkl.initial
ls -l *.initial 

echo "Copy M3 initial files."
cd ../../m3/src
cp -p ../../boot/M3Child.i3 M3Child.i3.initial
cp -p ../../boot/M3Tok.i3 M3Tok.i3.initial
cp -p ../../boot/M3MakeEst.m3 M3MakeEst.m3.initial
cp -p ../../boot/M3Sem.pkl M3Sem.pkl.initial
ls -l *.initial 

cd ../../scripts
