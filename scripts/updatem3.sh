
## -----------------------------------------------------------------------1- ##
## This file is part of the Schutz semantic editor.                          ##
## Copyright 1988..2020, Rodney M. Bates.                                    ##
## rodney.m.bates@acm.org                                                    ##
## Licensed under the MIT License.                                           ##
## -----------------------------------------------------------------------2- ##

# File updatem3.sh, subdirectory scripts, Schutz Semantic editor. 

# Copy various generated files for language m3 to places they will be sought. 

echo "  Copy generated source files for Modula-3 to m3/derived" 
cd ../m3/derived
cp -p ../../boot/M3Tok.i3 M3Tok.i3 
rm -f ../$TARGET/M3Tok_i.o # Force recompile.
rm -f ../$TARGET/M3Tok.io # Force recompile.
cp -p ../../boot/M3Child.i3 M3Child.i3 
rm -f ../$TARGET/M3Child_i.o # Force recompile.
rm -f ../$TARGET/M3Child.io # Force recompile.
cp -p ../../boot/M3InitTokStrings.m3 M3InitTokStrings.m3
rm -f ../$TARGET/M3InitTokStrings_m.o # Force recompile.
rm -f ../$TARGET/M3InitTokStrings.mo
cp -p ../../boot/M3MakeEst.m3 M3MakeEst.m3
rm -f ../$TARGET/M3MakeEst_m.o # Force recompile.
rm -f ../TARGET/M3MakeEst.mo
ls -l M3Tok.i3 M3Child.i3 M3InitTokStrings.m3 M3MakeEst.m3  

echo "  Copy generated pickle files for M3 to ldlbatch/derived" 
cd ../../ldlbatch/derived
cp -p ../../boot/M3Sem.pkl M3Sem.pkl
ls -l M3Sem.pkl 

echo "  Copy generated pickle files for M3 to edit/derived" 
cd ../../edit/derived
cp -p ../../boot/M3Sem.pkl M3Sem.pkl
ls -l M3Sem.pkl 

echo "  Copy generated pickle files to resources "
cd ../../resources
cp -p ../boot/M3Sem.pkl M3Sem.pkl
ls -l M3Sem.pkl
cd ../scripts 

