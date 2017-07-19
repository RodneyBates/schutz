
## -----------------------------------------------------------------------1- ##
## This file is part of the Schutz semantic editor.                          ##
## Copyright 1988..2017, Rodney M. Bates.                                    ##
## rodney.m.bates@acm.org                                                    ##
## Licensed under the MIT License.                                           ##
## -----------------------------------------------------------------------2- ##

# File veryclean.sh, subdirectory scripts, Schutz Semantic editor.

# Reset to an original state, ready for fullboot.sh

source ../setCompiler.sh 

cd ..
rm -fr ldlboot/$TARGET
rm -fr ldlbatch/$TARGET
rm -fr edit/$TARGET

rm -fr common/derived
rm -fr ldl0/derived
rm -fr ldl1/derived
rm -fr m3/derived
rm -fr ldlbatch/derived
rm -fr edit/derived

rm -fr boot

cd scripts 
