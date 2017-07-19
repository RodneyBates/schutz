
## -----------------------------------------------------------------------1- ##
## This file is part of the Schutz semantic editor.                          ##
## Copyright 1988..2017, Rodney M. Bates.                                    ##
## rodney.m.bates@acm.org                                                    ##
## Licensed under the MIT License.                                           ##
## -----------------------------------------------------------------------2- ##

# File removecompiled.sh, subdirectory scripts, Schutz Semantic editor. 

# Remove all compiled code. 

source ../setCompiler.sh 

rm -fr ../ldlboot/$TARGET
rm -fr ../ldlbatch/$TARGET
rm -fr ../edit/$TARGET
rm -fr ../schutzcommon/$TARGET
