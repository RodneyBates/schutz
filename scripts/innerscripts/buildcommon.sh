
## -----------------------------------------------------------------------1- ##
## This file is part of the Schutz semantic editor.                          ##
## Copyright 1988..2020, Rodney M. Bates.                                    ##
## rodney.m.bates@acm.org                                                    ##
## Licensed under the MIT License.                                           ##
## -----------------------------------------------------------------------2- ##

# File scripts/innerscripts/buildcommon.sh, Schutz Semantic editor. 

# Build and ship package schutzcommon.
# Call with pwd the topmost directory of the repository.

cd scripts
source ../setCompiler.sh
cd ..

cd schutzcommon 
echo "  In directory: `pwd`, Command: $M3C " 
if ! $M3C
then
  cd ..
  exit 1
fi

COMMAND="$M3C -ship"
echo "  In directory: `pwd`, Command: $COMMAND" 
if ! $COMMAND
then
  cd ..
  exit 1
fi
cd ..


