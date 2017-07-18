
## -----------------------------------------------------------------------1- ##
## This file is part of the Schutz semantic editor.                          ##
## Copyright 1988..2017, Rodney M. Bates.                                    ##
## rodney.m.bates@acm.org                                                    ##
## Licensed under the MIT License.                                           ##
## -----------------------------------------------------------------------2- ##

# File AddM3.sh, subdirectory scripts, Schutz Semantic editor. 

# Generate files needed to add language M3 to Schutz.  
# Must start with an LdlBatch built to work on Ldl1.

source ../setCompiler.sh 

UOPT="" #"" to parse with handwritten grammer, "-u" for generated. 

cd ../boot
rm -f M3.ldl1 
cp -p ../m3/src/M3.ldl1 M3.ldl1 

echo "========================================================================"
echo "Generate many files for M3:"
LDLBATCH="./LdlBatch -aAcefinprstgGRS $UOPT M3.ldl1"
echo "  In directory: boot, Command: ${LDLBATCH}" 
$LDLBATCH
RES=$?
if ! [ $RES = 0 ]
then
  exit 1
fi
cd ../scripts

if ! ./updatem3.sh
then
  exit 1
fi

echo "========================================================================"
echo "Build Lbe, using all generated files:"
echo "  In directory: edit, Command: $M3C" 
cd ../edit
if ! $M3C 
then
  echo "Build of Lbe failed."
  echo "But try running AddM3.sh."
  exit 1
fi
cd ../scripts 

echo "========================================================================"
echo "AddM3.sh, completed normally." 

