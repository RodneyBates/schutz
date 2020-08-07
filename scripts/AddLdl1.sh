
## -----------------------------------------------------------------------1- ##
## This file is part of the Schutz semantic editor.                          ##
## Copyright 1988..2020, Rodney M. Bates.                                    ##
## rodney.m.bates@acm.org                                                    ##
## Licensed under the MIT License.                                           ##
## -----------------------------------------------------------------------2- ##

# File AddLdl1.sh, subdirectory scripts, Schutz Semantic editor. 

# Generate files needed to add language Ldl1 to Schutz.  
# Must start with an LdlBatch built to work on Ldl0.

source ../setCompiler.sh 

UOPT="" #"" to parse with handwritten grammer, "-u" for generated. 

cd ../boot
rm -f Ldl1.ldl0 
cp -p ../ldl1/src/Ldl1.ldl0 Ldl1.ldl0 

echo "========================================================================"
echo "Generate many files for Ldl1:"
LDLBATCH="./LdlBatch -aAcefiknprstgGRS $UOPT Ldl1.ldl0"
echo "  In directory: ./boot, Command: $LDLBATCH" 
if ! $LDLBATCH
then
  exit 1
fi
cd ../scripts

if ! ./updateldl1.sh
then
  exit 1
fi

echo "========================================================================"
echo "Build LdlBatch, using all generated files:"
echo "  In directory: ldlbatch, Command: $M3C" 

cd ../schutzcommon 
echo "  In directory: `pwd`, Command: $M3C " 
if ! $M3C
then
  exit 1
fi

cd ../ldlbatch
if ! $M3C
then
  exit 1
fi
rm -f ../boot/LdlBatch 
if ! cp -p $TARGET/LdlBatch ../boot/LdlBatch
then
  exit 1
fi

echo "========================================================================"
echo "Build Lbe, using all generated files:"
echo "  In directory: edit, Command: $M3C" 

cd ../schutzcommon 
echo "  In directory: `pwd`, Command: $M3C " 
if ! $M3C
then
  exit 1
fi

cd ../edit
if ! $M3C
then
  exit 1
fi
cd ../scripts 

echo "========================================================================"
echo "AddLdl1.sh, completed normally." 
echo "========================================================================"

