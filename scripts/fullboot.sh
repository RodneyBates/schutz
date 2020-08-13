
## -----------------------------------------------------------------------1- ##
## This file is part of the Schutz semantic editor.                          ##
## Copyright 1988..2020, Rodney M. Bates.                                    ##
## rodney.m.bates@acm.org                                                    ##
## Licensed under the MIT License.                                           ##
## -----------------------------------------------------------------------2- ##

# File fullboot.sh, subdirectory scripts, Schutz Semantic editor. 

# Full rebootstrap of Schutz: Execute in the scripts directory,
# beside directories schutzcommon, ldlboot

source ../setCompiler.sh 

UOpt="" # "" to parse with handwritten grammar, "-u" for generated. 

echo "========================================================================"
echo "Set up derived directories"

if test ! -d ../boot 
then 
  echo "  Create working directory ./boot:"
  mkdir ../boot 
fi

./setupderived.sh

echo "========================================================================"
echo "Build LdlBoot using manually created Ldl0Tok.i3 and Ldl0Child.i3:"

echo "  Reset to handcoded versions of Ldl0Tok.i3 and Ldl0Child.i3"
cd ../ldl0/derived

#echo "    Rename old version to ./ldl0/derived/" `newversion Ldl0Tok.i3`
cp -p ../src/Ldl0Tok.i3.manual Ldl0Tok.i3
rm -f ../$TARGET/Ldl0Tok_i.o # Force recompile.
rm -f ../$TARGET/Ldl0Tok.io # Force recompile.
echo "    Ldl0Tok.i3 reset to manual version:"
ls -l Ldl0Tok.i3

#echo "    Rename old version to ./ldl0/derived/" `newversion Ldl0Child.i3`
cp -p ../src/Ldl0Child.i3.manual Ldl0Child.i3
rm -f ../$TARGET/Ldl0Child_i.o # Force recompile.
rm -f ../$TARGET/Ldl0Child.io # Force recompile.
echo "    Ldl0Child.i3 reset to manual version:"
ls -l Ldl0Child.i3

# Rebuild schutzcommon and ldlboot.
cd ../..
./scripts/innerscripts/buildcommon.sh

cd ldlboot 
echo "  In directory: `pwd`, Command: $M3C" 
if ! $M3C
then
  cd ..
  exit 1
fi

echo "  Copy executable LdlBoot to ./boot."
rm -f ../boot/LdlBoot 
if ! cp -p $TARGET/LdlBoot ../boot/LdlBoot
then
  cd ..
  exit 1
fi
cd ../scripts

echo "========================================================================"
echo "Generate new Ldl0Tok.i3 and Ldl0Child.i3:" 
cd ../boot
COMMAND="./LdlBoot -tc"
echo "  In directory: `pwd`, Command: $COMMAND" 
if ! $COMMAND
then
  cd ..
  exit 1
fi
echo "  Newly generated files in ./boot:"
ls -l Ldl0Tok.i3 Ldl0Child.i3 
cd ../scripts

echo "========================================================================"
echo "Rebuild LdlBoot using generated Ldl0Tok.i3 and Ldl0Child.i3:"

echo "  Copy generated versions of Ldl0Tok.i3 and Ldl0Child.i3 to ./ldl0/derived/"
cd ../ldl0/derived

#echo "    Rename old version to ./ldl0/derived/" `newversion Ldl0Tok.i3`
cp -p ../../boot/Ldl0Tok.i3 Ldl0Tok.i3
rm -f ../$TARGET/Ldl0Tok_i.o # Force recompile.
rm -f ../$TARGET/Ldl0Tok.io # Force recompile.
ls -l Ldl0Tok.i3

#echo "    Rename old version to ./ldl0/derived/" `newversion Ldl0Child.i3`
cp -p ../../boot/Ldl0Child.i3 Ldl0Child.i3
rm -f ../$TARGET/Ldl0Child_i.o # Force recompile.
rm -f ../$TARGET/Ldl0Child.io # Force recompile.
ls -l Ldl0Child.i3

# Rebuild schutzcommon and ldlboot.
cd ../..
./scripts/innerscripts/buildcommon.sh

cd ldlboot
echo "  In directory: `pwd`, Command: $M3C" 
if ! $M3C
then
  cd ..
  exit 1
fi

echo "  Copy executable LdlBoot to ./boot."
rm -f ../boot/LdlBoot 
if ! cp -p $TARGET/LdlBoot ../boot/LdlBoot
then
  cd ..
  exit 1
fi
cd ../scripts

echo "========================================================================"
echo "Regenerate and diff new Ldl0Tok.i3 and Ldl0Child.i3:"
cd ../boot
COMMAND="./LdlBoot -tc"
echo "  In directory: `pwd`, Command: $COMMAND" 
if ! $COMMAND  
then 
  cd ..
  exit 1
fi
cd ../scripts

echo "  diffing Ldl0Tok.i3:"
diff --ignore-space-change ../ldl0/derived/Ldl0Tok.i3 ../boot/Ldl0Tok.i3

echo "  diffing Ldl0Child.i3:"
diff --ignore-space-change ../ldl0/derived/Ldl0Child.i3 ../boot/Ldl0Child.i3

echo "========================================================================"
echo "Write Ldl0.ldl0, Ldl0-2.ldl0, and diff them:"
cd ../boot
COMMAND="./LdlBoot -al"
echo "  In directory: `pwd`, Command: $COMMAND"
if ! $COMMAND 
then
  cd ..
  exit 1
fi
cd ../scripts 

echo "  diffing first generation Ldl0.ldl0 and second generation Ldl0-2.ldl0:"
cd ../boot 
diff --ignore-space-change Ldl0.ldl0 Ldl0-2.ldl0
cd ../scripts

echo "========================================================================"
echo "Generate Ldl0MakeEst.m3:" 
cd ../boot
COMMAND="./LdlBoot -ad"
echo "  In directory: `pwd`, Command: $COMMAND" 
if ! $COMMAND 
then
  cd ..
  exit 1
fi
cd ../scripts

echo "========================================================================"
echo "Rebuild LdlBoot using generated Ldl0MakeEst.m3:"

echo "  Copy generated version of Ldl0MakeEst.m3 to ./ldl0/derived"
cd ../ldl0/derived
#echo "    Rename old version to ./ldl0/derived/" `newversion Ldl0MakeEst.m3`
cp -p ../../boot/Ldl0MakeEst.m3 Ldl0MakeEst.m3
rm -f ../$TARGET/Ldl0MakeEst_m.o # Force recompile.
rm -f ../$TARGET/Ldl0MakeEst.mo # Force recompile.
ls -l Ldl0MakeEst.m3

# Rebuild schutzcommon and ldlboot.
cd ../..
./scripts/innerscripts/buildcommon.sh

cd ldlboot
echo "  In directory: `pwd`, Command: $M3C" 
if ! $M3C
then
  cd ..
  exit 1
fi

echo "  Copy executable LdlBoot to ./boot."
rm -f ../boot/LdlBoot 
if ! cp -p $TARGET/LdlBoot ../boot/LdlBoot
then
  cd ..
  exit 1
fi
cd ../scripts

echo "========================================================================"
echo "Regenerate Ldl0MakeEst.m3:" 
cd ../boot
COMMAND="./LdlBoot -adE $UOpt"
echo "  In directory: `pwd`, Command: $COMMAND" 
if ! $COMMAND 
then
  cd ..
  exit 1
fi
cd ../scripts

echo "  diff Ldl0MakeEst.m3"
diff --ignore-space-change ../ldl0/derived/Ldl0MakeEst.m3 ../boot/Ldl0MakeEst.m3

echo "========================================================================"
echo "Generate many files:"
cd ../boot
COMMAND="./LdlBoot -aeEfgGklprRsS $UOpt"
echo "  In directory: `pwd`, Command: $COMMAND" 
if ! $COMMAND 
then
  cd ..
  exit 1
fi
cd ../scripts

echo "========================================================================"
echo "Build schutzcommon and LdlBoot, using all generated files:"
cd ..
./scripts/innerscripts/buildcommon.sh

cd ldlboot 
echo "  In directory: `pwd`, Command: $M3C" 
#echo "    (This is probably unnecessary.)" 
if ! $M3C
then
  cd ..
  exit 1
fi

echo "  Copy executable LdlBoot to ./boot."
rm -f ../boot/LdlBoot 
if ! cp -p $TARGET/LdlBoot ../boot/LdlBoot
then
  cd ..
  exit 1
fi
cd ../scripts 

echo "========================================================================"
echo "Build schutzcommon and LdlBatch, using all generated files:"

echo "  Copy Ldl0Sem.pkl to ./ldlbatch/derived "
cd ../ldlbatch/derived 
#echo "    Rename old version to ./ldlbatch/derived/" `newversion Ldl0Sem.pkl`
cp -p ../../boot/Ldl0Sem.pkl Ldl0Sem.pkl
ls -l Ldl0Sem.pkl

cd ../..
./scripts/innerscripts/buildcommon.sh

cd ldlbatch
echo "  In directory: `pwd`, Command: $M3C" 
if ! $M3C
then
  echo "Build of LdlBatch failed."
  echo "But try again after running AddLdl1.sh."
  cd ..
  exit 1
fi
echo "  Copy executable LdlBatch to ./boot."
rm -f ../boot/LdlBatch 
if ! cp -p $TARGET/LdlBatch ../boot/LdlBatch
then
  cd ..
  exit 1
fi
cd ../scripts 

echo "========================================================================"
echo "Build schutzcommon and Lbe, using all generated files:"

echo "  Copy Ldl0Sem.pkl to ./edit/derived"
cd ../edit/derived 

#echo "    Rename old version to ./edit/derived/" `newversion Ldl0Sem.pkl`
cp -p ../../boot/Ldl0Sem.pkl Ldl0Sem.pkl
ls -l Ldl0Sem.pkl*

cd ../..
./scripts/innerscripts/buildcommon.sh

cd edit
echo "  In directory: `pwd`, Command: $M3C" 
if ! $M3C
then
  echo "Build of Lbe failed."
  echo "But try again after running AddLdl1.sh and AddM3.sh."
  cd ..
  exit 1
fi
cd ../scripts 

echo "========================================================================"
echo "Copy final files to ./resources "
echo "  Copy Ldl0Sem.pkl to ./resources "
cd ../resources

#echo "    Rename old version to ./resources/" `newversion Ldl0Sem.pkl`
cp -p ../boot/Ldl0Sem.pkl Ldl0Sem.pkl
ls -l Ldl0Sem.pkl*
cd ../scripts 

echo "========================================================================"
echo "fullboot, completed normally." 
echo "========================================================================"
