
## -----------------------------------------------------------------------1- ##
## This file is part of the Schutz semantic editor.                          ##
## Copyright 1988..2017, Rodney M. Bates.                                    ##
## rodney.m.bates@acm.org                                                    ##
## Licensed under the MIT License.                                           ##
## -----------------------------------------------------------------------2- ##

# File setupderived.sh, subdirectory scripts, Schutz Semantic editor. 

# Create directories to hold generated files. 

# At top level: 
if test ! -d resources 
then 
  echo "  Create directory ./resources:"
  mkdir resources
fi

# In subdirectory ldl0: 
cd ../ldl0
if test ! -d derived 
then 
  echo "  Create directory ./ldl0/derived:"
  mkdir derived 
fi

if test ! -e derived/Ldl0Tok.i3
then
  echo "  Copy an initial Ldl0Tok.i3 into ./ldl0/derived:"
  cp -p src/Ldl0Tok.i3.initial derived/Ldl0Tok.i3
  rm -f $TARGET/Ldl0Tok_i.o # Force recompile.
  rm -f $TARGET/Ldl0Tok.io # Force recompile.
fi
if test ! -e derived/Ldl0Child.i3
then 
  echo "  Copy an initial Ldl0Child.i3 into ./ldl0/derived:"
  cp -p src/Ldl0Child.i3.initial derived/Ldl0Child.i3
  rm -f $TARGET/Ldl0Child_i.o # Force recompile.
  rm -f $TARGET/Ldl0Child.io # Force recompile.
fi
if test ! -e derived/Ldl0MakeEst.m3
then 
  echo "  Copy a dummy Ldl0MakeEst.m3 into ./ldl0/derived:"
  cp -p src/Ldl0MakeEst.m3.dummy derived/Ldl0MakeEst.m3
  rm -f $TARGET/Ldl0MakeEst_m.o # Force recompile.
  rm -f $TARGET/Ldl0MakeEst.mo # Force recompile.
fi

echo "  Contents of `pwd`/derived:" 
ls -l derived

#In subdirectory ldl1:
cd ../ldl1
if test ! -d derived 
then 
  echo "  Create directory ./ldl1/derived:"
  mkdir derived 
fi

if test ! -e derived/Ldl1Tok.i3
then 
  echo "  Copy an initial Ldl1Tok.i3 into ./ldl1/derived:"
  cp -p src/Ldl1Tok.i3.initial derived/Ldl1Tok.i3
  rm -f $TARGET/Ldl1Tok_i.o # Force recompile.
  rm -f $TARGET/Ldl1Tok.io # Force recompile.
fi
if test ! -e derived/Ldl1Child.i3
then 
  echo "  Copy an initial Ldl1Child.i3 into ./ldl1/derived:"
  cp -p src/Ldl1Child.i3.initial derived/Ldl1Child.i3
  rm -f $TARGET/Ldl1Child_i.o # Force recompile.
  rm -f $TARGET/Ldl1Child.io # Force recompile.
fi
if test ! -e derived/Ldl1MakeEst.m3
then 
  echo "  Copy a dummy Ldl1MakeEst.m3 into ./ldl1/derived:"
  cp -p src/Ldl1MakeEst.m3.dummy derived/Ldl1MakeEst.m3
  rm -f $TARGET/Ldl1MakeEst_m.o # Force recompile.
  rm -f $TARGET/Ldl1MakeEst.io # Force recompile.
fi

echo "  Contents of `pwd`/derived:" 
ls -l derived

#In subdirectory m3:
cd ../m3
if test ! -d derived 
then 
  echo "  Create directory ./m3/derived:"
  mkdir derived 
fi

if test ! -e derived/M3Tok.i3
then 
  echo "  Copy an initial M3Tok.i3 into ./m3/derived:"
  cp -p src/M3Tok.i3.initial derived/M3Tok.i3
  rm -f $TARGET/M3Tok_i.o # Force recompile.
  rm -f $TARGET/M3Tok.io # Force recompile.
fi
if test ! -e derived/M3Child.i3
then 
  echo "  Copy an initial M3Child.i3 into ./m3/derived:"
  cp -p src/M3Child.i3.initial derived/M3Child.i3
  rm -f $TARGET/M3Child_i.o # Force recompile.
  rm -f $TARGET/M3Child.io # Force recompile.
fi
if test ! -e derived/M3MakeEst.m3
then 
  echo "  Copy a dummy M3MakeEst.m3 into ./m3/derived:"
  cp -p src/M3MakeEst.m3.dummy derived/M3MakeEst.m3
  rm -f $TARGET/M3MakeEst_m.o  # Force recompile.
  rm -f $TARGET/M3MakeEst.mo  # Force recompile.
fi
if test ! -e derived/M3InitTokStrings.m3
then 
  echo "  Copy a dummy M3InitTokStrings.m3 into ./m3/derived:"
  cp -p src/M3InitTokStrings.m3.dummy derived/M3InitTokStrings.m3
  rm -f $TARGET/M3InitTokStrings_m.o  # Force recompile.
  rm -f $TARGET/M3InitTokStrings.mo  # Force recompile.
fi

echo "  Contents of `pwd`/derived:" 
ls -l derived
 
#In subdirectory ldlbatch:
cd ../ldlbatch
if test ! -d derived 
then 
  echo "  Create directory ./ldlbatch/derived:"
  mkdir derived 
fi

if test ! -e derived/Ldl0Sem.pkl 
then 
  echo "  Copy a NIL Ldl0Sem.pkl into ./ldlbatch/derived:"
  cp -p ../writenilpickle/src/NIL.pkl derived/Ldl0Sem.pkl
fi

if test ! -e derived/Ldl1Sem.pkl 
then 
  echo "  Copy a NIL Ldl1Sem.pkl into ./ldlbatch/derived:"
  cp -p ../writenilpickle/src/NIL.pkl derived/Ldl1Sem.pkl
fi

echo "  Contents of `pwd`/derived:" 
ls -l derived

#In subdirectory edit:
cd ../edit
if test ! -d derived 
then 
  echo "  Create directory ./edit/derived:"
  mkdir derived 
fi

if test ! -e derived/Ldl0Sem.pkl 
then 
  echo "  Copy a NIL Ldl0Sem.pkl into ./edit/derived:"
  cp -p -p ../writenilpickle/src/NIL.pkl derived/Ldl0Sem.pkl
fi

if test ! -e derived/Ldl1Sem.pkl 
then 
  echo "  Copy a NIL Ldl1Sem.pkl into ./edit/derived:"
  cp -p -p ../writenilpickle/src/NIL.pkl derived/Ldl1Sem.pkl
fi

if test ! -e derived/M3Sem.pkl 
then 
  echo "  Copy a NIL M3Sem.pkl into ./edit/derived:"
  cp -p -p ../writenilpickle/src/NIL.pkl derived/M3Sem.pkl
fi

echo "  Contents of `pwd`/derived:" 
ls -l derived

