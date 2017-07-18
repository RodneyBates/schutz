
## -----------------------------------------------------------------------1- ##
## This file is part of the Schutz semantic editor.                          ##
## Copyright 1988..2017, Rodney M. Bates.                                    ##
## rodney.m.bates@acm.org                                                    ##
## Licensed under the MIT License.                                           ##
## -----------------------------------------------------------------------2- ##

# File setupderived.sh, subdirectory scripts, Schutz Semantic editor. 

# Create directories to hold generated files. 

# ldl0 
cd ../ldl0
mkdir -p derived

if test ! -e derived/Ldl0Tok.i3
then  "  Copying a token Ldl0Tok.i3 into ./ldl0/derived"
  cp -p src/Ldl0Tok.i3.initial derived/Ldl0Tok.i3
fi
if test ! -e derived/Ldl0Child.i3
then 
  echo "  Copying a token Ldl0Child.i3 into ./ldl0/derived"
  cp -p src/Ldl0Child.i3.initial derived/Ldl0Child.i3
fi
if test ! -e derived/Ldl0MakeEst.m3
then 
  echo "  Copying a dummy Ldl0MakeEst.m3 into ./ldl0/derived"
  cp -p src/Ldl0MakeEst.m3.dummy derived/Ldl0MakeEst.m3
fi

echo "  Contents of `pwd`/derived:" 
ls -l derived

#ldl1
cd ../ldl1
mkdir -p derived

if test ! -e derived/Ldl1Tok.i3
then 
  echo "  Copying a token Ldl1Tok.i3 into ./ldl1/derived"
  cp -p src/Ldl1Tok.i3.initial derived/Ldl1Tok.i3
fi
if test ! -e derived/Ldl1Child.i3
then 
  echo "  Copying a token Ldl1Child.i3 into ./ldl1/derived"
  cp -p src/Ldl1Child.i3.initial derived/Ldl1Child.i3
fi
if test ! -e derived/Ldl1MakeEst.m3
then 
  echo "  Copying a dummy Ldl1MakeEst.m3 into ./ldl1/derived"
  cp -p src/Ldl1MakeEst.m3.dummy derived/Ldl1MakeEst.m3
fi

echo "  Contents of `pwd`/derived:" 
ls -l derived

#m3
cd ../m3
mkdir -p derived

if test ! -e derived/M3Tok.i3
then 
  echo "  Copying a token M3Tok.i3 into ./m3/derived"
  cp -p src/M3Tok.i3.initial derived/M3Tok.i3
fi
if test ! -e derived/M3Child.i3
then 
  echo "  Copying a token M3Child.i3 into ./m3/derived"
  cp -p src/M3Child.i3.initial derived/M3Child.i3
fi
if test ! -e derived/M3MakeEst.m3
then 
  echo "  Copying a dummy M3MakeEst.m3 into ./m3/derived"
  cp -p src/M3MakeEst.m3.dummy derived/M3MakeEst.m3
fi

echo "  Contents of `pwd`/derived:" 
ls -l derived
 
#ldlbatch
cd ../ldlbatch
mkdir -p derived

if test ! -e derived/Ldl0Sem.pkl 
then 
  echo "  Copying a NIL Ldl0Sem.pkl into ./ldlbatch/derived"
  cp -p ../writenilpickle/src/NIL.pkl derived/Ldl0Sem.pkl
fi

if test ! -e derived/Ldl1Sem.pkl 
then 
  echo "  Copying a NIL Ldl1Sem.pkl into ./ldlbatch/derived"
  cp -p ../writenilpickle/src/NIL.pkl derived/Ldl1Sem.pkl
fi

echo "  Contents of `pwd`/derived:" 
ls -l derived

#edit
cd ../edit
mkdir -p derived

if test ! -e derived/Ldl0Sem.pkl 
then 
  echo "  Copying a NIL Ldl0Sem.pkl into ./edit/derived"
  cp -p ../writenilpickle/src/NIL.pkl derived/Ldl0Sem.pkl
fi

if test ! -e derived/Ldl1Sem.pkl 
then 
  echo "  Copying a NIL Ldl1Sem.pkl into ./edit/derived"
  cp -p ../writenilpickle/src/NIL.pkl derived/Ldl1Sem.pkl
fi

if test ! -e derived/M3Sem.pkl 
then 
  echo "  Copying a NIL M3Sem.pkl into ./edit/derived"
  cp -p ../writenilpickle/src/NIL.pkl derived/M3Sem.pkl
fi

echo "  Contents of `pwd`/derived:" 
ls -l derived

