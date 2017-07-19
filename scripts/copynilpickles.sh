
## -----------------------------------------------------------------------1- ##
## This file is part of the Schutz semantic editor.                          ##
## Copyright 1988..2017, Rodney M. Bates.                                    ##
## rodney.m.bates@acm.org                                                    ##
## Licensed under the MIT License.                                           ##
## -----------------------------------------------------------------------2- ##

# File copynilpickles.sh, subdirectory scripts, Schutz Semantic editor. 

if test -e ../ldlbatch/derived
then 
  rm -fr ../ldlbatch/derived/Ldl0Sem.pkl
  cp -p ../writenilpickle/src/NIL.pkl ../ldlbatch/derived/Ldl0Sem.pkl
  rm -fr ../ldlbatch/derived/Ldl1Sem.pkl
  cp -p ../writenilpickle/src/NIL.pkl ../ldlbatch/derived/Ldl1Sem.pkl
fi 
if test -e ../edit/derived
then 
  rm -fr ../edit/derived/Ldl0Sem.pkl
  cp -p ../writenilpickle/src/NIL.pkl ../edit/derived/Ldl0Sem.pkl
  rm -fr ../edit/derived/Ldl1Sem.pkl
  cp -p ../writenilpickle/src/NIL.pkl ../edit/derived/Ldl1Sem.pkl
  rm -fr ../edit/derived/M3Sem.pkl
  cp -p ../writenilpickle/src/NIL.pkl ../edit/derived/M3Sem.pkl
fi 
