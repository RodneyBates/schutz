
## -----------------------------------------------------------------------1- ##
## This file is part of the Schutz semantic editor.                          ##
## Copyright 1988..2017, Rodney M. Bates.                                    ##
## rodney.m.bates@acm.org                                                    ##
## Licensed under the MIT License.                                           ##
## -----------------------------------------------------------------------2- ##

# File buildnilpickle.sh, subdirectory scripts, Schutz Semantic editor. 

startdir=`pwd`
source ../setCompiler.sh 
cd ../writenilpickle/src

if ! test -e NIL.pkl 
then 
  echo "Building WriteNILPickle" 
  if ! $M3C
  then
    echo "Build of WriteNILPickle failed." 
    cd $startdir
    exit 1
  fi
  cd ../[!s][!r][!c]* # This should catch any build directory.
  if ! ./writenilpickle
  then
    echo "Execution of writenilpkl failed." 
    cd $startdir
    exit 1
  fi 
  cp -p NIL.pkl ../src
  echo "NIL pickle generated and placed in writenilpickle/src" 
fi

cd $startdir


