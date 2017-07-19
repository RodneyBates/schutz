
## -----------------------------------------------------------------------1- ##
## This file is part of the Schutz semantic editor.                          ##
## Copyright 1988..2017, Rodney M. Bates.                                    ##
## rodney.m.bates@acm.org                                                    ##
## Licensed under the MIT License.                                           ##
## -----------------------------------------------------------------------2- ##

# File topickle2.sh, subdirectory scripts, Schutz Semantic editor.

# Set up to use Pickle2. 

for File in ../common/src/Files.m3 \
            ../common/src/PickleThread.m3 \
            ../common/src/SharedStrings.m3 \
            ../devel/src/UiDevel.m3 \
            ../ldlbatch/src/LdlBatch.m3 \
            ../ldlboot/src/LdlBootMain.m3 \
            ../writenilpickle/src/WriteNILPickle.m3
do 
  cp -p $File $File.~bakpickle2~
  sed -e 's/; IMPORT Pickle (\* The Pickle2 mess. \*)/; IMPORT Pickle2 AS Pickle (* The Pickle2 mess. *)/' $File > $File.temp
  if diff $File $File.temp > /dev/null 
  then 
    rm -f $File.temp
    echo "$File unchanged."
  else
    mv $File $File.~beforetopickle~
    mv $File.temp $File 
    echo "$File changed to use Pickle2."
  fi 
done
