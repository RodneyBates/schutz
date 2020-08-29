if [ "$1" = "" ] ; then DIR="."; else DIR="$1"; fi
#ABSPATH="`abspath $DIR`"
#echo $ABSPATH
ABSPATH="`cd $DIR; pwd`"
#echo $ABSPATH
#pwd 
#echo "cd" $DIR
cd $DIR
echo "-------------------------- In directory `pwd`" 
if test -f run.sh 
then
  ./run.sh
else  
  # LBE="../../Lbe @M3stackdump -u"   # Use generated grammar
  LBE="../../Lbe @M3stackdump "     # Use handwritten grammar 
  if test -f bases ; then BASES=`cat bases`; else BASES="`basename $ABSPATH`"; fi 
  if test -f suffixes ; then SUFFIXES=`cat suffixes`; else SUFFIXES="i3 m3 ig mg ldl0 ldl1"; fi 
  esall=0
  rm -f SUCCEEDED FAILED 
  for BASE in $BASES
  do
    CMD="$LBE -P $BASE.play -i -t 0"
    echo "Running test $BASE.play, using command $CMD"
    #$LBE -P $BASE.play -i -t 0 2>&1 > tmplog0 
    $CMD 2>&1 > tmplog0
    es0=$?
    if [ $es0 -ne 0 ] ; then esall=1 ; fi 
    cat tmplog0 | tee $BASE.log
    if [ $es0 -eq 0 ] 
    then 
      for SUFFIX in $SUFFIXES 
      do 
        if test -f $BASE.export.expected.$SUFFIX 
        then 
          diff $BASE.export.expected.$SUFFIX $BASE.export.$SUFFIX 2>&1 > tmplog1 
          es1=$?
          if [ $es1 -ne 0 ] ; then esall=1 ; fi 
          echo "diff $BASE.export.expected.$SUFFIX $BASE.export.$SUFFIX, result = ${es1}" 
          cat tmplog1 | tee -a $BASE.log
          if [ ${es1} -ne 0 ]
          then 
            echo "$BASE.export.$SUFFIX not as expected" 2>&1 | tee -a $BASE.log
          fi 
        fi 
      done 
    fi 
    rm -f tmplog0 
    rm -f tmplog1  

    if [ $esall -ne 0 ] # || [ $es1 -ne 0 ] 
    then
      touch FAILED 
      echo "######################### F A I L E D #########################" 2>&1 | tee -a $BASE.log
      echo "----------------------------- Failed in `pwd`" 2>&1 | tee -a $BASE.log
      exit 1
    else 

      #echo "diff $BASE.play $BASE.replay:" 
      #oldver=`newversion replay.diff`
      #diff $BASE.play $BASE.replay 2>&1 | tee replay.diff 
      #oldver=`newversion $BASE.play`
      #cp -p $BASE.replay $BASE.play

      touch SUCCEEDED
      touch LASTSUCCEEDED
      echo "-------------------------- Succeeded in `pwd`" 2>&1 | tee -a $BASE.log
      rm -f WriteDisplay 
      rm -f *~*~ 
      rm -f *replay*
    fi
  done
fi 
