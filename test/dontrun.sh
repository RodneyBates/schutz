if [ "$1" = "" ] ; then DIR="."; else DIR="$1"; fi
ABSPATH="`cd $DIR; pwd`"
cd $DIR
touch NOTRUN
echo "-------------- Not running In directory `pwd`" 
