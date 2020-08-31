echo "---------------- running test test11_145.play"
echo "---------------- Specialized." 
rm -f SUCCEEDED FAILED
LBE="../../Lbe"
$LBE -P test11_145.play -i -t 0 2>&1 | tee test11_145.log

diff test11_145.export1.expected.m3 test11_145.export1.m3 2>&1 > tmplog  
es1=$?
echo "diff test11_145.export1.expected.m3 test11_145.export1.m3, result = ${es1}" 
cat tmplog | tee -a test11_145.log
if [ ${es1} -ne 0 ]
then 
  echo "test11_145.export1 not as expected" 2>&1 | tee -a test11_145.log
fi 

diff test11_145.export2.expected.m3 test11_145.export2.m3 2>&1 > tmplog 
es1=$?
echo "diff test11_145.export2.expected.m3 test11_145.export2.m3, result = ${es1}" 
cat tmplog | tee -a test11_145.log
if [ ${es1} -ne 0 ]
then 
  echo "test11_145.export2 not as expected" 2>&1 | tee -a test11_145.log
fi 

rm -f tmplog
rm -f WriteDisplay 
rm -f *~*~ 

if [ $es1 -ne 0 ] 
then
  touch FAILED 
  echo "##################### F A I L E D ########################" 2>&1 | tee -a test11_145.log
  exit 1
else 
  touch SUCCEEDED 
  touch LASTSUCCEEDED 
  echo "Succeeded" 2>&1 | tee -a test11_145.log
fi

