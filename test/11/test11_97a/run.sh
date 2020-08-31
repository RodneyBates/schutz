echo "---------------- running test test11_97a.play"
echo "---------------- Specialized." 
rm -f SUCCEEDED FAILED 
LBE="../../Lbe"
$LBE -P test11_97a.play -i -t 0 2>&1 | tee test11_97a.log

diff test11_97a.export1.expected.m3 test11_97a.export1.m3 2>&1 > tmplog 
es1=$?
echo "diff test11_97a.export1.expected.m3 test11_97a.export1.m3, result = ${es1}" 
cat tmplog | tee -a test11_97a.log
if [ ${es1} -ne 0 ]
then 
  echo "test11_97a.export1 not as expected" 2>&1 | tee -a test11_97a.log
fi 

diff test11_97a.export2.expected.m3 test11_97a.export2.m3 2>&1 > tmplog 
es2=$?
echo "diff test11_97a.export2.expected.m3 test11_97a.export2.m3, result = ${es1}" 
cat tmplog | tee -a test11_97a.log
if [ ${es2} -ne 0 ]
then 
  echo "test11_97a.export2 not as expected" 2>&1 | tee -a test11_97a.log
fi 

diff test11_97a.export3.expected.m3 test11_97a.export3.m3 2>&1 > tmplog 
es3=$?
echo "diff test11_97a.export3.expected.m3 test11_97a.export3.m3, result = ${es1}" 
cat tmplog | tee -a test11_97a.log
if [ $es3 -ne 0 ] 
then 
  echo "test11_97a.export3 not as expected" 2>&1 | tee -a test11_97a.log
fi 

rm -f tmplog 
rm -f WriteDisplay 
rm -f *~*~ 

if [ $es1 -ne 0 -o $es2 -ne 0 -o $es3 -ne 0 ] 
then
  touch FAILED 
  echo "##################### F A I L E D ########################" 2>&1 | tee -a test11_97a.log
  exit 1
else 
  touch SUCCEEDED 
  touch LASTSUCCEEDED 
  echo "Succeeded" 2>&1 | tee -a test11_97a.log
fi

