failed="SchutzFailedFileNames"
find . -name FAILED > $failed
if [ -s $failed ]
then 
  echo "Failed tests:"
  cat $failed
else
  echo "All tests succeeded."
fi 
