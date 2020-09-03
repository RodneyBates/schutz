notrun="SchutzNotrunFileNames"
failed="SchutzFailedFileNames"

find . -name NOTRUN > $notrun
if [ -s $notrun ]
then 
  echo "Tests that were not run:"
  cat $notrun
fi

find . -name FAILED > $failed
if [ -s $failed ]
then 
  echo "Failed tests:"
  cat $failed
else
  echo "All tests succeeded."
fi 
