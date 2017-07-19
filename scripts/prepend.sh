headerfile="$1"
for file in `ls *$2`
do
  cp -p $file $file.save
  cat $headerfile $file > $file.new
  mv $file.new $file 
  echo $file
done 