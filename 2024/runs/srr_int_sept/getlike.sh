for file in term*; do
  #echo $file;awk 'NR==673{print $1}' "$file"
  #echo $file;awk 'NR==667{print $1+$2+$3+$4+$5}' "$file"
  echo $file;awk 'NR==667{print $1,$2,$3,$4,$5}' "$file"
done
