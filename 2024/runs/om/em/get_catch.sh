pm -iprint 444
awk 'NR==2{print $5}' extra_sd.rep >catch.dat
cat catch.dat
