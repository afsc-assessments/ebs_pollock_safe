#!/bin/bash
#mv -f pm.dat tmp.dat
# unalias cp
/bin/cp -f control.dat t1.ctl
#awk 'NR==4{print "control.dat"} NR!=4 {print $0}' tmp.dat >pm.dat
#for j in `seq 3 5`;
for j in `seq 1 2`;
  do
  awk -v jval=$j 'NR==64{print jval} NR!=64 {print $0}' control.dat >t1.ctl
  for i in `seq $2 $1`;
    do
      awk -v rrr=$i 'NR==157{print rrr} NR!=157 {print $0}' t1.ctl >control.dat
      ./pm -nox -iprint 500 
	    /bin/cp -f pm.std retro/r_$i.std
	    /bin/cp -f pm.rep retro/r_$i.rep
	    /bin/cp -f F40_t.rep retro/r_F40_$i.rep
	    /bin/cp -f selgrid.rep retro/r_selgrid_$i.rep
    done    
		mv retro retro$j
		mkdir retro
  done    
/bin/cp -f t1.ctl control.dat

#!/bin/bash

#arg1=$1  # Capture the first command-line argument
#
## Use awk and pass the bash argument using -v
#awk -v bash_arg="$arg1" 'BEGIN { print "The argument passed is: " bash_arg }'