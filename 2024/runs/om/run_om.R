system("make")
system("pm -binp orig.bar -phase 22")
system("cp pm.bar orig.bar; cp pm.rep orig.rep")
system("pm -binp orig.bar -phase 22 -iseed 123 -nohess")
i=1
for (i in 1:10) {
  system("pm -ind sim.dat")
  system(paste0("cp pm.bar sim_",i,".bar; cp pm.rep sim_",i,".rep ;cp sim_1.dat sim_",i,".dat"))
  system("pm -ind sim.dat -binp pm.bar -phase 22 -iseed 123 -nohess >NUL")
}