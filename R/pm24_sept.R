rm(list=ls())
.THEME=ggthemes::theme_few()
.OVERLAY=TRUE
library(ebswp)
library(tidyverse)
theme_set(.THEME)
thisyr    <<- 2023
lastyr    <<- thisyr-1
nextyr    <<- thisyr+1
thismod   <<- 1

source(here::here("R","print_Tier3_tables.R"))
# The model specs

#--Main models to presesnt in Sept   -----------
# Read report file and create gmacs report object (a list):
mod_names <- c("Last year",
               "Cond SRR 1.3 Mt",
               "Cond SRR 1.3 Mt, full",
               "Drop CPUE",
               "sigR 0.53",
               "sigR 0.57",
               "sigR 0.61",
               "sigR 0.65",
               "sigR 0.69",
               "sigR 0.73",
               "sigR 0.77",
               "sigR 0.81"
               )
mod_dir <- c(
  "lastyr",
  "condSRR_sept",
  "condSRR_sept2",
  "dropCPUE_sept",
  "sigr",
  )
# WARNING, commented out line will re-run all the models in the mod_dir directories within "runs"
# Won't do tier 3 spm (proj) model in the subdirectory at the moment
#---Read in the results for modelsl already run--------------
#run_model(rundir="2023_runs")
#run_proj(rundir="2023_runs")
modlst<-get_results(rundir="~/_mymods/afsc-assessments/ebs_pollock_safe/2024/runs")
M <<- modlst[[thismod]]
.MODELDIR<<-paste0("~/_mymods/afsc-assessments/ebs_pollock_safe/2024/runs",
									 mod_dir,"/")
get_results
sigR <- .67
sigRsq <- .67^2
R <- exp(rnorm(10000, 0,sigR))
R
getwd()
write.table(R, "tt.dat")
Rbart-mean(R)
Rbar
rbar<-mean(log(R))
mean(log(R)-log(Rbar/exp(.5*sigRsq)))


mean(R*exp(-.5*sigRsq))
exp(mean(log(R)))


#---Covariance diagonal extraction--------
#---Mohno rho read-----
rhodf      <- read.csv("doc/data/mohnrho.csv",header=T)
rhoMohn10  <-  rhodf[11,3]
rhoMohn20  <-  rhodf[21,3]
rhoMohn10

# Figure captions
fc  <- (read_csv("doc/data/fig_captions.csv"))
figcap <<- fc$cap; figlab <<- fc$label; fnum <<- fc$no
reffig <<- function(i){ cat(paste0("\\ref{fig:",figlab[fnum==i],"}")) }

#![Results of the EBS pollock model for recent spawning biomass estimates comparing the base model using the covariance matrix with the one where only the diagonal is applied.]
#(doc/figs/mod_diag_ssb.pdf){#fig-diagssb}
printfig <<- function(tmp,i){
  cat(paste0("\n![",figcap[fnum==i],"](doc/figs/",tmp,"){#fig-",figlab[fnum==i],"}\n") )
             }
# printfig <<- function(tmp,i){ cat(paste0("\n![",figcap[fnum==i],"\\label{fig:",figlab[fnum==i],"}](doc/figs/",tmp,")   \n ")) }

# Table captions
tc  <- (read_csv("doc/data/table_captions.csv"))
tablab <- tc$label
tabcap <- paste0("\\label{tab:",tablab,"}",tc$cap);
#tc
reftab <<- function(i){ cat(paste0("@tbl-",tablab[i])) }
#reffig(1)
#tabcap[1]
# tap <- data_frame(t=c(1,2),c=c(1,2))
#printtab <<- function(tmp,i){ cat(paste0("\n![",tabcap[fnum==i],"](doc/figs/",tmp,"){#tbl-",tablab[fnum==i],"}\n") )
printtab <<- function(tmp,i){tab <- xtable(tmp, digits=0, auto=TRUE,caption = tabcap[i], label = paste0("tab:",tablab[i])); print(tab, caption.placement = "top", include.rownames = FALSE, sanitize.text.function = function(x){x}) }
#print(tablab)

#source("../R/Do_Plots.R")
#source("../R/Do_MCMC.R")
#source("../R/Do_Proj.R")

