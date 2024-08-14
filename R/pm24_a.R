# setwd("~/_mymods/ebswp/doc")
rm(list = ls())
.THEME <- ggthemes::theme_few()
.OVERLAY <- TRUE
# install.packages("ggridges")
# source("R/prelims.R")
# source("prelims.R")
library(ebswp)
library(tidyverse)
thisyr <<- 2023
lastyr <<- thisyr - 1
nextyr <<- thisyr + 1
thismod <<- 1

source("tools/print_Tier3_tables.R")
# The model specs

#--Main models to presesnt in Sept   -----------
# Read report file and create gmacs report object (a list):
mod_names <- c(
  "Last year", # 1
  "m1",
  "m2 ",
  "m3",
  "m4",
  "m5",
  "m6",
  "m7",
  "m8"
)
# BTS age compositions included through 2023
# "Sept version",
# "2023 AVO point",
# "2022 ATS age updated",
# "added 2022 catch-age",
# "BTS to 2023",
# "BTS db Age")
# BTS age compositions included through 2023
# but with Hulsons BTS input sample sizes

# "Catch update",     #2
# "AVO new",     #3
# "AVO full",    #4
# "Tuned22",     #5
# "Ageing Error",#6
# "Diag cov BTS",#7
# "GenGam",      #8
### "SSB=mean ",   #9
# "SSB Emp. wt-age", #10
# "SSB RE wt-age") #10
mod_dir <- c(
  "m0",
  "m1",
  "m2",
  "m3",
  "m4",
  "m5",
  "m6",
  "m7",
  "m8"
)

mod_names <- c("MSY=1.2","Fmsy=F35%", "Base est.")
mod_dir <- c("condmn", "condF35", "m8")
# WARNING,  commented out line will re-run all the models in the mod_dir directories within "runs"
# Won't do tier 3 spm (proj) model in the subdirectory at the moment
#---Read in the results for modelsl already run--------------
# run_model(rundir="2023_runs")
# run_proj(rundir="2023_runs")
modlst <- get_results(rundir = "2023_runs")
# names(modlst)
M <<- modlst[[thismod]]
#M <<- modlst[[3]];saveRDS(M, "~/m8.rds")

.MODELDIR <<- paste0("2023_runs/", mod_dir, "/")

# Q sum ages 3 and younger from M$N over time

names(M)
# Compare SRR

df <- data.frame(Year = 2024:2073, catch = t(M$future_catch), SER = t(M$future_SER), R = t(M$future_R), SSB = t(M$future_SSB[, -1]))
glimpse(df)
glimpse(dfl)
dfl <- df |> pivot_longer(cols = 2:33, names_to = "var", values_to = "value")
dfl |>
  filter(str_detect(var, pattern = "SER")) |>
  ggplot(aes(x = Year, y = value, color = var)) +
  geom_line() +
  labs(title = "EBS Pollock ", x = "Year", y = "relative to mean") +
  ggthemes::theme_few() +
  xlim(c(2024, 2073))
p1 <- dfl |>
  filter(str_detect(var, pattern = "SSB")) |>
  ggplot(aes(x = Year, y = value, color = var)) +
  geom_line() +
  labs(title = "EBS Pollock ", x = "Year", y = "relative to mean") +
  ggthemes::theme_few() +
  xlim(c(2024, 2073))
plotly::ggplotly(p1)
p2 <- dfl |>
  filter(str_detect(var, pattern = "catch")) |>
  ggplot(aes(x = Year, y = value, color = var)) +
  geom_line() +
  labs(title = "EBS Pollock ", x = "Year", y = "relative to mean") +
  ggthemes::theme_few() +
  xlim(c(2024, 2073))
plotly::ggplotly(p2)
dfl |> transmute(var = ifelse(str_detect(var, pattern = "catch"), value, ""))
# , str_detect(var,pattern= "SSB")) |> ggplot(aes(x=catch,y=value,color=var)) + geom_line() + labs(title="EBS Pollock ",x="Year",y="relative to mean") +
# ggthemes::theme_few() + xlim(c(2024,2073))
glimpse(dfl) #|> mutate(Year=ifelse(var=="catch",Year+1,Year)) |>
#  ggplot(aes(x=Year,y=value,color=var)) + geom_line() + labs(title="EBS Pollock ",x="Year",y="relative to mean") +
#  ggthemes::theme_few() + xlim(c(2024,2073))

#|> mutate(rel_forage=forage/mean(forage),rel_SSB=SSB/mean(SSB))
M$future_catch
M$future_R
M$future_SSB
M$future_SER

df <- data.frame(Year = 1964:2023, forage = rowSums(M$N), SSB = M$SSB[, 2]) |> mutate(rel_forage = forage / mean(forage), rel_SSB = SSB / mean(SSB))
glimpse(df)
mean(df$forage)
mean(df$SSB)
quantile(df$forage, c(0.05, .2))
df |> ggplot(aes(x = Year, y = forage)) +
  geom_line() +
  labs(title = "Total pollock forage (age 1-3)", x = "Year", y = "Abundance (thousands)") +
  ggthemes::theme_few() +
  xlim(c(1980, 2024)) +
  geom_hline(yintercept = 26000, color = "red") +
  geom_hline(yintercept = 33000, color = "blue", type = 2)

df |>
  pivot_longer(cols = 4:5, names_to = "var", values_to = "value") |>
  ggplot(aes(x = Year, y = value, color = var)) +
  geom_line() +
  labs(title = "EBS Pollock ", x = "Year", y = "relative to mean") +
  ggthemes::theme_few() +
  xlim(c(1980, 2024))

df |> ggplot(aes(x = Year, y = SSB)) +
  geom_line() +
  labs(title = "Spawning biomass of pollock ", x = "Year", y = "Biomass (t) ") +
  ggthemes::theme_few() +
  xlim(c(1980, 2024)) +
  geom_hline(yintercept = 2257, color = "red")

# Loop over terminal years to compute mean catch and SSB and catch advice
i <- 2000
df1 <- NULL
for (i in 2000:2022) {
  # get values from the time series
  lastidx <- i - 1977 + 14
  cat_mn <- mean(M$obs_catch[14:lastidx])
  cat_this <- M$obs_catch[lastidx]
  ssb_mn <- mean(M$SSB[14:lastidx, 2])
  ssb_this <- (M$SSB[lastidx, 2])
  ssb_next <- (M$SSB[lastidx + 1, 2])
  # cat_next   <- (ssb_next/ssb_this)^0.5 * cat_this
  cat_next <- (ssb_next / ssb_mn)^0.95 * cat_this
  df1 <- data.frame(Year = i, Mean_catch = cat_mn, Current_catch = cat_this, ABC = cat_next, ssb_mn = ssb_mn, ssb_this = ssb_this, ssb_next = ssb_next) |> rbind(df1)
}

df1 |>
  pivot_longer(cols = c(3:5, 7), names_to = "var", values_to = "value") |>
  mutate(Year = ifelse(var == "ABC", Year + 1, Year)) |>
  ggplot(aes(x = Year, y = value, shape = var, color = var)) +
  geom_point() +
  geom_line() +
  labs(title = "EBS Pollock ", x = "Year", y = "relative to mean") +
  theme_bw() +
  xlim(c(2000, 2024))

glimpse(df1)


tab_fit(modlst, mod_scen = c(2:9)) |> gt::gt()
# tab_ref(modlst[c(2:9)]) |> gt::gt() |>  gt::fmt_markdown()
# names(modlst)
# Save result so it can be used by the document
# save(modlst,file="doc/novmod.rdata")
# names(modlst)
# plot_avo(modlst[3:5])

#---Covariance diagonal extraction--------
#---Mohno rho read-----
rhodf <- read.csv("doc/data/mohnrho.csv", header = T)
rhoMohn10 <- rhodf[11, 3]
rhoMohn20 <- rhodf[21, 3]
rhoMohn10

# Figure captions
fc <- (read_csv("doc/data/fig_captions.csv"))
figcap <<- fc$cap
figlab <<- fc$label
fnum <<- fc$no
reffig <<- function(i) {
  cat(paste0("\\ref{fig:", figlab[fnum == i], "}"))
}

# ![Results of the EBS pollock model for recent spawning biomass estimates comparing the base model using the covariance matrix with the one where only the diagonal is applied.]
# (doc/figs/mod_diag_ssb.pdf){#fig-diagssb}
printfig <<- function(tmp, i) {
  cat(paste0("\n![", figcap[fnum == i], "](doc/figs/", tmp, "){#fig-", figlab[fnum == i], "}\n"))
}
# printfig <<- function(tmp,i){ cat(paste0("\n![",figcap[fnum==i],"\\label{fig:",figlab[fnum==i],"}](doc/figs/",tmp,")   \n ")) }

# Table captions
tc <- (read_csv("doc/data/table_captions.csv"))
tablab <- tc$label
tabcap <- paste0("\\label{tab:", tablab, "}", tc$cap)
# tc
reftab <<- function(i) {
  cat(paste0("@tbl-", tablab[i]))
}
# reffig(1)
# tabcap[1]
# tap <- data_frame(t=c(1,2),c=c(1,2))
# printtab <<- function(tmp,i){ cat(paste0("\n![",tabcap[fnum==i],"](doc/figs/",tmp,"){#tbl-",tablab[fnum==i],"}\n") )
printtab <<- function(tmp, i) {
  tab <- xtable(tmp, digits = 0, auto = TRUE, caption = tabcap[i], label = paste0("tab:", tablab[i]))
  print(tab, caption.placement = "top", include.rownames = FALSE, sanitize.text.function = function(x) {
    x
  })
}
# print(tablab)

# source("../R/Do_Plots.R")
# source("../R/Do_MCMC.R")
# source("../R/Do_Proj.R")
