library(scales)
setwd("~/_mymods/afsc-assessments/ebs_pollock_safe")
source("~/_mymods/afsc-assessments/ebs_pollock_safe/R/pm24.R")

if (domcmc) {
  # source("../R/plotMCMCpairs.R")
  # .plotMCMCpairs(tt)
  .MODELDIR <- mod_dir <- c(
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
  thismod <- 2
  library(GGally)
  getwd()
  mc <- read_table(paste0(.MODELDIR[thismod], "/mcmc/mceval.rep"), col_names = FALSE)
  mclen <- length(mc[, 1])
  dim(mc)
  # mc <- data.frame(read.table("mceval.rep"))
  # mc <- mc[,c(1:11,37)]
  names(mc) <- c(
    "ObjFun",
    "Catch",
    "BTS",
    "ATS",
    "ATS_age1",
    "CPUE",
    "AVO",
    "Recruit",
    "Copepods",
    "F_pen",
    "Fsh_age",
    "BTS_age",
    "ATS_age",
    "Fsh_len",
    "Selex",
    "Sel_dev",
    "Priors",
    "x1",
    "x2",
    "x3",
    "x4",
    "M", "Steepness", "lnR0", "Fmsyr", "SPR_msy",
    "SER_msy", "B0", "Bmsy", "B100",
    "B2024",
    "R2019",
    "B2025",
    "B/mean(B)", 
    "B25/B20%", "DynB0", "q"
  ) # ,"x","y")

  library(scales)
  library(ggthemes)
  p1 <- ggplot(mc, aes(x = R2019)) +
    geom_density(fill = "lemonchiffon") +
    xlab("2018 Year class at age 1 (millions)") +
    theme_few(base_size = 15) +
    scale_x_continuous(limits = c(0, 124000), label = comma, breaks = c(0, 50000, 100000, 120000))
  p1
  p2 <- mc %>%
    transmute(
      R2019 = R2019,
      BTS = BTS - mean(BTS),
      ATS = ATS - mean(ATS),
      AVO = AVO - mean(AVO),
      FshAge = Fsh_age - mean(Fsh_age),
      BTSAge = BTS_age - mean(BTS_age),
      ATSAge = ATS_age - mean(ATS_age),
    ) %>%
    pivot_longer(
      cols = c(2:7),
      names_to = "type",
      values_to = "NLL"
    ) %>%
    select(type, NLL, R2019) %>%
    ggplot(aes(x = R2019, y = NLL, color = type)) +
    ggthemes::theme_few() +
    scale_x_continuous(limits = c(0, 124000), label = comma, breaks = c(50000, 100000)) +
    geom_point(alpha = .1, size = .1) +
    facet_wrap(. ~ type, scales = "free") +
    geom_smooth() +
    ylim(c(-20, 20)) +
    xlab("")
  p2
  library(patchwork)
  p3 <- p2 / p1
  p3
  ggsave("~/_mymods/afsc-assessments/ebs_pollock_safe/doc/figs/post_profile.png", plot = p3, width = 8, height = 10, units = "in")

  # ==Do a figure of 2023 SSB and 2018 YC
  names(mc)
  dim()
  #install.packages("ggExtra")
  library(ggExtra)
  mc %>%
    select(B2025, R2019) %>%
    ggplot(aes(x = R2019, y = B2025)) +
    geom_point(color = "forestgreen", alpha = .04) +
    theme_few() +
    geom_density_2d() +
    ylab("Spawning biomass next year (kt)") +
    xlab("2018 year class at age 1 (millions of fish)")
  p1 <- mc %>%
    select(B2025, R2019) %>%
    ggplot(aes(x = R2019, y = B2025)) +
    geom_point(color = "forestgreen", alpha = .2) +
    theme_few(base_size = 16) +
    ylab("Spawning biomass next year (kt)") +
    xlab("2018 year class at age 1 (millions of fish)")
  p2 <- ggMarginal(p1, type = "density", fill = "salmon")
  #ggsave("~/_mymods/afsc-assessments/ebs_pollock_safe/doc/figs/post_profile.png", plot = p3, width = 8, height = 10, units = "in")
  ggsave("doc/figs/ssb_v_2018.png", plot = p2, width = 8, height = 6, units = "in")
  p2

getwd()
  dim(mc)
  mc$Chain <- 1
  mc$Iteration <- 1:length(mc$Chain)
  mct <- mc # %>% filter(lnR0<12,Bmsy>500,Bmsy<6000,Fmsyr<2,Steepness<.78)
  max(mct$Steepness)

  p1 <- mct %>%
    select(Steepness, lnR0, DynB0, B2025, Bmsy, Fmsyr) %>%
    ggpairs(aes(fill = "lemonchiffon", alpha = .5), upper = NULL, lower = list(continuous = wrap("points", alpha = 0.1, size = 0.1))) + theme_classic()
  p1
  ggsave("doc/figs/mcmc_pairs.png", plot = p1, width = 9, height = 9, units = "in")
  # head(mc)
  # mc.t <- select(mc,Iteration,ObjFun,q,Steepness,lnR0,B18,"B/mean(B)") %>% gather(Parameter,value,-Iteration) #%>% head
  # ggplot(mc.t,aes(x=Iteration,y=value)) + geom_line() + .THEME + facet_wrap(~Parameter,scales="free")
  # summary(mc)

  # table of Means and CVs
  # for (i in 2:17) print(c(names(mc)[i],round(median(mc[,i]),3), round(mean(mc[,i]),3), paste0(round(100*sqrt(var(mc[,i]))/mean(mc[,i]),0),"%") ))
  # ggplot(mc.t,aes(x=Iteration,y=value)) + geom_line() + .THEME + facet_wrap(~Parameter,scales="free")
  names(mc)

  M <- M[[1]]
  M$future_SSB[, ]

  names(M)
  p1 <- mc %>%
    select(B2025) %>%
    ggplot(aes(B2025)) +
    geom_density(fill = "lemonchiffon", alpha = .5) +
    theme_few() +
    xlab(paste(nextyr, "Female spawning biomass")) +
    geom_vline(xintercept = M$future_SSB[1, 1], col = "grey") +
    geom_vline(xintercept = median(mc$B2025), size = 1, col = "red", linetype = "dashed")
  p1
  ggsave("doc/figs/mcmc_marg.png", plot = p1, width = 7, height = 4, units = "in")
  # Fmsyr
  M <- modlst[2]
  Fmsy2 <- M$fit$est[M$fit$names == "Fmsy2"]
  p1 <- mc %>%
    select(Fmsyr) %>%
    ggplot(aes(Fmsyr)) +
    geom_density(fill = "lemonchiffon", alpha = .5) +
    theme_few() +
    xlab("Fmsy rate") +
    xlim(c(0, 1.5)) +
    geom_vline(aes(xintercept = median(mc$Fmsyr), color = "median"), linetype = "dotted", size = 1) +
    geom_vline(aes(xintercept = mean(mc$Fmsyr), color = "mean"), linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = mean(mclen / mean(1 / mc$Fmsyr)), color = "harmonic_mean"), linetype = "solid", size = 1) +
    scale_color_manual(name = "Statistics", values = c(median = "blue", mean = "red", harmonic_mean = "darkgreen")) +
    scale_linetype_manual(name = "Statistics", values = c(median = "dotted", mean = "dashed", harmonic_mean = "solid"))
  p1
  ggsave("doc/figs/mcmc_marg_fmsy.png", plot = p1, width = 7, height = 4, units = "in")
  # geom_vline(xintercept=median(mc$Fmsyr),size=1,col="darkgreen",linetype=4)  +
  #          geom_vline(xintercept=Fmsy2,col="grey", ) +
  # geom_vline(xintercept=mean(mc$Fmsyr),size=1,col="blue",linetype="dotted")  +
  # geom_vline(xintercept=mclen/sum(1/mc$Fmsyr) ,size=1,col="red",linetype="dashed") ;p1
  # Get P B2021 < 20% Bzero
  msst <- .2 * M$b0

glimpse(mc)
  mc %>%
    filter(B2025 < msst) %>%
    summarise(n() / mclen * 100)
  prob_less_50_Bmsy <- mc %>%
    filter((B2024 / Bmsy) < .5) %>%
    summarise(n() / mclen * 100)
  # head(mc.t)
  # q
  mcppl <- read_csv(paste0(.MODELDIR[thismod], "/mcmc/mceval_ppl.csv"), col_names = FALSE)
  names(mcppl) <- c("Index", "Pd", "draw", "Year", "Obs", "Exp", "Sim", "VarObs")
  unique(mcppl$Index)  
  idx <- "BTS"
  ylim <- c(0, 15000)
  idx <- "ATS"
  ylim <- c(0, 10000)
  idx <- "AVO"
  ylim <- c(0, 6)
   idx=c("ATS","AVO")
  obs <- mcppl %>%
    filter(Index == idx, draw == 1) %>%
    transmute(Year, Obs, type = "Obs")
  tmpdf <- mcppl %>%
    filter(Index == idx) %>%
    select(Year, Exp, Sim) %>%
    sample_frac(.5) # %>% pivot_longer(cols=2:3,names_to="type",values_to="Biomass")
  ggplot(data = tmpdf) +
    geom_point(aes(x = jitter(Year), y = Sim), color = "grey", alpha = .2) +
    geom_point(aes(x = jitter(Year, .3), y = Exp), color = "yellow", alpha = .2, size = .8) +
    ylim(ylim) +
    theme_few() +
    ylab("Index") +
    xlab("Year") +
    geom_point(data = obs, aes(x = Year, y = Obs), size = 3) #+ facet_grid(.~idx)

  ggsave("doc/figs/ppd_ATS.png", width = 7, height = 4, units = "in")
  ggsave("doc/figs/ppd_AVO.png", width = 7, height = 4, units = "in")

  ## dd.g <- pivot_longer(dd,cols=2:20,names_to="Assessment",values_to="Biomass")
}
