mod_names <- c("Last year",
               "Cond SRR 1.3 Mt",
               "Drop CPUE"
)
mod_dir <- c(
  "lastyr",
  "condSRR_sept",
  "dropCPUE_sept"
)
# WARNING, commented out line will re-run all the models in the mod_dir directories within "runs"
# Won't do tier 3 spm (proj) model in the subdirectory at the moment
#---Read in the results for modelsl already run--------------
#run_model(rundir="2023_runs")
#run_proj(rundir="2023_runs")
modlst<-get_results(rundir="~/_mymods/afsc-assessments/ebs_pollock_safe/2024/runs")
M <<- modlst[[thismod]]
M <<- modlst[[2]]
.MODELDIR<<-paste0("~/_mymods/afsc-assessments/ebs_pollock_safe/2024/runs",
                   mod_dir,"/")
names(M)
M <- read_rep(here::here("2024","runs","test","pm.rep"))
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
 +
df <- data.frame(Year = 1964:2023, forage = rowSums(M$N), SSB = M$SSB[, 2]) |> mutate(rel_forage = forage / mean(forage), rel_SSB = SSB / mean(SSB))
glimpse(df)
meanForage <- mean(df$forage)
meanForage
mean(df$SSB)
qfor <- quantile(df$forage, c(0.05, .2))
df |> ggplot(aes(x = Year, y = forage)) +
  geom_line() +
  labs(title = "Total pollock forage (age 1-3)", x = "Year", y = "Abundance (thousands)") +
  ggthemes::theme_few() +
  xlim(c(1980, 2024)) +
  geom_hline(yintercept = qfor[1], color = "red") +
  geom_hline(yintercept = qfor[2], color = "blue", type = 2)

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
for (resp in c(0.05,0.5,0.95)){
  for (i in 2000:2022) {
    # get values from the time series
    lastidx <- i - 1977 + 14
    cat_mn <- mean(M$obs_catch[14:lastidx])
    cat_this <- M$obs_catch[lastidx]
    ssb_mn <- mean(M$SSB[14:lastidx, 2])
    ssb_this <- (M$SSB[lastidx, 2])
    ssb_next <- (M$SSB[lastidx + 1, 2])
    # cat_next   <- (ssb_next/ssb_this)^0.5 * cat_this
    cat_next <- (ssb_next / ssb_mn)^resp * cat_this
    df1 <- data.frame(Year = i, Mean_catch = cat_mn, Current_catch = cat_this, ABC = cat_next, ssb_mn = ssb_mn, ssb_this = ssb_this, ssb_next = ssb_next, resp=resp) |> rbind(df1)
  }
}

glimpse(df1)
df1 |>
  pivot_longer(cols = c(3:5, 7), names_to = "var", values_to = "value") |>
  mutate(Year = ifelse(var == "ABC", Year + 1, Year)) |>
  ggplot(aes(x = Year, y = value, shape = var, color = var)) +
  geom_point() +
  geom_line() +
  labs(title = "EBS Pollock ", x = "Year", y = "relative to mean") +
  theme_bw() + facet_grid(resp~.) +
  xlim(c(2000, 2024))

glimpse(df1)
kkk
