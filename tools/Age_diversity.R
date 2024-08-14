  df <- data.frame(Year=M$Yr,Age=M$H,Measure="Population Age\n diversity")
  df <- rbind(df,data.frame(Year=M$Yr,Age=M$avg_age_mature,Measure="SSB Age\n diversity"))
  p1 <- df %>% filter(Year>1979) %>% ggplot(aes(x=Year,y=Age,color=Measure)) + geom_line(linewidth=1) + ggthemes::theme_few() + scale_x_continuous(limits=c(1980,2023), breaks=seq(1980,2022,5));p1
  ggsave("doc/figs/age_diversity.pdf",plot=p1,width=8,height=3.0,units="in")

