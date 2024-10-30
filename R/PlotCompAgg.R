# Function to plot aggregate Age comps
# Read n a model result
names(M) #|> grep("sam")
M$sam_fsh
dim(M$pobs_fsh)
# tmp <- as.data.frame(M$pobs_fsh[,-1]-M$phat_fsh[,-1] );names(tmp) <- 1:15 
tmp <- data.frame(type="obs", M$pobs_fsh[,-1] );tmp <- rbind(tmp, data.frame(type="pred", M$phat_fsh[,-1] ))
names(tmp) <- c("type",1:15 )
t1 <- cbind(tmp,year=M$pobs_fsh[,1], N=M$sam_fsh, src="Fishery") |> pivot_longer(cols=2:16,names_to = "age", values_to="prop")

tmp <- data.frame(type="obs", M$pobs_bts[,-1] );tmp <- rbind(tmp, data.frame(type="pred", M$phat_bts[,-1] ))
names(tmp) <- c("type",1:15 )
t2 <- cbind(tmp,year=M$pobs_bts[,1], N=M$sam_bts, src="Bottom-trawl") |> pivot_longer(cols=2:16,names_to = "age", values_to="prop")

tmp <- data.frame(type="obs", M$pobs_ats[,-1] );tmp <- rbind(tmp, data.frame(type="pred", M$phat_ats[,-1] ))
names(tmp) <- c("type",1:15 )
t3 <- cbind(tmp,year=M$pobs_ats[,1], N=M$sam_ats, src="Acoustic-trawl") |> pivot_longer(cols=2:16,names_to = "age", values_to="prop")

df <- rbind(t1,t2,t3)
df$age <- as.numeric(df$age)
glimpse(df)
df |> pivot_wider(names_from = type, values_from=prop) |> filter(!(src=="Acoustic-trawl" & age==1))  |> 
  group_by(age,src) |>  summarise(obs = sum(obs*N), pred=sum(pred*N)) #|> 
  pivot_longer(cols=3:4, names_to="type", values_to="N") |> 
  ggplot(aes(x=age, y=N, color=type, fill=type) ) + geom_line() + geom_bar() +theme_few() +
           facet_grid(src~., scales='free')
df |> pivot_wider(names_from = type, values_from=prop) |> filter(!(src=="Acoustic-trawl" & age==1))  |> 
  group_by(age,src) |>  summarise(obs = sum(obs*N), pred=sum(pred*N)) |> 
  ggplot(aes(x=age, y=pred) ) + geom_col(aes(x=age, y=obs),color="brown", fill='salmon') +theme_few(base_size=18 ) +
    scale_y_continuous(label = comma) + ylab("Sum of effective sample sizes x proportions") + xlab("Age") +
    geom_line() + geom_point(size=3) + facet_grid(src~., scales='free')
ggsave("doc/figs/mod_fit_agg_comp.png", width = 8.4, height = 8, units = "in")
