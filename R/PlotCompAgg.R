# Function to plot aggregate Age comps
# Read n a model result
names(M) #|> grep("sam")
M$sam_fsh
dim(M$pobs_fsh)
df <- (data.frame(  N = M$sam_fsh, M$pobs_fsh[,-1]-M$phat_fsh[,-1])
df <- rbind(data.frame( 
                 N = M$sam_fsh,
                 M$pobs_fsh[,-1]-M$phat_fsh[,-1],
                 src = "Fishery"),
            data.frame( 
                 N = M$sam_bts,
                 M$pobs_bts[,-1]-M$phat_bts[,-1],
                 src = "Bottom trawls survey"),
            data.frame( 
                 N = M$sam_ats,
                 M$pobs_ats[,-1]-M$phat_ats[,-1],
                 src = "Acoustic-trawl survey")
)
library(dplyr)
names(df) <- c("N", 1:15,"src")

# Assuming your dataframe is called 'df'
library(dplyr)

# Assuming your dataframe is called 'df'
result <- df %>%
  group_by(src) %>%                           # Group by the 'src' column
  mutate(across(2:16, ~ . * df[[1]])) %>%     # Multiply columns 2:16 by column 1
  summarise(across(2:16, sum, na.rm = TRUE))  # Sum each column within the group, ignoring NA values

print(result)
result <- df |>  filter(src=="Fishery") |> 
  mutate(across(2:16, ~ . * df[[1]])) %>%  # Multiply columns 2:16 by column 1
  summarise(across(2:16, sum)) #%>%         # Sum each of the modified columns
  unlist()                                 # Convert to a vector

print(result)
            
                 
glimpse(df)
df <- data.frame(NULL)
for (i in 1:dim(M$pobs_fsh)[1]){
  df <- df + M$sam_fsh[i](M$pobs_fsh[i,-1])-(M$phat_fsh[i,-1])
}


