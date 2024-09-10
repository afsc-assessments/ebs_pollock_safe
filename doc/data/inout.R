radian
library(tidyverse)
library(googledrive)
library(lubridate)
library(ggthemes)
library(patchwork)
#install.packages("gmt")
## Read in pollock catch data
.inseasdir <- "~/_mymods/ebswp/data/fishery/"
inseas<-read_csv(paste0(.inseasdir,"inseas.csv")) names(inseas) <- tolower( names(inseas) )
names(inseas)
summary(inseas$year)
glimpse(inseas)
#names(inseas) <-c( "v","haul","cruise","yr", "lon","lat","v_type","dur","fdepth","bdepth","otc", "pollwt","pollno", "pcodwt","pcodno", "sablwt","sablno", "atkawt","atkano", "herrwt","herrno", "chinno","chumno","date")
names(inseas) <-c( "v","haul","cruise","date","lat","lon","v_type","dur","fdepth","bdepth","otc","pollwt","pollno","sablwt","sablno","herrwt","herrno","chinno","chumno","deploy_date","deploy_latitude","deploy_longitude_converted","cdq_code","nmfs_area", "performance","yr")
#names(inseas) <-c( "v", "haul", "cruise","mm",  "dd", "yr", "lon","lat","v_type","dur","fdepth","bdepth","otc", "pollwt","pollno", "pcodwt","pcodno", "sablwt","sablno", "atkawt","atkano", "herrwt","herrno", "chinno","chumno","date","Time","dlat","dlon","CDQ","area","xx")
drive_download("inseas_all.csv",overwrite=TRUE)
drive_download("hdr.csv")
inseas <- read_csv("inseas_all.csv",col_names=FALSE)
names(inseas) <- names(read_csv("hdr.csv"))
glimpse(inseas)

library(lubridate)
inseas$date <-   ymd(paste0(inseas$yr,"-",inseas$mm,"-",inseas$dd))

summary(inseas$yr)
#inseas$date <-   ymd(paste0(inseas$yr,"-",inseas$mm,"-",inseas$dd))
#d$date <- ymd(paste(d$yr,"-",d$mm,"-",d$dd,sep=""))
d <- inseas %>% mutate(lon=ifelse(!is.na(lons),360+(lons+lone)/2,360+lone),lat=ifelse(!is.na(lats),(lats+late)/2,late))
glimpse(d)
#d %>% filter(yr==1991) %>% transmute(lats,lons,late,lone,lat, lon)
#d %>% group_by(yr) %>% summarise(mean(lon)) %>% tail()
# names(d) d$dlat <- d$deploy_latitude d$dlat <- floor(d$dlat/100)+(d$dlat-100*floor(d$dlat/100))/60 d$dlon <- d$deploy_longitude_converted
d$week <- week(d$date)
d$seas<-d$week>=25
d$seas[d$seas]<-"B"
d$seas[d$seas!="B"]<-"A"
glimpse(d)
dtmp <- d %>% filter(!is.na(pollwt),pollwt/otc/1e3>.8) %>% 
       select(lon,lat,pollwt,pollno,yr,seas) #%>% group_by(yr) %>% summarise(sum(pollwt)/1e6) 
glimpse(dtmp)
dtmp %>% filter(yr==1991) 
# This shows that for tows w/ extrapolated weight, about 96% of the catch is for tows >80% pollock (compared to OTC)
sum(dtmp$pollwt)/(d %>% filter(yr>1990,!is.na(pollno), !is.na(pollwt)) %>% summarise(sum(pollwt)))
write.table(dtmp,"ins.dat",row.names=FALSE,col.names=FALSE,quote=FALSE)
q()
tmp <- d %>% filter(!is.na(pollno), !is.na(pollwt), yr==2008) %>% group_by(week) %>%summarise(sum(otc),avgwt=sum(pollwt)/sum(pollno)) 
tmp

gmt begin pribs
	gmt set GMT_THEME cookbook
	gmt select ins.dat -V -Fpribs.dat -Ir >inside_pribs.dat
	gmt select ins.dat -Fpribs.dat -If >outside_pribs.dat
	gmt select ins.dat -V -Fstgeorge.dat -Ir >inside_stg.dat
	gmt select ins.dat -V -Fstgeorge.dat -If >outside_stg.dat
	gmt select ins.dat -V -Fstpaul.dat -Ir >inside_stp.dat
	gmt select ins.dat -V -Fstpaul.dat -If >outside_stp.dat
	wc *dat
	gmt pscoast -R180/205/52/62/0/5 -Gbrown -Di -Bx5 -By2 -JM8i 
	gmt plot pribs.dat -Wthick,100, -i0,1 -h1 -L
	gmt plot inside_pribs.dat -W1 -i0,1 -h1 -Sc0.1 
	gmt plot outside_pribs.dat -G180/180/180 -i0,1 -h1 -Sc0.2
gmt end show
# Now read in results and make dataframe
df <- rbind(data.frame(read.table("inside_stp.dat"),forage_domain="in_StP"),
data.frame(read.table("inside_stg.dat"),forage_domain="in_StG"),
data.frame(read.table("outside_stp.dat"),forage_domain="out_StP"),
data.frame(read.table("outside_stg.dat"),forage_domain="out_StG"),
data.frame(read.table("inside_pribs.dat"),forage_domain="in_pribs"),
data.frame(read.table("outside_pribs.dat"),forage_domain="out_pribs")
)
names(df) <- c("lon","lat","pollwt","pollno","year","seas","forage_domain")
glimpse(df)
df_jerem <- df %>% group_by(year,seas,forage_domain)%>%summarise(pollwt=sum(pollwt),pollno=sum(pollno)) #%>% 
#ungroup() %>% group_by(forage_domain) %>%summarise(sum(pollwt))
write.csv(df_jerem,"Jerem_poll.csv",row.names=FALSE)
	gmt plot pribs.dat -Wfaint -i0,1 -h1 -Sc.1 
pscoast("-R180/205/52/66/0/5 -JM4i  -Gtan -Di -Bx10 -By5 ")
#pscoast(demo.coast)
pribs <-read.csv("pribs.dat",header=FALSE)
psxy(pribs)
#psxy(demo.xy)
     pstext(demo.text, "-J -R -F+f+a+j -O -K")
     psbar(demo.bar, ref=66)
     psclose()
du <- read.csv("pollock_cpue.csv")
du <- read.csv("nrs.csv")