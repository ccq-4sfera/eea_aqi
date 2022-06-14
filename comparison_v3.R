# Downloads observations + CAMS (Tracasa-EEA) + CAMS (Forecast and Analysis), 
#       calculates CAMS_adjusted for all, checks which is best, compares AQI
# Author: Cristina Carnerero
# Date: 24/11/2021


library(lubridate)
library(tidyverse)
library(plyr)
library(dplyr)
library(openair)


#TODO check why some observations are missing -> because we have no cams (outside cams boundaries?)
#TODO check discrepancies with AD942



ptm <- proc.time() #start counting time

## PARAMETERS ----
pollutant.list = c("NO2","O3","SO2","PM10")
# start_date = "2021-11-15"
# end_date = "2021-11-25"
# compare_datetime = "2021-12-12 10:00"  #yyyy-mm-dd HH:MM, in UTC
compare_datetime = now(tz="UTC")  #yyyy-mm-dd HH:MM, in UTC
# end_date = today()
# start_date = today()-ddays(12)
end_date = date(compare_datetime)
start_date = end_date-ddays(12)
N = 100  #number of (worst) stations to compare
# ---


## FUNCTIONS ----
source('./download_aqblobs_cams.R')
source('./calc_cams_adjusted.R')
source('./netcdf_to_df.R')
source('./get_web_data.R')
source('./get_app_data.R')
source('./calc_AQI.R')

# function to fill gaps in column A with values in column B. Column B may be in a different df (df2)
gap_fill <- function(df1,columnA_name,columnB_name,df2=df1){
  gap_filled <- ifelse(is.na(df1[[columnA_name]]), 
                       df2[[columnB_name]], 
                       df1[[columnA_name]])
  return(gap_filled)
}

## MAIN ----
# NOTE Web time is UTC+1 END
#      App time is UTC (END?)
#      Aqblobs (observations and CAMS) time is UTC+1 END
#      CAMS Forecast and Analysis time is UTC BEGIN


dt_compare <- as.POSIXct(compare_datetime,format="%Y-%m-%d %H:%M",tz="UTC")
dt = format(dt_compare, "%Y-%m-%dT%H") #call is in UTC

##.Get worst AQI in web ----
data_web <- get_app_map()
names(data_web) <- c("aqi","station")

#.... Get topN, i.e., N stations with worst globalAQI (higher AQI) ----
topN <- setorder(data_web,-aqi) %>% slice(1:N)

station.list <- topN$station


##.Download Observations and CAMS (TRACASA-EEA) ----
# get vector containing all days between dates
date_seq = seq(as.Date(start_date),as.Date(end_date),by="day")

# read stations metadata
print("Preparing metadata of stations...")
df_metadata <- fread("https://aqblobs.blob.core.windows.net/airquality-e/E2aInventory.csv.gz",showProgress=F)
df_metadata$url_CAMS <- df_metadata$url
df_metadata$url_CAMS <- gsub("aqblobs", "dis2datalake", df_metadata$url_CAMS, fixed=T)
df_metadata$url_CAMS <- gsub("-e", "-derivated/CAMS", df_metadata$url_CAMS, fixed=T)


dtable <- read_delim("https://discomap.eea.europa.eu/map/fme/metadata/PanEuropean_metadata.csv",
                     delim="\t",col_types = cols())

print("Getting Observations...")
n=0
missing_obs=0

station.list <- "AD0942A"
for(station in station.list){
  # station <- "AD0942A"
  n=n+1
  print(paste0("Obs Station ",n,"/",N,": ",station))
  
  #try to get observations for station
  tryCatch(
    obs_cams <- download_aqblobs_cams(station,start_date,end_date,pollutant.list),
    error=function(e){print(paste0("No observations found for ",station))}
  )
  
  #if observations found, continue
  if(exists("obs_cams")){

    # add prefix "val_" to column names
    # names(obs_cams)[c(-1,-2)] <- paste0("val_",names(obs_cams)[c(-1,-2)])
    
    # substitute PM10 with PM10_cams, PM2.5_cams with pm10run, pm10run_cams, pm25run_cams...
    # if("pm10run" %in% colnames(obs_cams)){obs_cams$PM10 <- obs_cams$pm10run}
    # if("pm2.5run" %in% colnames(obs_cams)){obs_cams$PM2.5 <- obs_cams$pm2.5run}
    # if("pm10run_cams" %in% colnames(obs_cams)){obs_cams$PM10_cams <- obs_cams$pm10run_cams}
    # if("pm2.5run_cams" %in% colnames(obs_cams)){obs_cams$PM2.5_cams <- obs_cams$pm2.5run_cams}
    # if("index_PM10" %in% colnames(obs_cams)){obs_cams$index_PM10 <- obs_cams$index_pm10run}
    # if("index_pm10run_cams" %in% colnames(obs_cams)){obs_cams$index_PM10_cams <- obs_cams$index_pm10run_cams}
    # if("index_pm2.5run_cams" %in% colnames(obs_cams)){obs_cams$index_PM2.5_cams <- obs_cams$index_pm2.5run_cams}
    
    # drop from poll.list pollutants not measured in that station
    poll.list <- intersect(pollutant.list,colnames(obs_cams))
    
    
    ##.Calculate all CAMS adjusted ----
    # adjust CAMS TRACASA
    obs_cams_adj <- calc_cams_adjusted(obs_cams,poll.list)
    obs_cams_adj$station <- station
    
    
    # df_adj <- obs_cams_adj
    
    
    #if obs are NA, copy value of cams_adj
    for(pol in poll.list){
      obs_cams_adj[[paste0(pol,"_gapFill_eea")]] <- gap_fill(obs_cams_adj,pol,paste0(pol,"_cams_adj"))
    }
    
    
    
    
    #...
    # Runing means of PMs ----
    if("PM10_gapFill_eea" %in% names(obs_cams_adj)){
      obs_cams_adj <- rollingMean(obs_cams_adj, pollutant = "PM10_gapFill_eea", width = 24,
                                  align = "right", new.name = "PM10_gapFill_eea", data.capture = 75)
    }
    if("PM2.5_gapFill_eea" %in% names(obs_cams_adj)){
      obs_cams_adj <- rollingMean(obs_cams_adj, pollutant = "PM2.5_gapFill_eea", width = 24,
                                  align = "right", new.name = "PM2.5_gapFill_eea", data.capture = 75)
    }
    
    
    #...
    
    
    
    #.... Get AQ indexes----
    # index_obs <- obs_cams %>% select(c("date",paste0("index_",poll.list)))
    # index_cams <- obs_cams %>% select(c("date",matches("^index_.*_cams$")))
    
    # calculate AQI (with decimals) of observations and cams
    index_obs <- calc_AQI_dec(obs_cams,poll.list) #using observations without gap filling
    # index_obs <- calc_AQI_dec(obs_cams,poll.list,keep.all=T) #using observations without gap filling
    # index_cams <- calc_AQI_dec(obs_cams,poll.list,suffix="_cams")  #using cams
    index_cams <- calc_AQI_dec(obs_cams_adj,poll.list,suffix="_gapFill_eea") #using observations and gap filling with cams (eea methodology)
    # index_cams <- calc_AQI_dec(df_adj,poll.list,suffix="_gapFill_eea",keep.all=T) #using observations and gap filling with cams (eea methodology)
    
    ##.Calc Global AQI for observations ----
    
    #calculate global index (worst pollutant): max of all indexes
    index_cams$aqi <- apply(index_cams[,c(-1,-2)],1,max,na.rm=TRUE)

    # index_cams <- index_cams %>% filter_all(all_vars(is.finite(.)))  #remove infinites (when all indexes were NAs)
    # index_cams <- index_cams %>% filter_at(vars(aqi), all_vars(is.finite(.)))
    index_cams[index_cams==-Inf] <- NA
    
    # Find culprit
    #get column name of the column with higher AQI
    # index_cams$culprit <- colnames(index_cams)[apply(index_cams,1,which.max)]
    
    index_cams$culprit <- NA
    index_cams <- index_cams %>%
      dplyr::filter(!is.na(across(c(-date,-station)))) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(culprit = names(across(c(-date,-station,-aqi)))[which.max(across(c(-date,-station,-aqi)))])
    
    #index_cams$culprit <- names(index_cams)[which(index_cams==index_cams$aqi,arr.ind=T)[,"col"]][1:nrow(index_cams)]
    
    #get pollutant name (columns are "index_{pollutant}")
    index_cams$culprit <- substring(index_cams$culprit,7) %>% as.character()
    
    df_cams <- full_join(obs_cams_adj,index_cams, by=c("date","station"))
    
    
    #same for unadjusted
    index_obs$aqi <- apply(index_obs[,c(-1,-2)],1,max,na.rm=TRUE)
    index_obs[index_obs==-Inf] <- NA
    # index_obs$culprit <- colnames(index_obs)[apply(index_obs,1,which.max)] #FIXME
    # index_obs$culprit <- names(index_obs)[which(index_obs==index_obs$aqi,arr.ind=T)[,"col"]][1:nrow(index_obs)]
    
    index_obs$culprit <- NA
    index_obs <- index_obs %>%
      dplyr::filter(!is.na(across(c(-date,-station)))) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(culprit = names(across(c(-date,-station,-aqi)))[which.max(across(c(-date,-station,-aqi)))])
      
    index_obs$culprit <- substring(index_obs$culprit,7) %>% as.character()
    # index_obs$StationCode <- station
    
    df_obs <- full_join(obs_cams,index_obs, by=c("date","station"))
    
    
    if(exists("top_obs")){
      # top_obs <- bind_rows(top_obs,data.frame("date"=index_cams$date, "StationCode"=station, "aqi"=index_cams$aqi, "culprit"=index_cams$culprit))
      top_obs <- bind_rows(top_obs,df_cams)
    }else{
      # top_obs <- data.frame("date"=index_cams$date, "StationCode"=station, "aqi"=index_cams$aqi, "culprit"=index_cams$culprit)
      top_obs <- df_cams
    }
    
    if(exists("top_obs_unadj")){
      # top_obs_unadj <- bind_rows(top_obs_unadj,data.frame("date"=index_obs$date, "StationCode"=station, "aqi"=index_obs$aqi, "culprit"=index_obs$culprit))
      top_obs_unadj <- bind_rows(top_obs_unadj,df_obs)
    }else{
      # top_obs_unadj <- data.frame("date"=index_obs$date, "StationCode"=station, "aqi"=index_obs$aqi, "culprit"=index_obs$culprit)
      top_obs_unadj <- df_obs
    }
    
  }else{
    missing_obs=missing_obs+1
  }
  rm(obs_cams,obs_cams_adj)
}


missing_stations_obs <- setdiff(station.list,unique(top_obs$station))

proc.time()-ptm   #print time elapsed running code, in seconds
# 90s (N=15)




#.... Get app AQI ----
# all times for each station with worst AQI in Web
print("Getting App data...")
n=0
for(s in station.list){
  n=n+1
  print(paste0("App Station ",n,"/",N,": ",s))
  #allow for missing stations in app
  
  s <- station
  tryCatch(
    {
      data <- get_app_data(station.Code=s,onlyAQI=FALSE) %>% 
        select(c(date,aqi,culprit,starts_with("val_"),starts_with("modelled")))
      data$station = s
    },
    error=function(e){print(paste0(s," not found in App"))}
  )
  
  if(exists("top_app")){
    # top_app <- bind_rows(top_app,
    #                      data.frame(date=data$date, StationCode=s, aqi=data$aqi, culprit=data$culprit))
    top_app <- bind_rows(top_app, data)
  }else{
    # top_app <- data.frame(date=data$date, StationCode=s, aqi=data$aqi, culprit=data$culprit)
    top_app <- data
  }
}

# before merging, we need to homogeneize timezones: app to UTC+1 (+2hours)
top_app$date <- top_app$date+dhours(2)

#convert culprit from factor to character
top_app$culprit <- as.character(top_app$culprit)



# TIMES
# app: UTC begin
# web: UTC+1 end
# obs: UTC+1 end


# merge all tops
tops_all <- left_join(top_obs_unadj,top_app,by=c("date","station"),suffix=c("","_app")) %>%
  left_join(top_obs,by=c("date","station"),suffix=c("_obs_unadj","_obs"))


# drop rows with NA in aqi (app or obs)
tops <- tops_all %>% 
  dplyr::filter(!is.na(aqi_app) & !is.na(aqi_obs) & !is.na(aqi_obs_unadj))


# homogeneize AQI levels (web starts from zero, all others from 1)
# tops$aqi_web <- tops$aqi_web+1 
# tops_withNA$aqi_web <- tops_withNA$aqi_web+1

# Get last integer in aqi columns (floor)
# tops_floor <- tops_floor %>% mutate_at(c("aqi_app","aqi_obs"), floor)
tops_floor <- tops %>% mutate_at(c("aqi_app","aqi_obs","aqi_obs_unadj"), floor)


# web index goes beyond 6... homogeneize to max 6 like app and obs
# tops_floor$aqi_web[tops_floor$aqi_web>6] <- 6
# tops_floor_withNA$aqi_web[tops_floor_withNA$aqi_web>6] <- 6





#.RESULTS ----
total <- nrow(tops_floor)

# rows_withNA <- 100-sum(!complete.cases(tops_floor_withNA))/nrow(tops_floor_withNA)*100
# rows_missing <- nrow(tops_floor_withNA)-total





# compare3----
# missing observations
obs_NA <- sum(is.na(tops_floor$aqi_obs))
obs_NA <- obs_NA/total*100
#obs_NA=rows_missing


all_equal <- sum(tops_floor$aqi_obs_unadj==tops_floor$aqi_app & tops_floor$aqi_obs_unadj==tops_floor$aqi_obs,na.rm=T)
all_equal <- all_equal/total*100
df_all_equal <- tops_floor[tops_floor$aqi_obs_unadj==tops_floor$aqi_app & tops_floor$aqi_obs_unadj==tops_floor$aqi_obs,]

all_diff <- sum(tops_floor$aqi_obs_unadj!=tops_floor$aqi_app & tops_floor$aqi_obs_unadj!=tops_floor$aqi_obs  & tops_floor$aqi_app!=tops_floor$aqi_obs,na.rm=T)
all_diff <- all_diff/total*100
df_all_diff <- tops_floor[tops_floor$aqi_obs_unadj!=tops_floor$aqi_app & tops_floor$aqi_obs_unadj!=tops_floor$aqi_obs  & tops_floor$aqi_app!=tops_floor$aqi_obs,]

one_diff <- 100-all_equal-all_diff-obs_NA

unadj_diff <- sum(tops_floor$aqi_obs_unadj!=tops_floor$aqi_app & tops_floor$aqi_obs_unadj!=tops_floor$aqi_obs & tops_floor$aqi_app==tops_floor$aqi_obs,na.rm=T)
unadj_diff <- unadj_diff/total*100

app_diff <- sum(tops_floor$aqi_app!=tops_floor$aqi_obs_unadj & tops_floor$aqi_app!=tops_floor$aqi_obs & tops_floor$aqi_obs_unadj==tops_floor$aqi_obs,na.rm=T)
app_diff <- app_diff/total*100

obs_diff <- sum(tops_floor$aqi_obs!=tops_floor$aqi_obs_unadj & tops_floor$aqi_obs!=tops_floor$aqi_app & tops_floor$aqi_obs_unadj==tops_floor$aqi_app,na.rm=T)
obs_diff <- obs_diff/total*100

gaps <- sum(tops_floor$aqi_obs_unadj!=tops_floor$aqi_obs,na.rm=T)
gaps <- gaps/total*100
df_gaps <- tops_floor[tops_floor$aqi_obs_unadj!=tops_floor$aqi_obs,]

#all_dif is the same as unadj_adj_dif and culprit are different



# save results in a dataframe
results <- data.frame("Type"="compare AQI",
                      "Stations"=length(unique(tops$station)),
                      "Missing stations"=length(missing_stations_obs),
                      "Total samples"=total,
                      "Missing obs (%)"=obs_NA,
                      "All equal (%)"=all_equal,
                      "All different (%)"=all_diff,
                      "One different (%)"=one_diff,
                      "unadj != adj = app (%)"=unadj_diff,
                      "app != adjusted = unadj (%)"=app_diff,
                      "adj != unadj = app (%)"=obs_diff,
                      "Gaps (%)"=gaps,
                      check.names = FALSE)




#compare CULPRIT----

all_equal_culprit <- sum(tops_floor$culprit_obs_unadj==tops_floor$culprit_app & tops_floor$culprit_obs_unadj==tops_floor$culprit_obs,na.rm=T)
all_equal_culprit <- all_equal_culprit/total*100
df_all_equal_culprit <- tops_floor[tops_floor$culprit_obs_unadj==tops_floor$culprit_app & tops_floor$culprit_obs_unadj==tops_floor$culprit_obs,]

all_diff_culprit <- sum(tops_floor$culprit_obs_unadj!=tops_floor$culprit_app & tops_floor$culprit_obs_unadj!=tops_floor$culprit_obs  & tops_floor$culprit_app!=tops_floor$culprit_obs,na.rm=T)
all_diff_culprit <- all_diff_culprit/total*100
df_all_diff_culprit <- tops_floor[tops_floor$culprit_obs_unadj!=tops_floor$culprit_app & tops_floor$culprit_obs_unadj!=tops_floor$culprit_obs  & tops_floor$culprit_app!=tops_floor$culprit_obs,]

one_diff_culprit <- 100-all_equal_culprit-all_diff_culprit-obs_NA

unadj_diff_culprit <- sum(tops_floor$culprit_obs_unadj!=tops_floor$culprit_app & tops_floor$culprit_obs_unadj!=tops_floor$culprit_obs & tops_floor$culprit_app==tops_floor$culprit_obs,na.rm=T)
unadj_diff_culprit <- unadj_diff_culprit/total*100

app_diff_culprit <- sum(tops_floor$culprit_app!=tops_floor$culprit_obs_unadj & tops_floor$culprit_app!=tops_floor$culprit_obs & tops_floor$culprit_obs_unadj==tops_floor$culprit_obs,na.rm=T)
app_diff_culprit <- app_diff_culprit/total*100

obs_diff_culprit <- sum(tops_floor$culprit_obs!=tops_floor$culprit_obs_unadj & tops_floor$culprit_obs!=tops_floor$culprit_app & tops_floor$culprit_obs_unadj==tops_floor$culprit_app,na.rm=T)
obs_diff_culprit <- obs_diff_culprit/total*100

gaps_culprit <- sum(tops_floor$culprit_obs_unadj!=tops_floor$culprit_obs,na.rm=T)
gaps_culprit <- gaps_culprit/total*100
df_gaps_culprit <- tops_floor[tops_floor$culprit_obs_unadj!=tops_floor$culprit_obs,]


results <- results %>% bind_rows(data.frame("Type"="compare culprits",
                      "Stations"=length(unique(tops$station)),
                      "Missing stations"=length(missing_stations_obs),
                      "Total samples"=total,
                      "Missing obs (%)"=obs_NA,
                      "All equal (%)"=all_equal_culprit,
                      "All different (%)"=all_diff_culprit,
                      "One different (%)"=one_diff_culprit,
                      "unadj != adj = app (%)"=unadj_diff_culprit,
                      "app != adjusted = unadj (%)"=app_diff_culprit,
                      "adj != unadj = app (%)"=obs_diff_culprit,
                      "Gaps (%)"=gaps_culprit,
                      check.names = FALSE))






# plot timeseries of AQI web,app,obs for each station
for(sc in unique(tops$station)){
  df <- tops %>% dplyr::filter(station==sc) %>%
    select(c("date",contains("aqi")))
  print(ggplot(reshape2::melt(df, id.vars = "date")) +
          geom_line(aes(x = date, y = value, colour = variable))+
          ggtitle(sc)+
          theme_bw())
}

png(paste0("./output/topAQI/pieChart_detailed_NAs_N",N,"_",dt,".png"),width=1000,height=700,res=100)
pie(c(all_equal,all_diff,obs_NA,unadj_diff,app_diff,obs_diff),
    labels=c(paste("All equal\n",round(all_equal,1),"%"),
             paste("All different",round(all_diff,1),"%"),
             paste("Obs missing",round(obs_NA,1),"%"),
             paste("One different (obs_unadj)",round(unadj_diff,1),"%"),
             paste("One different (app)",round(app_diff,1),"%"),
             paste("One different (obs_adj)",round(obs_diff,1),"%")),
    main=paste0("Top ",N," stations, ",total," samples \n",tops$date[1],"~",tops$date[total]),
    col=terrain.colors(6,rev=T),
    radius=1.05,
    cex=0.8)
dev.off()




# compare only equal culprits ----
# filter only those with equal culprit before comparison

total_all_equal_culprit <- nrow(df_all_equal_culprit)
# missing observations
obs_NA_filtered <- sum(is.na(df_all_equal_culprit$aqi_obs))
obs_NA_filtered <- obs_NA_filtered/total_all_equal_culprit*100


all_equal_filtered <- sum(df_all_equal_culprit$aqi_obs_unadj==df_all_equal_culprit$aqi_app & df_all_equal_culprit$aqi_obs_unadj==df_all_equal_culprit$aqi_obs,na.rm=T)
all_equal_filtered <- all_equal_filtered/total_all_equal_culprit*100
df_all_equal_filtered <- tops_floor[df_all_equal_culprit$aqi_obs_unadj==df_all_equal_culprit$aqi_app & df_all_equal_culprit$aqi_obs_unadj==df_all_equal_culprit$aqi_obs,]

all_diff_filtered <- sum(df_all_equal_culprit$aqi_obs_unadj!=df_all_equal_culprit$aqi_app & df_all_equal_culprit$aqi_obs_unadj!=df_all_equal_culprit$aqi_obs  & df_all_equal_culprit$aqi_app!=df_all_equal_culprit$aqi_obs,na.rm=T)
all_diff_filtered <- all_diff_filtered/total_all_equal_culprit*100
df_all_diff_filtered <- tops_floor[df_all_equal_culprit$aqi_obs_unadj!=df_all_equal_culprit$aqi_app & df_all_equal_culprit$aqi_obs_unadj!=df_all_equal_culprit$aqi_obs  & df_all_equal_culprit$aqi_app!=df_all_equal_culprit$aqi_obs,]

one_diff_filtered <- 100-all_equal_filtered-all_diff_filtered-obs_NA_filtered

unadj_diff_filtered <- sum(df_all_equal_culprit$aqi_obs_unadj!=df_all_equal_culprit$aqi_app & df_all_equal_culprit$aqi_app==df_all_equal_culprit$aqi_obs,na.rm=T)
unadj_diff_filtered <- unadj_diff_filtered/total_all_equal_culprit*100

app_diff_filtered <- sum(df_all_equal_culprit$aqi_app!=df_all_equal_culprit$aqi_obs_unadj & df_all_equal_culprit$aqi_obs_unadj==df_all_equal_culprit$aqi_obs,na.rm=T)
app_diff_filtered <- app_diff_filtered/total_all_equal_culprit*100
df_app_diff_filtered <- tops_floor[df_all_equal_culprit$aqi_app!=df_all_equal_culprit$aqi_obs_unadj & df_all_equal_culprit$aqi_obs_unadj==df_all_equal_culprit$aqi_obs,]

obs_diff_filtered <- sum(df_all_equal_culprit$aqi_obs!=df_all_equal_culprit$aqi_obs_unadj & df_all_equal_culprit$aqi_obs_unadj==df_all_equal_culprit$aqi_app,na.rm=T)
obs_diff_filtered <- obs_diff_filtered/total_all_equal_culprit*100

#unadj!=adj
gaps_filtered <- sum(df_all_equal_culprit$aqi_obs_unadj!=df_all_equal_culprit$aqi_obs,na.rm=T)
gaps_filtered <- gaps_filtered/total_all_equal_culprit*100
df_unadj_adj_diff_filtered <- tops_floor[df_all_equal_culprit$aqi_obs_unadj!=df_all_equal_culprit$aqi_obs,]



# save results in a dataframe
results <- results %>% bind_rows(data.frame("Type"="compare AQI only for those with same culprit",
                          "Stations"=length(unique(df_all_equal_culprit$station)),
                          "Missing stations"=0,
                          "Total samples"=total_all_equal_culprit,
                          "Missing obs (%)"=obs_NA_filtered,
                          "All equal (%)"=all_equal_filtered,
                          "All different (%)"=all_diff_filtered,
                          "One different (%)"=one_diff_filtered,
                          "unadj != adj = app (%)"=unadj_diff_filtered,
                          "app != adjusted = unadj (%)"=app_diff_filtered,
                          "adj != unadj = app (%)"=obs_diff_filtered,
                          "Gaps (%)"=gaps_filtered,
                          check.names = FALSE))


write.csv(results,paste0("./output/topAQI/results_N",N,"_",dt,".csv"))

#TODO estudiar filtrant per si tenim gaps o no per separat




# compare2----

# AQI equal or different in % of samples 

all_equal <- sum(tops_floor$aqi_app==tops_floor$aqi_obs,na.rm=T)
all_equal <- all_equal/total*100

all_diff <- 100-all_equal


# missing observations
obs_NA <- sum(is.na(tops_floor$aqi_obs))
obs_NA <- obs_NA/total*100
#obs_NA=rows_missing

print(paste0("All AQI equal in ",sprintf("%.0f",all_equal),"% of samples")) #94%
print(paste0("All AQI different in ",sprintf("%.0f",all_diff),"% of samples")) #0%


# save results in a dataframe
results <- data.frame("Stations"=N,
                      "Missing stations"=length(missing_stations_obs),
                      "Total samples"=total,
                      "Full samples"=total,
                      "Missing obs (%)"=obs_NA,
                      "All equal (%)"=all_equal,
                      "All different (%)"=all_diff,
                      check.names = FALSE)
write.csv(results,paste0("./output/topAQI/results_N",N,"_",dt,".csv"))





# plot timeseries of AQI web/app,obs for each station
for(sc in unique(tops$station)[1]){
  df <- tops %>% dplyr::filter(station==sc) %>%
                 select(c("date",contains("aqi")))
  print(ggplot(reshape2::melt(df, id.vars = "date")) +
          geom_line(aes(x = date, y = value, colour = variable))+
          ggtitle(sc)+
          theme_bw())
}

#Pie chart
# png(paste0("./output/topAQI/pieChart_N",N,"_",dt,".png"),width=800,height=500,res=100)
# pie(c(all_equal,all_diff,one_diff),
#     labels=c(paste("All equal\n",round(all_equal,1),"%"),
#              paste("All different",round(all_diff,1),"%"),
#              paste("One different",round(one_diff,1),"%")),
#     main=paste0("Top ",N," stations, ",total," samples \n ",dt),
#     col=terrain.colors(3),
#     radius=1)
# dev.off()

#Pie chart specifying which different
png(paste0("./output/topAQI/pieChart_detailed_NAs_N",N,"_",dt,".png"),width=1000,height=700,res=100)
pie(c(all_equal,all_diff,obs_NA),
    labels=c(paste("All equal\n",round(all_equal,1),"%"),
             paste("All different",round(all_diff,1),"%"),
             paste("Obs missing",round(obs_NA,1),"%")),
    main=paste0("Top ",N," stations, ",total," samples \n",tops$date[1],"~",tops$date[total]),
    col=terrain.colors(6,rev=T),
    radius=1.05,
    cex=0.8)
dev.off()

proc.time()-ptm   #print time elapsed running code, in seconds
#400s (6.5min) for N=100
#1963s (33min) for N=500



