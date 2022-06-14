# Download aqblobs and CAMS for one station between two dates for a list of pollutants
# This file works on AQ index calculation for comparison
# Restricted & confidential: 4sfera Innova SLU (B55061973)
# Cristina Carnerero, Anna Ripoll and Jaume Targa
# Updated: 02/12/2021


## LIBRARIES ----
#install packages if not already installed
.packages = c("tidyverse","dplyr","lubridate","data.table","vroom","openair")
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

library(tidyverse)
library(dplyr)
library(lubridate)
library(data.table)
library(vroom)
library(openair)


## example of PARAMETERS ----
# poll.list = c("NO2","O3","SO2","PM10","PM2.5")
# station.Code = "AD0942A"
# start_date = "2021-10-01"
# end_date = "2021-11-30"
## - - - - - - - -


#TODO filter invalid!


download_aqblobs_cams <- function(station.Code,start_date,end_date,poll.list,get_cams=TRUE){
  # generate date sequence from sart and end date
  date_seq = seq(as.Date(start_date),as.Date(end_date),by="day")
  
  # read metadata
  if(!exists("df_metadata")){
    df_metadata <- fread("https://aqblobs.blob.core.windows.net/airquality-e/E2aInventory.csv.gz")
    #prepare CAMS url for later
    if(get_cams){
      df_metadata$url_CAMS <- df_metadata$url
      df_metadata$url_CAMS <- gsub("aqblobs", "dis2datalake", df_metadata$url_CAMS, fixed = T)
      df_metadata$url_CAMS <- gsub("-e", "-derivated/CAMS", df_metadata$url_CAMS, fixed = T)
    }
  }
  
  
  # filter selected station in metadata file to find sampling points (SPO)
  list.SPO <- df_metadata %>%
    dplyr::filter (station == station.Code) %>%
    dplyr::filter (pollutant %in% poll.list)
  list.SPO <- distinct(list.SPO, pollutant, .keep_all = TRUE)
  
  #. Access and join SPO timeseries ----
  for (i in 1:nrow(list.SPO)){
    SPO <- list.SPO$spo[i]
    poll <- list.SPO$pollutant[i]
    
    # Join aqblobs
    
    # get the years and months between start_date and end_date
    years_months <- unique(paste0(format.Date(date_seq, "%Y"),"_",format.Date(date_seq,"%m")))
    
    df_temp <- df_metadata %>%
      dplyr::filter(spo == SPO) %>%
      dplyr::filter(grepl(paste(years_months,collapse="|"),url)) %>%
      dplyr::distinct()
    
    df_url <- df_temp$url %>% unique()
    
    if(length(df_url>0)){
      df <- vroom(df_url,progress=FALSE,show_col_types = FALSE)
      
      rm(df_url)
      
      df$SamplingPoint <- SPO
      df$pollutant <- poll
      df$station <- station.Code
      
      if(exists("df_all") && is.data.frame(get("df_all"))){
        df_all <- rbind.fill(df_all, df)
      }else{
        df_all <- df
      }
      
    ##. Download CAMS ----
      if(get_cams){
        df_temp$url_CAMS <- gsub(paste0("E_",SPO,"_",poll), "CAMS", df_temp$url_CAMS, fixed = T)
        
        # transform month number to number with no leading zeros (e.g. August is 8, not 08)
        df_temp$url_CAMS <- gsub("_0", "_", df_temp$url_CAMS, fixed = T)
        cams_url <- df_temp$url_CAMS %>% unique()
        
        cams <- vroom(cams_url,progress=FALSE,show_col_types = FALSE)
      }
    }
  }
  
  df_all$date <- lubridate::ymd_hms(df_all$datetime_end, tz='Etc/GMT+1')
  df_all$value_numeric <- as.numeric(df_all$value_numeric)
  
  data <- df_all %>% select(c(station,pollutant,date,value_numeric))
  
  
  # pollutants in columns
  data_col <- data %>% 
    select(c(station,date)) %>%
    distinct()

  data <- data %>% select(c(date,pollutant,value_numeric))
  
  data_temp <- data %>% 
    distinct(across(c(date,pollutant)),.keep_all = TRUE) %>% 
    pivot_wider(names_from = pollutant, values_from = value_numeric)
  data_col <- left_join(data_col, data_temp, by = "date")
  
  ##. add CAMS ----
  if(get_cams){
    cams$dt <- lubridate::ymd_hms(cams$dt, tz='Etc/GMT+1')
    colnames(cams) <-  sub("val_", "", colnames(cams))
    data_col <- full_join(data_col, cams, by = c ("date" = "dt"))
  }

  
  # Runing means of PMs ----
  # if("PM10" %in% names(data_col)) {
  #   data_col <- rollingMean(data_col, pollutant = "PM10", width = 24,
  #                           align = "right", new.name = "pm10run", data.capture = 75)
  # }
  # if("PM2.5" %in% names(data_col)) {
  #   data_col <- rollingMean(data_col, pollutant = "PM2.5", width = 24,
  #                           align = "right", new.name = "pm2.5run", data.capture = 75)
  # }
  # if("PM10_cams" %in% names(data_col)) {
  #   data_col <- rollingMean(data_col, pollutant = "PM10_cams", width = 24,
  #                           align = "right", new.name = "pm10run_cams", data.capture = 75)
  # }
  # if("PM2.5_cams" %in% names(data_col)) {
  #   data_col <- rollingMean(data_col, pollutant = "PM2.5_cams", width = 24,
  #                           align = "right", new.name = "pm2.5run_cams", data.capture = 75)
  # }
  
  
  
  # data_col <- data_col %>% dplyr::filter(date>=start_date & date<=end_date)
  
  return(data_col)
}
