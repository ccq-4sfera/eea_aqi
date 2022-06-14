# Download app station info and map

#args
# station.Code = "AD0942A"
# date_hour = "2021-12-01 09:00"
library(httr)

# TIME is UTC (end)
get_app_data <- function(station.Code,onlyAQI=TRUE){
  # Station level info
  url_station <- paste0("https://dis2datalake.blob.core.windows.net/airquality-derivated/AQI/current/",station.Code,".json")
  
  # api calls
  station.json <- GET(url_station)
  station.content <- content(station.json)
  
  #json to df
  df_station <- as.data.frame(t(t(unlist(station.content))))
  
  # Tidy up dataframe ----
  # rownames are {datetime}.000Z.{variable}
  # separate rows names to extract datetime and variables
  aux <- strsplit(rownames(df_station),split="Z.",fixed = TRUE)
  aux <- t(as.data.frame(aux))
  # rownames(aux) <- NULL
  
  dates <- aux[,1]
  dates <- as.POSIXct(dates,format="%Y-%m-%dT%H:%M:%S",tz="UTC")
  variables <- aux[,2]

  # copy date and variable as columns to original dataframe
  df_station <- df_station %>% mutate(date=dates, variable=variables)
  
  rownames(df_station) <- NULL
  colnames(df_station) <- c("value","date","variable")
  
  # kind of unstack in dplyr...
  df <- df_station %>% pivot_wider(names_from="variable",values_from="value")
  
  # convert aqi to numeric (is factor now)
  df$aqi <- as.numeric(as.character(df$aqi))
  
  colnames(df)[colnames(df)=="StationCode"] <- "station"
  
  if(onlyAQI){
    df <- df %>% select("date",starts_with("aqi_"),"culprit")
    df <- df %>% mutate_if(is.factor,as.character) %>% mutate_if(is.character,as.numeric)
  }

  return(df)
}


#Downloads global AQI for all stations for a given time
#TIME is GMT
get_app_map <- function(date_hour=format(now(),"%Y-%m-%dT%H")){
  # yyyy-mm-ddThh
  date_fmt <- format(as.POSIXct(date_hour), "%Y-%m-%dT%H")
  
  # get json
  url_map <- paste0("https://dis2datalake.blob.core.windows.net/airquality-derivated/AQI/map/",date_fmt,".json")
  
  json <- GET(url_map)
  json_data <- fromJSON(file=url_map)
  
  # convert json to readable data.table
  df <- as.data.table(json_data)
  
  #TODO check/ask what is _cp
  df <- df %>% select(!ends_with("_cp")) #drop rounded AQI
  
  # df contains global AQI (worst of all pollutants) per station
  # df[station.Code] gives a value between 1.3 and 5.5
  
  df <- as.data.frame(t(as.matrix(df)))
  df$StationCode <- rownames(df)
  colnames(df)[1] <- "Value"
  rownames(df) <- NULL
  
  return(df)
}

