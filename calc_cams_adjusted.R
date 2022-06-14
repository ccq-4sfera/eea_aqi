# Function to calculate CAMS adjusted
# Author: Cristina Carnerero
# Date: 24/11/2021

source('./rollingsum_v2.R')

# method = "difference" or "multiplicative"
adj_factor <- function(df,column_name,column_name_mod,N=4,DC=75,today=FALSE,method="difference"){
  # calculate date and time of previous days at the same hour, for each row
  for(i in 1:N){
    if(today){i=i-1}
    df[paste0("date_",i,"daysAgo")] = df$date-ddays(i)
  }
  
  # Using indexed data_table ----
  df2 = data.table(df) #convert dataFrame to dataTable
  df2 = setkey(df2,"date") #set date as index to lookup values afterwards
  
  vals_obs = NULL
  for(i in 1:N){
    # value of that date at the specified column
    val_idaysAgo <- df2[.(df[paste0("date_",i,"daysAgo")]), ..column_name] 
    colnames(val_idaysAgo) <- paste0(column_name,"_",i,"daysAgo")
    vals_obs = bind_cols(vals_obs,val_idaysAgo)
  }
  
  vals_mod = NULL
  for(i in 1:N){
    # value of that date at the specified column
    val_idaysAgo <- df2[.(df[paste0("date_",i,"daysAgo")]), ..column_name_mod] 
    colnames(val_idaysAgo) <- paste0(column_name,"_",i,"daysAgo")
    vals_mod = bind_cols(vals_mod,val_idaysAgo)
  }
  
  if(method=="difference"){
    factor <- vals_obs-vals_mod
  }else if(method=="multiplicative"){
    factor <- vals_obs/vals_mod
  }
  
  factor$avg <- rowMeans(factor,na.rm = TRUE)
  
  # if there were less than K non-NA values to sum, overwrite with NA
  K=round(N*DC/100)
  if(K==0){K=1} #if DC=0%, force K to be 1 instead of 0 to fix dif giving 0 instead of NA if there are no values 
  factor$avg[rowSums(!is.na(factor))<=K] = NA
  
  adj_factor <- factor %>% select(avg)
  
  return(adj_factor)
}


# CAMS_adjusted = model*rollingSum_observations/rollingSum_model
# Arguments: df: contains at least date, observations of pollutants in poll.list and (raw) cams
#            poll.list: list of pollutant names
#            N: number of previous days to consider
#            DC: minimum data coverage (0-100) to calculate the sum, with less DC, rollingsum gives NA
#            today: include today in N previous days?
# Returns df containing date, original observations and cams, and cams_adjusted for all pollutants
calc_cams_adjusted <- function(df,poll.list,cams_name="_cams",N=4,DC=75,today=FALSE){
  cams.list = paste0(poll.list,cams_name)
  df <- df %>% select(c("date",all_of(poll.list),all_of(cams.list)))

  for(pol in poll.list){
  # Multiplicative method (for O3)
    # if(pol=="O3"){
    if(FALSE){
      
      # rollingSum_obs = calc_rollingSum(df,pol,N,DC,today)
      # rollingSum_mod = calc_rollingSum(df,paste0(pol,cams_name),N,DC,today)
      
      mod = t(df[paste0(pol,cams_name)]) # we need to transpose it to multiply later
      
      #FIXME this is wrong!!!
      # CAMS_adjusted = model*rollingSum_observations/rollingSum_model
      # df[paste0(pol,cams_name,"_adj")] = mod*rollingSum_obs/rollingSum_mod
      
      #CAMS_adjusted = model*avg_4prevDays(obs/model)
      avg_ratio <- adj_factor(df,pol,paste0(pol,cams_name),N,DC,today,method="multiplicative")
      df[paste0(pol,cams_name,"_adj")] = mod*avg_ratio
      
  
  # Difference method (for NO2, PM10 and PM2.5)
    # }else if(pol %in% c("NO2","PM10","PM2.5","PM25")){
      }else if(pol %in% c("O3","NO2","PM10","PM2.5","PM25")){
      
      # CAMS_adjusted = model+average_4prevDays(obs-model)
      mod <- t(df[paste0(pol,cams_name)]) # we need to transpose it to multiply later
      
      avg_dif <- adj_factor(df,pol,paste0(pol,cams_name),N,DC,today,method="difference")
      df[paste0(pol,cams_name,"_adj")] = mod+avg_dif
  
  # No correction for SO2    
    }else{ 
      df[paste0(pol,cams_name,"_adj")] = df[pol]
    }
  }
  
  return(df)
}



#backup
# calc_cams_adjusted <- function(df,poll.list,cams_name="_cams",N=4,DC=0,today=FALSE){
#   cams.list = paste0(poll.list,cams_name)
#   obs_cams <- df %>% select(c("date",all_of(poll.list),all_of(cams.list)))
#   
#   for(pol in poll.list){
#     # Multiplicative method
#     rollingSum_obs = calc_rollingSum(obs_cams,pol,N,DC,today)
#     rollingSum_mod = calc_rollingSum(obs_cams,paste0(pol,cams_name),N,DC,today)
#     
#     mod = t(obs_cams[paste0(pol,cams_name)]) # we need to transpose it to multiply later
#     
#     # CAMS_adjusted = model*rollingSum_observations/rollingSum_model
#     obs_cams[paste0(pol,cams_name,"_adj")] = mod*rollingSum_obs/rollingSum_mod
#   }  
#   return(obs_cams)
# }