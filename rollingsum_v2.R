# Rolling sum of N previous days at the same hour
# Author: Cristina Carnerero
# Date: 11/11/2021

library(dplyr)
library(lubridate)

# N=10
# DC=75
# column_name="O3"

# Calculate the sum of the values of N previous days at the same hour of the day
# The result is added as an extra column in the original dataframe
# If any of the 4 values are missing the result is NA
# Arguments: df: dataframe containing date and values in a colum
#            column_name: name of column containing values to be summed
#            N: number of previous days to include
#            DC: % of data coverage (% of non-NA values when adding values of N days)
#            today: TRUE to include the same day + (N-1) previoys dats. By default (FALSE), N previous days
# example of use: 
# data_result <- calc_rollingSum4days(data,"O3")
calc_rollingSum <- function(df,column_name,N=4,DC=75,today=FALSE){
  
  # use only date and column of interest (to optimize time and memory of calculation)
  # df = df %>% select("date",column_name)

  # calculate date and time of previous days at the same hour, for each row
  for(i in 1:N){
    if(today){i=i-1}
    df[paste0("date_",i,"daysAgo")] = df$date-ddays(i)
  }
  
  # Using indexed data_table ----
  df2 = data.table(df) #convert dataFrame to dataTable
  df2 = setkey(df2,"date") #set date as index to lookup values afterwards
  
  vals_to_sum = NULL
  for(i in 1:N){
    # value of that date at the specified column
    val_idaysAgo <- df2[.(df[paste0("date_",i,"daysAgo")]), ..column_name] 
    colnames(val_idaysAgo) <- paste0(column_name,"_",i,"daysAgo")
    vals_to_sum = bind_cols(vals_to_sum,val_idaysAgo)
  }
  
  # calculate the sum of all columns
  vals_to_sum$rollingSum <- rowSums(vals_to_sum,na.rm = TRUE)
  
  # if there were less than K non-NA values to sum, overwrite with NA
  K=round(N*DC/100)
  if(K==0){K=1} #if DC=0%, force K to be 1 instead of 0 to fix rowSum giving 0 instead of NA if there are no values 
  vals_to_sum$rollingSum[rowSums(!is.na(vals_to_sum))<=K] = NA
  
  # select only the column with the result
  rollingSum <- vals_to_sum %>% select(rollingSum)
  
  return(rollingSum)
}


