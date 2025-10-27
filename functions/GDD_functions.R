#Growing degree day
gdd_model <- function(tmean, tmax, t_ref, gdd_crit){
  
  gdd <- cumsum(
    ifelse(
      tmean >= t_ref, tmean - t_ref, #t_mean > ref contribution
      ifelse(tmax >= t_ref, (tmax - t_ref)/2, #t_max > ref but t_mean < ref contribution
             0) #t_max < ref contribution
    )
  )
  
  #Select day of leave out 
  doy <- unlist(which(gdd >= gdd_crit)[1]) 
  
  return(doy)
}

#-------------------------------------------------------------------------------

#MAE LOESS function
# run model and compare to true values
# returns the RMSE
mae_gdd <- function(par, data) {
  
  df_mod <- data$model
  df_val <- data$validation
  
  # calculate phenology predictions
  # and put in a data frame
  predict <- df_mod |>
    group_by(year) |>
    summarise(
      prediction = gdd_model(
        tmean = tmean,
        tmax = tmax,
        t_ref = par[1],
        gdd_crit = par[2]
      )
    )
  
  predict <- na.omit(predict)
  mae <- mean(abs(predict$prediction - df_val$doy), na.rm = T)
  
  # return rmse value
  return(mae)
}