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