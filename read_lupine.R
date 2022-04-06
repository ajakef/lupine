## R naming conventions are in a sorry state: https://journal.r-project.org/archive/2012-2/RJournal_2012-2_Baaaath.pdf. I'm just going to follow Python convention. 

lupine_read_single = function(filename){
  ## Read a single Lupine data file, including time interpolation.
  ## filename: file name to read, including path
  ## returns a data frame
  nn = numeric()
  data = scan(filename, what = list(uC_sec = nn, acc_x = nn, acc_y = nn, acc_z = nn, gyr_x = nn, gyr_y = nn, gyr_z = nn, temp_degC = nn, batt_voltage = nn, lat = nn, lon = nn, year = nn, month = nn, date = nn, hour = nn, min = nn, sec = nn, HDOP = nn, satellites = nn, fix_uC_sec = nn), skip = 3, sep = ',', flush = TRUE)
  data = interpolate_time(data)
  return(as.data.frame(data))
}


lupine_plot_health = function(data){
  par(mfrow = c(2,1), mar = c(3,3,3,1), mgp = c(1.75, 0.5, 0))
  plot(data$datetime, data$batt_voltage, type = 'l', xlab = 'Time', ylab = 'Battery (V)', main = 'Battery', xaxt = 'n')
  axis.POSIXct(1, data$datetime, format = '%H:%M:%S')
  plot(data$datetime, data$temp_degC, type = 'l', xlab = 'Time', ylab = 'Temp (deg C)', main = 'Temperature', xaxt = 'n')
  axis.POSIXct(1, data$datetime, format = '%H:%M:%S')
}

lupine_plot_intervals = function(data){
  plot(data$datetime[-1], diff(data$datetime))
}

lupine_file_stats = function(filenames){
  nfiles = length(filenames)
  nn = rep(NaN, nfiles)#numeric()
  pp = rep(as.POSIXct(NA), nfiles)
  cc = rep('', nfiles)
  output = list(start_time = pp, end_time = pp, min_batt = nn, max_batt = nn, dt01 = nn, dt99 = nn, length = nn, file = cc)
  output = as.data.frame(output)
  for(i in 1:length(filenames)){
    print(filenames[i])
    data = try(lupine_read_single(filenames[i]))
    if(class(data) == 'try-error'){
      next
    }
    output$start_time[i] = strftime(min(data$datetime), tz = 'GMT')
    output$end_time[i] = strftime(max(data$datetime), tz = 'GMT')
    output$min_batt[i] = min(data$batt_voltage)
    output$max_batt[i] = max(data$batt_voltage)
    output$dt01[i] = quantile(diff(data$uC_sec), 0.01)
    output$dt99[i] = quantile(diff(data$uC_sec), 0.99)
    output$length[i] = nrow(data)
    output$file[i] = filenames[i]
  }
  return(output)
}

################################################
################################################
## helper functions below

find_valid_gps_lines = function(data){
  ## helper function to identify valid gps lines before interpolating non-gps line times
  return( !is.na(data$year) &
          !is.na(data$month) &
          !is.na(data$date) &
          !is.na(data$hour) &
          !is.na(data$min) &
          !is.na(data$sec) &
          !is.na(data$lat) &
          !is.na(data$lon) &
          !is.na(data$HDOP) &
          !is.na(data$satellites) &
	  data$year >= 2021 &
	  data$year < 2030 &
	  data$month > 0 &
	  data$month < 13 &
	  data$date > 0 &
	  data$date < 32 &
	  data$hour >= 0 &
	  data$hour < 24 &
	  data$min >= 0 &
	  data$min < 60 &
	  data$sec >= 0 &
	  data$sec < 61 &
	  data$lat >= -90 &
	  data$lat <= 90 &
	  data$lon >= -180 &
	  data$lon <= 180 &
	  data$satellites >= 3
	  )
}


t_fmt = function(x, fmt){
  ## extract year, hour, etc. from a POSIXct and return it as a number in UTC
  return(as.numeric(strftime(x, fmt, tz = 'GMT')))
}

interpolate_time = function(data){
  ## use a linear fit to estimate time during non-GPS samples
  ## there might be time zone headaches with this

  ## identify valid gps lines
  w = find_valid_gps_lines(data)
  ## verify that there are at least 2 GPS times
  if(sum(w) < 2){
    stop('Need at least 2 GPS times to interpolate')
  }
  data$datetime = as.POSIXct(NA)
  data$datetime[w] = as.POSIXct(paste(data$year, format = data$month, data$date, data$hour, data$min, data$sec)[w], format = '%Y %m %d %H %M %S', tz = 'GMT')

  datetime_num = as.numeric(data$datetime)
  
  reg_line = lm(datetime_num[w] ~ data$uC_sec[w])$coefficients

  datetime_num[!w] = reg_line[1] + reg_line[2] * data$uC_sec[!w]

  data$datetime[!w] = as.POSIXct('1970-01-01', tz = 'GMT') + datetime_num[!w]

  data$year[!w] = t_fmt(data$datetime[!w], '%Y')
  data$month[!w] = t_fmt(data$datetime[!w], '%m')
  data$date[!w] = t_fmt(data$datetime[!w], '%d')
  data$hour[!w] = t_fmt(data$datetime[!w], '%H')
  data$min[!w] = t_fmt(data$datetime[!w], '%M')
  data$sec[!w] = t_fmt(data$datetime[!w], '%S')
  return(data)
}
