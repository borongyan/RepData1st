Interval2Minutes <- function( interval ){
  interval <- as.integer( interval )
  hours_part <- floor( interval / 100 )
  minutes_part <- interval - 100 * hours_part
  minutes <- hours_part * 60 + minutes_part
minutes
}
