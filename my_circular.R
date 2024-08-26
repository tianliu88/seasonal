
my_circular <- function(x, month_s = 1, month_e = 12, year = 2020) {
  if (!requireNamespace("tidyverse", quietly = TRUE)) {
    install.packages("tidyverse")
  }
  if (!requireNamespace("circular", quietly = TRUE)) {
    install.packages("circular")
  }
  library(tidyverse)
  library(circular)
  month_s <- month_s
  month_e <- month_e
  year <- year
  month1 <- seq(month_s, month_e, by = 1)
  dat_var <- data.frame(month = 1:12, 
                        day_n = c(31, ifelse(leap_year(year), 29, 28), 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)) |> 
    filter(month %in% month1) |> 
    mutate(cases = x, 
           cums = cumsum(day_n) - (day_n / 2), 
           d = cums / sum(day_n) * 360)
  
  days_in_year <- sum(dat_var$day_n)
  
  dat_var1 <- map2(dat_var$d, dat_var$cases, ~ rep(.x, .y)) |> as_vector()
  angles <- circular(dat_var1, units = "degrees")
  
  mean_direction <- mean.circular(angles)
  mean_direction <- (mean_direction + 360) %% 360
  r_bar <- rho.circular(angles)
  
  mean_day_of_year <- (as.numeric(mean_direction) / 360) * days_in_year
  
  rayleigh_test <- rayleigh.test(angles)
  rayleigh_test <- append(rayleigh_test, list(z = sum(dat_var$cases) * (r_bar ^ 2)))
  
  ci_angle <- 180 / pi * ((-2 * log(r_bar)) ^ 0.5)
  
  peak_start_angle <- mean_direction - ci_angle
  peak_end_angle <- mean_direction + ci_angle
  peak_start_day <- (as.numeric(peak_start_angle) / 360) * days_in_year
  peak_end_day <- (as.numeric(peak_end_angle) / 360) * days_in_year
  
  origin1 <- ymd(str_c(year, month_s, "01", sep = "-")) - 1
  peak_date <- as.Date(mean_day_of_year, origin = origin1)
  peak_start_date <- as.Date(peak_start_day, origin = origin1)
  peak_end_date <- as.Date(peak_end_day, origin = origin1)
  
  my_result <- list(
    r_bar = r_bar, 
    mean_direction = mean_direction, 
    s = ci_angle, 
    rayleigh_test = rayleigh_test, 
    peak = list(c(peak_date, peak_start_date, peak_end_date)), 
    angles = angles
  )
  return(my_result)
  
}