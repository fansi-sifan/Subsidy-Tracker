
library('dplyr')

factor2num <- function(x)as.numeric(gsub(",", "", gsub("[$]", "", x)))
