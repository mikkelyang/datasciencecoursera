# Title     : Pollutant Mean
# Objective : To calculate the pollutant mean of csv files given
# Created by: mikkelyang
# Created on: 11/10/2020

pollutantmean <- function (directory, pollutant, id=1:332) {
    id <- formatC(id, width = 3, format = "d", flag="0")
    data <- vector()
    for (i in id) {
        to_add <- read.csv(paste0(directory, "/", i, ".csv"))[pollutant]
        to_add <- to_add[!is.na(to_add)]
        data <- c(data, to_add)
    }
    mean(data)
}

pollutantmean("specdata", "nitrate")
