# Title     : Complete
# Objective : Check for complete values
# Created by: mikkelyang
# Created on: 11/10/2020

complete <- function (directory, id=1:332){
    id_loop <- formatC(id, width = 3, format = "d", flag="0")
    nobs <- vector()
    for (i in id_loop){
        df <- read.csv(paste0(directory, "/", i, ".csv"))
        df$count <- with(df, ifelse(!is.na(nitrate) & !is.na(sulfate), 1, 0))
        nobs <- c(nobs, sum(df['count']))
    }
    df <- data.frame(id, nobs)
    return (df)
}

corr <- function (directory, threshold=0){
    df <- complete(directory)
    ids <- df['id'][df['nobs']>threshold]
    ids <- formatC(ids, width = 3, format = "d", flag="0")
    if (ids != "") {
        results <- vector()
        for (i in ids) {
            df <- read.csv(paste0(directory, "/", i, ".csv"))
            correlation <- cor(df['sulfate'], df['nitrate'], use = "complete.obs")
            results <- c(results, correlation)
        }
        return(results)
    } else{
        return (0)
    }
}


cr <- corr("specdata", 2000)
n <- length(cr)
cr <- corr("specdata", 1000)
cr <- sort(cr)
print(c(n, round(cr, 4)))