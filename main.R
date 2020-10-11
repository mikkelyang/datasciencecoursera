#library(swirl)
#install_from_swirl("R Programming")

makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                print("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

r<-c(3, 5, 9, 18, 17)
aVector <- makeVector(r)
print(aVector$get())
# set the cache by running cachemean()
#cachemean(aVector)
# now, access the mean from the cache
#cachemean(aVector)