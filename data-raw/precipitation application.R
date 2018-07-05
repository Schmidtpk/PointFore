# load data
load("./data-raw/precipitation data.RData")

prec <- prec_London
Temp <- temp_London

N <- dim(prec)[1]

cur.forecast <- 'HRES'

interval.time <- (dim(prec)[1]-N):dim(prec)[1]
Y <- prec[interval.time,"24h", c('Y')]
X <- prec[interval.time,"24h", c(paste0('X_',cur.forecast))]
Temp <- Temp[interval.time, "24h", c(paste0('Z_',cur.forecast))]

precipitation <- data.frame(Y=Y,X=X)

devtools::use_data(precipitation,overwrite = TRUE)
