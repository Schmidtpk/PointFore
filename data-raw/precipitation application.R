# load data
library(PointFore)
library(ggplot2)
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

#devtools::use_data(precipitation,overwrite = TRUE)



####################################### Paper



instruments <- c("lag(lag(Y))","X")
instrumentsA <- c("X","X^2","lag(lag(Y))")
instruments2 <- c("lag(Y)","X","X^2","X^3","lag(Y-X)^2")

##### Constant expectile
res <- estimate.functional(iden.fct = expectiles, model = constant,
                           instruments = instruments,
                           Y = Y, X=X)
summary(res)
plot(res,hline = TRUE)

#p-value < 10^-8
#coef

inv.logit(0.174)
#0.54-expectile

##### linear on forecast

# Expectile
res <- estimate.functional(iden.fct = expectiles,
                           model = probit_linear,
                           instruments = instruments,
                           state = X,
                           Y = Y, X = X)
summary(res$gmm)
#p-value 0.48
plot(res)




logistic0 <- function(stateVariable,theta) logistic_linear(stateVariable, theta)*(stateVariable>0)


# Expectile alternative model with 0
res <- estimate.functional(iden.fct =   expectiles ,model = logistic0,
                           theta0 = c(0,0),
                           instruments = instruments,
                           state = X,
                           Y = Y, X=X)
summary(res)
#p-value 0.48


probit0 <- function(stateVariable,theta) probit_linear(stateVariable, theta)*(stateVariable>0)

res <- estimate.functional(iden.fct =   expectiles ,
                           model = probit0,
                           theta0 = c(0,0),
                           instruments = instruments,
                           state = X,
                           Y = Y, X=X)
summary(res)
#0.73
plot(res)



threshold <- 0

mean(X<=threshold)
#[1] 0.3667883

plot(res,limits = c(0.001,15),hline = TRUE#,conf.levels = c(0.9,0.99)
     )+
  geom_point(data=data.frame(x=c(0,0),y=c(0,.395),shape=c(1,2)),
             aes(x=x,y=y,shape=as.factor(shape)),
             ,size=3,show.legend = FALSE)+
  scale_shape_manual(values=c(16,1))+
  scale_y_continuous("expectile level")+
  xlab("predicted precipitation")+theme_classic(20)

#ggsave('./data-raw/prec.pdf', height = 5, width=8)


# other information set X_t Y_t-2
res <- estimate.functional(iden.fct =   expectiles ,
                           model = probit0,
                           theta0 = c(0,0),
                           instruments = c("X","lag(lag(Y))"),
                           state = X,
                           Y = Y, X=X)
summary(res)
plot(res)

# other information set X_t Y_t-2
res <- estimate.functional(iden.fct =   expectiles ,
                           model = probit0,
                           theta0 = c(0,0),
                           instruments = c("X","lag(X)"),
                           state = X,
                           Y = Y, X=X)
summary(res)
plot(res)


#### state as lag Y
res <- estimate.functional(iden.fct =   expectiles ,model = probit0,
                           theta0 = c(0,0),
                           instruments = instruments,
                           state = lag(Y),
                           Y = Y, X=X)
summary(res)
#p-value 0.48






### Spline models
res <- estimate.functional(iden.fct =   expectiles ,model = probit_spline3,
                           instruments = instruments2,
                           state = X,
                           Y = Y, X=X)
summary(res)
#p-value 0.11
plot(res)



res <- estimate.functional(iden.fct =   expectiles ,model = probit_spline2,
                           instruments = instruments2,
                           state = X,
                           Y = Y, X=X)
summary(res)
#p-value 0.05
plot(res)



### Time series

## Only 2013

upper.limit <- 20
plot.dat <- data.frame(X=X,Y=Y, Date = as.Date(names(Y), "%d-%m-%Y"))

plot.dat <- subset(plot.dat, year(Date)==2013)


#cut upper limit for plot
if(sum(plot.dat$Y>upper.limit)>0) {warning(paste('In total', sum(plot.dat$Y>upper.limit), 'points above ylim'))}
plot.dat$Y[plot.dat$Y>upper.limit]<- upper.limit
if(sum(plot.dat$X>upper.limit)>0) {warning(paste('In total', sum(plot.dat$X>upper.limit), 'points above ylim'))}
plot.dat$X[plot.dat$X>upper.limit]<- upper.limit



sset.plot <- subset(plot.dat, month(Date)< 7 & year(Date)==2013)

p <- ggplot(sset.plot)+
  geom_line(aes(x=Date,y=Y))+
  geom_point(aes(x=Date,y=X), color = 'red', size = 2, shape=4)+
  ylab('precipitation')+
  xlab('')+
  ylim(0,upper.limit)+
  theme_classic(15)+ coord_fixed(ratio=2)+scale_x_date(labels = date_format("%b %Y"))

p
#ggsave(p, filename = 'prec2013b.pdf', width = 10, height = 3)

