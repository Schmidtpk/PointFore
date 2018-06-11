

#' Constant specification model
#'
#' @param stateVariable state variable
#' @param theta parameter
#'
#' @return numeric level
#' @export
constant <- function(stateVariable, theta)
{
  return(min(1,max(theta,0)))
}



#' logistic specification model
#'
#' @param stateVariable state variable
#' @param theta parameter
#'
#' @return numeric level
#' @export
logistic <- function(stateVariable, theta)
{
  if(length(theta)!=2){stop("Wrong dimension of parameter theta for logistic model")}
  if(identical(rep(0,length(stateVariable)),stateVariable)){stop("State variable is zero")}

  return(boot::inv.logit(stateVariable*theta[2]+theta[1]))
}

#' Estimate Functional
#'
#' Estimates the parameter in a specification model for state-dependent quantile or expectile forecasts.
#'
#' @param iden.fct identification function. Standard choice is quantiles. Alternative is expectiles.
#' @param model specification model
#' @param theta0 starting value for optimization
#' @param Y realized values
#' @param X forecasts
#' @param stateVariable state variable(s)
#' @param instruments instruments (descriptive string or matrix of actual intsruments)
#' @param ... other parameters for gmm function (see ?gmm)
#'
#' @return Object of type resPF
#' @export
#'
#' @examples
#' estimate.functional(Y=GDP[,1],X=GDP[,2])
#' estimate.functional(Y=GDP[,1],X=GDP[,2],model=logistic,theta0=c(0,0),stateVariable = GDP[,2])
estimate.functional <- function(iden.fct = quantiles,
                                model = constant,
                                theta0 = 0.5,
                                Y, X, stateVariable=NULL,
                                instruments = 'forecast',
                                ...)
{
  ##### helper variables
  extra <- 3
  TT <- length(Y)-extra
  T <- TT+extra
  # Interval
  int  <- (extra+1):T
  int1 <- (extra):(T-1)
  int2 <- (extra-1):(T-2)
  int3 <- (extra-2):(T-3)


  ############ Instruments
  if(class(instruments)=='character')
  {

    if(instruments=="constant")
    {
      w <- cbind(rep(1,TT))

    } else if(instruments=='forecast')
    {

      w <- cbind(rep(1,TT), # constant
                 Y[int1], # lagged observation
                 X[int])  # forecast

    } else if(instruments=='forecastabs')
    {

      w <- cbind(rep(1,TT), # constant
                 Y[int1], # lagged observation
                 X[int],
                 abs(X[int]))  # forecast

    } else if (instruments=='outcome2')##############################
    {

      w <- cbind( rep(1,length(int)),                    #constant
                  Y[int1],
                  Y[int2])


    } else if (instruments=='ff66')##############################
    {

      w <- cbind( rep(1,length(int)),                    #constant
                  X[int],                   #current value of forecast
                  Y[int1]-X[int1],     #lagged value of forecast error
                  (Y[int1]-X[int1])^2 ,      #lagged squared forecast error
                  Y[int2]-X[int2],    # two lags forecast error
                  X[int1],     #lagged value of forecast
                  (Y[int2]-X[int2])^2            #two lags of squared forecast error
      )


    }  else if (instruments=='patton.correct2')##############################
    {

      w <- cbind( rep(1,length(int)),                    #constant
                  X[int],                   #current value of forecast
                  Y[int1]-X[int1],     #lagged value of forecast error
                  Y[int2]-X[int2],      #lagged squared forecast error
                  Y[int1],
                  Y[int2],
                  rep(3, length(int)),
                  rep(4, length(int))# substituted in gmm with lagged V(x,y,theta)
      )


    } else if (instruments=='patton.short')##############################
    {

      w <- cbind( rep(1,length(int)),                    #constant
                  X[int],                   #current value of forecast
                  Y[int1]-X[int1],     #lagged value of forecast error
                  Y[int1],
                  rep(3, length(int))
      )


    } else if (instruments=='lagff66')##############################
    {

      w <- cbind( rep(1,length(int)),                    #constant
                  X[int1],                   #current value of forecast
                  Y[int2]-X[int2],     #lagged value of forecast error
                  (Y[int2]-X[int2])^2 ,      #lagged squared forecast error
                  Y[int3]-X[int3],    # two lags forecast error
                  X[int2],     #lagged value of forecast
                  (Y[int3]-X[int3])^2            #two lags of squared forecast error

      )




    } else {stop('Instrument description unknown')}

    # If instruments are specified via character variables, use interval definition
    Y <- Y[int]
    X <- X[int]


    #if state vector use appropriate subvector
    if(is.null(dim(stateVariable)))
      stateVariable<- stateVariable[(1+length(stateVariable)-length(Y)) :length(stateVariable)]
    else #use appropriate submatrix
      stateVariable<- stateVariable[(1+dim(stateVariable)[1]-length(Y)) :dim(stateVariable)[1],]


  } else
  {
    w <- instruments
  }


  ########### Identification function

  V <- function(theta,x,y,stateVariable)
  {
    return(iden.fct(x=x,y=y,stateVariable=stateVariable, theta=theta,model=model))
  }



  ######### Checks

  if(dim(w)[1]!=length(Y) | dim(w)[1]!=length(X)){stop('Wrong dimensions')}
  if(dim(w)[1]<length(theta0)){stop('Not enough moment conditions')}


  # Determines the algorithm used in the GMM estimation (optim for multidimensional, nlminb for one-dimensional paramter space)
  if (length(theta0)>1){optfct <- 'optim'} else { optfct <- 'nlminb'}

  #safe length of model variable
  if(is.null(dim(stateVariable)))
    model.dim <- 1
  else
    model.dim<-dim(stateVariable)[2]

  # Generates function g
  g <- function(theta, m_data)
  {
    x <- m_data[,1]
    y <- m_data[,2]

    z <- m_data[,3:(ncol(m_data)-model.dim)]

    stateVariable <- m_data[,(ncol(m_data)-model.dim+1):ncol(m_data)]

    diag(as.vector(V(theta=theta,x=x,y=y,stateVariable=stateVariable)))%*%cbind(z)
  }


  if(is.null(stateVariable)){stateVariable <- rep(0,length(X))}

  matrix_data <-cbind(X, Y, w, stateVariable)


  res <- gmm::gmm(g, x=matrix_data,t0=theta0,...)

  return(structure(list(
              gmm = res,
              iden.fct = iden.fct,
              model = model,
              instruments = instruments,
              stateVariable = stateVariable,
              V=V,
              call=match.call()
              ),class="pointfore"))
}


quantiles<- function(x,y,stateVariable,theta,model)
{
  (y<=x)-model(stateVariable=stateVariable, theta=theta)
}

expectiles<- function(x,y,stateVariable,theta,model)
{
  abs((y<=x)-model(stateVariable=stateVariable, theta=theta))*(x-y)
}

summary.pointfore <- function(res,...)
{
  print(res$call)
  summary(res$gmm)
}



