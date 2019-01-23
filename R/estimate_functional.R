#' Lagging variables for use in estimate functional
#'
#' @param vector vector to be lagged
#' @param lag number of lags
#'
#' @return lagged vector of same length with NAs at beginning
#' @export
#'
#' @examples
#' lag(c(1,2,3))
#' lag(c(1,2,3,4),lag=2)
lag <- function(vector, lag=1)
{
  if(lag<0)
    stop("Lag needs to be non-negative")

  return(c(rep(NA,lag),vector[1:(length(vector)-lag)]))
}

#' Constant specification model
#'
#' @param stateVariable state variable
#' @param theta parameter
#' @param ... ...
#'
#' @return numeric level
#' @export
constant <- function(stateVariable, theta,...)
{
  return(min(1,max(theta,0)))
}



#' logistic-linear specification model
#'
#' @param stateVariable state variable
#' @param theta parameter
#' @param ... ...
#'
#' @return numeric level
#' @export
logistic_linear <- function(stateVariable, theta,...)
{
  if(length(theta)!=2){stop("Wrong dimension of parameter theta for logistic model")}

  return(boot::inv.logit(stateVariable*theta[2]+theta[1]))
}


#' linear specification model with probit link
#'
#' @param stateVariable state variable
#' @param theta parameter
#' @param ... other parameters
#'
#' @return numeric level
#' @export
probit_linear <- function(stateVariable, theta,...)
{
  if(length(theta)!=2){stop("Wrong dimension of parameter theta for probit linear model")}

  return(stats::pnorm(stateVariable*theta[2]+theta[1]))
}

#' break specification model with probit link
#'
#' @param stateVariable state variable
#' @param theta parameter
#' @param ... ...
#'
#' @return numeric level
#' @export
probit_break <- function(stateVariable, theta,...)
{
  if(length(theta)!=2){stop("Wrong dimension of parameter theta for probit break model")}

  return(stats::pnorm((stateVariable>0)*theta[2]+(stateVariable <= 0)*theta[1]))
}




#' cubic spline specification model with probit link
#'
#' @param stateVariable state variable
#' @param theta parameter
#' @param ... ...
#'
#' @return numeric level
#' @export
probit_spline3 <- function(stateVariable, theta,...)
{
  if(length(theta)!=4){stop("Wrong dimension of parameter theta for cubic probit model")}

  return(stats::pnorm(stateVariable^3*theta[4]+stateVariable^2*theta[3]+stateVariable*theta[2]+theta[1]))
}




#' quadratic spline specification model with probit link
#'
#' @param stateVariable state variable
#' @param theta parameter
#' @param ... ...
#'
#' @return numeric level
#' @export
probit_spline2 <- function(stateVariable, theta,...)
{
  if(length(theta)!=3){stop("Wrong dimension of parameter theta for quadratic probit model")}

  return(stats::pnorm(stateVariable^2*theta[3]+stateVariable*theta[2]+theta[1]))
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
#' @param stateVariable state variable(s) as vector or matrix of column vectors.
#' @param instruments instruments (list of character describing instruments or matrix of actual intsruments). Use "const" for just the constant as instrument. Standard ist c("X","lag(Y)"), which uses the constant, the forecast and the lagged value of the outcome.
#' @param other_data optional for construction of instruments
#' @param prewhite logical or integer. Should the estimating functions be prewhitened? Standard is FALSE.
#' If TRUE or greater than 0 a VAR model of order as.integer(prewhite) is fitted. (see ?gmm)
#' @param kernel choose kernel for HAC-covariance estimation (see ?gmm). Standard is "Bartlett" Kernel as proposed in Newey and West (1987).
#' @param bw function describing bandwidth selection (see ?gmm for alternatives). Standard is that the bandwidth depends on the sample length $T$ by $m(T)=T^{1/5}$.
#' @param ... other parameters for gmm function (see ?gmm)
#'
#' @return Object of type pointfore
#' @export
#'
#' @examples
#' estimate.functional(Y=GDP$observation,X=GDP$forecast,
#' instruments=c("X","lag(Y)"))
estimate.functional <- function(iden.fct = quantiles,
                                model = constant,
                                theta0 = NULL,
                                Y, X,
                                stateVariable=NULL,
                                other_data = NULL,
                                instruments = c("X","lag(Y)"),
                                prewhite = F,
                                kernel="Bartlett",
                                bw = bwNeweyWest1987,
                                ...)
{

  #if character, construct as described
  if(class(instruments)=='character')
  {

      #start with constant
      w <- rep(1,length(Y))

      #add further instruments if const is not only value
      if(!(length(instruments)==1 & grepl("const",instruments[1])))
      {

        #define dataframe
        if(is.null(other_data))
          other_data <- data.frame(X=X,Y=Y)
        else
          other_data <- cbind(data.frame(X=X,Y=Y),other_data)

        #construct row for each instrument
        for(inst_cur in instruments)
        {

          if(grepl("Y",inst_cur) & !grepl("lag",inst_cur))
            warning("Y without lags is not a valid instrument as it is not in the information set of the forecaster.")

          #add instrument
          w <- cbind(w,eval(parse(text=inst_cur),other_data))
        }

        #keep only complete cases
        compl <- complete.cases(w)
        message(paste("Drop ", length(Y)-sum(compl), "case(s) because of chosen instruments"))
        w <- w[compl,]
        Y <- Y[compl]
        X <- X[compl]
        stateVariable <- stateVariable[compl]

      }


  } else if(class(instruments)%in% c("vector","matrix"))
    { # use instrument data as given

      w <- instruments
  } else
  {
    stop("instruments has to be of class matrix, vector or character")
  }


  # Construct Identification function
  V <- function(theta,x,y,stateVariable,...)
  {
    return(iden.fct(x=x,y=y,stateVariable=stateVariable, theta=theta,model=model))
  }


  ### Checks

  if(is.matrix(w))
  {
    if(dim(w)[1]!=length(Y) | dim(w)[1]!=length(X)){stop('Wrong dimensions')}
    if(dim(w)[1]<length(theta0)){stop('Not enough moment conditions')}

    #check if matrix invertible
    if(qr(w)$rank!=ncol(w))
      stop("Matrix of instruments does not have full rank. Choice of instruments may be invalid.")

  } else
  {
    if(length(theta0)>1)
      stop("Not enough moment conditions")
  }


  #choose theta0 automatically if not given
  if(is.null(theta0))
  {
    message("Choose parameter theta0 automatically.")


    if(sum(sapply(c(constant), identical, model))>0)
                {theta0 <- 0.5}
    else {if(sum(sapply(c(probit_spline2), identical, model))>0)
                {theta0 <- rep(0,3)}
    else {if(sum(sapply(c(probit_spline3), identical, model))>0)
                {theta0 <- rep(0,4)}
    else {if(sum(sapply(c(probit_linear,logistic_linear), identical, model))>0)
                {theta0 <- c(0,0)} else {stop("Model unknown, specify theta0.")}}}}
  }



  # Determines the algorithm used in the GMM estimation (optim for multidimensional, nlminb for one-dimensional paramter space)
  if (length(theta0)>1){optfct <- 'optim'} else { optfct <- 'nlminb'}

  stateV.cur <- if(is.null(stateVariable)) rep(0,length(X)) else stateVariable

  #safe length of model variable
  if(is.null(dim(stateV.cur)))
    model.dim <- 1
  else
    model.dim<-dim(stateV.cur)[2]

  # Generates function g
  g <- function(theta, m_data,...)
  {
    x <- m_data[,1]
    y <- m_data[,2]

    z <- m_data[,3:(ncol(m_data)-model.dim)]

    stateVariable <- m_data[,(ncol(m_data)-model.dim+1):ncol(m_data)]

    diag(as.vector(V(theta=theta,x=x,y=y,stateVariable=stateVariable,...)))%*%cbind(z)
  }


  matrix_data <-cbind(X, Y, w, stateV.cur)

  #apply gmm
  res <- gmm::gmm(g,
                  x = matrix_data,
                  t0 = theta0,
                  optfct = optfct,
                  prewhite = prewhite,
                  kernel = kernel,
                  bw = bw,
                  ...)


  #safe results
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

#' Identification function for state-dependent quantiles
#'
#' @param x forecast
#' @param y realization
#' @param stateVariable state variable
#' @param theta model parameter to be estimated
#' @param model model function
#' @param ... ...
#'
#' @export
quantiles<- function(x,y,stateVariable,theta,model,...)
{
  (y<=x)-model(stateVariable=stateVariable, theta=theta)
}


#' Identification function for state-dependent expectiles
#'
#' @param x forecast
#' @param y realization
#' @param stateVariable state variable
#' @param theta model parameter to be estimated
#' @param model model function
#' @param ... ...
#'
#' @export
expectiles<- function(x,y,stateVariable,theta,model,...)
{
  abs((y<=x)-model(stateVariable=stateVariable, theta=theta))*(x-y)
}


#' @export
summary.pointfore <- function(object,...)
{
  gmm.sum <- summary(object$gmm)

  list(call=object$call,
       coefficients=gmm.sum$coefficients,
#       vcov=vcov(object$gmm),
       Jtest = gmm.sum$stest)
}

#' Bandwidth as in Newey and West 1987
#'
#' Used in estimate functional to describe bandwidth selection as proposed in Newey and West (1987).
#' Applies bandwidth that includes lags accroding to $m(T)=T^{1/5}$.
#'
#' @param x object
#' @param ... ...
#'
#' @return returns the selected bandwidth parameter
#' @export
bwNeweyWest1987 <- function(x,...) {
  sandwich::bwNeweyWest(x,lag=nrow(gmm::estfun.gmmFct(x))^(0.2),...)
}


