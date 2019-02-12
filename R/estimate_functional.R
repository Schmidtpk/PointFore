#' Lagging variables for use in estimate functional
#'
#' @param vector vector to be lagged
#' @param lag number of lags
#'
#' @return lagged vector of same length with NAs at beginning
#' @export
#'
#' @examples
#' #lag example vector by one lag
#' lag(c(1,2,3))
#'
#' #lag example vector by two lags
#' lag(c(1,2,3,4),lag=2)
lag <- function(vector, lag=1)
{
  if(lag<0)
    stop("Lag needs to be non-negative")

  return(c(rep(NA,lag),vector[1:(length(vector)-lag)]))
}

#' Constant specification model
#'
#' All specification models can be used as parameter in \code{\link{estimate.functional}}.
#' Specification models are used to denote the quantile or expectile level
#' (depending on the identifciation function).
#' The constant specification model returns the parameter theta irrespective of
#' the state variable. If theta is not in the unit interval, the constant specification
#' model returns 0 or 1 (depending on which is closer).
#'
#' @param stateVariable state variable
#' @param theta parameter
#' @param ... ...
#'
#' @return numeric level
#' @export
#'
#' @family specification models
#'
#' @examples
#' # the returned level does not depend on the state variable
#' constant(0,.5)
#' constant(1,.5)
#'
#' # if theta is not in the unit interval, the constant specification model forces it to be so
#' constant(0, 2)
#' constant(0, -1)
constant <- function(stateVariable, theta,...)
{
  return(min(1,max(theta,0)))
}



#' linear logistic specification model
#'
#' All specification models can be used as parameter in \code{\link{estimate.functional}}.
#' Specification models are used to denote the quantile or expectile level
#' (depending on the identifciation function).
#' The linear logistic specification model depends linear on the state variable with a logistic link function.
#'
#'
#' @param stateVariable state variable
#' @param theta parameter
#' @param ... ...
#'
#' @return numeric level
#' @export
#'
#' @family specification models
#'
#' @examples
#' # plot linear logistic specification model with constant quantile/expectile level
#' plot(function(x) logistic_linear(x,theta=c(0,0)), xlim=c(-1,1))
#'
#'
#' # plot linear logistic specification model with state-dependent quantile/expectile level
#' plot(function(x) logistic_linear(x,theta=c(0,5)), xlim=c(-1,1))
logistic_linear <- function(stateVariable, theta,...)
{
  if(length(theta)!=2){stop("Wrong dimension of parameter theta for logistic model")}

  return(boot::inv.logit(stateVariable*theta[2]+theta[1]))
}


#' linear specification model with probit link
#'
#' All specification models can be used as parameter in \code{\link{estimate.functional}}.
#' Specification models are used to denote the quantile or expectile level
#' (depending on the identifciation function).
#' The linear probit specification model depends linear on the state variable with a probit link function.
#'
#' @param stateVariable state variable
#' @param theta parameter
#' @param ... other parameters
#'
#' @return numeric level
#' @export
#'
#' @family specification models
#'
#' @examples
#' # plot linear probit specification model with constant quantile/expectile level
#' plot(function(x) probit_linear(x,theta=c(0,0)), xlim=c(-1,1))
#'
#'
#' # plot linear probit specification model with state-dependent quantile/expectile level
#' plot(function(x) probit_linear(x,theta=c(0,5)), xlim=c(-1,1))
probit_linear <- function(stateVariable, theta,...)
{
  if(length(theta)!=2){stop("Wrong dimension of parameter theta for probit linear model")}

  return(stats::pnorm(stateVariable*theta[2]+theta[1]))
}

#' probit break specification model with probit link
#'
#' All specification models can be used as parameter in \code{\link{estimate.functional}}.
#' Specification models are used to denote the quantile or expectile level
#' (depending on the identifciation function).
#' The probit break specification model depends has a break at zero and a constant level above and below.
#' It applies the probit link function.
#'
#' @param stateVariable state variable
#' @param theta parameter
#' @param ... ...
#'
#' @return numeric level
#' @export
#'
#' @family specification models
#'
#' @examples
#' # plot break probit specification model with constant quantile/expectile level
#' plot(function(x) probit_break(x,theta=c(0,0)), xlim=c(-1,1))
#'
#'
#' # plot linear break specification model with state-dependent quantile/expectile level
#' plot(function(x) probit_break(x,theta=c(0,5)), xlim=c(-1,1))
probit_break <- function(stateVariable, theta,...)
{
  if(length(theta)!=2){stop("Wrong dimension of parameter theta for probit break model")}

  return(stats::pnorm((stateVariable>0)*theta[2]+(stateVariable <= 0)*theta[1]))
}




#' cubic spline specification model with probit link
#'
#' All specification models can be used as parameter in \code{\link{estimate.functional}}.
#' Specification models are used to denote the quantile or expectile level
#' (depending on the identifciation function).
#' This specification model depends through a cubic spline on the state
#' variable and applies a probit link function.
#'
#' @param stateVariable state variable
#' @param theta parameter
#' @param ... ...
#'
#' @return numeric level
#' @export
#'
#' @family specification models
#'
#' @examples
#' # plot example of cubic spline specification model with state-dependent quantile/expectile level
#' plot(function(x) probit_spline3(x,theta=c(0,2,1,-1)), xlim=c(-2,2))
probit_spline3 <- function(stateVariable, theta,...)
{
  if(length(theta)!=4){stop("Wrong dimension of parameter theta for cubic probit model")}

  return(stats::pnorm(stateVariable^3*theta[4]+stateVariable^2*theta[3]+stateVariable*theta[2]+theta[1]))
}




#' quadratic spline specification model with probit link
#'
#' All specification models can be used as parameter in \code{\link{estimate.functional}}.
#' Specification models are used to denote the quantile or expectile level
#' (depending on the identifciation function).
#' This specification model depends through a quadratic spline on the state
#' variable and applies a probit link function.
#'
#' @param stateVariable state variable
#' @param theta parameter
#' @param ... ...
#'
#' @return numeric level
#' @export
#'
#' @family specification models
#'
#' @examples
#' # plot example of quadratic spline specification model with state-dependent quantile/expectile level
#' plot(function(x) probit_spline2(x,theta=c(0,1,-1)), xlim=c(-2,2))
probit_spline2 <- function(stateVariable, theta,...)
{
  if(length(theta)!=3){stop("Wrong dimension of parameter theta for quadratic probit model")}

  return(stats::pnorm(stateVariable^2*theta[3]+stateVariable*theta[2]+theta[1]))
}




#' Estimate Functional
#'
#' Estimates the parameter in a specification model for state-dependent quantile or expectile forecasts.
#' For additional detail see the vignettes of the PointFore package.
#'
#' @param iden.fct identification function. Standard choice is \code{\link{quantiles}}. The alternative is \code{\link{expectiles}}.
#' @param model specification model. See \code{\link{constant}} for the simplest example and further suggestions.
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
#' @return Object of type \code{pointfore}. Use \code{summary} and \code{plot} methods to illustrate results.
#' @export
#'
#' @examples
#' # estimate constant quantile level of GDP forecast
#' res <- estimate.functional(Y=GDP$observation, X=GDP$forecast,
#' model=constant)
#' summary(res)
#' plot(res)
#'
#' # estimate constant quantile level with only the constant as instrument
#' res <- estimate.functional(Y=GDP$observation, X=GDP$forecast,
#' model=constant, instruments="const")
#' summary(res)
#'
#'\dontrun{
#' # estimate constant expectile level
#' res <- estimate.functional(Y=GDP$observation, X=GDP$forecast,
#' model=constant, instruments="const", iden.fct = expectiles)
#' summary(res)
#' plot(res)
#'
#' # estimate state-dependent quantile level with linear probit specification model
#' res <- estimate.functional(Y=GDP$observation, X=GDP$forecast,
#' stateVariable = GDP$forecast, model = probit_linear)
#' summary(res)
#' plot(res)
#' }
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



  # Determines the algorithm used in the GMM estimation (optim for multidimensional, nlminb for one-dimensional parameter space)
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
#' Main alternative to estimating state-dependent quantiles based on the quantile identification function are state-dependent \code{\link{expectiles}}.
#'
#' @param x forecast
#' @param y realization
#' @param stateVariable state variable
#' @param theta model parameter to be estimated
#' @param model model function
#' @param ... ...
#'
#' @export
#'
#' @family identification functions
#'
#' @examples
#' ### estimate expectation of identification function for quantile forecasts
#'
#' set.seed(1)
#' y <- rnorm(1000)
#' x <- qnorm(0.6)
#' # expectation of identification with quantile level 0.6 is zero
#' mean(quantiles(x,y,0,0.6,constant))
#' # expectation of identification function with different quantile level
#' # (0.5 is the median) is not zero
#' mean(quantiles(x,y,0,0.5, constant))
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
#' @family identification functions
#'
#' @export
#' @examples
#' ## Estimate expectile level for constant specification model with estimate.functional
#'
#' res <- estimate.functional(Y=GDP$observation, X=GDP$forecast,
#' model=constant,
#' instruments="const",
#' iden.fct = expectiles)
#' summary(res)
#' plot(res)
expectiles<- function(x,y,stateVariable,theta,model,...)
{
  abs((y<=x)-model(stateVariable=stateVariable, theta=theta))*(x-y)
}




#' Method for object of class pointfore
#'
#' It presents results from the \code{\link{estimate.functional}} estimation as summary does
#' for the \code{lm} or \code{gmm} class objects for example.
#' It also computes the test of overidentifying restrictions.
#'
#' @param object An object of class \code{pointfore}
#'
#' @param ... Other arguments when summary is applied to another class object
#'
#' @return It returns a list with the parameter estimates and their standard deviations,
#' t-stat and p-values. It also returns the J-test and p-value for the null hypothesis that
#' the forecast is generated by the postulated functional with an information set that contains
#' the instruments.
#'
#' @export
#' @examples
#' # estimate.functional generates a pointfore object...
#' res <- estimate.functional(Y=GDP$observation, X=GDP$forecast,
#' model=constant,
#' instruments="const")
#'
#' # ...which can be summarized with the \code{summary} function.
#' summary(res)
summary.pointfore <- function(object,...)
{
  gmm.sum <- summary(object$gmm)

  list(call=object$call,
       coefficients=gmm.sum$coefficients,
       Jtest = gmm.sum$stest)
}


bwNeweyWest1987 <- function(x,...) {
  sandwich::bwNeweyWest(x,lag=nrow(gmm::estfun.gmmFct(x))^(0.2),...)
}


