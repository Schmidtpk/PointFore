#' Plots object of class "pointfore"
#'
#' @param x object of class "pointfore"
#' @param conf.levels one or two confidence levels for pointwise confidence intervals
#' @param pdf logic if pdf estimate should be plotted
#' @param adjust.factor adjust factor for estimating pdf (controls smoothness)
#' @param limits 2-dimensional vector defining range of x-axis
#' @param ... other parameters
#'
#' @method plot pointfore
#'
#' @import ggplot2
#' @importFrom stats quantile complete.cases coef
#'
#' @return plot
#' @export
#'
#' @examples
#' res <- estimate.functional(Y=GDP[,1],X=GDP[,2],model=logistic,theta0=c(0,0),stateVariable = GDP[,2])
#' plot(res)
plot.pointfore <- function(x, conf.levels = c(0.6,0.9), pdf=TRUE, adjust.factor=1, limits=NULL,...)
{

  ..scaled.. <- NULL

  # to plot constant (state-independent models)
  if(is.null(x$stateVariable))
  {
    pdf<-FALSE
    x$stateVariable<-c(-1,0,1)
  }

  if(!is.vector(x$stateVariable))
    stop("Can only plot one-dimensional states")


  # safe coefficients
  theta <- c(coef(x$gmm))
  var_theta <- x$gmm$vcov

  # define function for level
  alpha <- function(y,theta) x$model(stateVariable = y, theta = theta)

  if(is.null(limits)){
    interval_state <- seq(quantile(x$stateVariable, probs = 0.01),quantile(x$stateVariable, probs = 0.99), length.out=20)
    limits <- interval_state[c(1,length(interval_state))]
  } else {
    if(length(limits)!=2) {stop('Limits not well-defined')}
    interval_state <- seq(limits[1],limits[2], length.out=20)
  }



  theta_random <- MASS::mvrnorm(1000,theta,var_theta)



  alpha_int <- numeric(length(interval_state))
  alpha_low <- numeric(length(interval_state))
  alpha_high <- numeric(length(interval_state))
  alpha_low2 <- numeric(length(interval_state))
  alpha_high2 <- numeric(length(interval_state))


  for ( i in 1:length(interval_state))
  {
    emp.distr <- apply(theta_random, 1,function(theta) x$model(interval_state[i],theta))

    alpha_int[i] <- mean(emp.distr)
    alpha_low[i] <- quantile(emp.distr,probs = (1-conf.levels[1])/2)
    alpha_high[i] <- quantile(emp.distr,probs = 1-(1-conf.levels[1])/2)
    alpha_low2[i] <- quantile(emp.distr,probs = (1-conf.levels[2])/2)
    alpha_high2[i] <- quantile(emp.distr,probs = 1-(1-conf.levels[2])/2)
  }


  ######## Create plot of quantile levels

  plot_data <- data.frame(cbind(interval_state,alpha_int, alpha_low,alpha_high, alpha_low2,alpha_high2))


  p.quantile <- ggplot()+
    geom_line(data=plot_data, aes(x=interval_state, y=alpha_int), size=1.2)+
    geom_ribbon(data=plot_data, aes(x=interval_state,ymin=alpha_low,ymax=alpha_high), alpha=0.2)+
    geom_ribbon(data=plot_data, aes(x=interval_state,ymin=alpha_low2,ymax=alpha_high2), alpha=0.4)


  if(!is.null(pdf))
    if(pdf==TRUE)
      p.quantile <- p.quantile +
    geom_density(data = data.frame(x$stateVariable),
                 aes(x=x$stateVariable,y=..scaled..),
                 adjust=adjust.factor,
                 fill="green",
                 alpha=.2)+
        coord_cartesian(xlim=limits)

  p.quantile <- p.quantile +scale_y_continuous("forecasted level", limits=c(0,1))+
    theme_classic()+  xlab("state variable")

  p.quantile
}

