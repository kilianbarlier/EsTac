#' The Moore function
#'
#' Function to compute the Moore equation.
#'
#' @param t a numeric vector.
#' @param p a vector of the four parameters.
#' @param type ("min" by default) indicates whether the function estimates a performance to be maximised ("max") or minimised ("min").
#'
#' @return f_moore() returns the evaluation of the Moore function.
#' @export
#'
#' @examples
#' age <- seq(10,40,0.1)
#' parameters <- c(10,0.2,1,0.3)
#' f_moore(age, parameters, type = "min")
f_moore <- function(t, p, type = "min"){

  if (type == "min"){
    equation <- p[1]*(exp(-p[2]*t)) + p[3]*(exp(p[4]*t))
  } else if (type == "max"){
    equation <- p[1]*(1-exp(-p[2]*t)) + p[3]*(1-exp(p[4]*t))
  }

  return(equation)
}


#' IMAP function
#'
#' Function to compute IMAP.
#'
#' @param t a numeric vector.
#' @param p a vector of the five parameters.
#' @param type  ("min" by default) indicates whether the function estimates a performance to be maximised ("max") or minimised ("min").
#'
#' @return f_imap() returns the evaluation of IMAP.
#' @export
#'
#' @examples
#' age <- seq(10,40,0.1)
#' parameters <- c(10,0.2,1,0.3,80)
#' f_imap(age, parameters, type = "min")
f_imap <- function(t, p, type = "min"){

  if (type == "min"){
    equation <- p[1]*exp((p[2]/p[3]) * (exp(-p[3]*t))) * (exp(p[4]*(t-p[5])))
  } else if (type == "max"){
    equation <- p[1]*exp((p[2]/p[3]) * (1-exp(-p[3]*t))) * (1-exp(p[4]*(t-p[5])))
  }

  return(equation)
}


#' compute_r2 function
#'
#' Function to compute the R2.
#'
#' @param x a numeric vector.
#' @param y a numeric vector.
#' @param para a vector of parameters.
#' @param FUN a function.
#'
#' @return compute_r2() returns the R2.
#' @export
#'
#' @examples
compute_r2 <- function(x, y, para, FUN){

  num <- sum((y - FUN(x,para))^2)
  denom <- sum((y - mean(y))^2)
  r2 <- 1 - (num / denom)

  return(r2)
}


#' compute_r2a function
#'
#' Function to compute the R2 adjusted.
#'
#' @param x a numeric vector.
#' @param y a numeric vector.
#' @param para a vector of the parameters.
#' @param FUN a function.
#'
#' @return compute_r2a() returns the R2 adjusted.
#' @export
#'
#' @examples
compute_r2a <- function(x, y, para, FUN){

  nbpara <- length(para)
  num <- (sum((y - FUN(x,para))^2) / (length(y) - nbpara - 1))
  denom <- (sum((y - mean(y))^2) / (length(y) - 1))
  r2a <- 1 - (num / denom)

  return(r2a)
}


#' plot_init function
#'
#' Function to plot the fit on data of a function.
#'
#' @param data a dataframe containing age performance data.
#' @param FUN a function.
#' @param age a vector containing age data.
#' @param para a vector of the parameters.
#' @param titre the title for the plot.
#'
#' @return plot_init() returns the plot of FUN fitting the data.
#'
#' @importFrom ggplot2 ggplot geom_point aes geom_line labs theme_bw theme element_text
#' @export
plot_init <- function(data, FUN, age, para, titre){

  fit <- data.frame(
    age = age,
    pred = FUN(age, para)
  )

  p <- ggplot(data) + geom_point(aes(x = age, y = perf), shape = 21, fill = "lightblue",
                                 color = "black", size = 2.5)
  p <- p + geom_line(data = fit, aes(x = age, y = pred), size = 2, color = "red") +
    labs(title = titre, x = "Âge", y = "Performance") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))

  return(p)
}


#' Models age performance relationship
#'
#' Function performing the least squares method to estimate the relation between age and performance.
#'
#' @param age a vector containing age data.
#' @param perf a vector containing performance data.
#' @param FUN the function to fit.
#' @param nbpara the number of parameters in FUN.
#' @param niter number of iterations (10 by default).
#' @param borne a vector containing the lower bound of the parameters (-Inf by default).
#' @param initial a vector containing initial values of the parameters (runif(nbpara) by default).
#' @param plot boolean indicating whether a graph of the result should be displayed (FALSE by default).
#' @param grid.age a vector of x.axis values (seq(min(age), max(age), 0.1) by default).
#'
#' @return estap() returns a list including results of the least squares method.
#'
#' @importFrom stats runif rnorm nlminb
#' @importFrom rlang fn_fmls
#' @export
estap <- function(age, perf, FUN, nbpara, niter = 10, borne = -Inf,
                  initial = runif(nbpara), plot = FALSE, grid.age = seq(min(age), max(age), 0.1)){

  SQ_methode <- function(p){
    sum((perf - FUN(age,p))^2)
  }

  res <- lapply(c(1:niter), function(x){
    sol <- nlminb(start = initial + rnorm(nbpara, 0, 1), SQ_methode)
    res <- nlminb(start = sol$par, SQ_methode, lower = borne)
    return(
      list(
        para = res$par,
        obj = res$objective
      )
    )
  })

  best <- which.min(unlist(lapply(res, function(x){x$obj})))
  best_param <- res[[best]]$para
  best_obj <- res[[best]]$obj

  fit <- FUN(grid.age,best_param)
  argument_list <- rlang::fn_fmls(fn = FUN)
  if (!is.null(argument_list$type)){
    type <- argument_list$type
  } else {
    type <- "max"
  }
  perf_pic <- round(fit[get(paste0("which.",type))(fit)],2)
  age_pic <- grid.age[get(paste0("which.",type))(fit)]

  performance <- data.frame(
    age = age,
    perf = perf
  )

  R2 <- compute_r2(age, perf, best_param, FUN)
  R2a <- compute_r2a(age, perf, best_param, FUN)

  if (plot == TRUE){
    p <- plot_init(performance, FUN, grid.age, best_param, "Least squares method")
    print(p)
  }

  final_result <- list(
    parametre = best_param,
    objectif = best_obj,
    R2 = R2,
    R2a = R2a,
    age_pic = age_pic,
    perf_pic = perf_pic
  )

  return(final_result)
}

