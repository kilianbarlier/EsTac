#' Models age performance relationship
#'
#' Function performing the least squares method to estimate the relation between age and performance.
#'
#' @param age a vector containing age data
#' @param performance a vector containing performance data
#' @param FUN the function to fit
#' @param nbpara the number of parameters in FUN
#' @param niter un vecteur numerique
#' @param borne a vector containing the lower bound of the parameters
#' @param initial a vector containing initial values of the parameters
#' @param plot boolean indicating whether a graph of the result should be displayed0
#' @param grid.age a vector of x.axis values
#'
#' @return estap() returns a list including results of the least squares method
#'
#' @importFrom stats runif rnorm nlminb
#' @importFrom ggplot2 ggplot geom_point aes geom_line theme_minimal
#' @export
estap <- function(age, performance, FUN, nbpara, niter = 10, borne = -Inf,
                  initial = runif(nbpara), plot = FALSE, grid.age = seq(min(age), max(age), 0.1)){

  SQ_methode <- function(p){
    sum((performance - FUN(age,p))^2)
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
  perf_pic <- round(fit[which.min(fit)],2)
  age_pic <- grid.age[which.min(fit)]

  perf <- data.frame(
    age = age,
    perf = performance
  )
  pred <- data.frame(
    age = grid.age,
    pred = fit
  )

  R2 <- 1 - (sum((performance-fit)^2) / sum((performance-mean(performance))^2))
  R2a <- 1 - ((sum((performance-fit)^2) / (length(performance)-nbpara-1)) / (sum((performance-mean(performance))^2) / (length(performance)-1)))

  if (plot == T){
    p <- ggplot() +
      geom_point(data = perf, aes(x = age, y = performance)) +
      geom_line(data = pred, aes(x = age, y = pred), col = "red") +
      theme_minimal()
    print(p)
  }

  result_final <- list(
    parametre = best_param,
    objectif = best_obj,
    R2 = R2,
    R2a = R2a,
    age_pic = age_pic,
    perf_pic = perf_pic
  )

  return(result_final)
}

