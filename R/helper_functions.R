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
