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


#' compute_couloir function
#'
#' Function to compute the performance deciles by age.
#'
#' @param data_raw_name a character stating the name of the dataset.
#' @param var_name_id a character stating the name of the id variable.
#' @param var_name_age a character stating the name of the age variable.
#' @param var_name_perf a character stating the name of the performance variable.
#'
#' @return compute_couloir() returns a dataset with the performance deciles computed by age.
#'
#' @importFrom sqldf sqldf
#' @export
#'
#' @examples
#' # compute_couloir("dt", "licence", "age", "temps")
compute_couloir <- function(data_raw_name, var_name_id, var_name_age, var_name_perf){
  ## ---- In case RMySQL library is loaded ---- ##
  options(sqldf.driver = "SQLite")

  ## ---- SQL query ---- ##
  data <- sqldf(paste0(
    "SELECT age_entier, decile, MIN(performance) AS perf_min, MAX(performance) AS perf_max ",
    "FROM (",
    "SELECT *, ntile(10) OVER (PARTITION BY age_entier ORDER BY performance) as decile ",
    "FROM (",
    "SELECT ",var_name_id,", ROUND(",var_name_age,") as age_entier, MIN(",var_name_perf,") as performance ",
    "FROM '",data_raw_name,"' ",
    "GROUP BY ",var_name_id,", ROUND(",var_name_age,")",
    ")",
    ") ",
    "GROUP BY decile, age_entier"
  ))

  return(data)
}

