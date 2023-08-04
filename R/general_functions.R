#' Import data from SQL database
#'
#' Function to connect to a SQL database and import data.
#'
#' @param query a SQL query.
#' @param args list of arguments for dbConnect function.
#'
#' @return get_data() returns a dataframe.
#'
#' @importFrom RSQLite SQLite
#' @importFrom DBI dbConnect dbDisconnect dbGetQuery
#' @export
get_data <- function(query, args){

  db <- do.call(dbConnect,args)
  on.exit(dbDisconnect(db))
  data <- dbGetQuery(db ,query)

  return(data)
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


#' compute_couloir_SQL function
#'
#' Function to compute the performance deciles by age.
#'
#' @param con ?.
#' @param data_raw_name a character stating the name of the dataset.
#' @param var_name_id a character stating the name of the id variable.
#' @param var_name_age a character stating the name of the age variable.
#' @param var_name_perf a character stating the name of the performance variable.
#' @param nb_couloir ?.
#'
#' @return compute_couloir_SQL() returns a dataset with the performance deciles computed by age.
#'
#' @importFrom DBI dbGetQuery
#' @export
compute_couloir_SQL <- function(con, data_raw_name, var_name_id, var_name_age, var_name_perf, nb_couloir){

  ## ---- SQL query ---- ##
  best <- paste0("SELECT MIN(",var_name_perf,") AS perf,",var_name_id,",Round(",var_name_age,") AS age FROM ",data_raw_name,"
                   GROUP BY ID,age")
  query <- paste0("SELECT *, ntile(",nb_couloir,") OVER (PARTITION BY best.age ORDER BY best.perf) as Decile  FROM (",best,") best")
  query2 <- paste0("SELECT best2.age, best2.Decile,min(best2.perf) AS perf FROM
                       ( ",query,") best2
                      GROUP BY Decile,age;")

  q <- dbGetQuery(con, query2)

  return(q)
}

