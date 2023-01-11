#' moyenne d’un vecteur
#' Une fonction pour faire une moyenne en enlevant les valeurs manquantes
#'
#' @param x un vecteur numerique
#'
#' @return la fonction renvoie la moyenne d'un vecteur
#' @import magrittr
#' @importFrom stats na.omit
#' moyenne(c(4,5))
#' @export
estac <- function(x){
  x <- x %>% na.omit()
  sum(x)/length(x)
}
