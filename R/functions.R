#' moyenne d’un vecteur
#' Une fonction pour faire une moyenne en enlevant les valeurs manquantes
#'
#' @param x un vecteur numerique
#'
#' @return la fonction renvoie la moyenne d'un vecteur
#' @import magrittr
#' @importFrom stats na.omit
#' @export
estac <- function(x){
  x <- x %>% na.omit()
  sum(x)/length(x)
}


#' moyenne d’un vecteur
#' Une fonction pour faire une moyenne en enlevant les valeurs manquantes
#'
#' @param x un vecteur numerique
#'
#' @return la fonction renvoie la moyenne d'un vecteur
#' @import ggplot2
#' @import stats
#' @export
estap=function(age,performance,FUN,nbpara,niter=10,borne=-Inf, initial=runif(nbpara),plot=FALSE,grid.age=seq(min(age),max(age),0.1)){

  SQ_methode=function(p){
    sum((performance-FUN(age,p))^2)
  }

  res=lapply(c(1:niter),function(x){
    sol=nlminb(start=initial + rnorm(nbpara,0,1),SQ_methode)
    res=nlminb(start = sol$par,SQ_methode,lower=borne)
    return(
      list(
        para=res$par,
        obj=res$objective
      )
    )
  })

  best=which.min(unlist(lapply(res,function(x){x$obj})))
  best_param=res[[best]]$para
  best_obj=res[[best]]$obj

  fit=FUN(grid.age,best_param)
  perf_pic=round(fit[which.min(fit)],2)
  age_pic=grid.age[which.min(fit)]

  perf=data.frame(
    age=age,
    perf=performance
  )
  pred=data.frame(
    age=grid.age,
    pred=fit
  )

  R2=1-(sum((performance-fit)^2)/sum((performance-mean(performance))^2))
  R2a=1-((sum((performance-fit)^2)/(length(performance)-nbpara-1))/(sum((performance-mean(performance))^2)/(length(performance)-1)))

  if(plot==T){
    p=ggplot()+
      geom_point(data=perf,aes(x=age,y=performance))+
      geom_line(data=pred,aes(x=age,y=pred),col="red")+
      theme_minimal()
    print(p)
  }

  result_final=list(parametre=best_param,objectif=best_obj,R2=R2,R2a=R2a,age_pic=age_pic,perf_pic=perf_pic)

  return(result_final)
}
