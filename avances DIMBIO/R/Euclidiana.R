#' Calcula distancia Mahalabonis
#'
#' @param Reales dataFrame datos Reles
#' @param Simulados dataFrame datos Simulados
#' @return Distancia Euclidiana entre \code{Reales} and \code{Simulados}.
#' @export
Euclidiana <- function(Reales, Simulados){
  if((dim(Reales)[1] == dim(Simulados)[1]) && (dim(Reales)[2] == dim(Simulados)[2])){
    euclidiana <- mean(sqrt(colSums((Reales-Simulados)^2)))
    return(euclidiana)
  }else{
    return("Dimesión de los dataFrames diferentes")
  }
}



