#' Calcula distancia Manhattan
#'
#' @param Reales dataFrame datos Reles
#' @param Simulados dataFrame datos Simulados
#' @return Distancia Manhattan entre \code{Reales} and \code{Simulados}.
#' @export
Manhattan <- function(Reales, Simulados){
  if((dim(Reales)[1] == dim(Simulados)[1]) && (dim(Reales)[2] == dim(Simulados)[2])){
    manhattan <- mean(colSums(abs(Reales-Simulados)))
    return(manhattan)
  }else{
    return("Dimensión de los dataFrames diferentes")
  }
}
