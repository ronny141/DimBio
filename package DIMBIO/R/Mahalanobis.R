#' Calcula distancia Mahalabonis
#'
#' @param Reales dataFrame datos Reles
#' @param Simulados dataFrame datos Simulados
#' @return Distancia Mahalanobis entre \code{Reales} and \code{Simulados}.
#' @export

Mahalanobis <- function(Reales, Simulados){

  if((dim(Reales)[1] == dim(Simulados)[1]) && (dim(Reales)[2] == dim(Simulados)[2])){
    lon <- dim(Reales)[2]

    proReales <- c(colMeans(Reales))

    restaReales <- sweep(Reales,2,proReales)
    proSimulados <- c(colMeans(Simulados))
    restaSimulados <- sweep(Simulados,2,proSimulados)
    for(i in 1:dim(restaReales)[2]){
      for(j in 1:dim(restaReales)[2]){
        if(i == j) {
          if(i == 1 & j == 1){
            aux <- var(restaReales[i])
            ve <- aux
          }else{
            aux <- var(restaReales[i])
            ve <- cbind(ve,aux)
          }
        }else{
          aux <- cov(restaReales[i], restaReales[j])
          ve <- cbind(ve, aux)
        }
      }
    }

    exp_var_cov <- matrix(ve,nrow=lon,byrow=T)
    for(i in 1:dim(restaSimulados)[2]){
      for(j in 1:dim(restaSimulados)[2]){
        if(i == j) {
          if(i == 1 & j == 1){
            aux <- var(restaSimulados[i])
            ve <- aux
          }else{
            aux <- var(restaSimulados[i])
            ve <- cbind(ve,aux)
          }
        }else{
          aux <- cov(restaSimulados[i], restaSimulados[j])
          ve <- cbind(ve, aux)
        }
      }
    }

    sim_var_cov <- matrix(ve,nrow=lon,byrow=T)
    pro_exp_sim <- (exp_var_cov + sim_var_cov)/2
    mat_inv <- solve(pro_exp_sim)
    subpro <- matrix(proReales - proSimulados)
    mahalanobis <- sqrt((t(subpro) %*% mat_inv) %*% subpro)
    return(as.numeric(mahalanobis))
  }else{
    return("Dimesión de los dataFrames diferentes")
  }

}
