#' Calcula distancia Mahalabonis
#'
#' @param Reales dataFrame datos Reles
#' @param Simulados dataFrame datos Simulados
#' @return Distancia Mahalanobis entre \code{Reales} and \code{Simulados}.
#' @examples
#' mahalanobis(data.frame(1:4, 4:7), data.frame(5:8, 3:6))

Mahalanobis <- function(Reales, Simulados){

  lon <- as.numeric(length(Reales))
  #promedio datos reales
  #proReales <- colMeans(Reales)
  #proReales

  #promedio datos Simulados
  #proSimulados <- colMeans(Simulados)
  #proSimulados
if(dim(Reales[[1]])==dim(Simulados[[1]])){
  print("hola")
}
  dim(Reales)
  a <- dim(Reales)
  b <- dim(Simulados)

  if(a == b){
    print("hola")
  }

  #########################################################
  #########################################################
  #////////
  #Log de Reales
  #Reales <- log10(Reales)
  #Reales

  #promedio de log de datos Reales
  proReales <- c(colMeans(Reales))
  #proReales

  #Reales - proReales
  restaReales <- sweep(Reales,2,proReales)
  #restaReales


  #promedio de log de datos Simulados
  proSimulados <- c(colMeans(Simulados))
  #proSimulados

  #Reales - proSimulados
  restaSimulados <- sweep(Simulados,2,proSimulados)
  #restaSimulados

  #Experimetal Varianza and Covarianza
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
  #exp_var_cov

  #Simulado Varianza and Covarianza
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
  #sim_var_cov

  #Promedio Experimental Simulado
  pro_exp_sim <- (exp_var_cov + sim_var_cov)/2
  #pro_exp_sim

  #matrix Inversa
  mat_inv <- solve(pro_exp_sim)
  #mat_inv

  #Resta de promedios Experimental, Simulado
  subpro <- matrix(proReales - proSimulados)
  #subpro

  #Primer paso de la multiplicacion
  mahalanobis <- sqrt((t(subpro) %*% mat_inv) %*% subpro)
  return(mahalanobis)


}
