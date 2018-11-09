#' Calcula Estadisticos Univariantes
#'
#' @param Reales dataFrame datos Reles
#' @param Simulados dataFrame datos Simulados
#' @param nombre_variable vector de nombres de variables, opcional
#' @return R^2, pearson, %RMSE, t-student, NS, MSEP, RMSEP, MAE, %MAE, U, MC, SC, RC, MV, AIC, bayes entre \code{Reales} and \code{Simulados}.
#' @export
#'
Univariantes <- function(Reales, Simulados, nombre_variable = NULL){
  if((dim(Reales)[1] == dim(Simulados)[1]) && (dim(Reales)[2] == dim(Simulados)[2])){
    if(!is.null(nombre_variable)){
      if(length(nombre_variable) != dim(Reales)[2]){
        return("Tamano de vector de nombre_variable debe ser igual al numero de variables")
      }
    }
    lon <- dim(Reales)[1]
    n_variable <- dim(Reales)[2]

    ################################### Estadisticos Univariantes ###########################

    for (i in 1:n_variable) {
      if(i==1){
        ve <- sqrt(sum((Reales[[i]] - Simulados[[i]])^2)/lon)*100/mean(Reales[[i]]) #%RMSE
        ve <- cbind(ve, 1 - (sum((Reales[[i]] - Simulados[[i]])^2)/sum((Reales[[i]]-mean(Reales[[i]]))^2)))#NS
        ve <- cbind(ve, msep <- (sum((Reales[[i]] - Simulados[[i]])^2))/lon) #MSEP
        ve <- cbind(ve, sqrt(msep)) #RMSEP
        ve <- cbind(ve, (sum(abs(Reales[[i]] - Simulados[[i]])))/lon) # MAE
        ve <- cbind(ve, (sum((abs(Reales[[i]] - Simulados[[i]]))/(abs(Reales[[i]]))))*(100/lon))# %MAE
        ve <- cbind(ve, sqrt(sum((Reales[[i]] - Simulados[[i]])^2)/lon) / (sqrt(sum((Reales[[i]])^2)/lon) + sqrt(sum((Simulados[[i]])^2)/lon)))#U
        ve <- cbind(ve, (cor(Reales[[i]], Simulados[[i]]))^2);   #r^2
        ve <- cbind(ve, pearson <- cor(Reales[[i]], Simulados[[i]])) #pearson
        ve <- cbind(ve, MV <- (lon / 2) * (log1p(sum((Reales[[i]]-Simulados[[i]])^2)/lon))) # MV
        ve <- cbind(ve, (-2 * MV)+2)#AIC
        ve <- cbind(ve,((mean(Reales[[i]]) - mean(Simulados[[i]]))^2)/(sum((Reales[[i]]-Simulados[[i]])^2)/lon))#MC
        ve <- cbind(ve,((sd(Simulados[[i]])-(pearson*sd(Reales[[i]])))^2)/(sum((Reales[[i]]-Simulados[[i]])^2)/lon))#SC
        ve <- cbind(ve,((1 - (pearson)^2)*((var(Reales[[i]]))*((lon-1)/lon)))/(sum((Reales[[i]]-Simulados[[i]])^2)/lon))#RC
        ve <- cbind(ve, (sum(Reales[[i]] - Simulados[[i]])/lon)/((sqrt((sum(((Reales[[i]]-Simulados[[i]])-(sum(Reales[[i]] - Simulados[[i]])/lon))^2))/(lon-1)))/lon)) # t_student
      }
      else{
        ve <- cbind(ve, sqrt(sum((Reales[[i]]-Simulados[[i]])^2)/lon)*100/mean(Reales[[i]]))#%RMSE
        ve <- cbind(ve, 1 - (sum((Reales[[i]]-Simulados[[i]])^2)/sum((Reales[[i]]-mean(Reales[[i]]))^2)))#NS
        ve <- cbind(ve, msep <- (sum((Reales[[i]]-Simulados[[i]])^2))/lon) #MSEP
        ve <- cbind(ve, sqrt(msep)) #RMSEP
        ve <- cbind(ve, (sum(abs(Reales[[i]] - Simulados[[i]])))/lon) # MAE
        ve <- cbind(ve, (sum((abs(Reales[[i]]-Simulados[[i]]))/(abs(Reales[[i]]))))*(100/lon))# %MAE
        ve <- cbind(ve, sqrt(sum((Reales[[i]]-Simulados[[i]])^2)/lon) / (sqrt(sum((Reales[[i]])^2)/lon) + sqrt(sum((Simulados[[i]])^2)/lon)))#U
        ve <- cbind(ve, ((cor(Reales[[i]], Simulados[[i]]))^2))#r^2
        ve <- cbind(ve, pearson <- cor(Reales[[i]], Simulados[[i]])) #pearson
        ve <- cbind(ve, MV <- (lon / 2) * (log1p(sum((Reales[[i]]-Simulados[[i]])^2)/lon))) # MV
        ve <- cbind(ve, (-2 * MV)+2)#AIC
        ve <- cbind(ve, ((mean(Reales[[i]])-mean(Simulados[[i]]))^2)/(sum((Reales[[i]]-Simulados[[i]])^2)/lon))#MC
        ve <- cbind(ve, ((sd(Simulados[[i]])-(pearson*sd(Reales[[i]])))^2)/(sum((Reales[[i]]-Simulados[[i]])^2)/lon))#SC
        ve <- cbind(ve, ((1 - (pearson)^2)*((var(Reales[[i]]))*((lon-1)/lon)))/(sum((Reales[[i]]-Simulados[[i]])^2)/lon))#RC
        ve <- cbind(ve, (sum(Reales[[i]] - Simulados[[i]])/lon)/((sqrt((sum(((Reales[[i]]-Simulados[[i]])-(sum(Reales[[i]] - Simulados[[i]])/lon))^2))/(lon-1)))/lon)) # t_student

      }

    }

    table_uni <- data.frame(matrix(ve,nrow=n_variable,ncol=15,byrow=T))
    colnames(table_uni ) <- c("%RMSE", "NS","MSEP","RMSEP","MAE","%MAE","U","R^2","pearson","MV","AIC","MC","SC","RC","t-student")
    if(!is.null(nombre_variable))
      rownames(table_uni ) <- nombre_variable

    table_uni$bayes <-(table_uni$MV/sum(table_uni$MV))
    return(data.frame(table_uni))
  }else{
    return("Dimesión de los dataFrames diferentes")
  }

}
