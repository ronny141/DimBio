Univariantes <- function(Reales, Simulados){
  lon <- as.numeric(length(Reales[[1]]))

  ##############Coeficiente de correlacion lineal R^2#########################

  rcuadrado <- (cor(Reales$datos.CBM, Simulados$datos.CBMs))^2

  ##############Coeficiente de correlacion de pearson#########################

  cor_pearson <- cor(Reales$datos.CBM, Simulados$datos.CBMs)

  ##############% RSME ########################################################

  rsme <- sqrt(sum((Reales$datos.CBM-Simulados$datos.CBMs)^2)/lon)*100/mean(Reales$datos.CBM)

  ############################ NS #############################################

  ns <- 1 - (sum((Reales$datos.CBM-Simulados$datos.CBMs)^2)/sum((Reales$datos.CBM-mean(Reales$datos.CBM))^2))

  ############################ U #############################################

  U <- sqrt(sum((Reales$datos.CBM-Simulados$datos.CBMs)^2)/lon) / (sqrt(sum((Reales$datos.CBM)^2)/lon) + sqrt(sum((Simulados$datos.CBMs)^2)/lon))

  ############################ MV #############################################

  MV <- (lon / 2) * (log1p(sum((Reales$datos.CBM-Simulados$datos.CBMs)^2)/lon))

  ############################ AIC #############################################

  AIC <- (-2 * MV)+2

  ############################ Bayes  al final#############################################

  #bayes <- mv / 2015#

  ############################ MC  #############################################

  MC <- ((mean(Reales$datos.CBM)-mean(Simulados$datos.CBMs))^2)/(sum((Reales$datos.CBM-Simulados$datos.CBMs)^2)/lon)

  ############################ SC  #############################################

  SC <- ((sd(Simulados$datos.CBMs)-(cor_pearson*sd(Reales$datos.CBM)))^2)/(sum((Reales$datos.CBM-Simulados$datos.CBMs)^2)/lon)

  ############################ RC  #############################################

  RC <- ((1 - (cor_pearson)^2)*((var(Reales$datos.CBM))*((lon-1)/lon)))/(sum((Reales$datos.CBM-Simulados$datos.CBMs)^2)/lon)

  ############################ t student para datos parados  ###################

  d_ <- (sum(Reales$datos.CBM - Simulados$datos.CBMs)/120)
  t_student <- d_/sqrt((sum(((Reales$datos.CBM-Simulados$datos.CBMs)-d_)^2))/119);

  ###################################### test F  ###############################

  #a <- c(0, 5, 10, 15, 20, 25)
  #b <- c(14.6, 24.5, 21.8, 34.5, 35.1, 43)

  #x_ <- mean(a)
  #x_

  #y_ <- mean(b)
  #y_

  # Sxy sum(xy) - n * x_ * y_
  # Sxx
  #Sxy <- sum(a*b) - 6 * x_ * y_
  #Sxy

  #Sxx <- sum(a^2) - 6 * x_^2
  #Sxx

  #b <- Sxy/Sxx
  #b

  #a_x_ <- (a - x_)
  #a_x_
  #yi <- y_ + b*a_x_
  #yi

  #S2yx = (sum((a - yi)^2))/4
  #S2yx

  #fisher <- (6*y_^2 + (2*y_)*(b-1))
  #fisher
  #b

  #lon <- as.numeric(length(Reales[[1]]))

  xi <- Reales$datos.CBM
  yi <- Simulados$datos.CBMs

  x_ <- mean(xi)
  y_ <- mean(yi)

  Sxy <- sum(xi*yi) - lon * x_ * y_

  Sxx <- sum(xi^2) - 6 * x_^2

  b <- Sxy/Sxx
  b

  Yi <- y_ + b*(xi - x_)
  Yi

  S2yx = (sum((xi - Yi)^2))/lon
  S2yx

  (sum((xi*(b-1))^2))
  fisher <- ((lon*(y_^2)) + (2*y_)*(b - 1)*(sum((xi*(b-1))^2))*(sum(xi^2)))/(2*S2yx)
  fisher
  ################################### Funciones Univariantes ###########################


  for (i in 1:dim(logReales)[2]) {
    if(i==1){
      ve <- sqrt(sum((Reales[[i]]-Simulados[[i]])^2)/lon)*100/mean(Reales[[i]]) #%RMSE
      ve <- cbind(ve,1 - (sum((Reales[[i]]-Simulados[[i]])^2)/sum((Reales[[i]]-mean(Reales[[i]]))^2)))#NS
      ve <- cbind(ve, sqrt(sum((Reales[[i]]-Simulados[[i]])^2)/lon) / (sqrt(sum((Reales[[i]])^2)/lon) + sqrt(sum((Simulados[[i]])^2)/lon)))#U
      ve <- cbind(ve,(cor(Reales[[i]], Simulados[[i]]))^2);   #r^2
      ve <- cbind(ve, pearson <- cor(Reales[[i]], Simulados[[i]])) #pearson
      ve <- cbind(ve,MV <- (lon / 2) * (log1p(sum((Reales[[i]]-Simulados[[i]])^2)/lon))) # MV
      ve <- cbind(ve, (-2 * MV)+2)#AIC
      ve <- cbind(ve,((mean(Reales[[i]])-mean(Simulados[[i]]))^2)/(sum((Reales[[i]]-Simulados[[i]])^2)/lon))#MC
      ve <- cbind(ve,((sd(Simulados[[i]])-(pearson*sd(Reales[[i]])))^2)/(sum((Reales[[i]]-Simulados[[i]])^2)/lon))#SC
      ve <- cbind(ve,((1 - (pearson)^2)*((var(Reales[[i]]))*((lon-1)/lon)))/(sum((Reales[[i]]-Simulados[[i]])^2)/lon))#RC
      d_ <- (sum(Reales$datos.CBM - Simulados$datos.CBMs)/lon)
      ve <- cbind(ve, d_/sqrt((sum(((Reales$datos.CBM-Simulados$datos.CBMs)-d_)^2))/(lon-1))) # t_student
    }
    else{
      ve <- cbind(ve, sqrt(sum((Reales[[i]]-Simulados[[i]])^2)/lon)*100/mean(Reales[[i]]))#%RMSE
      ve <- cbind(ve,1 - (sum((Reales[[i]]-Simulados[[i]])^2)/sum((Reales[[i]]-mean(Reales[[i]]))^2)))#NS
      ve <- cbind(ve, sqrt(sum((Reales[[i]]-Simulados[[i]])^2)/lon) / (sqrt(sum((Reales[[i]])^2)/lon) + sqrt(sum((Simulados[[i]])^2)/lon)))#U
      ve <- cbind(ve, ((cor(Reales[[i]], Simulados[[i]]))^2))#r^2
      ve <- cbind(ve, pearson <- cor(Reales[[i]], Simulados[[i]])) #pearson
      ve <- cbind(ve,MV <- (lon / 2) * (log1p(sum((Reales[[i]]-Simulados[[i]])^2)/lon))) # MV
      ve <- cbind(ve, (-2 * MV)+2)#AIC
      ve <- cbind(ve,((mean(Reales[[i]])-mean(Simulados[[i]]))^2)/(sum((Reales[[i]]-Simulados[[i]])^2)/lon))#MC
      ve <- cbind(ve,((sd(Simulados[[i]])-(pearson*sd(Reales[[i]])))^2)/(sum((Reales[[i]]-Simulados[[i]])^2)/lon))#SC
      ve <- cbind(ve,((1 - (pearson)^2)*((var(Reales[[i]]))*((lon-1)/lon)))/(sum((Reales[[i]]-Simulados[[i]])^2)/lon))#RC
      d_ <- (sum(Reales[[i]] - Simulados[[i]])/lon)
      ve <- cbind(ve, d_/sqrt((sum(((Reales[[i]]-Simulados[[i]])-d_)^2))/(lon-1))) # t_student

    }

  }

  table_uni <- data.frame(matrix(ve,nrow=4,ncol=11,byrow=T))
  colnames(table_uni ) <- c("%RMSE", "NS", "U","R^2","pearson","MV","AIC","MC","SC","RC","t-student")


  table_uni$bayes <-(table_uni$MV/sum(table_uni$MV))
  return(table_uni)
}

