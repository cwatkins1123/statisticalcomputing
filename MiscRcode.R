#MISC R CODE (Watkins)#
j <- 1
runpredict <- NA
hitpredict <- NA
outpredict <- NA
sensh <- NA
spech <- NA
sensr <- NA
specr <- NA
senso <- NA
speco <- NA
for(name in datanames){
  data <- get(name)
  data <- data[data$last == 1,]
  cvModel <- train(make.names(as.factor(hit))~ oneback + twoback +threeback+ pitch_number + zone,
                   data = data, method = "glm", family = binomial, 
                   metric = "ROC", 
                   trControl = trainControl(classProbs = TRUE, method = 'cv', 
                                            number = 5, summaryFunction = twoClassSummary))
  cvModel2 <- train(make.names(as.factor(runstf))~ oneback + twoback +threeback + pitch_number + zone,
                    data = data, method = "glm", family = binomial, 
                    metric = "ROC", 
                    trControl = trainControl(classProbs = TRUE, method = 'cv', 
                                             number = 5, summaryFunction = twoClassSummary))
  cvModel3 <- train(make.names(as.factor(out))~ oneback + twoback +threeback+ pitch_number + zone,
                    data = data, method = "glm", family = binomial, 
                    metric = "ROC", 
                    trControl = trainControl(classProbs = TRUE, method = 'cv', 
                                             number = 5, summaryFunction = twoClassSummary))
  
  runpredict[j] <- round(as.numeric(as.character(cvModel$results$ROC)),4)
  sensr[j] <- round(as.numeric(as.character(cvModel$results$Sens)),4)
  specr[j] <- round(as.numeric(as.character(cvModel$results$Spec)),4)
  hitpredict[j] <- round(as.numeric(as.character(cvModel2$results$ROC)),4)
  sensh[j] <- round(as.numeric(as.character(cvModel2$results$Sens)),4)
  spech[j] <- round(as.numeric(as.character(cvModel2$results$Spec)),4)
  outpredict[j] <- round(as.numeric(as.character(cvModel3$results$ROC)),4)
  senso[j] <- round(as.numeric(as.character(cvModel3$results$Sens)),4)
  speco[j] <- round(as.numeric(as.character(cvModel3$results$Spec)),4)
  j <- j+1
  rm(cvModel, cvModel2, cvModel3)
}
rm(j,name)

cvResults2 <- as.data.frame(cbind(datanames, hitpredict, sensh, spech, runpredict,
                                  specr, sensr, outpredict, speco, senso))


rm(data, hitpredict, sensh, spech, runpredict,
   specr, sensr, speco, senso, outpredict)


seq <- function(x){
  n <- nrow(x)
  s <- rep(0,n)
  for(i in 1:n){
    if(x$pitch_number[i] == 1)
    {
      s[i] <- x$ptype[i]
    }
    else{
      p <- x$pitch_number[i]
      j <- i
      sequence <- NA
      while(p > 0){
        sequence <- c(sequence, x$ptype[j])
        j <- j+1
        p <- p-1
      }
      sequence <- sequence[-1]
      s[i] <- toString(sequence)
    }
  }
  return(s)
}



hit <- function(x){
  n <- nrow(x)
  h <- rep(0,n)
  for(i in 1:n){
    if(x$events[i] == "single" | x$events[i] == "double" | x$events[i] == "triple"
       | x$events[i] == "home_run"){
      h[i] <- 1
    }
    else{
      h[i] <- 0
    }
  }
  return(h)
}

walk <- function(x){
  n <- nrow(x)
  w <- rep(0,n)
  for(i in 1:n){
    if(x$events[i] == "walk"){
      w[i] <- 1
    }
    else{
      w[i] <- 0
    }
  }
  return(w)
}

ob <- function(x){
  n <- nrow(x)
  o <- rep(0,n)
  for(i in 1:n){
    if(x$hit[i] + x$walk[i] == 0){
      o[i] <- 0
    }
    else{
      o[i] <- 1
    }
  }
  return(o)
}

out <- function(x){
  n <- nrow(x)
  o <- rep(0,n)
  for(i in 1:n){
    if(x$events[i] == "field_out"|x$events[i]== "strikeout" | 
       x$events[i] == "strikeout_double_play"|
       x$events[i] == "double_play"|x$events[i] == "grounded_into_double_play"|
       x$events[i] == "fielders_choice"|x$events[i] == "fielders_choice_out"|
       x$events[i] == "force_out" | x$events[i] == "sac_bunt"|
       x$events[i] == "sac_bunt_double_play"|x$events[i] == "sac_fly"|
       x$events[i] == "sac_fly_double_play"|x$events[i] == "triple_play"){
      o[i] <- 1
    }
    else{
      o[i] <- 0
    }
  }
  return(o)
}