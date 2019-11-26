#Pitcher Effectiveness Code
library(caret)
library(ggplot2)
#Analysis
slopecalc2 <- function(x){
  n <- nrow(x)
  j <- 1
  slope <- rep(0,n)
  for(i in 1:n){
    if(j >= n-5|| x[j,]$pitches < x[j+5,]$pitches){
      slope[i] <- NA
    }
    else{
      slope[i] <- round(lm(x$pitchereffectiveness[j:(j+5)]~x$pitches[j:(j+5)])$coefficients[[2]],2)
    }
    j <- j+1
  }
  return(slope)
}
#Run Prediction

runpred2 <- function(x){
  n <- nrow(x)
  j <- 1
  predrun <- rep(0,n)
  for(i in 1:n){
    if(j <=10|| x[j,]$pitches > x[j-10,]$pitches){
      predrun[i] <- NA
    }
    else{
      predrun[i] <- sum(x$runs[(j-1):(j-10)]) > 0
    }
    j <- j+1
  }
  return(predrun)
}

SP2 <- SP[!is.na(SP$predrun),]
SP2 <- SP2[!is.na(SP2$slope),]

cvModel <- train(make.names(as.factor(predrun))~ pitchereffectiveness + slope,
                 data = SP2, method = "glm", family = binomial, 
                 metric = "ROC", 
                 trControl = trainControl(classProbs = TRUE, savePredictions = T, method = 'cv', 
                                          number = 5, summaryFunction = twoClassSummary))

############ Plots #######################
maxgame <- SP[SP$player_name == "Max Scherzer" & SP$game_date == "2018-05-30",]
ggplot(aes(pitches, pitchereffectiveness), data = maxgame) + geom_point() + 
  ggtitle("Max Scherzer 5-30-2018") + theme(plot.title = element_text(hjust=0.5)) +
  xlab("Pitches")+ ylab("Pitcher Effectiveness")

bundygame <- SP[SP$player_name == "Dylan Bundy" & SP$game_date == "2018-05-08",]
ggplot(aes(pitches, pitchereffectiveness), data = bundygame) + geom_point() + 
  ggtitle("Dylan Bundy 5-8-2018") + theme(plot.title = element_text(hjust=0.5)) +
  xlab("Pitches")+ ylab("Pitcher Effectiveness")

ggplot(mergeSP, aes(x = ERAm, y = pitchereffectiveness)) + 
  geom_point()+ labs(title = "Pitcher Effectiveness vs ERA-")+xlab("ERA-" )+
  ylab("Pitcher Effectiveness") + theme(plot.title = element_text(hjust=0.5))+
  geom_text(x = 120, y = 27, label = "Correlation: -0.742", size = 5)

ggplot(mergeSP, aes(x = ERA, y = pitchereffectiveness)) + 
  geom_point()+ labs(title = "Pitcher Effectiveness vs ERA")+xlab("ERA" )+
  ylab("Pitcher Effectiveness") + theme(plot.title = element_text(hjust=0.5))+
  geom_text(x = 5, y = 23, label = "Correlation: -0.733", size = 5)



############# Functions #################
Calculate Pitcher Efficiency score
efficiencyscore <- function(x){
  total <- strikeballscore(x) + eventscore(x) + exitvelo(x)
  return(total)
}

strikeballscore <- function(x){
  count <- 0
  if(x$strikes > x$balls){
    count <- count + 0.5
  }
  else if(x$strikes == x$balls){
    count <- count
  }
  else{
    count <- count - 0.5
  }
  return(count)
}

eventscore <- function(x){
  count <- 0
  if(x$events == "single"){
    count <- count - 0.88
  }
  else if(x$events == "double"){
    count <- count - 1.25
  }
  else if(x$events == "triple"){
    count <- count - 1.58
  }
  else if(x$events == "home_run"){
    count <- count - 2.03
  }
  else if(x$events == "walk"){
    count <- count - 0.69
  }
  else if(x$events == "hit_by_pitch"){
    count <- count - 0.72
  }
  else if(x$events == "strikeout" || x$events == "field_out" || 
          x$events == "grounded_into_double_play" || 
          x$events == "strikeout_double_play" || x$events == "force_out"
          || x$events == "double_play" || x$events == "sac_bunt"
          || x$events == "fielders_choice_out" || x$events == "other_out"
          || x$events == "sac_fly_double_play"){
    count <- count + 0.5
  }
  else if(x$description == "swinging_strike"){
    count <- count + 0.5
  }
  else{
    count <- count 
  }
  return(count)
}

exitvelo <- function(x){
  count <- 0
  if(is.na(x$launch_speed)){
    count <- count
  }
  else if(x$launch_speed >= 95 & x$description != "foul"){
    count <- count - 0.5
  }
  else if(x$launch_speed <= 80 & x$description != "foul"){
    count <- count + 0.5
  }
  else{
    count <- count
  }
  return(count)
}
