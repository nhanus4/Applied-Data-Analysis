#Homework 1
#Simmon's Situation B
#Nichole Hanus
#Due 3/31/15



#Develop a function for the SituationB that asks for significance level as input
SituationB <- function(sig.level){


  
#Set the number of trials for simulation  
Trials <- 15000


#Counters to be used to calculate the P<.01 (represented as 'a'), 
#P<.05 (represented as 'b'), and P<.01 (represented as 'c') percentages
a <- 0


#i represents the counter for the number of Trials
i <- 0

#Start the While loop for the total number of assigned simulations (trials)
while( i < Trials){
  obs = 20
  
  x = rnorm(obs,0,1)
  y = rnorm(obs,0,1)
  
  ttest <- t.test(x,y)
  
  p_value <- ttest$p.value
  
#Begins interations to determine if P-value falls w/in certain categories
  if (p_value < sig.level){a <- a+1}

#If P-value is not significant, tries again w/ 10 more observations             
             else{
               obs <- 10
               
               x <- c(x,rnorm(obs,0,1))
               y <- c(y,rnorm(obs,0,1))
               
               ttest <- t.test(x,y)
               
               p_value <- ttest$p.value
               
               if (p_value < sig.level){a <- a+1}
               
             }
  i = i +1
}

#Calculates the percentages of certain P-values that arose in the simulation
P.value.percentage <- (a/Trials)


return(P.value.percentage)}

p.value.inputs <- c(.1,.05,.01)
False.Positive.Rates <- round(sapply(p.value.inputs,SituationB),3)
DF <- data.frame(P.values = c(".1",".05",".01"), False.Positive.Rates)
DF
