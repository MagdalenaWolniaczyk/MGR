library(CDM)

# 1st function - solution of the test - returns vector 0-1, if kid solved the exercise correctly or not

# ExamineeSkills - vector of skills of a child 
# Qmatrix - whole matrix of skills needed to solve each exercise in the test
# ModelName - string containing the ModelName name
# s - value of slipping parameter
# g - value of guessing parameter

SolveTest <- function(ExamineeSkills, Qmatrix, ModelName, s, g)
{
  GoodAnsProb <- matrix(0,dim(Qmatrix)[1],1)
  
  result <- matrix(0,dim(Qmatrix)[1],1)
  
  if(ModelName == "DINA")
    {
    for(i in 1:nrow(Qmatrix)) {
      Qvector <- Qmatrix[i,]
      eta <- prod(ExamineeSkills^Qvector)
      GoodAnsProb[i] <- (1-s[i])^eta * g[i]^(1-eta)
      result[i] <- rbinom(1, 1, GoodAnsProb[i])
    }
  }
  if(ModelName == "DINO")
    {
    for(i in 1:nrow(Qmatrix)) {
      Qvector <- Qmatrix[i,]
      eta <- 1 - prod((1-ExamineeSkills)^Qvector)
      GoodAnsProb[i] <- (1-s[i])^eta * g[i]^(1-eta)
      result[i] <- rbinom(1, 1, GoodAnsProb[i])
    }
  }
  if(ModelName == "NIDA")
    {
    if((nrow(s) == 1) & (nrow(g) == 1)){
      for(i in 1:nrow(Qmatrix)) {
        Qvector <- Qmatrix[i,]
        GoodAnsProb[i] <- prod(((g^(1-ExamineeSkills))*((1-s)^ExamineeSkills))^Qvector)
        result[i] <- rbinom(1, 1, GoodAnsProb[i])
      }
    }
  }
  
  if(ModelName == "GNIDA")
    {
    for(i in 1:nrow(Qmatrix)) {
      Qvector <- Qmatrix[i,]
      GoodAnsProb[i] <- prod(((g[i,]^(1-ExamineeSkills))*((1-s[i,])^ExamineeSkills))^Qvector)
      result[i] <- rbinom(1, 1, GoodAnsProb[i])
    }
  }
  return(result)
}

# 2nd function - randomly choose a sample of skills of chosen number of kids from all possible combinations
# (it contains solution for 8 skills (chosen because of the fractions), but it's easy to change it) 

ExamineesClass <- function(SkillsClass, NumberOfExaminees)
{
  indexes <- sample(1:dim(SkillsClass)[1], NumberOfExaminees, replace = F)
  return(SkillsClass[indexes,])
}

SkillProfiles <- function(SkillsClass, NumberOfExaminees)
{
  indexes <- sample(1:dim(SkillsClass)[1], NumberOfExaminees, replace = F)
  return(SkillsClass[indexes,])
}

SkillsClass <- function(K){
  return(expand.grid(replicate(K, c(0,1), simplify=FALSE)))
}

SkillsClass <- expand.grid(replicate(K, c(0,1), simplify=FALSE))

# 3rd function - randomly choose q-matrix from all possible combinations of skills needed 
# exercises can have the same skills vector, we choose the number of questions in the test

ChooseQmatrix <- function(SkillsClass, NumberOfItems)
{
  SkillsClass <- SkillsClass[-1,]
  indexes <- sample(1:dim(SkillsClass)[1], NumberOfItems, replace = TRUE)
  result <- SkillsClass[indexes,]
  rownames(result) <- NULL
  return(result)
}

#############################################################################################

# Function for checking how many kids have good classified skills

evaluate_classification <- function(estimation, skillprofile, I)
{
  similarity <- estimation$alpha.est == skillprofile
  ans <- c()
  for (i in 1:I){
    ans <- c(ans,sum(similarity[i,]))
  }
  result <- as.data.frame(table(ans))
  colnames(result) <- c('Correctly_spec', 'Num_of_students')
  #result_percentage <- as.data.frame(table(ans2)/100)
  return(result)
}

evaluate_classification_gdina <- function(estimation, skillprofile, I)
{
  similarity <- IRT.factor.scores(estimation, type = "MLE") == skillprofile
  ans <- c()
  for (i in 1:I){
    ans <- c(ans,sum(similarity[i,]))
  }
  result <- as.data.frame(table(ans))
  colnames(result) <- c('Correctly_spec', 'Num_of_students')
  #result_percentage <- as.data.frame(table(ans2)/100)
  return(result)
}

SolveTest2 <- function(ExamineeSkills, Qmatrix, s, g, ModelName)
{
  GoodAnsProb <- matrix(0,dim(Qmatrix)[1],1)
  result <- matrix(0,dim(Qmatrix)[1],1)
  
  if(ModelName == "DINA"){
    for(i in 1:nrow(Qmatrix)) {
      Qvector <- Qmatrix[i,]
      eta <- prod(ExamineeSkills^Qvector)
      GoodAnsProb[i] <- (1-s[i])^eta * g[i]^(1-eta)
      result[i] <- rbinom(1, 1, GoodAnsProb[i])
    }
  }
  return(result)
}

