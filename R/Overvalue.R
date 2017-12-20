#Overvalue.R

#' Valuable Traits function
#' 
#' @param GObj An object of a group of players with specific attributes
#' @details Calculates the premium ratio (Amount a club would need to pay for the player to the maximum rating a player can achieve)
#' @return Players who are valued at a premium
#' 

Salary_Ovr <- function(GObj)
{
  data <- get_data.Group(GObj)
  release <- get_release.Group(GObj)
  potential <- get_potential.Group(GObj)
  pot_clause_vector <- rep(NA, length(release))

  for (i in 1:length(release))
  {
    pot_clause_vector[i] <- release[i]/potential[i]
  }
  
  pot_clause_vector_na <- pot_clause_vector[!is.na(pot_clause_vector)]

  first_df <- cbind(data, pot_clause_vector)

  sum=0
  for (i in 1:length(pot_clause_vector_na))
  {
    cur_ratio <- pot_clause_vector_na[i]
    sum = pot_clause_vector_na[i]+sum
  }
  
  print(length(pot_clause_vector))
  avg_ratio <- sum/length(pot_clause_vector_na)

  overvalue_df <- dplyr::filter(first_df, pot_clause_vector > avg_ratio)
  return(overvalue_df)
  
}

get_ratio.Salary_Ovr <- function(OvrObj)
{
  return(OvrObj$pot_clause_vector)
}

get_ovr.Salary_Ovr <- function(OvrObj)
{
  return(OvrObj$overall)
}

get_pass.Salary_Ovr <- function(OvrObj)
{
  return(OvrObj$pas)
}

get_pac.Salary_Ovr <- function(OvrObj)
{
  return(OvrObj$pac)
}

get_salary.Salary.Ovr <- function(OvrObj)
{
  return(OvrObj$eur_wage)
}

get_phy.Salary_Ovr <- function(OvrObj)
{
  return(OvrObj$phy)
}

get_league.Salary_Ovr <- function(OvrObj)
{
  list <- OvrObj$league
  return(unique(list))
}

trait.Salary_Ovr <- function(OvrObj, trait)
{
  if (trait=="Passing")
  {
    trait <- get_pass.Salary_Ovr(OvrObj)
    return (trait)
  }
  
  else if (trait=="Pace")
  {
    trait <- get_pac.Salary_Ovr(OvrObj)
    return (trait)
  }
  
  else if (trait=="Physical")
  {
    trait <- get_phy.Salary_Ovr(OvrObj)
    return (trait)
  }
  
  else
  {
    print("Error: The trait you've entered is invalid")
  }
  
}

reg.Salary_Ovr <- function(OvrObj, trait)
{
  ratio <- get_ratio.Salary_Ovr(OvrObj)
  
  if (trait=="Passing")
  {
    t <- trait.Salary_Ovr(OvrObj, trait)
    reg <- lm(ratio ~ t)
    return (reg)
  }
  
  else if (trait=="Pace")
  {
    t <- trait.Salary_Ovr(OvrObj, trait)
    reg <- lm(ratio ~ t)
    return (reg)
  }
  
  else if (trait=="Physical")
  {
    t <- trait.Salary_Ovr(OvrObj, trait)
    reg <- lm(ratio ~ t)
    return (reg)
  }
  
  else
  {
    print("Error: The trait you've entered is invalid")
  }
}

reg2.Salary_Ovr <- function(OvrObj, trait)
{
  salary <- get_ratio.Salary_Ovr(OvrObj)
  
  if (trait=="Passing")
  {
    t <- trait.Salary_Ovr(OvrObj, trait)
    reg <- lm(salary ~ t)
    return (reg)
  }
  
  else if (trait=="Pace")
  {
    t <- trait.Salary_Ovr(OvrObj, trait)
    reg <- lm(salary ~ t)
    return (reg)
  }
  
  else if (trait=="Physical")
  {
    t <- trait.Salary_Ovr(OvrObj, trait)
    reg <- lm(salary ~ t)
    return (reg)
  }
  
  else
  {
    print("Error: The trait you've entered is invalid")
  }
}