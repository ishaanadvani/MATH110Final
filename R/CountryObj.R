#CountryObj.R

#' Country object constructor
#' 
#' @param leagueentry A specific league/country
#' @param dat Raw data
#' @details Creates objects based on the specific league/country entered
#' @return List with traits of players of a specific league

library(dplyr)

CountryObj <- function(leagueentry, dat)
{
  
  dat <- dplyr::filter(dat, grepl(leagueentry, league))
  
  out <- list(league = leagueentry, dat=dat)
  
  return (out)
}

get_data.CountryObj <- function(CountObj)
{
  return(CountObj$dat)
}

get_league.CountryObj <- function(CountObj)
{
  x <- c(CountObj$league)
  return (x)
}

get_salary.CountryObj <- function(CountObj)
{
  data <- get_data.CountryObj(CountObj)
  return(data$eur_wage)
}

get_pass.CountryObj <- function(CountObj)
{
  data <- get_data.CountryObj(CountObj)
  return(data$pas)
}

get_pac.CountryObj <- function(CountObj)
{
  data <- get_data.CountryObj(CountObj)
  return(data$pac)
}

get_phy.CountryObj <- function(CountObj)
{
  data <- get_data.CountryObj(CountObj)
  return(data$phy)
}

get_overall.CountryObj <- function(CountObj)
{
  data <- get_data.CountryObj(CountObj)
  return(data$overall)
}

get_skill.CountryObj <- function(CountryObj, trait)
{
  if (trait=="Passing")
  {
    traitinfo <- get_pass.CountryObj(CountryObj)
    return (traitinfo)
  }
  
  else if (trait=="Pace")
  {
    traitinfo <- get_pac.CountryObj(CountryObj)
    return (traitinfo)
  }
  
  else if (trait=="Physical")
  {
    traitinfo <- get_phy.CountryObj(CountryObj)
    return (traitinfo)
  }
  
  else
  {
    print("Error: The trait you've entered is invalid")
  }
}