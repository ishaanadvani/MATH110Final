#GroupObj.R

#' Group object constructor
#' 
#' @param min_ovr minimum overall for a given position
#' @param max_ovr maximum overall for a given position
#' @param min_age is the minimum age of a player
#' @param max_age is the maximum age of a player
#' @param dat raw data frame
#' @details Subsects data according to given conditions
#' @return List with the required players attributes

GroupObj <- function(min_ovr, max_ovr, min_age, max_age, dat)
{

  #browser()
  
  
    dat <- dplyr::filter(dat, dat$overall >= min_ovr,
                         dat$overall <= max_ovr,
                         dat$age >= min_age, dat$age <= max_age)
  
  out <- list(min_ovr=min_ovr, max_ovr=max_ovr,
              min_age=min_age,max_age=max_age,
              dat=dat)
  
  return (out)
}

get_overalls.Group <- function(group)
{
  x <- c(group$min_ovr, group$max_ovr)
  return (x)
}

get_age.Group <- function(group)
{
  x <- c(group$min_age, group$max_age)
  return (x)
}

get_release.Group <- function(group)
{
  dat <- get_data.Group(group)
  return(dat$eur_release)
}

get_potential.Group <- function(group)
{
  dat <- get_data.Group(group)
  return(dat$potential)
}

get_league.Group <- function(group)
{
  dat <- get_data.Group(group)
  list <- dat$league
  return(unique(list))
}

get_data.Group <- function(group)
{
  return(group$dat)
}