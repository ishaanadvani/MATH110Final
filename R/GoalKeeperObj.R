#GoalKeeperObj.R

GoalKeeperObj <- function(min_ovr, max_ovr, min_age, max_age, dat)
{
  dat <- dplyr::filter(dat, dat$overall >= min_ovr,
                       dat$overall <= max_ovr,
                       dat$age >= min_age, dat$age <= max_age, dat$gk > 0)
  
  out <- list(min_ovr=min_ovr, max_ovr=max_ovr,
              min_age=min_age,max_age=max_age,
              dat=dat)
  
  return (out)
}