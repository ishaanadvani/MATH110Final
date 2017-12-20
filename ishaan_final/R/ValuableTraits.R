#ValuableTraits.R

#' Valuable Traits function
#' 
#' @param OvrObj An object with players valued at a premium
#' @param traitlist Major traits possessed by players
#' @details Conducts regression analysis and returns plots of premium ratio to trait level
#' @return List with the required players attributes
#' 

ValuableTraits <- function (OvrObj, traitlist)
{
  for (i in 1:length(traitlist))
  {
    cur_trait <- traitlist[i]
    
    trait <- trait.Salary_Ovr(OvrObj, cur_trait)
    
    string1 <- paste0("Conducting a regression analysis for release to potential ratio and ", cur_trait)
    print(string1)
    
    cur_reg <- reg.Salary_Ovr(OvrObj, cur_trait)
    
    print(cur_reg)
    
    ratio <- get_ratio.Salary_Ovr(OvrObj)
    
    string <- paste0("Creating a plot to show the relationship between overprice ratio and ", cur_trait)
    print(string)
    
    p <- ggplot()
    
    p <- ggplot() + geom_point(mapping = aes(x=trait, y=ratio))
    
    print(p)
  }
  
}