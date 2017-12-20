#CountryTraits.R


#' 
#' @param CountryObj A Country Object
#' @param traitlist List of most important traits
#' @details Conducts correlation analysis between a skill and the wages and overall rating of a player
#' 
#I will use this function to figure out which traits are valued the most by country

CountryTraits <- function (CountryObj, traitlist)
{
    country_salary <- get_salary.CountryObj(CountryObj)
    country_overall <- get_overall.CountryObj(CountryObj)
    for (j in 1:length(traitlist))
    {
      cur_trait <- traitlist[j]
      cur_skill <- get_skill.CountryObj(CountryObj, cur_trait)
      print(paste0("The correlation between the wage paid and a players ", cur_trait, " is:"))
      print(cor(country_salary, cur_skill))
      print(paste0("The correlation between the overall and ", cur_trait, " is:"))
      print(cor(country_overall, cur_skill))
  }
}