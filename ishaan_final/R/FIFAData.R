#FIFAData.R

# load the dataset
load_filtered_dataset <- function()
{
  d <- read.csv("raw data/FIFA.csv", header=T, stringsAsFactors = F)
  return (d)
}