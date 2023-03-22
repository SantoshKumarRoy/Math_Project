#Line Plot
library(dplyr)
#Reading .csv file
data_set1 = read.csv("C:\\Users\\sumit\\OneDrive\\Desktop\\Maths Report\\VICTIMS_OF_KA_0.csv")
data_set1

#storing unique states and years from data set in 'states' and 'years'
states = unique(data_set1$STATE.UT)
states
years = unique(data_set1$YEAR)
years

# finding the mean of pupose of the year 2001:2012.
getCases <- function(pupose){
  total_crimes = c()
  years = 2001 : 2012
  
  for(x in years){
    d=0;
    for(y in states)
    {
      data = subset(data_set1, data_set1$YEAR == x & data_set1$STATE.UT==y & data_set1$Pupose==pupose)
      d =d+sum(data$Total.No..of.cases.reported)
      
    }
    
    total_crimes = c(total_crimes, d/35)
  }
  return(total_crimes)
}



total_cases = getCases("For Ransom") #change the reason
total_cases
#plotting graph
plot(years, total_cases, type = 'o', col = '#19A7CE', las = 2, lwd = 2,
     xlab = "Year", ylab = "Mean", ylim = c(0, 50), main = "Yearwise mean of Ransom")#change the name

