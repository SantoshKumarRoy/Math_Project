#Line Plot
library(dplyr)
#Reading .csv file
data_set1 = read.csv("C:\\Users\\sumit\\OneDrive\\Desktop\\Maths Report\\VICTIMS_OF_KA_0.csv")
data_set1

#storing usnique states and years from data set in 'states' and 'years'
states = unique(data_set1$STATE.UT)
years = unique(data_set1$YEAR)
years
#finding unique pupose 
pupose = unique(data_set1$Pupose)
# finding the number of males of each age group in each year 2001:2012
getMalesCases <- function(){
  total_crimes = c()
  years = 2001 : 2012
  
  for(x in years){
    data = subset(data_set1, data_set1$YEAR == x & data_set1$Pupose == "Total")
    data = sum(data$Total.Male)
    total_crimes = c(total_crimes, data)
  }
  return(total_crimes)
}

total_cases_males = getMalesCases()
# finding the number of females of each age group in each year 2001:2012
getFemalesCases <- function(){
  total_crimes = c()
  years = 2001 : 2012
  
  for(x in years){
    data = subset(data_set1, data_set1$YEAR == x & data_set1$Pupose == "Total")
    data = sum(data$Total.Female)
    total_crimes = c(total_crimes, data)
  }
  return(total_crimes)
}

total_cases_females = getFemalesCases()

years = c(2001:2012)

#plotting line graph comparing the cases of males to that of females
plot(years, total_cases_males, type = 'o', col = 'blue', las = 2, lwd = 2,
     xlab = "Year", ylab = "Cases", ylim = c(0, 50000), main = "Total cases year by year")
lines(years, total_cases_females, type = 'o', col = 'red', lwd = 2)

legend(x = "topright", legend = c("Males", "Females"), fill = c('blue', 'red'))


