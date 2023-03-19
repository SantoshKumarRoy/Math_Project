library("ggplot2")

Data_Set = read.csv("C:/Users/USER/Desktop/Math Project/VICTIMS_OF_KA_0.csv")

#retrieving the unique states name in the Data Set
states = unique(Data_Set$STATE.UT)

getData <- function(year){
  total_crimes = c()
  # traversing the data set on the column of states
  for(x in states)
  {
    
    data = subset(Data_Set, Data_Set$STATE.UT == x & Data_Set$YEAR == year & Data_Set$Pupose == "Total")
    # taking the grand Total value of the selected Data Frame for Ploting of the graph
    k = data$Grand.Total
    
    if(length(k) == 0)
    {
      k = 0
    }
    
    k = log1p(k)
    
    total_crimes = c(total_crimes, k)
  }
  return(total_crimes)
  
}

data_2001 = getData(2001)
data_2002 = getData(2002)
data_2003 = getData(2003)
data_2004 = getData(2004)
data_2005 = getData(2005)
data_2006 = getData(2006)
data_2007 = getData(2007)
data_2008 = getData(2008)
data_2009 = getData(2009)
data_2010 = getData(2010)
data_2011 = getData(2011)
data_2012 = getData(2012)


data_frame = data.frame(
  "states" = states,
  "Y2001" = data_2001,
  "Y2002" = data_2002,
  "Y2003" = data_2003,
  "Y2004" = data_2004,
  "Y2005" = data_2005,
  "Y2006" = data_2006,
  "Y2007" = data_2007,
  "Y2008" = data_2008,
  "Y2009" = data_2009,
  "Y2010" = data_2010,
  "Y2011" = data_2011,
  "Y2012" = data_2012
)

data_frame$id = 1:35

years = 2001:2012


label_data = data_frame
number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$id) /number_of_bar
label_data$hjust<-ifelse( angle < -90, 1, 0)
# this for the angle of states labels
label_data$angle<-ifelse(angle < -90,270.082,270.22)
label_data$dist = nchar(data_frame$states)/10

#==========================================================
x = 1:35

# for every year, change this y2012 according to the year

data_frame = data_frame[order(data_frame$Y2012), ]

p = ggplot(data_frame, aes(x = x, y = Y2012)) + 
  geom_bar(stat="identity", colour = data_frame$Y2012) +
  ylim(-3, 20)+
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1, 5),"cm")     # This remove unnecessary margin around plot
  ) +
  geom_text(data = label_data, aes(x=id, y=  12 - dist, label=data_frame$states)
            , color="black", fontface="bold",alpha=0.6, show.legend = TRUE,
            size=4, angle= label_data$angle, inherit.aes = FALSE )+
  geom_text(aes(x = 20, y = -1, label = "2012"), color="red", fontface="bold",alpha=0.6, 
            size=5)
#printing bar
p
