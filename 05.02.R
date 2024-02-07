#install packages
install.packages("plotly")
library(plotly) # load ploty package
install.packages("ggridges") # install ggridges package 
library(ggridges) # load ggridges package

# Importing and Cleaning/Preparing Data ---------------------------------------------
data(state) # Load the state dataset
View(state.x77) # Load a spreadsheet to visualize the dataset 

help(state) # Get more information on the dataset 

StateNew <- data.frame(state.x77) # Transform matrix into data frame 
StateFinal <- cbind(state.abb, StateNew, state.region) # Combine the three data sets

colnames(StateFinal)[1] <- "State" # Rename the 1st column
colnames(StateFinal)[10] <- "Region" # Rename the 10th column

#The grammar of ggplot2
hist1 <- ggplot(StateFinal, aes(x=Murder))+
  geom_histogram(binwidth = 1, fill='pink',color='black', alpha=0.8)+
  theme_minimal() + labs(x="Murder Rate", y="count")+
ggtitle("Murder rates distribution",) #basic colors, themes, labels, sorting groups
ggsave("histogram1.png", plot = hist1) #save graph 

ggplot(StateFinal, aes(x=Murder))+
  geom_density(fill='blue',color='yellow', alpha=0.1)+
  theme_minimal() + labs(x="Murder Rate", y="count")+
  ggtitle("Murder rates distribution",) #basic colors, themes, labels, sorting groups
ggsave("histogram1.png", plot = hist1) #save graph 

help("geom_density")

#exercise population histogram
hist2 <- ggplot(StateFinal, aes(x=Population))+
  geom_histogram (binwidth = 1000, fill="yellow", color="purple")+
  theme_minimal() + labs(x="Population", y="count")+
  ggtitle("Population distribution",)
ggsave("histogram2.png", plot = hist2)

#exercise population density
graph3 <- ggplot(StateFinal, aes(x=Population))+
  geom_density (fill="yellow", color="purple")+
  theme_minimal() + labs(x="Population", y="count")+
  ggtitle("Population distribution",)
ggsave("graph3.png", plot = graph3)

#Two variables (continuous and categorical) bar plot and ridgeline plot
ggplot(StateFinal, aes(x=Region, y=Murder))+
  geom_bar(stat = "identity") #basic showing
scale_fill_brewer(palette = "Set2")+
  theme(legend.position = "none") + theme_minimal() #basic colors

#exercise for population bar chart
barch1 <- ggplot(StateFinal, aes(x=Region, y=Population, fill=StateFinal$Region))+
  geom_bar(stat = "identity", width = 0.8)+ #basic showing
scale_fill_manual(values = c("blue","yellow", "red", "pink"))+
  theme(legend.position = "none") + theme_minimal()+ #basic colors
  labs(x="Population", y="count")+
ggtitle("Population distribution",)
ggsave("barch1.png", plot = barch1)

#Ridgeline Plot
ggplot(StateFinal, aes(x = Murder, y = Region,))+
  geom_density_ridges()+
  theme_ridges()+
  theme(legend.position = "none")

#two continuous variables - scatter plot
ggplot(StateFinal, aes(x=Illiteracy, y=Murder))+
  geom_point()

ggplot(StateFinal, aes(x=Illiteracy, y=Murder))+
  geom_smooth(method=lm, se=FALSE)

scatterplot1 <- ggplot(StateFinal, aes(x=Illiteracy, y=Murder))+
  geom_point()+
  geom_smooth(method=lm, color="pink", fill="blue", se=TRUE, level=.99) + theme_minimal()
ggsave("scatterplot1.pdf", plot = scatterplot1)

#Bubble Scatter Plot
plot1 <- ggplot(StateFinal, aes(x=Illiteracy, y=Murder, size=Population, color=Region))+
  geom_point(alpha=0.4)+
  scale_size(range = c(.1,30), name="Population (M) ")+theme_minimal()
ggsave("plot1.pdf", plot = plot1)

summary(lm(Murder~ Region, data = StateFinal))
summary(lm(Murder~ Illiteracy + Region, data = StateFinal))

ggplotly(plot1)

night_owlish <- "https://raw.githubusercontent.com/batpigandme/night-owlish/master/rstheme/night-owlish.rstheme"
rstudioapi::addTheme(night_owlish, apply = TRUE)
