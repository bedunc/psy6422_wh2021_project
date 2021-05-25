# This project uses 'Packrat' as a dependency management system
packrat::init
## load packages
require(tidyverse)
require(ggplot2)
require(here)
require(plotly)
require(knitr)


# load data file into environment
my_data <- read.csv(here('data', 'whr2021.csv')) 

#check data frame to verify
tibble(my_data)

#create dataframe with new values of average scores
df1 <- my_data %>%
  group_by(Regional.indicator) %>%
  summarise(avg.happiness.score = round(mean(Ladder.score), 2),
            avg.social.score = round(mean(Social.support), 2),
            avg.healthy.life.expectancy = round(mean(Healthy.life.expectancy), 2),
            avg.freedom = round(mean(Freedom.to.make.life.choices), 2),
            avg.generosity = round(mean(Generosity), 2),
            avg.perceptions.of.corruption = round(mean(Perceptions.of.corruption), 2)) 


#plot bar chart
plot1 <-  ggplot(df1, aes(x = reorder(Regional.indicator, +avg.happiness.score),
                          y = avg.happiness.score,
                          text = paste("Region: ", Regional.indicator,
                                       "<br>Average Happiness Score", avg.happiness.score))) + 
  geom_col(aes(fill = avg.happiness.score)) + #fill the graph according to average happiness score
  ggtitle('Average Happiness Score per Region') + #add title to the graph
  theme(axis.text.x = element_text(angle = 75, hjust = 1, vjust = 1), #change angle of labels on X axis
        legend.title = element_text(size = 9)) + #adjust aesthetics of title of the legend
  xlab('Regions') + #add label to X axis
  ylab('Average Happiness Score') + #add label to Y axis
  scale_fill_gradient(low = '#3f007d', high = '#dadaeb', #add colour gradient from low to high values
                      name = 'Average Happiness Score') #add title to legend

finalplot1 <- ggplotly(plot1, tooltip = 'text')

finalplot1 #print plot

#save plot
ggsave(here('figures', 'avghappyscore_per_region.png'), 
       plot = plot1)


#Visualisation 2
plot2 <- ggplot(my_data, aes(x = Logged.GDP.per.capita, #add x variable
                             y = Ladder.score, #add y variable
                             group = Regional.indicator, #group by Regional Indicator
                             text = paste("Region: ", Regional.indicator, #add text for hover text
                                          "<br>Country: ", Country.name,
                                          "<br>Happiness Score: ", Ladder.score,
                                          "<br>Logged GDP per Capita: ", Logged.GDP.per.capita))) +
  geom_point(aes(colour = Regional.indicator)) + #plots have colour differentiation by regional indicator
  ggtitle("Logged GDP per Capita and Happiness Score per Region") + #add title to the graph
  theme(legend.title = 'Regional Indicator') + #add title to the legend
  xlab('Logged GDP per Capita') + #add x axis label
  ylab('Happiness Score') + #add y axis label
  scale_colour_brewer(palette = "RdYlBu") + #add colour palette from ColourBrewer
  theme_bw() #set theme to white background

finalplot2 <- ggplotly(plot2, tooltip = 'text') #make graph interactive with hover text

finalplot2 #print plot

#save plot
ggsave(here('figures', 'GDP_vs_happyscore_per_region.png'), 
       plot = plot2)
