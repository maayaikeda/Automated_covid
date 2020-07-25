# load library to read excel
library(readxl)
# set directory to spreadsheet
setwd("~/Documents/Home 2020")
# read spread sheet. sheet1 - total cases; sheet2 -- active cases
totalcases <- read_excel("covid_us.xlsx", sheet =1)
library(reshape2)
# reformat so that its a vector
tcases <- melt(totalcases , id.vars = 0:1)
# make sure it looks right
head(tcases)
# do the same for active cases
activecases <- read_excel("covid_us.xlsx", sheet =2)
acases <- melt(activecases , id.vars = 0:1)

# extract values from active cases
acases <- acases$value
# combine it with total cases to create a new table
covidUS <- cbind(tcases, acases)
# make sure it looks right
head(covidUS)
# rename columns
names(covidUS) <- c("states","date", "tcases", "acases")
# cheke to see if it looks right
head(covidUS)
# Instructions: to run, chose state you want red in, and chose dates (dats) yyyymmdd
# choose state 
chosen_state = "Arizona"
# choose dates
dats = c(20200324:20200331, 20200401:20200430, 20200501:20200531, 20200601:20200630, 20200701:20200717)
length_dat = length(dats)
# make sure you didn't make mistakes
sprintf("You chose to plot %s days worth of data", length_dat) 
# if the number of days printed here doesn't look right, go over the dates selected

# count the number of rows
rows = 1:length(covidUS$acases)
# create an empty column named "colored"
covidUS[,"colored"] <- NA
# check to see if it looks right
head(covidUS)
# fill in the "colored" column so that only the "chosen state" is red and rest are labeled gray
for (r in rows){
  if (covidUS$states[r] == chosen_state){
    covidUS$colored[r] <- "red"
  } else { 
    covidUS$colored[r] <- "gray"
  }
}


# check to see if it looks good again
head(covidUS,10)
# This part rearranges the rows so all the rows that are red/chosen state are at the bottom of 
# the data table. This is an easy way to make sure that the red dot is on the top layer when
# plotted 
choST <- subset(covidUS, states == chosen_state)
NchoST <- subset(covidUS, states != chosen_state)
covidUS <- rbind(NchoST, choST)

# Calculate the maximum values and round it up for x axis and y axis
max_xaxis <- ceiling(max(tcases$value)/100)*100
max_yaxis <- ceiling(max(acases)*100)/100

# load ggplot
library(ggplot2)
# Create the plots and save them as jpegs
for (dat in dats) {
  today_data <- subset(x = covidUS, subset = date == dat)
  # change the path to your file
  mypath <- file.path("~","Documents","Home 2020",paste("US_", chosen_state, "_", dat, ".jpg", sep = ""))
    p <- ggplot(today_data, aes(x=tcases, y=acases, color=colored)) + geom_point(size=3)
    p <- p + theme_classic(base_size =10)
    p <- p + scale_color_manual(values=c("gray", "red"), name = "States", labels = c("Other", chosen_state)) 
    p <- p + labs(x = "Cumulative/total rate per 100,000", y = "Active Cases per 100,000")
    p <- p + theme(axis.title.x = element_text(size=16, face="bold"), axis.title.y = element_text(size=16, face="bold"))
    p <- p + scale_y_continuous(limits=c(0, max_yaxis)) + scale_x_continuous(limits=c(0, max_xaxis))
    p <- p +theme(legend.position = c(0.1, 0.9))
    p <- p + theme(legend.text = element_text(face="bold", size=14), legend.title = element_text(face="bold", size=14))
    p <- p + geom_abline(intercept = 0, slope = 1)
    p <- p + annotate(geom="text", x=300, y=750, label="The closer to this line, \n the faster it's spreading",color="red", fontface="bold")
    p <- p+ annotate("segment", x = 1500, y = 250, xend = 2000, yend= 250, arrow = arrow(length = unit(5, "mm"), type="closed"), size = 4)
    p <- p + annotate(geom="text", x=1800, y=350, label="More total cases \n since the outbreak",color="blue", fontface="bold")
    p <- p + ggtitle(dat)
  jpeg(file=mypath)
  print(p)
  dev.off()
}
