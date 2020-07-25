library(readxl)
setwd("~/Documents/Home 2020")
coviduscases <- read_excel("covid_us.xlsx", sheet =1)
library(reshape2)
cases <- melt(coviduscases , id.vars = 0:1)
cases
covidusdeath <- read_excel("covid_us.xlsx", sheet =2)
death <- melt(covidusdeath , id.vars = 0:1)

death <- death$value

covidUS <- cbind(cases, death)
head(covidUS)
names(covidUS) <- c("states","date", "cases", "death")
head(covidUS)
# Instructions: to run, chose state you want red in, and chose dates (dats) yyyymmdd
chosen_state = "Arizona"
dats = c(20200324:20200331, 20200401:20200430, 20200501:20200531, 20200601:20200630, 20200701:20200717)
length_dat = length(dats)
# make sure you didn't make mistakes
sprintf("You chose to plot %s days worth of data", length_dat) 

# skip
#library(readxl)
#covidUS <- read_excel("Documents/Home 2020/Covid_US_all.xlsx")
#library(ggplot2)
# end skip

rows = 1:length(covidUS$death)
covidUS[,"colored"] <- NA

head(covidUS)

for (r in rows){
  if (covidUS$states[r] == chosen_state){
    covidUS$colored[r] <- "red"
  } else { 
    covidUS$colored[r] <- "gray"
  }
}



head(covidUS,10)

choST <- subset(covidUS, states == chosen_state)
NchoST <- subset(covidUS, states != chosen_state)
covidUS <- rbind(NchoST, choST)

yaxis <- max(death)
xaxis <- max(cases$value)

max_xaxis <- ceiling(max(cases$value)/100)*100
max_yaxis <- ceiling(max(death)*100)/100
#max_xaxis <- 1000
#max_yaxis <- 2200

library(readxl)
#covidUS <- read_excel("Documents/Home 2020/Covid_US_all.xlsx")
library(ggplot2)

for (dat in dats) {
  today_data <- subset(x = covidUS, subset = date == dat)
  mypath <- file.path("~","Documents","Home 2020",paste("US_", chosen_state, "_", dat, ".jpg", sep = ""))
    p <- ggplot(today_data, aes(x=cases, y=death, color=colored)) + geom_point(size=3)
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
