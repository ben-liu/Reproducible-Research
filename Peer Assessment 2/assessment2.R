# setwd("C:/Users/Ben/Google Drive/GitHub/Reproducible-Research/Peer Assessment 2/")

# setwd("")

URL<- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2'
download.file(URL,destfile="data.csv")

data<- read.csv(data.csv)
