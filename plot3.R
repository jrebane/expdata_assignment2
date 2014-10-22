## Load required libraries
require(dplyr)
require(ggplot2)
require(ggthemes)

## Loading and preprocessing the data

fURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
zipFileName <- "./data.zip"
summaryFileName <- "./summarySCC_PM25.rds"
codeFileName <- "./Source_Classification_Code.rds"

## If csv file not in working directory, download from repository and extract to working directory
if (!file.exists(summaryFileName)){
        message("Downloading and unpacking zip file")
        zipFile <- download.file(url = fURL,destfile = zipFileName, mode='wb')
        unzip(zipFileName)
} else {message("Data file found in working directory")}

## Reading data into table from txt file and convert date from character to POSIXLT
if (!exists("NEI")){
        ## Calculate a rough estimate of how much memory the dataset will require
        ## in memory before reading into R. Make sure your computer has enough memory
        fileSizeMb <- file.info(summaryFileName)$size*1.0e-6
        memLimit <- memory.limit()
        
        ## Exit with error message if computer does not have enough memory
        if (fileSizeMb > memLimit){
                stop("Insufficient memory to read table - aborting script.")
        } else {
                message("Reading table from summary data file")
                NEI <- readRDS(summaryFileName) 
        }} else { message("Using summary data table from memory")}

if (!exists("SCC")){
        ## Calculating a rough estimate of how much memory the dataset will require
        ## in memory before reading into R. Make sure your computer has enough memory
        fileSizeMb <- file.info(codeFileName)$size*1.0e-6
        memLimit <- memory.limit()
        
        ## Exit with error message if computer does not have enough memory
        if (fileSizeMb > memLimit){
                stop("Insufficient memory to read table - aborting script.")
        } else {
                message("Reading table from code data file")
                SCC <- readRDS(codeFileName) 
        }} else { message("Using code data table from memory")}

## Subsetting Data to Include only Baltimore Values
baltdata <- NEI[NEI$fips == "24510",]

## Using dplyr to summarize emissions by the total for type and year
balttypedata <- baltdata %>%
        group_by(type, year) %>%
        summarise(TotalEmissions = sum(Emissions))

## Setting year as factor variable
balttypedata$year <- as.factor(balttypedata$year)

## Preparing chart for printing
title <- "Total Tons of Emissions in Baltimore, MD by type from 1999 to 2008"
xlab <- "Years"
ylab <- expression('Tons of PM'[2.5]*" Emissions")
table <- ggplot(balttypedata, aes(x=year, y=TotalEmissions, fill=type)) + 
        facet_wrap(~type) +
        geom_bar(stat='identity') +
        geom_smooth(aes(group=1), method="lm", se=FALSE, size=2, colour="black") +
        theme_economist(base_size = 10.5, base_family = "sans", horizontal = TRUE) +
        scale_fill_economist() + 
        xlab(xlab) + 
        ylab(ylab) + 
        ggtitle(title)

## Creating sub-directory for "figures" if it doesn't already eixst
subDir <- "figures"
if (!file.exists(subDir)){
        dir.create(subDir)
} 

## Loading the graphics package
png(file="./figures/plot3.png", width=640, height=640)

## Printing Graph
print(table)

## Turning off the graphics package
dev.off()