## How have emissions from motor vehicle sources changed from 1999-2008 in 
## Baltimore City?

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

## read data into table from txt file and convert date from character to POSIXLT
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
        ## Calculate a rough estimate of how much memory the dataset will require
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

## Subset NEI Data by Baltimore and Motor Vehicle Factors

mobilesources <- grepl('Mobile Sources', SCC$SCC.Level.One)
motorcodes <- SCC[mobilesources,]

baltmotor <- NEI[(NEI$SCC %in% motorcodes$SCC) & NEI$fips=="24510", ]

## Using dplyr to summarize Emissions in Baltimore from Motor Vehicles per Year
baltmotor <- baltmotor %>%
        group_by(year) %>%
        summarise(TotalEmissions = sum(Emissions))

## Prepare chart for printing
title <- "Total Motor Vehicle Emissions in Baltimore from 1999 to 2008"
xlab <- "Years"
ylab <- expression('Total PM'[2.5]*" Emissions")
table <- ggplot(baltmotor, aes(x=year, y=TotalEmissions)) + 
        geom_line(colour = "#014d64", size = 0.75) +
        geom_point(size = 3) +
        theme_economist() + 
        scale_colour_economist() + 
        xlab(xlab) + 
        ylab(ylab) + 
        ggtitle(title)

## Loading the graphics package
png(file="./figures/plot5.png", width=640, height=480)

## Printing Graph
print(table)

## Turning off the graphics package
dev.off()