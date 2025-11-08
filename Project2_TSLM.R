# PROJECT 2
# Business Forecasting

#IDENTIFY THE MOST IMPORTANT COMPONENT OF THE METALS INDEX


# PRELIMINARIES ======
setwd("C:/Users/giaco/OneDrive/Desktop/UNH/COURSES/Business Forecasting/Week 10")
options(digits = 3, scipen = 9999, stringasFactors = FALSE)
remove(list = ls())
graphics.off()


# =============  #
# HW  ===========
# Which metal is the most important feature in the Metals Index?
# Run both an TSLM and a Boruta Random Forest model of scaled real prices metals data.

# # Data is from 1959 through 2010.

# create barplot of the TSLM model coefficients.
# create a Boruta most important feature barplot
 
# Assemble the two graphs into one Word-cum-pdf page.  
# Include a brief commentary on the results (eg. 1 paragraph)

# label properly and post it to the bucket labeled mostImportant.
# by November 21, 2025
# ===========================  # 

#load libraries
suppressWarnings({
  suppressPackageStartupMessages({
    library(tidyverse)
    library(pdfetch)
    library(lubridate)  
    library(ggplot2)
    library(forecast)
    library(broom)
    library(dplyr)
    library(tidyr)
    library(tsbox)
    library(tsibble)
    library(timetk)
    library(TSstudio)
    library(rio)
    library(readxl)
    library(tidyr)
    library(stringr)
    library(dygraphs)
    library(quantmod)
  })
})

suppressPackageStartupMessages({
  suppressMessages({
    library(tidyverse)
    library(forecast)
    library(rio)
    library(lubridate)
    library(pacman)
    library(tsbox)
    library(TSstudio)
    library(vars)
    library(tseries)
    library(tsutils)
    library(reghelper)
    library(dygraphs)
    library(foreign)                          
    library(wooldridge)
    library(fpp3)
  })
})


#tungsten
tungsten <- import("https://pubs.usgs.gov/sir/2012/5188/tables/tungsten.xlsx", skip = 2)
tungsten[1:2] = NULL
tungsten = na.omit(tungsten)  # to remove the NAs
tungsten.ts <- ts(tungsten, start=c(1959), end=c(2010), frequency=1) #Convert to annual ts
length(tungsten.ts)


#copper
copper <- import("https://pubs.usgs.gov/sir/2012/5188/tables/copper.xlsx", skip = 2)
copper[1:2] = NULL
copper = na.omit(copper)  # to remove the NAs
copper.ts <- ts(copper, start=c(1850), frequency=1) #Convert to annual ts
dim(copper.ts)
ts_plot(copper.ts)


#chromium
chromium <- import("https://pubs.usgs.gov/sir/2012/5188/tables/chromium.xlsx", skip = 2)
chromium[1:2] = NULL
chromium = na.omit(chromium)  # to remove the NAs
chromium.ts <- ts(chromium, start=c(1940), frequency=1) #Convert to annual ts
autoplot(chromium.ts)


#nickel
nickel <- import("https://pubs.usgs.gov/sir/2012/5188/tables/nickel.xlsx", skip = 2)
nickel[1:2] = NULL
nickel = na.omit(nickel)  # to remove the NAs
nickel.ts <- ts(nickel, start=c(1840), frequency=1) #Convert to annual ts
dygraph(nickel.ts)

#tin
tin <- import("https://pubs.usgs.gov/sir/2012/5188/tables/tin.xlsx", skip = 2)
tin[1:2] = NULL
tin = na.omit(tin)  # to remove the NAs
tin.ts <- ts(tin, start=c(1880), frequency=1) #Convert to annual ts
ts_plot(tin.ts)

#CPI for real prices
cpi_monthly <- pdfetch_FRED("CPIAUCSL")               # monthly CPI
cpi.ts      <- ts(cpi_monthly, start = c(1947,1), frequency = 12)
dim(cpi.ts)
cpi_annual.ts  <- aggregate(cpi.ts, nfrequency = 1, FUN = mean)  # annual mean CPI


#Let's make the time series all the same length

tungsten.ts   = window(tungsten.ts, start = c(1959), end = c(2010), frequency = 1)
copper.ts     = window(copper.ts, start = c(1959), end = c(2010), frequency = 1)
chromium.ts   = window(chromium.ts, start = c(1959), end = c(2010), frequency = 1)
nickel.ts     = window(nickel.ts, start = c(1959), end = c(2010), frequency = 1)
tin.ts        = window(tin.ts, start = c(1959), end = c(2010), frequency = 1)
cpi.ts        = window(cpi_annual.ts, start = c(1959), end = c(2010), frequency = 1)


#Check Length
length(cpi.ts)
length(tungsten.ts)
length(copper.ts)
length(chromium.ts)
length(nickel.ts)
length(tin.ts)


#Now let's convert nominal prices to real prices
tungsten.real <- (tungsten.ts / cpi.ts) * 100
copper.real   <- (copper.ts   / cpi.ts) * 100
chromium.real <- (chromium.ts / cpi.ts) * 100
nickel.real   <- (nickel.ts   / cpi.ts) * 100
tin.real      <- (tin.ts      / cpi.ts) * 100


#Combine all real price series
metals.real.ts <- cbind(tungsten.real, copper.real, chromium.real, nickel.real, tin.real)

#Plot all metals in one graph
plot(metals.real.ts[,1], type = "l", col = 1, lwd = 2,
     main = "Real Metal Prices (1959–2010, CPI-Adjusted)",
     xlab = "Year", ylab = "Price (real, base=100)")

lines(metals.real.ts[,2], col = 2, lwd = 2)
lines(metals.real.ts[,3], col = 3, lwd = 2)
lines(metals.real.ts[,4], col = 4, lwd = 2)
lines(metals.real.ts[,5], col = 5, lwd = 2)

legend("topleft",
       legend = c("Tungsten", "Copper", "Chromium", "Nickel", "Tin"),
       col = 1:5, lty = 1, lwd = 2, bty = "n")


#LET'S CREATE METAL INDEX

#Tidy your real metal time series
metals_real <- broom::tidy(
  cbind(
    tungsten.real,
    copper.real,
    chromium.real,
    nickel.real,
    tin.real
  )
)

#Filter to your actual data window (1959–2010)
metals_real <- metals_real %>%
  filter(index >= 1959 & index <= 2010)

#Convert from long to wide format
metals_real_wide <- metals_real %>%
  pivot_wider(names_from = series, values_from = value)

#Compute your synthetic Metals Index (mean of all 5 metals)
metals_real_wide$MetalsIndex <- rowMeans(
  metals_real_wide[, 2:6], na.rm = TRUE
)

#Quick check
head(metals_real_wide)


#NOW WE NEED TO SCALE THE DATASET
metals_real_wide.scaled = scale(metals_real_wide[ , 2:7])
head(metals_real_wide.scaled)




#NOW TSLM MODELS

#Convert scaled data frame back to a time series
metals_real_wide.scaled.ts <- ts(metals_real_wide.scaled, start = 1959, frequency = 1)

#Model 1: Only metals (real prices)
mymodel0 <- tslm(MetalsIndex ~ tungsten.real + copper.real + chromium.real + nickel.real + tin.real,
                 data = metals_real_wide.scaled.ts)
summary(mymodel0)
accuracy(mymodel0)
barplot(mymodel0$coefficients[2:6],
        main = "Metal Importance (No Trend/Season)",
        ylab = "Coefficient",
        col = "steelblue",
        las = 2)

# Increase bottom margin to make labels visible
par(mar = c(7, 4, 4, 2))  # bottom, left, top, right

barplot(mymodel0$coefficients[2:6],
        names.arg = c("Tungsten", "Copper", "Chromium", "Nickel", "Tin"),  # set names explicitly
        main = "Metal Importance (No Trend/Season)",
        ylab = "Coefficient",
        col = "steelblue",
        las = 2)  # vertical labels




#Model 2: Metals + trend (no seasonal data)
mymodel <- tslm(MetalsIndex ~ tungsten.real + copper.real + chromium.real + nickel.real + tin.real + trend,
                data = metals_real_wide.scaled.ts)
summary(mymodel)
accuracy(mymodel)
barplot(mymodel$coefficients[2:11],
        main = "Metal Importance (With Trend and Season)",
        ylab = "Coefficient",
        col = "darkorange",
        las = 2)


#The model with trend is worst, but both of them show that tungsten is the metal with biggest impact



#NOW BORUTA MODEL

library(Boruta)
boruta_data <- metals_real_wide.scaled

# Run Boruta
set.seed(123)
boruta_model <- Boruta(MetalsIndex ~ ., data = as.data.frame(boruta_data), doTrace = 2)

# Plot importance
plot(boruta_model, las = 2, cex.axis = 0.8, main = "Boruta Variable Importance for Metals Index")

# Increase bottom margin
par(mar = c(7, 4, 4, 2))

# Plot Boruta without the default x-axis label
plot(boruta_model, 
     las = 2,          # vertical labels
     cex.axis = 0.8,   # axis label size
     cex.names = 0.8,  # variable name size
     main = "Boruta Variable Importance for Metals Index",
     xlab = "")        # remove default "Attribute" label



# Extract confirmed features
final_features <- getSelectedAttributes(boruta_model, withTentative = TRUE)
final_features

#Also boruta confirms tungsten as the most important feature
