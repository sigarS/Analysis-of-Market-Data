library(data.table)
library(ggplot2)
library(tidyr)
library(dplyr)
file_path = "C:/Users/jerem/Documents/R/Quantium/Task 2/"
data = fread(paste0(file_path, "QVI_data.csv"))
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))


data[, YEARMONTH := format(as.Date(data$DATE, "%Y%m"), "%Y%m") ]

##getting data for trial period


measure_over_time = data %>% group_by(STORE_NBR,YEARMONTH) %>% 
  summarise(totSales = sum(TOT_SALES),
            nCustomers = uniqueN(LYLTY_CARD_NBR),
            nTxnPerCust = uniqueN(TXN_ID)/nCustomers, 
            avgPricePerUnit = mean(TOT_SALES/PROD_QTY))
measure_over_time = data.table(measure_over_time)

storesWithFullObs <- unique(measure_over_time[, .N, STORE_NBR][N == 12, STORE_NBR])
preTrialMeasures <- measure_over_time[YEARMONTH < 201902 & STORE_NBR %in%            storesWithFullObs, ]

## function for calculating correlation
calculateCorrelation <- function(inputTable, metricCol, storeComparison) {
calcCorrTable = data.table(Store1 = numeric(), Store2 = numeric(), corr_measure =
numeric())
storeNumbers <- unique(inputTable[, STORE_NBR])
for (i in storeNumbers) {
calculatedMeasure = data.table("Store1" = storeComparison,
"Store2" = i,
"corr_measure" = cor(inputTable[STORE_NBR == storeComparison, eval(metricCol)], inputTable[STORE_NBR == i, eval(metricCol)])
)
calcCorrTable <- rbind(calcCorrTable, calculatedMeasure)
}
return(calcCorrTable)
}

#### Create a function to calculate a standardised magnitude distance for a measure,
calculateMagnitudeDistance <- function(inputTable, metricCol, storeComparison) {
  calcDistTable = data.table(Store1 = numeric(), Store2 = numeric(), YEARMONTH =
                               numeric(), measure = numeric())
  storeNumbers <- unique(inputTable[, STORE_NBR])
  for (i in storeNumbers) {
    calculatedMeasure = data.table("Store1" = storeComparison
                                   , "Store2" = i
                                   , "YEARMONTH" = inputTable[STORE_NBR ==
                                                                storeComparison, YEARMONTH]
                                   , "measure" = abs(inputTable[STORE_NBR ==
                                                                  storeComparison, eval(metricCol)]
                                                     - inputTable[STORE_NBR == i,
                                                                  eval(metricCol)])
    )
    calcDistTable <- rbind(calcDistTable, calculatedMeasure)
  }
  #### Standardise the magnitude distance so that the measure ranges from 0 to 1
  minMaxDist <- calcDistTable[, .(minDist = min(measure), maxDist = max(measure)),
                              by = c("Store1", "YEARMONTH")]
  distTable <- merge(calcDistTable, minMaxDist, by = c("Store1", "YEARMONTH"))
  distTable[, magnitudeMeasure := 1 - (measure - minDist)/(maxDist - minDist)]
  finalDistTable <- distTable[, .(mag_measure = mean(magnitudeMeasure)), by =
                                .(Store1, Store2)]
  return(finalDistTable)
}

### finding trial store
trial_store <- 77
corr_nSales <- calculateCorrelation(preTrialMeasures, quote(totSales), 
                                    trial_store)
corr_nCustomers <- calculateCorrelation(preTrialMeasures, quote(nCustomers),trial_store )
magnitude_nSales <- calculateMagnitudeDistance(preTrialMeasures, quote(totSales),
                                               trial_store)
magnitude_nCustomers <- calculateMagnitudeDistance(preTrialMeasures,
                                                   quote(nCustomers), trial_store)


corr_weight <- 0.5
score_nSales <- merge(corr_nSales, magnitude_nSales, by = intersect(names(corr_nSales),names(magnitude_nSales)))[, scoreNSales := corr_weight*corr_nSales$corr_measure + (1-corr_weight)* magnitude_nSales$mag_measure]
score_nCustomers <- merge(corr_nCustomers, magnitude_nCustomers, by = intersect(names(corr_nCustomers),names(magnitude_nCustomers)))[, scoreNCust := corr_weight*corr_measure + (1-corr_weight)*mag_measure]
score_Control <- merge(score_nSales,score_nCustomers , by = c("Store1", "Store2"))
score_Control[, finalControlScore := scoreNSales * 0.5 + scoreNCust * 0.5]

score_Control = score_Control[order(-finalControlScore)]
control_store = score_Control[2,]$Store2

pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store,
"Trial",
ifelse(STORE_NBR == control_store,
"Control", "Other stores"))
][, totSales := mean(totSales), by = c("YEARMONTH",
"Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"))
][YEARMONTH < 201903 , ]


ggplot(pastSales, aes(TransactionMonth, totSales, color = Store_type)) +
geom_line() +
labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")


pastCustomers <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store,
                                                         "Trial",
                                                         ifelse(STORE_NBR == control_store,
                                                                "Control", "Other stores"))
                                  ][, nCustomers := mean(nCustomers), by = c("YEARMONTH",
                                                                         "Store_type")
                                    ][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"))
                                      ][YEARMONTH < 201903 , ]


ggplot(pastSales, aes(TransactionMonth, nCustomers, color = Store_type)) +
  geom_line() +
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")

scalingFactorForControlSales <- preTrialMeasures[STORE_NBR == trial_store &
YEARMONTH < 201902, sum(totSales)]/preTrialMeasures[STORE_NBR == control_store &
YEARMONTH < 201902, sum(totSales)]

scaledControlSales <- measureOverTimeSales[STORE_NBR == control_store, ][ ,
controlSales := totSales * scalingFactorForControlSales]

percentageDiff <- merge(scaledControlSales,pastSales[Store_type == "Trial"], by = "YEARMONTH")[,percentageDiff := 100*(totSales.x - totSales.y)/totSales.x ]
stdDev <- sd(percentageDiff[YEARMONTH < 201902 , percentageDiff]
)

degreesOfFreedom <- 7
percentageDiff[, tValue := abs(totSales.x - mean(totSales.y))/stdDev
][, TransactionMonth := TransactionMonth.x
]
pastSales <- pastSales[Store_type %in% c("Trial", "Control"), ]


pastSales_Controls95 <- pastSales[Store_type == "Control",
                                  ][, totSales := totSales * (1 + stdDev * 2)
                                    ][, Store_type := "Control 95th % confidence
interval"]
#### Control store 5th percentile
pastSales_Controls5 <- pastSales[Store_type == "Control",
                                 ][, totSales := totSales * (1 - stdDev * 2)
                                   ][, Store_type := "Control 5th % confidence
interval"]
trialAssessment <- rbind(pastSales, pastSales_Controls95, pastSales_Controls5)
#### Plotting these in one nice graph
ggplot(trialAssessment, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_rect(data = trialAssessment[ YEARMONTH < 201905 & YEARMONTH > 201901 ,],
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), ymin = 0 , ymax =
                  Inf, color = NULL), show.legend = FALSE) +
  geom_line() +
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")




scalingFactorForControlCust <- preTrialMeasures[STORE_NBR == trial_store & YEARMONTH < 201902, sum(nCustomers)]/preTrialMeasures[STORE_NBR == control_store & YEARMONTH < 201902, sum(nCustomers)]
measureOverTimeCusts <- measureOverTimeSales
scaledControlCustomers <- measureOverTimeCusts[STORE_NBR == control_store, ][ , controlCustomer := nCustomers * scalingFactorForControlCust]

percentageDiff <- merge(scaledControlCustomers,pastCustomers[Store_type == "Trial"], by = "YEARMONTH")[,percentageDiff := 100*(nCustomers.x - nCustomers.y)/nCustomers.x ]
stdDev <- sd(percentageDiff[YEARMONTH < 201902 , percentageDiff]
)
  ```