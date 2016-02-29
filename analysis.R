
library(xtable)
library(ggplot2)

# preparation

storm.data.raw <- read.csv("./data.csv")

# part 1
sumByEventType <- aggregate(x=storm.data.raw[, c("INJURIES", "FATALITIES")], by=list(EVTYPE = storm.data.raw$EVTYPE), FUN=sum)
sortedByInjuries <- sumByEventType[with(sumByEventType, order(INJURIES, decreasing = TRUE)), c("EVTYPE","INJURIES")]
sortedByFatalities <- sumByEventType[with(sumByEventType, order(FATALITIES, decreasing = TRUE)), c("EVTYPE","FATALITIES")]

iTable <- xtable(head(sortedByInjuries, 20))
print(iTable, type="html")

fTable <- xtable(head(sortedByFatalities, 20))
print(fTable, type="html")

fatalitiesPlot <- ggplot(head(sortedByFatalities, 20)) + geom_bar(aes(EVTYPE, FATALITIES), stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Top 20 Storm Events By Fatalities") + theme(legend.position = "right")

print(fatalitiesPlot)

injuriesPlot <- ggplot(head(sortedByInjuries, 20)) + geom_bar(aes(EVTYPE, INJURIES), stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Top 20 Storm Events By Injuries") + theme(legend.position = "right")

print(injuriesPlot)

health.scatterPlot <- ggplot(sumByEventType) + geom_point(aes(INJURIES, FATALITIES)) + geom_text(aes(x=INJURIES, y=FATALITIES, label=EVTYPE, hjust=-1, vjust=1))
print(health.scatterPlot)

midrange <- sumByEventType[(sumByEventType$FATALITIES != 0 & sumByEventType$INJURIES != 0 & sumByEventType$EVTYPE != "TORNADO"), ];
health.scatterPlot.nonZero.nonTornado <- ggplot(midrange) + geom_point(aes(INJURIES, FATALITIES)) + geom_text(aes(x=INJURIES, y=FATALITIES, label=EVTYPE, hjust=-1, vjust=1))
print(health.scatterPlot.nonZero.nonTornado)

## part 2

expandCosts <- function(cost, exp) {
  if (exp == "K" || exp == "k") {
    return (cost * 1000); 
  } else if (exp == "M" || exp == "m") {
    return (cost * 1000000); 
  } else if (exp == "B" || exp == "b") {
    return (cost * 1000000000)
  } else {
    return (cost);
  }
}

storm.data.damages <- storm.data.raw[, c("EVTYPE", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")];
storm.data.damages.dn <- data.frame(storm.data.raw, PROPDMGRAW=mapply(FUN = expandCosts, cost=storm.data.damages$PROPDMG, exp=storm.data.damages$PROPDMGEXP), CROPDMGRAW=mapply(FUN=expandCosts, cost=storm.data.damages$CROPDMG, exp=storm.data.damages$CROPDMGEXP));

sumDamagesByType <- aggregate(x=storm.data.damages.dn[, c("PROPDMGRAW", "CROPDMGRAW")], by=list(EVTYPE = storm.data.damages.dn$EVTYPE), FUN = sum)
sortedByProperty <- sumDamagesByType[with(sumDamagesByType, order(PROPDMGRAW, decreasing = TRUE)), ]; 
sortedByCrop <- sumDamagesByType[with(sumDamagesByType, order(CROPDMGRAW, decreasing=TRUE)), ]

# barchart for top 20 

top20PropertyDamageBarPlot <- ggplot(head(sortedByProperty, 20)) + geom_bar(aes(EVTYPE, PROPDMGRAW), stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Top 20 Storm Events by Property Damage $") + theme(legend.position = "right")
print(top20PropertyDamageBarPlot)

top20CropDamageBarPlot <- ggplot(head(sortedByCrop, 20)) + geom_bar(aes(EVTYPE, CROPDMGRAW), stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Top 20 Storm Events by Crop Damage $") + theme(legend.position = "right")
print(top20CropDamageBarPlot)

damages.scatterPlot <- ggplot(sumDamagesByType) + geom_point(aes(PROPDMGRAW, CROPDMGRAW)) + geom_text(aes(x=PROPDMGRAW, y=CROPDMGRAW, label=EVTYPE))
print(damages.scatterPlot)

damages.nonZero <- sumDamagesByType[(sumDamagesByType$PROPDMGRAW != 0 | sumDamagesByType$CROPDMGRAW != 0), ]
damages.nonZero.scatterPlot <- ggplot(damages.nonZero) + geom_point(aes(PROPDMGRAW, CROPDMGRAW)) + geom_text(aes(x=PROPDMGRAW, y=CROPDMGRAW, label=EVTYPE))