library(scales)
library(ggplot2)
library(ggrepel)

options(scipen=5)




# Subcategories scatterplot
filePath <- "graphData.csv"
minDotSize <- 4 # Change dot sizes
maxDotSize <- 9
dotBorderWidth <- .8 # Border around dot

salesData <- read.csv(filePath, header=TRUE)
aggData <- aggregate(salesData[,c("Dollar.Sales", "Dollar.Sales.LY", "Unit.Sales", "Unit.Inventory")], by=list(salesData$Product.Category, salesData$Product.Subcategory), FUN=sum)
names(aggData) <- c("Product.Category", "Product.Subcategory", "Dollar.Sales", "Dollar.Sales.LY", "Unit.Sales", "Unit.Inventory")
aggData$Sales.Comparison <- (aggData$Dollar.Sales - aggData$Dollar.Sales.LY)/abs(aggData$Dollar.Sales.LY)
aggData$Inventory.Turnover <- aggData$Unit.Sales/aggData$Unit.Inventory * 52/5

ggplot(aggData, aes(x=Sales.Comparison, y=Inventory.Turnover)) + 
  geom_point(shape=21, aes(size = Dollar.Sales, fill = Product.Category, stroke = dotBorderWidth)) +
  geom_label_repel(aes(label=Product.Subcategory), size = 4, box.padding = .8, label.padding = .2) +
  ggtitle("Product Performance: Subclasses by Volume") +
  xlab("Sales Comparison (YOY)") +
  ylab("Inventory Turnover") +
  theme(panel.grid.minor = element_line(color="azure3", size = 0.15),
        panel.grid.major = element_line(color="grey", size = .5),
        panel.background = element_rect(fill="whitesmoke")) +
  scale_size_continuous(range = c(minDotSize, maxDotSize)) + 
  scale_x_continuous(labels = scales::percent) + 
  guides(size=guide_legend(title = "Sales Volume ($)", stroke = dotBorderWidth), fill = guide_legend(title = "Product Category", override.aes=list(size=5.8)))



# Top k performing products scatterplot.
topK <- 20 # How many products to feature.
minDotSize <- 3
maxDotSize <- 8
dotBorderWidth <- .6

salesData$Sales.Comparison <- (salesData$Dollar.Sales - salesData$Dollar.Sales.LY)/abs(salesData$Dollar.Sales.LY)
salesData$Inventory.Turnover <- salesData$Unit.Sales/salesData$Unit.Inventory * 52/5
salesData <- salesData[order(-salesData$Dollar.Sales),]

ggplot(head(salesData, topK), aes(x=Sales.Comparison, y=Inventory.Turnover)) + 
  geom_point(shape=21, aes(size = Dollar.Sales, fill = Product.Subcategory, stroke = dotBorderWidth)) +
  geom_label_repel(aes(label=Product.Code), size = 4, box.padding = .8, label.padding = .2) +
  ggtitle(paste("Product Performance: Top", topK, "Product Codes by Volume")) +
  xlab("Sales Comparison (YOY)") +
  ylab("Inventory Turnover") +
  theme(panel.grid.minor = element_line(color="azure3", size = 0.15),
        panel.grid.major = element_line(color="grey", size = .5),
        panel.background = element_rect(fill="whitesmoke")) +
  scale_size_continuous(range = c(minDotSize, maxDotSize)) + 
  scale_x_continuous(labels = scales::percent) + 
  guides(size=guide_legend(title = "Sales Volume ($)", stroke = dotBorderWidth), fill = guide_legend(title = "Product Subcategory", override.aes=list(size=5.8)))





# Rose chart for YOY sales comparison, by fiscal month.
filePath <- "yearData.csv"
increment <- 2500000 # increment for sales calue ticks.
tyColorRGB <- c(38, 97, 156) # RGB values for cimpany color.
lyColor <- "orangered3" # Color of LY comparison border.

fiscalYearData <- read.csv(filePath, header=TRUE)
recentMonth <- length(which(!is.na(fiscalYearData$Sales)))
tyColorRGB <- tyColorRGB/255
tyColor <- rgb(tyColorRGB[1], tyColorRGB[2], tyColorRGB[3])
tyColorRGB <- (3 * tyColorRGB^2 + 1)^(1/2)/2
mutedTYColor <- rgb(tyColorRGB[1], tyColorRGB[2], tyColorRGB[3])
fiscalYearData$Sales.Comparison <- percent((fiscalYearData$Sales - fiscalYearData$Sales.LY)/abs(fiscalYearData$Sales.LY))
fiscalYearData$Sales.Comparison[fiscalYearData$Sales.Comparison == "NA%"] <- ""
fiscalYearData$fpLabels <- paste(fiscalYearData$Fiscal.Month, fiscalYearData$Sales.Comparison, sep="\n")

ggplot(fiscalYearData) + 
  geom_col(aes(x=Fiscal.Month, y = Sales), width = 1.01, fill = c(rep(mutedTYColor, recentMonth-1), tyColor), color = c(rep(mutedTYColor, recentMonth-1), tyColor), size = .1) + 
  geom_col(aes(x=Fiscal.Month, y = Sales.LY), width = 1, fill = NA, color = lyColor, size = 1) + 
  labs(x='Fiscal Period', title = "Sales Comparison by Fiscal Month") + 
  theme(panel.grid.minor = element_line(color="lightgrey", size = 0.1), 
        panel.grid.major = element_line(color="lightgrey", size = .3), 
        panel.background = element_rect(fill="whitesmoke"), 
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_text(size = 11, face="bold", vjust = .5),
        axis.text.y=element_text(size = 10, face="bold"),
        axis.line = element_line(color = "whitesmoke"),
        plot.title=element_text(size=18, face="bold", hjust = 0.5)) + 
  scale_y_discrete(labels = dollar_format(), limits = seq(increment, increment * (max(max(fiscalYearData$Sales, na.rm = TRUE), max(fiscalYearData$Sales.LY, na.rm = TRUE))%/%increment), increment)) +
  scale_x_discrete(labels = fiscalYearData$fpLabels) + 
  coord_polar(theta = "x", start = pi/12)

