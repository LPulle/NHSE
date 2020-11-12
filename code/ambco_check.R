## Clear Existing Objects
## This is lazy but ensures everything is clear when we run
#rm(list=ls(all=T))

## Load packages - check if installed first - if not install them
if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
if (!require(rvest)) install.packages("rvest"); library(rvest)
detach("package:dplyr"); library(dplyr) #ensure dplyr is loaded last

## Find the latest ambco url
html <- xml2::read_html("https://www.england.nhs.uk/statistics/statistical-work-areas/ambulance-quality-indicators/")
linknum <- html %>%  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  stringr::str_which(pattern="AmbCO-to")
url <- rvest::html_attr(rvest::html_nodes(html, "a"), "href")[linknum]

## Read the csv from url
ambco <- read.csv(url)

## Filter to month required
ambco_filtered <- ambco %>%  
  dplyr::filter(Year == 2020, Month == 2) ##change this if checking a different month

## Or read the current file from csv - don't include id column
# ambco_filtered <- read.csv("ambco_filtered.csv")

## Identify region and all england rows
## ambco_filtered[-grep("Y[0-9][0-9]", ambco_filtered$Org.Code),] #region codes are Y+nn
regions <- grep("Y[0-9][0-9]", ambco_filtered$Org.Code)
england <- which(ambco_filtered$Org.Code == "Eng")

## Identify columns which are to be weighted and those to be summed
weighted <- union(which(substring(names(ambco_filtered), nchar(names(ambco_filtered)), nchar(names(ambco_filtered))) == "m"), 
                  which(substring(names(ambco_filtered), nchar(names(ambco_filtered))-1, 
                                  nchar(names(ambco_filtered))) %in% c("50", "90")))
## first 5 columns are not metrics all metric columns which aren't weighted are summed
#names(ambco_filtered)[-c(1:5)]
summed <- names(ambco_filtered)[-weighted][-c(1:5)] 

## set all summed columns as integers and replace NAs with 0
## Suppress warnings as we are going to coerce invalid characters into numeric columns
ambco_filtered[,c(summed)] <- suppressWarnings(apply(ambco_filtered[,c(summed)], 2, as.integer))
ambco_filtered[is.na(ambco_filtered)] <- 0

## apply down summed columns for non region non england rows and cbind with same for england 
ambcofiltered_summed <- cbind(data.frame(total=apply(ambco_filtered[-regions ,c(summed)][-england,], 2, sum)), 
                         t(ambco_filtered[england, c(summed)]))
names(ambcofiltered_summed)[2] <- "England"

## take the first 2 characters of the weighted columns and replace the last letter with a "n" to find the divisor column
numdivmap <- data.frame(cbind(num=names(ambco_filtered[,weighted]), div=paste0(substring(names(ambco_filtered[,weighted]),1,2),"n")  ))

## set all weighted columns as integers and replace NAs with 0
## Suppress warnings as we are going to coerce invalid characters into numeric columns
ambco_filtered[,weighted] <- suppressWarnings(apply(ambco_filtered[,weighted], 2, as.numeric))
ambco_filtered[is.na(ambco_filtered)] <- 0

## Do the weighted average by sum product for each trust
weightedt <- sapply(c(1:length(weighted)), function(x) {
sum(ambco_filtered[-regions, weighted][-england,][,x] *
ambco_filtered[-regions, numdivmap$div[match(names(ambco_filtered[-regions, weighted][-england,]), 
                                        numdivmap$num)]][-england,][,x])}) 
weightede <- as.numeric(ambco_filtered[england, numdivmap$div[match(names(ambco_filtered[england, weighted]), numdivmap$num)]])

## divide by the england figure and combine calculated and value in download into data frame
ambcofiltered_weighted <- cbind(data.frame(total=weightedt/weightede), t(ambco_filtered[england, weighted]))
names(ambcofiltered_weighted)[2] <- "England"

## Map trusts to regions
RegionMap <- cbind(ambco_filtered[-regions,c("Org.Code", "Org.Name")], 
      ambco_filtered[match(ambco_filtered$Region, ambco_filtered$Org.Code),c("Org.Code", "Org.Name")] %>% 
        filter(!is.na(Org.Code)))
names(RegionMap)[c(3:4)] <- c("Region.Code", "Region.Name")

## Regions with more than 1 trust
Regions.gt.1trust <- data.frame(sort(table(RegionMap$Region.Name), decreasing = T)) %>% filter(Freq > 1)
#RegionMap[which(RegionMap$Region.Name %in% Regions.gt.1trust$Var1),]
rc <- unique(RegionMap$Region.Code[which(RegionMap$Region.Name %in% Regions.gt.1trust$Var1)])

#getwd()
#write.csv(RegionMap, file="RegionMap.csv", row.names = F)

## Loop through each region with more than 1 trust for summed columns then repeat for the weighted ones
mylist <- vector("list", length=length(rc))
mylistw <- vector("list", length=length(rc))
i <- 1
for (i in (1:length(rc))) {
  j <- rc[i]
  k <- which(ambco_filtered$Org.Code %in% RegionMap$Org.Code[which(RegionMap$Region.Code == rc[i])])
  m <- which(ambco_filtered$Org.Code == j)
  
  ## summed columns
  mylist[[i]] <- cbind(data.frame(total=apply(ambco_filtered[k ,summed], 2, sum)), region=t(ambco_filtered[m, summed]))
  
  ## weighted columns
  weightedew <- sapply(c(1:length(weighted)), function(x) {
    sum(ambco_filtered[k, weighted][,x] * ambco_filtered[k, numdivmap$div[match(names(ambco_filtered[k, weighted]), 
                                                                      numdivmap$num)]][,x])})
  weightedtw <- as.numeric(ambco_filtered[m, numdivmap$div[match(names(ambco_filtered[m, weighted]), numdivmap$num)]])
  mylistw[[i]] <- cbind(weightedew / weightedtw, t(ambco_filtered[m, weighted]))
  
  }

rm(i,j,k,m)

## add names to mylist and mylistw
names(mylist) <- rc
mylist <- lapply(seq_along(mylist), function(i) {
  colnames(mylist[[i]]) <- c(names(mylist)[i], "Region") 
  mylist[[i]]})

names(mylistw) <- rc
mylistw <- lapply(seq_along(mylistw), function(i) {
  colnames(mylistw[[i]]) <- c(names(mylistw)[i], "Region") 
  mylistw[[i]]})

## check 1st row of data frame for each item in list
#lapply(c(1:length(mylist)), function(i) head(mylist[[i]],1))
#lapply(c(1:length(mylistw)), function(i) head(mylistw[[i]],1))


####################################
## Tests
####################################

## 7 significant figs seems to work for most of the metrics
sigfigs <- 7

## test summed columns = england
table(ambcofiltered_summed$total == ambcofiltered_summed$England)
## test weighted columns = england to sigfigs sigfigs
table(format(ambcofiltered_weighted$total, scientific=T, digits=sigfigs) == format(ambcofiltered_weighted$England, scientific=T, digits=sigfigs))

## test summed columns match region
testregions <- data.frame(lapply(c(1:length(mylist)), function(i) mylist[[i]][,1]==mylist[[i]][,2]))
names(testregions) <- rc #c("Region1", "Region2", "Region3")
table(rbind(testregions[,1], testregions[,2], testregions[,3]))
## test weighted columns match region
testregionsw <- data.frame(lapply(c(1:length(mylistw)), 
			function(i) format(mylistw[[i]][,1], scientific=T, digits=sigfigs)==format(mylistw[[i]][,2], scientific=T, digits=sigfigs))
				)
names(testregionsw) <- rc #c("Region1", "Region2", "Region3")
table(rbind(testregionsw[,1], testregionsw[,2], testregionsw[,3]))

## test single trust to single region
RegionMapSingle <- RegionMap[-which(RegionMap$Region.Code %in% rc),][-england,]

SingleRegion <- ambco_filtered[which(ambco_filtered$Org.Code %in% RegionMapSingle$Org.Code),-c(1:4)] %>% arrange(Org.Name) %>% select(-1) == ambco_filtered[which(ambco_filtered$Org.Code %in% RegionMapSingle$Region.Code),-c(1:4)] %>% arrange(Org.Name) %>% select(-1)
cbind(data.frame(`TRUE` = length(which(SingleRegion == T))),
data.frame(`FALSE` = length(which(SingleRegion == F))))

## if there is a mismatch anywhere output both trusts and regions data
if(data.frame(`FALSE` = length(which(SingleRegion == F))) > 0) {
print(ambco_filtered[which(ambco_filtered$Org.Code %in% RegionMapSingle$Org.Code),-c(1:4)] %>% arrange(Org.Name))
print(" ")
print(ambco_filtered[which(ambco_filtered$Org.Code %in% RegionMapSingle$Region.Code),-c(1:4)] %>% arrange(Org.Name))
}

## Get unique list of all the metrics
#ambco_filtered %>% 
#	pivot_longer(!c(1:5), names_to ="Metric", values_to="value") %>%
#	select(Metric) %>% unique() %>% data.frame()

## Output calculations
#write.csv(rbind(ambcofiltered_summed, ambcofiltered_weighted), file = "ambco_check.csv", row.names = T)
#write.csv(
#rbind(cbind(data.frame(mylist[[1]]), data.frame(mylist[[2]]), data.frame(mylist[[3]])),
#cbind(data.frame(mylistw[[1]]), data.frame(mylistw[[2]]), data.frame(mylistw[[3]])))
#file="ambco_check2.csv")
