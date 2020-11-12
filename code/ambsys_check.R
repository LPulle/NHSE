## Clear Existing Objects
## This is lazy but ensures everything is clear when we run
#rm(list=ls(all=T))

## Load packages - check if installed first - if not install them
if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
if (!require(rvest)) install.packages("rvest"); library(rvest)
detach("package:dplyr"); library(dplyr) #ensure dplyr is loaded last

## Find the latest ambsys url
html <- xml2::read_html("https://www.england.nhs.uk/statistics/statistical-work-areas/ambulance-quality-indicators/")
linknum <- html %>%  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  stringr::str_which(pattern="AmbSYS-to-2020")
url <- rvest::html_attr(rvest::html_nodes(html, "a"), "href")[linknum]

## Read the csv from url
ambsys <- read.csv(url)

## Filter to month required
ambsys_filtered <- ambsys %>%  
  dplyr::filter(Year == 2020, Month == 9) ##change this if checking a different month

## Or read filtered file from csv 
#ambsys_filtered <- read.csv("C:/Users/pullel/OneDrive - NHS ?NEL CSU/NHSE PAT/Analysis/ambsys_filtered.csv")


## Identify region and all england rows
## ambsys_filtered[-grep("Y[0-9][0-9]", ambsys_filtered$Org.Code),] #region codes are Y+nn
regions <- grep("Y[0-9][0-9]", ambsys_filtered$Org.Code)
england <- which(ambsys_filtered$Org.Code == "Eng")

## Identify columns which are to be summed
summed <- which(names(ambsys_filtered) %in%
c("A0","A1","A2","A7","A8","A9","A10","A11","A12","A13",
"A14","A18","A19","A20","A21","A22","A23","A24","A27","A30",
"A33","A36","A49","A50","A53","A56","A57","A58","A59","A60",
"A61","A62","A65","A68","A71","A74","A75","A76","A77","A78",
"A79","A80","A81","A82","A85","A88","A91","A94","A97","A100",
"A103","A106","A107","A110","A111","A112","A113","A115","A116",
"A119","A120","A17","A39","A40","A41","A42","A43","A44","A45",
"A46","A47","A48","A54","A55"))

meanlookup <- data.frame(metric =
c("A3","A15","A25","A28","A31","A34","A37","A51","A63",
"A66","A69","A72","A83","A86","A89","A92","A95","A98",
"A101","A104","A108","A117","A121"),
num =
c("A2","A14","A24","A27","A30","A33","A36","A50","A62",
"A65","A68","A71","A82","A85","A88","A91","A94","A97",
"A100","A103","A107","A116","A120"),
denom =
c("A1","A13","A8","A9","A10","A11","A12","A49","A58",
"A59","A60","A61","A74","A75","A76","A77","A78","A79",
"A80","A81","A106","A115","A119"))

meaned <- which(names(ambsys_filtered) %in% meanlookup$metric)

weightedlookup <- data.frame(num = 
c("A4","A5","A6","A16","A26","A29","A32","A35","A38","A52",
"A84","A87","A90","A93","A96","A99",
"A102","A105","A109","A114","A118","A122"),
denom=
c("A1","A1","A1","A13","A8","A9","A10","A11","A12","A49",
"A74","A75","A76","A77","A78","A79","A80","A81",
"A106","A1","A115","A119"))

weighted <- which(names(ambsys_filtered) %in% weightedlookup$num)

## set all summed and meaned columns as integers and replace NAs with 0
## Suppress warnings as columns with invalid characters will be coerced into numeric columns
ambsys_filtered[,c(summed)] <- suppressWarnings(apply(ambsys_filtered[,c(summed)], 2, as.integer))
ambsys_filtered[,c(meaned)] <- suppressWarnings(apply(ambsys_filtered[,c(meaned)], 2, as.integer))
ambsys_filtered[is.na(ambsys_filtered)] <- 0

## apply down summed columns for non region non england rows and cbind with same for england 
ambsysfiltered_summed <- cbind(data.frame(total=apply(ambsys_filtered[-regions ,c(summed)][-england,], 2, sum)), 
                         t(ambsys_filtered[england, c(summed)]))
names(ambsysfiltered_summed)[2] <- "England"

## apply down meaned columns for non region non england rows and cbind with same for england 
ambsysfiltered_meaned <- sapply(c(1:length(meaned)), function(x) {
sum(ambsys_filtered[-regions, meanlookup$num[
			match(names(ambsys_filtered[-regions, meaned][-england,]), 
                  meanlookup$metric)]][-england,][,x]) / sum(ambsys_filtered[-regions, meanlookup$denom[
			match(names(ambsys_filtered[-regions, meaned][-england,]), 
                  meanlookup$metric)]][-england,][,x])}

) %>% round(, digits = 0) %>% data.frame() %>% cbind(., t(ambsys_filtered[england, meaned]))
names(ambsysfiltered_meaned) <- c("total", "England")


##########################################################
## debugging stuff here
##########################################################

ambsys_filtered_long <- ambsys_filtered %>% mutate_all(as.character) %>%
	pivot_longer(!c(1:5), names_to ="Metric", values_to="value")
allmetrics <- ambsys_filtered_long$Metric %>% unique()

notchecked <- allmetrics[-which(allmetrics %in% union(row.names(ambsysfiltered_summed),
row.names(ambsysfiltered_meaned)))]

#ambsys_filtered[,notchecked]
#data.frame(names(ambsys_filtered[,notchecked]))
#length(weighted)

##########################################################
## end of debugging
##########################################################


## set all weighted columns as integers and replace NAs with 0
## Suppress warnings as we are going to coerce invalid characters into numeric columns
ambsys_filtered[,weighted] <- suppressWarnings(apply(ambsys_filtered[,weighted], 2, as.numeric))
ambsys_filtered[is.na(ambsys_filtered)] <- 0

## Do the weighted average by sum product for each trust
weightedt <- sapply(c(1:length(weighted)), function(x) {
sum(ambsys_filtered[-regions, weighted][-england,][,x] *
ambsys_filtered[-regions, weightedlookup$denom[match(names(ambsys_filtered[-regions, weighted][-england,]), 
                                        weightedlookup$num)]][-england,][,x])}) 
weightede <- as.numeric(ambsys_filtered[england, weightedlookup$denom[match(names(ambsys_filtered[england, weighted]), weightedlookup$num)]])

## divide by the england figure and combine calculated and value in download into data frame
ambsysfiltered_weighted <- cbind(data.frame(total=round(weightedt/weightede,0)), t(ambsys_filtered[england, weighted]))
names(ambsysfiltered_weighted)[2] <- "England"


## Map trusts to regions
RegionMap <- cbind(ambsys_filtered[-regions,c("Org.Code", "Org.Name")], 
      ambsys_filtered[match(ambsys_filtered$Region, ambsys_filtered$Org.Code),c("Org.Code", "Org.Name")] %>% 
        filter(!is.na(Org.Code)))
names(RegionMap)[c(3:4)] <- c("Region.Code", "Region.Name")

## Regions with more than 1 trust
Regions.gt.1trust <- data.frame(sort(table(RegionMap$Region.Name), decreasing = T)) %>% filter(Freq > 1)
#RegionMap[which(RegionMap$Region.Name %in% Regions.gt.1trust$Var1),]
rc <- unique(RegionMap$Region.Code[which(RegionMap$Region.Name %in% Regions.gt.1trust$Var1)])



## Loop through each region with more than 1 trust for summed columns
mylist <- vector("list", length=length(rc)) #summed
mylistw <- vector("list", length=length(rc)) #weighted
mylistm <- vector("list", length=length(rc)) #meaned

i <- 1
for (i in (1:length(rc))) {
  j <- rc[i]
  k <- which(ambsys_filtered$Org.Code %in% RegionMap$Org.Code[which(RegionMap$Region.Code == rc[i])])
  m <- which(ambsys_filtered$Org.Code == j)
  
  ## summed columns
  mylist[[i]] <- cbind(data.frame(total=apply(ambsys_filtered[k ,summed], 2, sum)), region=t(ambsys_filtered[m, summed]))
  
#cbind(data.frame(total=apply(ambsys_filtered[-regions ,c(summed)][-england,], 2, sum)), t(ambsys_filtered[england, c(summed)]))

  ## meaned columns
mylistm[[i]] <- sapply(c(1:length(meaned)), function(x) {
sum(ambsys_filtered[k, meanlookup$num[
			match(names(ambsys_filtered[k, meaned]), 
                  meanlookup$metric)]][,x]) / sum(ambsys_filtered[k, meanlookup$denom[
			match(names(ambsys_filtered[k, meaned]), 
                  meanlookup$metric)]][,x])}

) %>% round(, digits = 0) %>% data.frame() %>% cbind(., t(ambsys_filtered[m, meaned]))

  ## weighted columns - this will need rewriting
  weightedew <- sapply(c(1:length(weighted)), function(x) {
    sum(ambsys_filtered[k, weighted][,x] * ambsys_filtered[k, weightedlookup$denom[match(names(ambsys_filtered[k, weighted]), 
                                                                      weightedlookup$num)]][,x])})
  weightedtw <- as.numeric(ambsys_filtered[m, weightedlookup$denom[match(names(ambsys_filtered[m, weighted]), weightedlookup$num)]])
  mylistw[[i]] <- cbind(round(weightedew / weightedtw, 0), t(ambsys_filtered[m, weighted]))
  
  }

rm(i,j,k,m)

## add names to mylist and mylistw
names(mylist) <- rc
mylist <- lapply(seq_along(mylist), function(i) {
  colnames(mylist[[i]]) <- c(names(mylist)[i], "Region") 
  mylist[[i]]})

names(mylistm) <- rc
mylistm <- lapply(seq_along(mylistm), function(i) {
  colnames(mylistm[[i]]) <- c(names(mylistm)[i], "Region") 
  mylistm[[i]]})

names(mylistw) <- rc
mylistw <- lapply(seq_along(mylistw), function(i) {
  colnames(mylistw[[i]]) <- c(names(mylistw)[i], "Region") 
  mylistw[[i]]})


## check 1st row of data frame for each item in list
#lapply(c(1:length(mylist)), function(i) head(mylist[[i]],1))
#lapply(c(1:length(mylistm)), function(i) head(mylistm[[i]],1))
#lapply(c(1:length(mylistw)), function(i) head(mylistw[[i]],1))
#str(mylist)

####################################
## Tests
####################################

## test summed columns = england
table(ambsysfiltered_summed$total == ambsysfiltered_summed$England)
## test meaned columns = england
table(ambsysfiltered_meaned[,1] == ambsysfiltered_meaned[,2])
## test weighted columns = england rounded to 0 dps
table(round(ambsysfiltered_weighted$total,0) == signif(ambsysfiltered_weighted$England,10))

## test summed columns match region
testregions <- data.frame(lapply(c(1:length(mylist)), function(i) mylist[[i]][,1]==mylist[[i]][,2]))
names(testregions) <- rc #c("Region1", "Region2", "Region3")
table(rbind(testregions[,1], testregions[,2], testregions[,3]))

## test meaned columns match region
testregionsm <- data.frame(lapply(c(1:length(mylistm)), function(i) mylistm[[i]][,1]==mylistm[[i]][,2]))
names(testregionsm) <- rc #c("Region1", "Region2", "Region3")
table(rbind(testregionsm[,1], testregionsm[,2], testregionsm[,3]))


## test weighted columns match region
testregionsw <- data.frame(lapply(c(1:length(mylistw)), function(i) signif(mylistw[[i]][,1],10)==signif(mylistw[[i]][,2],10)))
names(testregionsw) <- rc #c("Region1", "Region2", "Region3")
table(rbind(testregionsw[,1], testregionsw[,2], testregionsw[,3]))

## test single trust to single region
RegionMapSingle <- RegionMap[-which(RegionMap$Region.Code %in% rc),][-england,]
SingleRegion <- ambsys_filtered[which(ambsys_filtered$Org.Code %in% RegionMapSingle$Org.Code),-c(1:4)] %>% arrange(Org.Name) %>% select(-1) == ambsys_filtered[which(ambsys_filtered$Org.Code %in% RegionMapSingle$Region.Code),-c(1:4)] %>% arrange(Org.Name) %>% select(-1)
cbind(data.frame(`TRUE` = length(which(SingleRegion == T))),
data.frame(`FALSE` = length(which(SingleRegion == F))))

## if there is a mismatch anywhere output both trusts and regions data
if(data.frame(`FALSE` = length(which(SingleRegion == F))) > 0) {
print(ambsys_filtered[which(ambsys_filtered$Org.Code %in% RegionMapSingle$Org.Code),-c(1:4)] %>% arrange(Org.Name))
print(" ")
print(ambsys_filtered[which(ambsys_filtered$Org.Code %in% RegionMapSingle$Region.Code),-c(1:4)] %>% arrange(Org.Name))
}

## Output calculations
#write.csv(rbind(ambsysfiltered_summed, ambsysfiltered_meaned, ambsysfiltered_weighted), file = "ambsys_check.csv", row.names = T)
#write.csv(
#rbind(cbind(data.frame(mylist[[1]]), data.frame(mylist[[2]]), data.frame(mylist[[3]])),
#cbind(data.frame(mylistm[[1]]), data.frame(mylistm[[2]]), data.frame(mylistm[[3]])),
#cbind(data.frame(mylistw[[1]]), data.frame(mylistw[[2]]), data.frame(mylistw[[3]]))),
#file="ambsys_check2.csv")
