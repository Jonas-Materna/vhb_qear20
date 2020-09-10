#Install required packages
list.of.packages <- c("tidyverse", "ggplot2", "dplyr", "RColorBrewer", "sp")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

########################################################################################
#####Load the raw data, rename variables and data cleaning#####
########################################################################################
#Get the file paths
insolvency.file     <- paste0(getwd(),"/raw_data/insolvency_filings_de_julaug2020_incomplete.csv")

#Read the csv. File encoding is set to "UTF-8" in order to deal with umlauts
insolvency.data     <- read.csv(insolvency.file, sep=",", fileEncoding="UTF-8")


########################################################################################
#### Issue 2: Summarize insolvency data#####
########################################################################################

#Dimensions (rows and columns) of the data
dim(insolvency.data) ## 10035 rows and 6 columns

#How many observations does the insolvency dataset have?
nrow(insolvency.data) ##counts the rows -> 10035


#Are there duplicates?
dup <- duplicated(insolvency.data)
sum(dup) #There are duplicates. 
insolvency.data <- insolvency.data[!dup,] ##Deletes the 680 duplicates

#count observations
nrow(insolvency.data) ## There are 9355 observations after deleting the duplicates

#How are they distributed across filing types and register courts?
Subject <- table(insolvency.data$subject)
Subject ##Gives the absolute numbers of filing types

court <- table(insolvency.data$insolvency_court)
court ##Gives the absolute numbers of filings across courts

## Or with group_by and summarize function
library(dplyr)
insolvency.data %>%
  group_by(subject) %>%
  summarize(freq = n())

insolvency.data %>%
  group_by(insolvency_court) %>%
  summarize(freq = n())

##Calculate relative frequency tables
insolvency.data %>%
  group_by(subject) %>%
  summarize(freq = n()) %>%
  mutate(rel.freq = freq / sum(freq))

insolvency.data %>%
  group_by(insolvency_court) %>%
  summarize(freq = n()) %>%
  mutate(rel.freq = freq / sum(freq))

#How many files per district are there. Be careful: The same court-file number might be used by multiple courts
temp <- data.frame(insolvency.data$insolvency_court, insolvency.data$court_file_number)
temp <- unique(temp)
temp <- table(temp$insolvency.data.insolvency_court)
View(temp)


#GGplot the subjects
library(ggplot2)
subjects <- data.frame((Subject)) 
colnames(subjects) <- c("Subject", "Frequency")
p<-ggplot(data=subjects) +
  geom_bar(stat="identity",fill="steelblue", aes(x=Subject, y=Frequency))+
  coord_flip()+
  ggtitle("Observations by Subject")+
  theme(axis.text=element_text(size=12))
p
ggsave("output/Subject.eps", width = 20, height = 8, units = "cm")


#Generate a Map with openings by Kreis and Bundesland
gadm3 <- readRDS(paste0(getwd(),"/raw_data/gadm36_DEU_3_sp.rds"))
gadm2 <- readRDS(paste0(getwd(),"/raw_data/gadm36_DEU_2_sp.rds"))
gadm1 <- readRDS(paste0(getwd(),"/raw_data/gadm36_DEU_1_sp.rds"))

#Get the opnenings
opening.data <- insolvency.data[insolvency.data$subject == "Eröffnungen",]

#Match the city names
cities <- (gadm3$NAME_3)
num <- data.frame()
pb <- txtProgressBar(min = 0, max = length(opening.data$domicile_debtor), style = 3)
for(c in 1:length(opening.data$domicile_debtor)){
  
  rown <- "NA"
  
  #Look for exact match
  if(!opening.data$domicile_debtor[c]==""){
    rown <- which(cities == opening.data$domicile_debtor[c])
    
    #Fuzzy match if noch exact match is found
    if(!length(rown)>0){
      rown <- agrep(opening.data$domicile_debtor[c], cities)
    }
    
    #Save it 
    if(length(rown)>0){
      num <- rbind(num, rown)
    }else{
      num <- rbind(num, "NA")
    }
    setTxtProgressBar(pb, c)
    
  }
}

#Get sum for each city
insolvencies <- data.frame(gadm3$NAME_1, gadm3$NAME_2, gadm3$NAME_3)
for(c in 1:length(gadm3$NAME_1)){
  insolvencies$insolvencies[c] <- sum(num==c)
}


#Plot by Kreis
library(RColorBrewer)
library(sp)
kreis.insolvencies <- data.frame(gadm2$NAME_2)
for(k in 1:length(gadm2$NAME_2)){
  kreis.insolvencies$insolvencies[k] <- sum(insolvencies[insolvencies$gadm3.NAME_2 == gadm2$NAME_2[k],4])
}

col_no <- as.factor(as.numeric( cut(kreis.insolvencies$insolvencies, 
                                    breaks = c(0, 5, 10, 15, 20, 30, 40, 50,60,Inf))))
levels(col_no) <- c("1-5", "5-10", "10-15",
                    "15-20", "20-30", "30-40", "40-50", "50-60", ">60")
gadm2$col_no <- col_no
myPalette<-brewer.pal(9,"Purples")
setEPS()
postscript("output/Landkreis.eps")
spplot(gadm2, "col_no", col=grey(.9), col.regions=myPalette,
       main="'Eröffnungen' by Landkreis")
dev.off()



#Plot by Bundesland
bundesl.insolvencies <- data.frame(gadm1$NAME_1)
for(b in 1:length(gadm1$NAME_1)){
  bundesl.insolvencies$insolvencies[b] <- sum(insolvencies[insolvencies$gadm3.NAME_1 == gadm1$NAME_1[b],4])
}

col_no <- as.factor(as.numeric( cut(bundesl.insolvencies$insolvencies, 
                                    breaks = c(0, 10, 20, 30, 40, 50, 100, 150,200, Inf))))
levels(col_no) <- c("0-10", "10-20", "20-30",
                    "30-40", "40-50", "50-100", "100-150", "150-200", ">200")
gadm1$col_no <- col_no
myPalette<-brewer.pal(9,"Purples")
setEPS()
postscript("output/Bundesland.eps")
spplot(gadm1, "col_no", col=grey(.9), col.regions=myPalette,
       main="'Eröffnungen' by Bundesland")
dev.off()
