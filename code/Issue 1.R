#Install required packages
list.of.packages <- c("tidyverse", "expss")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

############################################
#Load the raw data and rename variables
############################################
#Get the file paths
insolvency.file <- paste0(getwd(),"/raw_data/insolvency_filings_de_julaug2020_incomplete.csv")
orbis.file      <- paste0(getwd(),"/raw_data/orbis_wrds_de.csv")

#Read the csv. File encoding is set to "UTF-8" in order to deal with umlauts
insolvency.data <- read.csv(insolvency.file, sep=",", fileEncoding="UTF-8")
orbis.data     <- read.csv(orbis.file, sep=",", fileEncoding="UTF-8")


############################################
#Codebook Insolvency Data
############################################

#Gets the names of all variables and variable class
str(insolvency.data)

#Transform the date from character to date
insolvency.data$date <- as.Date(insolvency.data$Date)

#Transform the subject to categorical and get it's categories
insolvency.data$subject <- factor(insolvency.data$Subject)
subjects <- unique(insolvency.data$Subject)

#Check if there are any duplicated rows
dup <- duplicated(insolvency.data)
sum(dup)>0  #There are duplicates. Delete them
insolvency.data <- insolvency.data[!dup,]

#Check if the court_file_number is a unique identifier
length(unique(insolvency.data$court_file_number)) == length(insolvency.data$court_file_number) #No! Even after deleting duplicates there might be multiple entries for each ID

#E.g. 32 IN 103/13, 15 IN 87/20 or 10 IN 27/20
insolvency.data[insolvency.data$court_file_number=="10 IN 27/20",]

##For categorial variables: What are the categories? What do they stand for?
#subject is a categorial variable and hast to be changed from character to factor
insolvency.data$subject <- as.factor(insolvency.data$subject) #Set variable as categorial variable
levels(insolvency.data$subject) #Gives the categories

############################################
#Codebook Orbis
############################################

class(orbis.data) #dataframe

length(orbis.data) # contains 43 variables

#Check if X is the unique identifier 
length(unique(orbis.data$X)) == length(orbis.data$X) # TRUE --> X is the unique identifier in the orbis.data

#Gets the names of all variables and variable class
str(orbis.data) 

summary(orbis.data)

head(orbis.data, 10)

dim(orbis.data)

library(dplyr)

glimpse(orbis.data) ##Displays the type and a preview of all columns as a row so that it's very easy to take in

##For categorial variables: What are the categories? What do they stand for?
#major_sector, nace2_main_sector, category_of_company, status_str, legalfrm, indepind, listed, conscode, filing_type, accpractice, audstatus and source are categorial variables
#they have to be changed from character to factor

orbis.data$major_sector <- as.factor(orbis.data$major_sector) #Set variable as categorial variable
levels(orbis.data$major_sector) #Gives the categories

orbis.data$nace2_main_section <- as.factor(orbis.data$nace2_main_section)
levels(orbis.data$nace2_main_section)

orbis.data$category_of_company <- as.factor(orbis.data$category_of_company)
levels(orbis.data$category_of_company)

orbis.data$status_str <- as.factor(orbis.data$status_str)
levels(orbis.data$status_str)

orbis.data$legalfrm <- as.factor(orbis.data$legalfrm)
levels(orbis.data$legalfrm)

orbis.data$indepind <- as.factor(orbis.data$indepind)
levels(orbis.data$indepind)

orbis.data$listed <- as.factor(orbis.data$listed)
levels(orbis.data$listed)

orbis.data$conscode <- as.factor(orbis.data$conscode)
levels(orbis.data$conscode)

orbis.data$filing_type <- as.factor(orbis.data$filing_type)
levels(orbis.data$filing_type)

orbis.data$accpractice <- as.factor(orbis.data$accpractice)
levels(orbis.data$accpractice)

orbis.data$audstatus <- as.factor(orbis.data$audstatus)
levels(orbis.data$audstatus)

orbis.data$source <- as.factor(orbis.data$source)
levels(orbis.data$source)


#########################################


library(expss)
insolvency.data <- apply_labels(insolvency.data,
                                insolvency_court="Court responsible for conducting the insolvency proceedings in accordance with paragraph 3 of the Insolvenzverordnung",
                                court_file_number ="Identifier for a case of a certain court (does not identify observation). Contains information on the department and judge in charge, type of insolvency proceeding and opening year",
                                subject = "Information on the current status of the insolvency proceedings",
                                name_ebtor = "Name of the subject filing for insolveny",
                                domicile_debtor = "headquarters of the company")

orbis.data <- apply_labels(orbis.data,
                           X="Unique identifier",
                           ctryiso = "Country ISO codes",
                           bvdid ="Bureau van Dijk’s company identifier, which uniquely identifies each company",
                           name_internat = "International company name. For example ä is translated to ae ",
                           name_native = "Domestic company name",
                           major_sector = "Bureau van Dijk’s industry classification. There are 19 categories",
                           nace2_main_section = "The European Union’s industry classification. There are 21 categories.",
                           naceccod2 = "EU’s four digit number classifying the industry",
                           ussicpcod = "US’s four digit number  classifying the industry (Standard Industrial Classification)",
                           category_of_company = "Bureau van Dijk’s company size categories",
                           status_str= "Categories of legal statuses",
                           legalfrm= "National legal form of a company ",
                           indepind= "Bureau van Dijk’s independence indicator characterising the degree of independence of a company with regard to its shareholders.",
                           listed = "Current Listing Status",
                           year= "Year of the observation",
                           closdate= "Last day of the fiscal year",
                           conscode= "Consolidation code indicating what kind of statements are available",
                           filing_type = "Type of the filing",
                           accpractice = "Accounting Practice",
                           audstatus = "Audit status: indicates the status of a statement with regard to its auditing",
                           source = "Sources for financial accounts of publicly quoted companies",
                           fias = "Fixed Assets: Total amount (after depreciation) of non current assets (Intangible assets + Tangible assets + Other fixed assets).",
                           cuas = "Current Assets: Total amount of current assets (Stocks + Debtors + Other current assets)",
                           stok = "Stocks: Total inventories (raw materials + in progress + finished goods)",
                           debt = "Debtors: Trade receivables (from clients and customers only)",
                           ocas = "Other current Assets: All other current assets such as receivables from other sources (taxes, group Companies), short term investment of money and Cash at bank and in hand.",
                           cash = "Cash and cash equivalent: Detail of the Other current assets =Only the amount of cash at bank and in hand of the Company.",
                           toas = "Total assets: Total assets (Fixed assets + Current assets)",
                           shfd = "Shareholders funds:(Capital + Other shareholders funds)",
                           ncli = "Non current liabilities:Long term liabilities of the company (Long term financial debts + other long term liabilities and provisions)",
                           culi = "Current liabilities:Current liabilities of the company (Loans + Creditors + Other current liabilities)",
                           loan = "Loans: Short term financial debts (e.g. to credit institutions + part of Long term financial debts payable within the year, bonds, etc.)",
                           cred = "Creditors: Debts to suppliers and contractors (trade creditors)",
                           ocli = "Other current liabilities: Other current liabilities such as pension, personnel costs, taxes, intragroup debts, accounts received in advance, etc.",
                           tshf = "Total shareholders funds and liabilities: Total Shareholders funds and liabilities (Shareholders funds + Non current liabilities + Current liabilities)",
                           empl = "Number of employees",
                           opre = "Operating Turnover / Revenue",
                           turn = "Sales",
                           cost = "Cost of Goods Sold",
                           fipl = "Financial Profit / Loss",
                           taxa = "Taxation",
                           exex = "Extraordinary and other Expenses",
<<<<<<< HEAD
                           pl = "Profit (Loss) for Period")                       
=======
                           pl = "Profit (Loss) for Period")  

>>>>>>> 8a5262ee8f80ec114cf6afc07b5f7f3afae6d9ac
