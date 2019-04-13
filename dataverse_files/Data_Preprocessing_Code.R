################################################################
# Data Pre-Processing for "Partners In Crime" 
# Averell Schmidt and Kathryn Sikkink
# Last Updated: July 5, 2018
################################################################

#-------------------------------------------------------------------
# This code produces the dataset used to conduct the analyses presented 
# in the main article and supplementary materials. Note that the imputation 
# model in this codes takes roughly 43 hours to run. We ran this pre-processing
# code using the research computing environment at Harvard's Institute for 
# Quantitative Social Science. We then conducted all subsequent analyses 
# on our own computers. 
#-------------------------------------------------------------------

# Install and load required packages if they are missing
rm(list=ls())
list.of.packages <- c("foreign", "plyr", "dplyr", "DataCombine", "Amelia")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages)}
lapply(list.of.packages, require, character.only = TRUE)
rm(list.of.packages, new.packages)

# Set work drive 
setwd("/nfs/home/A/als789/shared_space/cab_proj/Schmidt_Sikkink/Data Preprocessing") # This must be set to your own working drive

#----------------------------------------------------------------------------------
#             Data-preprocessing for Polity IV 
#----------------------------------------------------------------------------------
# Marshall, Gurr, Jaggers (2014) "Political Regime Characteristics and Transitions, 1800-2013," 
# POLITY IV PROJECT, available at: http://www.systemicpeace.org/inscr/p4v2014.xls
#----------------------------------------------------------------------------------

# 2. Load Polity IV data
p4 <- read.csv("p4v2015.csv")

#----------------------------------------------------------------------------------
# Note: There are a few inconsistencies between the COW country codes used in the Polity IV dataset
# and those the Fariss dataset. There are discrepencies for the states of the former Yugoslavia, 
# Ethiopia, the Palestinian Territories, and Vietnam. The following code addresses these issues in the
# Polity IV dataset (Palestinian Territories are dealt with below when processing the Fariss dataset). 
#-----------------------------------------------------------------------------------

p4$ccode[p4$year > 1993	& p4$ccode ==	529] <- 530 # fixes Ethiopia
p4$ccode[p4$year > 1976	& p4$ccode ==	818] <- 816 # fixes Vietnam
p4$ccode[p4$ccode ==	341] <- 699 # fixes Kosovo (note: Fariss drops Kosovo)
p4$ccode[p4$year >= 2006 & p4$ccode == 348] <- 341 # fixes Montenegro
p4 <- subset(p4, !(p4$ccode ==345 & p4$year >= 1992)) # removes Yugoslavia post-1992
p4$ccode[p4$year >= 2007 & p4$ccode == 342] <- 345 # fixes Serbia
p4 <- subset(p4, p4$ccode !=342 | p4$year != 2006)# removes duplicate created by split of Serbia and Montenegro in 2006
p4$ccode[p4$year <= 2006 & p4$year >= 2003 & p4$ccode == 347] <- 345 # fixes Serbia & Montenegro
p4$ccode[p4$year <= 2002 & p4$year >= 1992 & p4$ccode == 347] <- 345 # cleans up Yugoslavia for years 1992-2002
p4$cyear <- ((p4$ccode*10000)+p4$year) # updates cyear accordingly
p4 <- subset(p4, (p4$year > 1990 & p4$year < 2012)) # drop unnecessary years

#----------------------------------------------------------------------------------
#               Data-preprocessing for Global Terrorism Database
#----------------------------------------------------------------------------------
# Data: National Consortium for the Study of Terrorism and Responses to Terrorism (START). 
# (2016). Global Terrorism Database [Data file]. 
# Retrieved from https://www.start.umd.edu/gtd
#----------------------------------------------------------------------------------

# Version 08/2014 is available in the Data Preprocessing folder
gtd <- read.csv("globalterrorismdb_0814dist.csv")

# Drop unnecessary years
gtd <- subset(gtd, (gtd$iyear > 1990 & gtd$iyear < 2012))

# Drop small countries and those not included in Polity IV database
gtd <- subset(gtd, country != 10) # Antigua and Barbuda
gtd <- subset(gtd, country != 17) # Bahamas 
gtd <- subset(gtd, country != 20) # Barbados
gtd <- subset(gtd, country != 22) # Belize
gtd <- subset(gtd, country != 31) # Brunei
gtd <- subset(gtd, country !=	40) #	Cayman Islands
gtd <- subset(gtd, country !=	57) #	Dominica
gtd <- subset(gtd, country !=	70) #	French Guiana
gtd <- subset(gtd, country !=	71) #	French Polynesia
gtd <- subset(gtd, country !=	89) #	Hong Kong
gtd <- subset(gtd, country !=	422) # International
gtd <- subset(gtd, country !=	117) # Macau
gtd <- subset(gtd, country !=	122) # Maldives
gtd <- subset(gtd, country !=	124) # Malta
gtd <- subset(gtd, country !=	127) # Martinique
gtd <- subset(gtd, country !=	189) # St. Kitts and Nevis
gtd <- subset(gtd, country !=	190) # St. Lucia
gtd <- subset(gtd, country !=	143) # New Caledonia
gtd <- subset(gtd, country !=	220) # Vanuatu
gtd <- subset(gtd, country !=	226) # Wallis and Futuna

# Change country names for merging with Polity IV to generate COW country code
levels(gtd$country_txt)[levels(gtd$country_txt) == "Bosnia-Herzegovina"] <- "Bosnia"
levels(gtd$country_txt)[levels(gtd$country_txt) == "Congo (Brazzaville)"] <- "Congo Brazzaville"
levels(gtd$country_txt)[levels(gtd$country_txt) == "Congo (Kinshasa)"] <- "Congo Kinshasa"
levels(gtd$country_txt)[levels(gtd$country_txt) == "Corsica"] <- "France"
levels(gtd$country_txt)[levels(gtd$country_txt) == "Timor-Leste"]	<- "East Timor"
levels(gtd$country_txt)[levels(gtd$country_txt) == "Great Britain"] <- "United Kingdom"
levels(gtd$country_txt)[levels(gtd$country_txt) == "Myanmar"]	<- "Myanmar (Burma)"
levels(gtd$country_txt)[levels(gtd$country_txt) == "North Korea"]	<- "Korea North"
levels(gtd$country_txt)[levels(gtd$country_txt) == "Northern Ireland"] <- "United Kingdom"
levels(gtd$country_txt)[levels(gtd$country_txt) == "South Korea"] <- "Korea South"
levels(gtd$country_txt)[levels(gtd$country_txt) == "Puerto Rico"] <- "United States"
levels(gtd$country_txt)[levels(gtd$country_txt) == "United Arab Emirates"] <- "UAE"
levels(gtd$country_txt)[levels(gtd$country_txt) == "Virgin Islands (U.S.)"] <- "United States"
levels(gtd$country_txt)[levels(gtd$country_txt) == "Serbia-Montenegro"] <- "Serbia and Montenegro"
levels(gtd$country_txt)[levels(gtd$country_txt) == "West Bank and Gaza Strip"] <- "Israel" 
levels(gtd$country_txt)[levels(gtd$country_txt) == "Western Sahara"] <- "Morocco"

# Generate terrorist incident count and merge with Polity IV to generate country year code
gtd$gtd <- 1 # Basis for a variable counting terrorist attacks in each country year
gtd <- gtd %>% group_by(country_txt, iyear) %>% summarise(gtd = sum(gtd, na.rm = TRUE))
names(gtd)[names(gtd)=="country_txt"] <- "country"
cow_key <- unique(subset(p4, select = c(ccode, country)))
gtd <- merge(cow_key, gtd, by="country", all.x = TRUE, all.y = TRUE)
gtd <- subset(gtd, select = c(country, ccode, gtd, iyear)) # Drop unnecessary variables generated by merge
gtd$cyear <- ((gtd$ccode * 10000) + gtd$iyear) # Generate country year ID for merging

#---------------------------------------------------------------
#    Merging of GTD data with Polity 4 Datasets 
#---------------------------------------------------------------

p4 <- merge(gtd, p4, by="cyear", all = TRUE)
# Clean up variable names and duplicate observations
p4 <- subset(p4, is.na(p4$year) == FALSE) # Drops duplicate observations for Kosovo, Ethiopia, Montenegro and East Timor
p4$country.x <- p4$ccode.x <- p4$iyear <- NULL
names(p4)[names(p4)=="country.y"] <- "country"
names(p4)[names(p4)=="ccode.y"] <- "ccode"

#----------------------------------------------------------------------------------
#          Data-preprocessing of Human Rights Data (from Fariss dataset)
#----------------------------------------------------------------------------------
# Data: Fariss, Christopher, 2014, "Latent Human Rights Protection Scores Version 2", 
# available at: http://dx.doi.org.ezp-prod1.hul.harvard.edu/10.7910/DVN/24872, Harvard Dataverse, V4 
# Download "HumanRightsProtectionScores_v2.04.csv" and save it in the working directory 
# in order to run this replication code.
#----------------------------------------------------------------------------------

fariss <- read.csv("HumanRightsProtectionScores_v2.04.csv")
fariss <- subset(fariss, !(fariss$COW > 666 & fariss$COW < 667)) # removes The Palestinian Territories
fariss$cyear <- ((fariss$COW*10000)+fariss$YEAR) # updates cyear 

#---------------------------------------------------------------
#    Merging of Fariss and Polity 4 Datasets 
#---------------------------------------------------------------
# This code merges the Fariss and Polity 4 datasets and drops country-years
# outside the 1992-2011 period as well as those without polity2 scores
#---------------------------------------------------------------

data <- merge(fariss, p4, by="cyear", all = TRUE)
data$YEAR[is.na(data$YEAR)] <- data$year[is.na(data$YEAR)] # Fixes missing YEAR values created by merge
data <- tbl_df(data) %>% filter(YEAR > 1990 & YEAR < 2012 ) # This drops years outside the 1991-2011 period
data <- subset(data, data$cyear !=5291993) # Removes Ethiopia in 1993, which is a blank observation 
data$COW[is.na(data$COW)] <- ((data$cyear - data$YEAR)/10000)[is.na(data$COW)] # Creates COW for Kosovo and South Sudan 

#---------------------------------------------------------------
# This code creates dummy variables from the OSJI report, denoting when and whom participated in the U.S. RDI program.
# (Note: In this analysis, variable are generated for all countries that paricipated in the RDI program -- both 
# actively and passively -- in the article only active participants are considered.)
#---------------------------------------------------------------

# Expand time series so that all countries are represented in each year. This is needed  
# to create a normalized time sereis index. Country years for which a country did not exist are dropped below.  
data <- TimeExpand(data, GroupVar = 'COW', TimeVar = 'YEAR')
data$cyear <- (data$COW * 10000 + data$YEAR)

# This creates treatment variables
data$part_t <- 0				
data$part_t[data$YEAR >=	2002	& data$COW ==	20	] <- 1	#	Canada
data$part_t[data$YEAR >=	2001	& data$COW ==	200	] <- 1	#	United Kingdom
data$part_t[data$YEAR >=	2002	& data$COW ==	205	] <- 1	#	Ireland
data$part_t[data$YEAR >=	2001	& data$COW ==	211	] <- 1	#	Belgium
data$part_t[data$YEAR >=	2003	& data$COW ==	230	] <- 1	#	Spain
data$part_t[data$YEAR >=	2001	& data$COW ==	235	] <- 1	#	Portugal
data$part_t[data$YEAR >=	2001	& data$COW ==	255	] <- 1	#	Germany
data$part_t[data$YEAR >=	2002	& data$COW ==	290	] <- 1	#	Poland
data$part_t[data$YEAR >=	2003	& data$COW ==	305	] <- 1 	#	Austria
data$part_t[data$YEAR >=	2003	& data$COW ==	316	] <- 1	#	Czech Republic
data$part_t[data$YEAR >=	2002	& data$COW ==	325	] <- 1	#	Italy
data$part_t[data$YEAR >=	2004	& data$COW ==	339	] <- 1 	#	Albania
data$part_t[data$YEAR >=	2003	& data$COW ==	343	] <- 1	#	Macedonia
data$part_t[data$YEAR >=	2005	& data$COW ==	344	] <- 1	#	Croatia
data$part_t[data$YEAR >=	2001	& data$COW ==	346	] <- 1	#	Bosnia-Herzegovina
data$part_t[data$YEAR >=	2002	& data$COW ==	350	] <- 1	#	Greece
data$part_t[data$YEAR >=	2002	& data$COW ==	352	] <- 1	#	Cyprus
data$part_t[data$YEAR >=	2002	& data$COW ==	360	] <- 1	#	Romania
data$part_t[data$YEAR >=	2002	& data$COW ==	368	] <- 1	#	Lithuania
data$part_t[data$YEAR >=	2002	& data$COW ==	372	] <- 1	#	Georgia
data$part_t[data$YEAR >=	2001	& data$COW ==	373	] <- 1	#	Azerbaijan
data$part_t[data$YEAR >=	2002	& data$COW ==	375	] <- 1	#	Finland
data$part_t[data$YEAR >=	2001	& data$COW ==	380	] <- 1	#	Sweden
data$part_t[data$YEAR >=	2003	& data$COW ==	390	] <- 1	#	Denmark
data$part_t[data$YEAR >=	2001	& data$COW ==	395	] <- 1	#	Iceland
data$part_t[data$YEAR >=	2002	& data$COW ==	420	] <- 1	#	Republic of the Gambia
data$part_t[data$YEAR >=	2001	& data$COW ==	435	] <- 1	#	Mauritania
data$part_t[data$YEAR >=	2003	& data$COW ==	501	] <- 1	#	Kenya
data$part_t[data$YEAR >=	2002	& data$COW ==	520	] <- 1	#	Somalia
data$part_t[data$YEAR >=	2003	& data$COW ==	522	] <- 1	#	Djibouti
data$part_t[data$YEAR >=	2002	& data$COW ==	530	] <- 1	#	Ethiopia
data$part_t[data$YEAR >=	2003	& data$COW ==	552	] <- 1	#	Zimbabwe
data$part_t[data$YEAR >=	2003	& data$COW ==	553	] <- 1	#	Malawi
data$part_t[data$YEAR >=	2003	& data$COW ==	560	] <- 1	#	South Africa
data$part_t[data$YEAR >=	2002	& data$COW ==	600	] <- 1	#	Morocco
data$part_t[data$YEAR >=	2004	& data$COW ==	615	] <- 1 	#	Algeria
data$part_t[data$YEAR >=	2004	& data$COW ==	620	] <- 1	#	Libya
data$part_t[data$YEAR >=	2002	& data$COW ==	630	] <- 1	#	Iran
data$part_t[data$YEAR >=	2002	& data$COW ==	640	] <- 1	#	Turkey
data$part_t[data$YEAR >=	2001	& data$COW ==	651	] <- 1	#	Egypt
data$part_t[data$YEAR >=	2001	& data$COW ==	652	] <- 1	#	Syria
data$part_t[data$YEAR >=	2001	& data$COW ==	663	] <- 1	#	Jordan
data$part_t[data$YEAR >=	2003	& data$COW ==	670	] <- 1	#	Saudi Arabia
data$part_t[data$YEAR >=	2005	& data$COW ==	679	] <- 1	#	Yemen
data$part_t[data$YEAR >=	2002	& data$COW ==	696	] <- 1	#	United Arab Emirates
data$part_t[data$YEAR >=	2001	& data$COW ==	700	] <- 1 	#	Afghanistan
data$part_t[data$YEAR >=	2002	& data$COW ==	704	] <- 1	#	Uzbekistan
data$part_t[data$YEAR >=	2001	& data$COW ==	770	] <- 1	#	Pakistan
data$part_t[data$YEAR >=	2003	& data$COW ==	780	] <- 1	#	Sri Lanka
data$part_t[data$YEAR >=	2002	& data$COW ==	800	] <- 1	#	Thailand
data$part_t[data$YEAR >=	2004	& data$COW ==	820	] <- 1	#	Malaysia
data$part_t[data$YEAR >=	2002	& data$COW ==	850	] <- 1	#	Indonesia
data$part_t[data$YEAR >=	2001	& data$COW ==	900	] <- 1 	#	Australia

data$active_t <- 0				
data$active_t[data$YEAR >=	2002	& data$COW ==	20	] <- 1	#	Canada
data$active_t[data$YEAR >=	2001	& data$COW ==	200	] <- 1	#	United Kingdom
data$active_t[data$YEAR >=	2001	& data$COW ==	255	] <- 1	#	Germany
data$active_t[data$YEAR >=	2002	& data$COW ==	290	] <- 1	#	Poland
data$active_t[data$YEAR >=	2002	& data$COW ==	325	] <- 1	#	Italy
data$active_t[data$YEAR >=	2004	& data$COW ==	339	] <- 1	#	Albania
data$active_t[data$YEAR >=	2003	& data$COW ==	343	] <- 1	#	Macedonia
data$active_t[data$YEAR >=	2001	& data$COW ==	346	] <- 1	#	Bosnia-Herzegovina
data$active_t[data$YEAR >=	2002	& data$COW ==	360	] <- 1	#	Romania
data$active_t[data$YEAR >=	2002	& data$COW ==	368	] <- 1	#	Lithuania
data$active_t[data$YEAR >=	2002	& data$COW ==	372	] <- 1	#	Georgia
data$active_t[data$YEAR >=	2001	& data$COW ==	373	] <- 1	#	Azerbaijan
data$active_t[data$YEAR >=	2001	& data$COW ==	380	] <- 1	#	Sweden
data$active_t[data$YEAR >=	2002	& data$COW ==	420	] <- 1	#	Republic of the Gambia
data$active_t[data$YEAR >=	2001	& data$COW ==	435	] <- 1	#	Mauritania
data$active_t[data$YEAR >=	2003	& data$COW ==	501	] <- 1	#	Kenya
data$active_t[data$YEAR >=	2002	& data$COW ==	520	] <- 1	#	Somalia
data$active_t[data$YEAR >=	2003	& data$COW ==	522	] <- 1	#	Djibouti
data$active_t[data$YEAR >=	2002	& data$COW ==	530	] <- 1	#	Ethiopia
data$active_t[data$YEAR >=	2003	& data$COW ==	552	] <- 1	#	Zimbabwe
data$active_t[data$YEAR >=	2003	& data$COW ==	553	] <- 1	#	Malawi
data$active_t[data$YEAR >=	2003	& data$COW ==	560	] <- 1	#	South Africa
data$active_t[data$YEAR >=	2002	& data$COW ==	600	] <- 1	#	Morocco
data$active_t[data$YEAR >=	2004	& data$COW ==	615	] <- 1	#	Algeria
data$active_t[data$YEAR >=	2004	& data$COW ==	620	] <- 1	#	Libya
data$active_t[data$YEAR >=	2002	& data$COW ==	630	] <- 1	#	Iran
data$active_t[data$YEAR >=	2002	& data$COW ==	640	] <- 1	#	Turkey
data$active_t[data$YEAR >=	2001	& data$COW ==	651	] <- 1	#	Egypt
data$active_t[data$YEAR >=	2001	& data$COW ==	652	] <- 1	#	Syria
data$active_t[data$YEAR >=	2001	& data$COW ==	663	] <- 1	#	Jordan
data$active_t[data$YEAR >=	2003	& data$COW ==	670	] <- 1	#	Saudi Arabia
data$active_t[data$YEAR >=	2005	& data$COW ==	679	] <- 1	#	Yemen
data$active_t[data$YEAR >=	2002	& data$COW ==	696	] <- 1	#	United Arab Emirates
data$active_t[data$YEAR >=	2001	& data$COW ==	700	] <- 1	#	Afghanistan
data$active_t[data$YEAR >=	2002	& data$COW ==	704	] <- 1	#	Uzbekistan
data$active_t[data$YEAR >=	2001	& data$COW ==	770	] <- 1	#	Pakistan
data$active_t[data$YEAR >=	2002	& data$COW ==	800	] <- 1	#	Thailand
data$active_t[data$YEAR >=	2004	& data$COW ==	820	] <- 1	#	Malaysia
data$active_t[data$YEAR >=	2002	& data$COW ==	850	] <- 1	#	Indonesia
data$active_t[data$YEAR >=	2001	& data$COW ==	900	] <- 1	#	Australia

# This generates dummy variable noting the treatment and control groups
data <- tbl_df(data) %>% group_by(COW) %>% mutate(active_d = max(active_t), part_d = max(part_t)) 

#--------------------------------------------------------------------------
# This code creates a staggered time series where year 1 is, for participants, the year that
# countries first began participation in the program and, for non-participants, 2001.
# Blank country years are deleted after merging with covariate dataset. 
#--------------------------------------------------------------------------

data <- data[order(data$COW, data$YEAR),] # Needs to be sorted by group and year first 
data$t <- data$part_t # Creates vector and assigns 1 to all treatment country-years
for (i in 1:length(data$t)){ # This step creates a series for treatment countries beginning at year 1 
  if (data$t[i] == 1){
    data$t[i] <- data$t[i] + data$t[i-1]
  }
}
data <- tbl_df(data) %>% group_by(COW) %>% mutate(t = (max(t)+ YEAR - 2011)) # this step creates negative years for treatment group
for (i in 1:length(data$YEAR)){ # This replaces the time series for all non-participant countries with 2000 = to year 0   
  if (data$part_d[i] == 0){ 
    data$t[i] <- (data$YEAR[i] - 2000) # The number here sets year 0  
  }
}

#----------------------------------------------------------------------------------
#                     Data-preprocessing and merge for Covariates
#----------------------------------------------------------------------------------
# Data for covariates is taken from the Quality of Governance database. There are a few 
# problems with these data that must be cleaned up before it can be merged with the other data. 
#----------------------------------------------------------------------------------

# Import QOG data
qog <- read.csv("qog_std_ts_jan16.csv") 

# Code to clean QOG and merge it with other data
qog <- tbl_df(qog) %>% filter(year > 1990 & year < 2012) # Drops unnecessary years
names(qog)[names(qog)=="ccodecow"] <- "COW" # Rename ccodecow COW
qog <- subset(qog, !(qog$ccode == 890 & qog$year > 2002)) # Removes Yugoslavia post 2002                            
qog$COW[qog$year >= 2007	& qog$ccode ==	688] <- 345 # Assigns COW code to Serbia 2007-2011
qog$COW[qog$year <= 2006 & qog$year >= 2003 & qog$ccode == 891] <- 345 # assigns COW code to Serbia & Montenegro in 2003-2006
qog <- subset(qog, (is.na(qog$COW) == FALSE)) # This addresses the remaining missing values including: 
    # Ethiopia (-1992); Serbia; Sudan (2012-); USSR; Serbia and Montenegro; France (-1962); 
    # Malaysia (-1965); Cyprus (-1974); Tibet; Pakistan (-1970); Vietnam, North 
qog$cyear <- ((qog$COW * 10000) + qog$year) #creates a key (cyear) for merge

# Merging of QOG with other dataset
d <- merge(data, qog, by="cyear", all = TRUE)
names(d)[names(d)=="COW.x"] <- "COW" # Rename ccodecow COW
d <- subset(d, (is.na(d$COW) == FALSE)) # This drops observations from West Germany, East Germany; North Yemen; 
# South Yemen; and South Vietnam left over from QOG database (these countries do not exist during study period)
data <- d # Backs up the data as "d" before cleaning it up


#----------------------------------------------------------------------------------
#       Importing of US Trade Data to dataset
#----------------------------------------------------------------------------------
# US bilateral trade data for all countries is taken from the US census bureau 
# website (https://www.census.gov/foreign-trade/balance/index.html).  
# The file itself in csv format is in the Data Preprocessing folder. 
#----------------------------------------------------------------------------------

# Import US Census Bureau data 
trade <- read.csv("country.csv")
# Drop US regional, global, and organizational trade data (e.g. US trade with Asia, North America, IOs, etc.) 
trade <- subset(trade, (trade$CTY_CODE > 1000 & trade$CTY_CODE < 8000)) 
# Drop unnecessary years
trade <- subset(trade, (trade$year > 1990 & trade$year < 2012)) 
# Rename countries with different names
levels(trade$CTYNAME)[levels(trade$CTYNAME) == "French Guiana"] <- "Guyana"
levels(trade$CTYNAME)[levels(trade$CTYNAME) == "Slovakia"] <- "Slovak Republic"
levels(trade$CTYNAME)[levels(trade$CTYNAME) == "Yugoslavia (former)"] <- "Yugoslavia"
levels(trade$CTYNAME)[levels(trade$CTYNAME) == "Bosnia and Herzegovina"] <- "Bosnia"
levels(trade$CTYNAME)[levels(trade$CTYNAME) == "United Arab Emirates"] <- "UAE"
levels(trade$CTYNAME)[levels(trade$CTYNAME) == "Republic of Yemen"] <- "Yemen"
levels(trade$CTYNAME)[levels(trade$CTYNAME) == "Burma"] <- "Myanmar (Burma)"
levels(trade$CTYNAME)[levels(trade$CTYNAME) == "Korea, North"] <- "Korea North"
levels(trade$CTYNAME)[levels(trade$CTYNAME) == "Korea, South"] <- "Korea South"
levels(trade$CTYNAME)[levels(trade$CTYNAME) == "Cote d'Ivoire"] <- "Ivory Coast"
levels(trade$CTYNAME)[levels(trade$CTYNAME) == "Congo (Brazzaville)"] <- "Congo Brazzaville"
levels(trade$CTYNAME)[levels(trade$CTYNAME) == "Congo (Kinshasa)"] <- "Congo Kinshasa"
levels(trade$CTYNAME)[levels(trade$CTYNAME) == "Cabo Verde"] <- "Cape Verde"
# Compute total trade (imports plus exports)
trade$totaltrade <- (trade$IYR + trade$EYR)
# Drop unnecessary data (all but name, year, trade)
trade <- tbl_df(trade)%>% subset(select = c(year, CTYNAME, totaltrade)) 
# Rename columns appropriately for merge
names(trade)[names(trade)=="year"] <- "YEAR" # rename year YEAR
names(trade)[names(trade)=="CTYNAME"] <- "country" # rename CTYNAME country
# Merge by name and year with original data
data <- merge(data, trade, by=c("country","YEAR"), all.x = TRUE, all.y = TRUE)
data <- filter(data, is.na(data$cyear) == FALSE) 
# Rescale total trade so that it is not in millions, but logged dollars, add 1 to deal with zeros being logged to negative infinity
data$totaltrade <- log(data$totaltrade*1000000 + 1)

#----------------------------------------------------------------------------------
#       Adding of US Military Assistance data to dataset
#----------------------------------------------------------------------------------
# US military assistnace data is downloaded from USAID's Aid Explorer 
# website: https://explorer.usaid.gov/data.html
# A direct link to the data is: https://explorer.usaid.gov/prepared/us_foreign_aid_country.csv
# The file in .csv format is in the Data Preprocessing folder.
# Note: in the year 1976, several entries have "1976tq" in the fiscal_year column. To 
# avoid this column being recognized by R as a factor, we have deleted these entries in excel 
# before loading the data here. Also note that we use obligations (what the USG has legally 
# committed itself to) as the variable of interest rather than disbursements (what has been 
# paid out of government accounts), because data for disbursements is not available before 2000. 
#----------------------------------------------------------------------------------
 
# Import US Military Assistance data
aid <- read.csv("us_foreign_aid_country.csv") 
# Drop unnecessary years
aid <- subset(aid, (aid$fiscal_year > 1990 & aid$fiscal_year < 2012)) 
# Drop economic aid and obligations (we focus on disbursements)
aid <- filter(aid, (aid$assistance_category_name == "Military" & aid$transaction_type_name == "Obligations"))
# Rename relevant countries
levels(aid$country_name)[levels(aid$country_name) == "Cabo Verde"] <- "Cape Verde"
levels(aid$country_name)[levels(aid$country_name) == "China (P.R.C.)"] <- "China"
levels(aid$country_name)[levels(aid$country_name) == "Congo (Brazzaville)"] <- "Congo Brazzaville"
levels(aid$country_name)[levels(aid$country_name) == "Congo (Kinshasa)"] <- "Congo Kinshasa"
levels(aid$country_name)[levels(aid$country_name) == "Czechia"] <- "Czech Republic"
levels(aid$country_name)[levels(aid$country_name) == "Korea Republic"] <- "Korea South"
levels(aid$country_name)[levels(aid$country_name) == "Korea, Democratic Republic"] <- "Korea North"
levels(aid$country_name)[levels(aid$country_name) == "Cote d'Ivoire"] <- "Ivory Coast"
levels(aid$country_name)[levels(aid$country_name) == "Bosnia and Herzegovina"] <- "Bosnia"
levels(aid$country_name)[levels(aid$country_name) == "Timor-Leste"] <- "East Timor"
levels(aid$country_name)[levels(aid$country_name) == "United Arab Emirates"] <- "UAE"
levels(aid$country_name)[levels(aid$country_name) == "Serbia and Montenegro (former)"] <- "Serbia and Montenegro"
levels(aid$country_name)[levels(aid$country_name) == "Sudan (former)"] <- "Sudan"
levels(aid$country_name)[levels(aid$country_name) == "Burma (Myanmar)"] <- "Myanmar (Burma)"

# Drop unnecessary data (all but name, year, trade)
aid <- tbl_df(aid)%>% subset(select = c(fiscal_year, country_name, current_amount, constant_amount)) 
# Rename columns appropriately for merge
names(aid)[names(aid)=="fiscal_year"] <- "YEAR" # rename fiscal_year YEAR
names(aid)[names(aid)=="country_name"] <- "country" # rename country_name country
# Merge by name and year with original data
data <- merge(data, aid, by=c("country","YEAR"), all.x = TRUE, all.y = TRUE)
data <- filter(data, is.na(data$cyear) == FALSE)
# Rescale constant_amount variable so that it is in logged dollars
data$constant_amount <- log(data$constant_amount)

#--------------------------------------------------------------------------
# Cleaning Up and Saving Dataset before Imputation
#--------------------------------------------------------------------------

# Drops non-independent, small countries as identified by the Polity IV project.
# (note: this also drops missing country years generated by the expansion above -- see lines 186-190)
data <- tbl_df(data) %>% filter(is.na(polity2) == FALSE) 
data$exconst[data$exconst < 0] <- NA # fixes -88s in exconst data
data <- subset(data, COW !=2)# Drops the United states
# Note: there are 31 duplicated rows created by the addition of the aid and trade data above specifically for Guyana, Ethiopia, Israel, and Sudan. 
# View(data[duplicated(data$cyear),]) # This code shows the duplicated rows
data <- data[!duplicated(data$cyear), ] # This code removes the duplicated rows

# This computes a dummy variable noting whether or not the country is in transition. We defined transition 
# as any country in existence for less than 5 years (from the durable variable in the Polity4 dataset)
data$trans <- 0
for (i in 1:length(data$durable)){  
  if (data$durable[i] > 5){
    data$trans[i] <- 0 
  }else if(data$durable[i] <= 5){
    data$trans[i] <- 1
  }  
}

#Computing log GDP Per Capita and Log Population
data$log_pop <- log(data$wdi_pop) # Log of WDI Population
data$log_gdppc <- log(data$wdi_gdppccon) #Log of WDI PC GDP in constant 2005 USD 

# Invert Scale on PTS data so that it aligns with Fariss and CIRI 
data$Amnesty <- (data$Amnesty - 6) * -1
data$State <- (data$State - 6) * -1

# Create a categorical vairable noting whether or not countries are democracies based on whether a country's average polity score was above or below 7. 
quant_vals <- tbl_df(data) %>% group_by(COW) %>% summarise(polity2 = mean(polity2, na.rm=T), active = max(active_d))
states_above <- unique(subset(quant_vals, polity2 >= 7, select = c(COW, active, polity2)))
states_below <- unique(subset(quant_vals, polity2 < 7, select = c(COW, active, polity2)))

# Create relevant variables in main dataset
data$above_polity <- as.integer((data$COW %in% states_above$COW)) # Generates a dummy variable for countries with 7 or above average polity scores 
data$below_polity <- as.integer((data$COW %in% states_below$COW)) # Generates a dummy variable for countries with less than 7 average polity scores 

# This drops variables not used in analysis
data <- tbl_df(data)%>%  
  subset(select = c(cyear, YEAR, COW, country, t,
                    active_d, active_t, 
                    latentmean, Amnesty, State, 
                    DISAP, KILL, TORT, POLPRIS, 
                    above_polity, below_polity,
                    ucdp_type3, gtd, trans, polity2,
                    log_pop, log_gdppc, 
                    totaltrade, constant_amount))

# Gives variables in dataset better names
names(data)[names(data)=="totaltrade"] <- "log_UStrade"
names(data)[names(data)=="constant_amount"] <- "log_USmilaid"
# set as dataframe
data <- as.data.frame(data) 
# drop unnecessary data 
aid <- cow_key <- d <- fariss <- gtd <- i <- p4 <- qog <- quant_vals  <- states_above  <-  states_below <- trade <- NULL 
save(data, file = "pic_data_not_imputed.RData") # Save unimputed data

#-----------------------------------------------
#    Multiple Imputation
#-----------------------------------------------

head(data) # inspect data
dim(data) # How many variables? 

# Number of missing values in data
sapply(data, function(x) sum(is.na(x)))

# Code to determine number of imputations necessary (Lall 2016)
NAs <- function(x) {
  as.vector(apply(x, 2, function(x) length(which(is.na(x)))))
}
NAs(data) # same as sapply funciton above
mean(NAs(data)/nrow(data))*100 # Number of Imputations Necessary 

# Set Number of Imputations
imps <- ceiling(mean(NAs(data)/nrow(data))*100)

# Vector of variable names
vars <- c("active_t", "latentmean", "Amnesty", "State", "DISAP", "KILL", "TORT", "POLPRIS",  
          "ucdp_type3", "gtd", "trans", "polity2", "log_pop", "log_gdppc", "log_UStrade", "log_USmilaid")

## Run MI model (takes about two days): 
set.seed(02138)
system.time(data.out <- amelia(data, m = imps, cs = "COW", ts = "YEAR", intercs = TRUE,
                   # next line excludes index variables and three derived variables from imputation model
                   idvars = c("cyear", "country", "t", "active_d", "above_polity", "below_polity"), 
                   lags = vars, # Lag all variables 
                   polytime = 3, p2s = 2, empri = 0.05*nrow(data))) 

# Save
save(data.out, file = "pic_data_imputed.RData")

# Clean Up Imputed Dataset for Analysis
# Create Dem_Part and Auto_Part
data.out <- transform(data.out,  Dem_Part = above_polity * active_t)
data.out <- transform(data.out,  Auto_Part = below_polity * active_t)

# Construct Lagged DVs and PHYSINT and lag_physint variables 
for (i in 1:imps){
  data.out$imputations[[i]] <- ddply(data.out$imputations[[i]], .(COW), transform, lag_tort = c(NA, TORT[-length(TORT)])) # Lags TORT 1 year
  data.out$imputations[[i]] <- ddply(data.out$imputations[[i]], .(COW), transform, lag_kill = c(NA, KILL[-length(KILL)])) # Lags KILL 1 year
  data.out$imputations[[i]] <- ddply(data.out$imputations[[i]], .(COW), transform, lag_disap = c(NA, DISAP[-length(DISAP)])) # Lags DISAP 1 year
  data.out$imputations[[i]] <- ddply(data.out$imputations[[i]], .(COW), transform, lag_polpris = c(NA, POLPRIS[-length(POLPRIS)])) # Lags POLPRIS 1 year
  data.out$imputations[[i]] <- ddply(data.out$imputations[[i]], .(COW), transform, lag_state = c(NA, State[-length(State)])) # Lags State 1 year
  data.out$imputations[[i]] <- ddply(data.out$imputations[[i]], .(COW), transform, lag_amnesty = c(NA, Amnesty[-length(Amnesty)])) # Lags Amnesty 1 year
  data.out$imputations[[i]] <- ddply(data.out$imputations[[i]], .(COW), transform, lag_latentmean = c(NA, latentmean[-length(latentmean)])) # Lags Fariss 1 year
  data.out$imputations[[i]] <- ddply(data.out$imputations[[i]], .(COW), transform, PHYSINT = KILL + POLPRIS + TORT + DISAP) # Computes PHYSINT
  data.out$imputations[[i]] <- ddply(data.out$imputations[[i]], .(COW), transform, lag_physint = lag_kill + lag_polpris + lag_tort + lag_disap) # Lag PHYSINT 1 year
}

# Drop observations in 1991
for (i in 1:imps){
  data.out$imputations[[i]] <- subset(data.out$imputations[[i]], data.out$imputations[[i]]$YEAR != 1991) 
}

data <- data.out # rename data.out data

# Save Dataset
save(data, file = "pic_data.RData")

