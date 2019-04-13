
####################################################################
# Replication Code for "Partners In Crime" 
# Averell Schmidt and Kathryn Sikkink
# Last Updated: July 5, 2018
####################################################################

#-------------------------------------------------------------------
# This code reproduces the analysis presented in the article as well as
# additional tests that are noted in the article, but only presented 
# in the supplementary materials. These include analysis using data 
# from the Political Terror Scale and the CIRI torture and extrajudicial
# killing scores. An explanation of each variable used in this analysis 
# and a presentation of the results of this analysis are available in the 
# supplementary materials. 
# 
# This replication file consists of seven sections:
#      1. Set Up
#      2. Descriptive Statistics and Analysis of Missing Data
#      3. Data visualization
#      4. Difference-of-Means and Balance Table
#      5. Linear Models
#      6. Replication of analysis using treatment year as time-series index
#      7. Replication of analysis after dropping country-years in which annual
#         US State Department or Amnesty International human rights reports mention 
#         one or more of the prisoners detained in the RDI program 
#-------------------------------------------------------------------


############################################
# Section 1: Set Up
############################################

#---------------------------------------------------------------------------
# In this section we set the working drive, load/install necessary packages, 
# and describe our data files.
#---------------------------------------------------------------------------

# Load (and install, if necessary) required packages 
rm(list=ls()) # Clear workspace
list.of.packages <- c("foreign", "gdata", "plyr", "dplyr", "tidyr", "ggplot2", "stargazer", "plm", "Amelia", "MKmisc", "lmtest")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages)}
lapply(list.of.packages, require, character.only = TRUE)
rm(list.of.packages, new.packages)

# Set Working Drive
setwd("/Users/Avery/Desktop/Schmidt_Sikkink_rep/") # This must be set to your own working drive

## Data: There are three datasets used in this analysis: 
# 1. "pic_data_not_imputed.RData" - this is the initial dataset before imputation
# 2. "pic_data_imputed.RData" - this is the initial imputed dataset, contraining observations from 1991-2011 
# 3. "pic_data.RData" - this is the final dataset used in analysis


################################################################
# Section 2: Descriptive Statistics and Analysis of Missing Data
################################################################

#--------------------------------------------------------------------------
# In this section we present descriptive statistics of our initial and imputed
# datasets and present a number of diagonistic plots to assess the performance
# of our imputation model. 
#--------------------------------------------------------------------------

# Descriptive Statistics of Non-Imputed Dataset
load("pic_data_not_imputed.RData") # Load unimputed data
stargazer(data, digits = 3) # Descriptive statistics of unimputed data

# Descriptative Statistics Imputed Dataset
load("pic_data_imputed.RData") # Load Imputed Data
imps <- length(data.out$imputations) # Set number of imputations

# Combine imputed datasets for plotting
## Drop country variable because it is a factor
for (i in 1:imps){data.out$imputations[[i]] <- data.out$imputations[[i]][, !(names(data.out$imputations[[i]]) %in% "country")]}
# Drop observations in 1991
for (i in 1:imps){data.out$imputations[[i]] <- subset(data.out$imputations[[i]], data.out$imputations[[i]]$YEAR != 1991) }
# Take mean of imputed datasets 
data <- (data.out$imputations[[1]] + 
         data.out$imputations[[2]] + 
         data.out$imputations[[3]] + 
         data.out$imputations[[4]] + 
         data.out$imputations[[5]] + 
         data.out$imputations[[6]] + 
         data.out$imputations[[7]] + 
         data.out$imputations[[8]] + 
         data.out$imputations[[9]])/imps
stargazer(data, digits = 3) # Descriptive statistics of imputed data

# Diagnostics of Imputation Model 
load("pic_data_imputed.RData") # Re-Load Imputed Data
summary(data.out) # Summary of imputation model, note normal convergence and consistency of chain lengths

# Missingness Map
missmap(data.out, legend = F, col = c("indianred", "dodgerblue"),  
        main = "Missing Values in Initial, Non-Imputed, Dataset",
        y.labels = NULL, y.at = NULL, 
        y.cex = 0.6, x.cex = 0.85, margins = c(0, 0))
dev.print(jpeg, "miss_map.jpeg", res=200, height=12, width=10, units="in")

# Alternative Countries:
data <- data[,c("YEAR", "COW", "country", "ucdp_type3", "gtd", "log_USmilaid")]

# TSCS Plots of Saudi Arabia and Niger
# Global Terrorism Database
tscsPlot(data.out, cs = "670", main = "GTD for Saudi Arabia", var = "gtd", ylim = c(-150, 150))
dev.print(jpeg, "gtd_plot1.jpeg", res=200, height=6, width=6, units="in")
tscsPlot(data.out, cs = "436", main = "GTD for Niger", var = "gtd", ylim = c(-150, 150))
dev.print(jpeg, "gtd_plot2.jpeg", res=200, height=6, width=6, units="in")

# US Military Assistance
tscsPlot(data.out, cs = "670", main = "US Military Assistance for Saudi Arabia", var = "log_USmilaid", ylim = c(0, 25))
dev.print(jpeg, "usma_plot1.jpeg", res=200, height=6, width=6, units="in")
tscsPlot(data.out, cs = "436", main = "US Military Assistance for Niger", var = "log_USmilaid", ylim = c(0, 25))
dev.print(jpeg, "usma_plot2.jpeg", res=200, height=6, width=6, units="in")

# UCDP
tscsPlot(data.out, cs = "670", main = "Internal Conflicts for Saudi Arabia", var = "ucdp_type3", ylim = c(-2, 4))
dev.print(jpeg, "ucdp_plot1.jpeg", res=200, height=6, width=6, units="in")
tscsPlot(data.out, cs = "436", main = "Internal Conflicts for Niger", var = "ucdp_type3", ylim = c(-2, 4))
dev.print(jpeg, "ucdp_plot2.jpeg", res=200, height=6, width=6, units="in")

# Overimpution of Internal Conflicts, Amnesty Score, Terrorist Attacks, and US Military Aid
overimpute(data.out, var = "ucdp_type3")
dev.print(jpeg, "over_ucdp.jpeg", res=200, height=6, width=6, units="in")
overimpute(data.out, var = "Amnesty")
dev.print(jpeg, "over_amnesty.jpeg", res=200, height=6, width=6, units="in")
overimpute(data.out, var = "gtd")
dev.print(jpeg, "over_gtd.jpeg", res=200, height=6, width=6, units="in")
overimpute(data.out, var = "log_USmilaid")
dev.print(jpeg, "over_usma.jpeg", res=200, height=6, width=6, units="in")


############################################
# Section 3: Data Visualization 
############################################

#--------------------------------------------------------------------
# In this section we plot six figures showing human rights trends across 
# eight indicators. The first two figures (Part A) are of global human rights trends; 
# the second two (Part B) break global trends down by participation; and the final 
# two (Part C) compare at the human rights practices of non-democracies that 
# participated in the program with those that did not. 
#--------------------------------------------------------------------

load("pic_data.RData") # Load final dataset and take average of imputed datasets for plotting (same as above)
data.out <- data
for (i in 1:imps){data.out$imputations[[i]] <- data.out$imputations[[i]][, !(names(data.out$imputations[[i]]) %in% "country")]} # Drop "country" variable because it is a factor
data <- (data.out$imputations[[1]] + data.out$imputations[[2]] + data.out$imputations[[3]] + data.out$imputations[[4]] + data.out$imputations[[5]] + data.out$imputations[[6]] + data.out$imputations[[7]] + data.out$imputations[[8]] + data.out$imputations[[9]])/imps # Get average values of variables across imputed datasets

#--------------------------------------------
# Part A. Plotting Global Human Rights Trends 
#--------------------------------------------

# Compute global-year averages for all DVs
trends <- data %>% group_by(YEAR) %>% 
  summarise("Physical Integrity Score (CIRI)" = mean(PHYSINT, na.rm = TRUE),
            "Latent Variable Model Score (Fariss)" = mean(latentmean, na.rm = TRUE),
            "State Department Score (PTS)" = mean(State, na.rm = TRUE), 
            "Amnesty International Score (PTS)"  = mean(Amnesty, na.rm = TRUE),
            "Extrajudicial Killing Score (CIRI)" = mean(KILL, na.rm = TRUE),
            "Political Imprisonment Score (CIRI)" = mean(POLPRIS, na.rm = TRUE),
            "Torture Score (CIRI)" = mean(TORT, na.rm = TRUE), 
            "Disappearance Score (CIRI)" = mean(DISAP, na.rm = TRUE))
# List of DV names
DVs <- c("Physical Integrity Score (CIRI)", "Latent Variable Model Score (Fariss)", "Disappearance Score (CIRI)", "Political Imprisonment Score (CIRI)", "Amnesty International Score (PTS)", "State Department Score (PTS)", "Extrajudicial Killing Score (CIRI)", "Torture Score (CIRI)")
graph_data <- gather(trends, type, value, DVs) # Reformat data for graphs 
graph_data$type <- factor(graph_data$type, levels = DVs) # Converts labels to factors to order plot facets 

# The following code creates two dataframes used to set the y-axis range on the facet plots 
facetlims1 <- as.data.frame(cbind(YEAR = c(rep(1992:2011, times = 4)), 
                                  value = c(rep(c(0,8), 10), rep(c(-0.5,1), 10), rep(c(0,2), 10), rep(c(0,2), 10))))
facetlims2 <- as.data.frame(cbind(YEAR = c(rep(1992:2011, times = 4)),
                                  value = c(rep(c(1,5), 10), rep(c(1,5), 10), rep(c(0,2), 10), rep(c(0,2), 10))))
facetlims1$type <- rep(DVs[1:4], times = 1, each = 20) # Assign variable names
facetlims2$type <- rep(DVs[5:8], times = 1, each = 20)
facetlims1$type <- factor(facetlims1$type, levels = DVs) # Converts labels to factors to order plot facets 
facetlims2$type <- factor(facetlims2$type, levels = DVs)

# Subset data to be graphed
graph1 <- subset(graph_data, (graph_data$type %in% DVs[1:4]))
graph2 <- subset(graph_data, (graph_data$type %in% DVs[5:8]))

# Create plots
ggplot(graph1, aes(x=YEAR, y=value)) +
  geom_line() + theme_bw() + facet_wrap(~type, ncol=2, scales = "free") +
  #ggtitle("Respect for Human Rights, 1992-2011") +
  labs(x = "Year", y = "Lower Score = More Abuse; Higher Score = More Respect") +
  geom_vline(xintercept = seq(2001, 2005, by = 0.001), colour="grey", linetype = "solid", alpha = 0.01) +
  geom_blank(data = facetlims1) +
  theme(legend.position= "bottom", legend.title=element_blank(), 
        plot.title = element_text(hjust = 0.5, size=14), 
        text = element_text(size = 13),
        axis.title = element_text(size = 13), axis.text = element_text(size = 10))
ggsave("Figure_1.jpeg", plot = last_plot(), width = 7.5, height = 7.5, units = "in")
#dev.print(tiff, "Figure_1.tiff", res=1200, height=7.5, width=7.5, units="in") # For publication quality resolution

ggplot(graph2, aes(x=YEAR, y=value)) +
  geom_line() + theme_bw() + facet_wrap(~type, ncol=2, scales = "free") +
  #ggtitle("Respect for Human Rights, 1992-2011") +
  labs(x = "Year", y = "Lower Score = More Abuse; Higher Score = More Respect") +
  geom_vline(xintercept = seq(2001, 2005, by = 0.001), colour="grey", linetype = "solid", alpha = 0.01) +
  geom_blank(data = facetlims2) +
  theme(legend.position= "bottom", legend.title=element_blank(), 
        plot.title = element_text(hjust = 0.5, size=14), 
        text = element_text(size = 13),
        axis.title = element_text(size = 13), axis.text = element_text(size = 10))
ggsave("Figure_1b.jpeg", plot = last_plot(), width = 7.5, height = 7.5, units = "in")
#dev.print(tiff, "Figure_1b.tiff", res=1200, height=7.5, width=7.5, units="in")


#---------------------------------------------------------
# Part B. Plotting of Dependent Variables by Participation 
#---------------------------------------------------------

# Compute global-year averages for democracies and non-democracies for all DVs
trends <- data %>% group_by(YEAR, active_d) %>% 
  summarise("Physical Integrity Score (CIRI)" = mean(PHYSINT, na.rm = TRUE),
            "Latent Variable Model Score (Fariss)" = mean(latentmean, na.rm = TRUE),
            "State Department Score (PTS)" = mean(State, na.rm = TRUE), 
            "Amnesty International Score (PTS)"  = mean(Amnesty, na.rm = TRUE),
            "Extrajudicial Killing Score (CIRI)" = mean(KILL, na.rm = TRUE),
            "Political Imprisonment Score (CIRI)" = mean(POLPRIS, na.rm = TRUE),
            "Torture Score (CIRI)" = mean(TORT, na.rm = TRUE), 
            "Disappearance Score (CIRI)" = mean(DISAP, na.rm = TRUE))

graph_data <- gather(trends, type, value, DVs) # Reformat data for graphs 
graph_data$type <- factor(graph_data$type, levels = DVs) # Converts labels to factors to order plot facets 

# Relabel active_d for legend
for (i in 1:length(graph_data$active_d)){    
  if (graph_data$active_d[i] == 0){ 
    graph_data$active_d[i] <- "Other States" 
  }else if (graph_data$active_d[i] == 1){
    graph_data$active_d[i] <- "Active Participants"
  }
}

# Subset data to be graphed
graph1 <- subset(graph_data, (graph_data$type %in% DVs[1:4]))
graph2 <- subset(graph_data, (graph_data$type %in% DVs[5:8]))

# Construct matrices to call min and max values for y-axis on facet plots 
facetlims1 <- as.data.frame(cbind(YEAR = c(rep(1992:2011, times = 4, each = 2)), 
                                  active_d = c(rep(0:1, times = 80)),
                                  value = c(rep(c(0,8), 20), rep(c(-0.5,1), 20), rep(c(0,2), 20), rep(c(0,2), 20))))
facetlims2 <- as.data.frame(cbind(YEAR = c(rep(1992:2011, times = 4, each = 2)),
                                  active_d = c(rep(0:1, times = 80)),
                                  value = c(rep(c(1,5), 20), rep(c(1,5), 20), rep(c(0,2), 20), rep(c(0,2), 20))))
# Assign variable names
facetlims1$type <- rep(DVs[1:4], times = 1, each = 40)
facetlims2$type <- rep(DVs[5:8], times = 1, each = 40)
# Converts labels to factors to order plot facets 
facetlims1$type <- factor(facetlims1$type, levels = DVs) 
facetlims2$type <- factor(facetlims2$type, levels = DVs)

# Create plots
ggplot(graph1, aes(x=YEAR, y=value, group=active_d)) +   
  geom_line(aes(linetype = factor(active_d))) + theme_bw() + facet_wrap(~type, ncol=2, scales = "free") +
  geom_blank(data = facetlims1) + 
  labs(x = "Year", y = "Lower Score = More Abuse; Higher Score = More Respect") +
  #ggtitle("Average Respect for Human Rights by Participation, 1992-2011") +
  geom_vline(xintercept = seq(2001, 2005, by = 0.001), colour="grey", linetype = "solid", alpha = 0.01) +
  theme(legend.position= "bottom", legend.title=element_blank(), 
        plot.title = element_text(hjust = 0.5, size=14), 
        text = element_text(size = 13),
        axis.title = element_text(size = 13), axis.text = element_text(size = 10))
ggsave("Figure_2.jpeg", plot = last_plot(), width = 7.5, height = 7.5, units = "in")
#dev.print(tiff, "Figure_2.tiff", res=1200, height=7.5, width=7.5, units="in")

ggplot(graph2, aes(x=YEAR, y=value, group=active_d)) +   
  geom_line(aes(linetype = factor(active_d))) + theme_bw() + facet_wrap(~type, ncol=2, scales = "free") +
  geom_blank(data = facetlims2) + 
  labs(x = "Year", y = "Lower Score = More Abuse; Higher Score = More Respect") +
  #ggtitle("Average Respect for Human Rights by Participation, 1992-2011") +
  geom_vline(xintercept = seq(2001, 2005, by = 0.001), colour="grey", linetype = "solid", alpha = 0.01) +
  theme(legend.position= "bottom", legend.title=element_blank(), 
        plot.title = element_text(hjust = 0.5, size=14), 
        text = element_text(size = 13),
        axis.title = element_text(size = 13), axis.text = element_text(size = 10))
ggsave("Figure_2b.jpeg", plot = last_plot(), width = 7.5, height = 7.5, units = "in")
#dev.print(tiff, "Figure_2.tiff", res=1200, height=7.5, width=7.5, units="in")

#-----------------------------------------------------------------------------
# Part C. Plotting of Dependent Variables by Participation for Less-Democratic States
#-----------------------------------------------------------------------------

# Compute global-year averages for democracies and non-democracies for all DVs
trends <- data %>% group_by(YEAR, active_d) %>% 
  filter(below_polity == 1) %>% 
  summarise("Physical Integrity Score (CIRI)" = mean(PHYSINT, na.rm = TRUE),
            "Latent Variable Model Score (Fariss)" = mean(latentmean, na.rm = TRUE),
            "State Department Score (PTS)" = mean(State, na.rm = TRUE), 
            "Amnesty International Score (PTS)"  = mean(Amnesty, na.rm = TRUE),
            "Extrajudicial Killing Score (CIRI)" = mean(KILL, na.rm = TRUE),
            "Political Imprisonment Score (CIRI)" = mean(POLPRIS, na.rm = TRUE),
            "Torture Score (CIRI)" = mean(TORT, na.rm = TRUE), 
            "Disappearance Score (CIRI)" = mean(DISAP, na.rm = TRUE))

graph_data <- gather(trends, type, value, DVs) # Reformat data for graphs 
graph_data$type <- factor(graph_data$type, levels = DVs) # Converts labels to factors to order plot facets 

# Relabel active_d for legend
for (i in 1:length(graph_data$active_d)){    
  if (graph_data$active_d[i] == 0){ 
    graph_data$active_d[i] <- "Other States" 
  }else if (graph_data$active_d[i] == 1){
    graph_data$active_d[i] <- "Active Participants"
  }
}

# Subset data to be graphed
graph1 <- subset(graph_data, (graph_data$type %in% DVs[1:4]))
graph2 <- subset(graph_data, (graph_data$type %in% DVs[5:8]))

# Adjust y-axis for Fariss data 
facetlims1_auto <- facetlims1
facetlims1_auto$value <- c(rep(c(0,8), 20), rep(c(-1,0.5), 20), rep(c(0,2), 20), rep(c(0,2), 20))

# Create Plots
ggplot(graph1, aes(x=YEAR, y=value, group=active_d)) +   
  geom_line(aes(linetype = factor(active_d))) + theme_bw() + facet_wrap(~type, ncol=2, scales = "free") +
  geom_blank(data = facetlims1_auto) + 
  labs(x = "Year", y = "Lower Score = More Abuse; Higher Score = More Respect") +
  #ggtitle("Respect for Human Rights among Non-Democracies by Participation, 1992-2011") +
  geom_vline(xintercept = seq(2001, 2005, by = 0.001), colour="grey", linetype = "solid", alpha = 0.01) +
  theme(legend.position= "bottom", legend.title=element_blank(), 
        plot.title = element_text(hjust = 0.5, size=14), 
        text = element_text(size = 13),
        axis.title = element_text(size = 13), axis.text = element_text(size = 10)) 
# Save Figure 3 to working drive 
ggsave("Figure_3.jpeg", plot = last_plot(), width = 7.5, height = 7.5, units = "in")
#dev.print(tiff, "Figure_3.tiff", res=1200, height=7.5, width=7.5, units="in")

ggplot(graph2, aes(x=YEAR, y=value, group=active_d)) +   
  geom_line(aes(linetype = factor(active_d))) + theme_bw() + facet_wrap(~type, ncol=2, scales = "free") +
  geom_blank(data = facetlims2) + 
  labs(x = "Year", y = "Lower Score = More Abuse; Higher Score = More Respect") +
  #ggtitle("Respect for Human Rights among Non-Democracies by Participation, 1992-2011") +
  geom_vline(xintercept = seq(2001, 2005, by = 0.001), colour="grey", linetype = "solid", alpha = 0.01) +
  theme(legend.position= "bottom", legend.title=element_blank(), 
        plot.title = element_text(hjust = 0.5, size=14), 
        text = element_text(size = 13),
        axis.title = element_text(size = 13), axis.text = element_text(size = 10)) 
# Save Figure 3b to working drive 
ggsave("Figure_3b.jpeg", plot = last_plot(), width = 7.5, height = 7.5, units = "in")
#dev.print(tiff, "Figure_3b.tiff", res=1200, height=7.5, width=7.5, units="in")


##################################################
# Section 4. Difference-of-Means and Balance Table
##################################################

#--------------------------------------------------------------------
# In this section we conduct a series of difference-of-means tests comparing global human rights 
# practices before and after the onset of the RDI program (Part A); and a series of difference-of-means 
# teststo check the comparability of participants and non-participants before the beginning
# of the RDI program (Part B). 
#--------------------------------------------------------------------

#---------------------------------------------------------------------------------------------
# Part A. Difference-of-Means Tests of Human Rights Before/After 2001, 2002, 2003, 2004, 2005
#---------------------------------------------------------------------------------------------

# Clear workspace and re-load data
#load("pic_data.RData")
#data.out <- data
cutoff_years <- c(2001, 2002, 2003, 2004, 2005) # Creat vectorof relevant cutoff years
DVs <- c("PHYSINT", "Latent", "State", "Amnesty", "KILL", "POLPRIS", "TORT", "DISAP") # Vector of dependent variable names

# Function that creates dataset of states' average dependent variable values for before/after cutoff dates  
average_dv_pre_post <- function(year, df){
  post_years <- df %>% group_by(COW) %>% filter(YEAR >= year) %>% 
    summarise(PHYSINT = mean(PHYSINT, na.rm = TRUE), Latent = mean(latentmean, na.rm = TRUE),
              State = mean(State, na.rm = TRUE), Amnesty = mean(Amnesty, na.rm = TRUE),
              KILL = mean(KILL, na.rm = TRUE), POLPRIS = mean(POLPRIS, na.rm = TRUE),
              TORT = mean(TORT, na.rm = TRUE), DISAP = mean(DISAP, na.rm = TRUE))
  post_years$pre_post <- 1
  pre_years <- df %>% group_by(COW) %>% filter(YEAR < year) %>% 
    summarise(PHYSINT = mean(PHYSINT, na.rm = TRUE), Latent = mean(latentmean, na.rm = TRUE),
              State = mean(State, na.rm = TRUE), Amnesty = mean(Amnesty, na.rm = TRUE),
              KILL = mean(KILL, na.rm = TRUE), POLPRIS = mean(POLPRIS, na.rm = TRUE),
              TORT = mean(TORT, na.rm = TRUE), DISAP = mean(DISAP, na.rm = TRUE))
  pre_years$pre_post <- 0
  out <- as.data.frame(rbind(pre_years, post_years))
  out
}

data.out.ttest <- data.out # Create object to hold datasets for analysis

# Run Analysis 
for(i in 1:length(cutoff_years)){
  for(j in 1:length(data.out$imputations)){data.out.ttest$imputations[[j]] <- average_dv_pre_post(cutoff_years[i], data.out$imputations[[j]])}
  cat("\n \n \n \n", "Analysis With Cutoff Year =", cutoff_years[i],"\n \n")
  for(k in 1:length(DVs)){
    out <- mi.t.test(data.out.ttest$imputations, x = DVs[k], y = "pre_post", alternative = c("two.sided"), var.equal = FALSE)
    print(out)
  }
}

# Outcome: Fariss is statistically better, not worse; more torture after cutoff when using 2001, 2002, 
# and 2003 (all at significance level of 10%), but not any of the other years. The rest is all balanced. 
# The outcome of this test for the year 2004 is presented in the supplementary materials. 

#-------------------------------------------------------------------------
# Part B. Testing Balance of Participants and Non-Participants, 1998-2000. 
#-------------------------------------------------------------------------

# Function that gets country averages for all variables during a specified period
balance_data <- function(start_year, stop_year, df){
  out <- df %>% group_by(COW) %>% 
    filter(YEAR >= start_year & YEAR <= stop_year) %>%
    summarise(active_d = mean(active_d), PHYSINT = mean(PHYSINT), 
              Latent = mean(latentmean),State = mean(State), Amnesty = mean(Amnesty),
              KILL = mean(KILL), POLPRIS = mean(POLPRIS), TORT = mean(TORT), 
              DISAP = mean(DISAP), ucdp_type3 = mean(ucdp_type3), trans = mean(trans), 
              log_pop = mean(log_pop), log_gdppc = mean(log_gdppc), polity2 = mean(polity2), 
              gtd = mean(gtd), log_UStrade = mean(log_UStrade), log_USmilaid = mean(log_USmilaid))
  out <- as.data.frame(out)
  out
}

data.out.baltest <- data.out # Create object to hold datasets for analysis
vars <- c("PHYSINT", "Latent", "State", "Amnesty", "KILL", "POLPRIS", "TORT", "DISAP",
          "ucdp_type3", "trans", "log_pop", "log_gdppc", "polity2", "gtd", "log_UStrade", "log_USmilaid")

# Run Analysis 
for(j in 1:length(data.out$imputations)){data.out.baltest$imputations[[j]] <- balance_data(1998, 2000, data.out$imputations[[j]])} # Subset relevant data
cat("\n \n \n \n", "Balance between Participants and Non-Participants from 1998 until 2000 \n \n") # Print title
for(k in 1:length(vars)){ # Loop through difference of means tests for all variables and print results
    out <- mi.t.test(data.out.baltest$imputations, x = vars[k], y = "active_d", alternative = c("two.sided"), var.equal = FALSE)
    print(out)
}

# The outcome of these tests is summarized in a table in the Supplementary Materials. The key takeaway
# is that the only statistically significant difference is in terms of population. Participants
# are more populous than non-participants.

############################################
# Section 4. Linear Models
############################################

#-----------------------------------------------------------------------------------------
# In this section we compute a series of linear models for panel data in order
# to assess the correlation between participation in the RDI program and changes
# in state human rights practices. First, we load the data and define functions to compute
# standard errors and four linear panel models from imputed datasets (Part A). All models are 
# the same, with the exception of the addition of control variables and the decomposition of 
# the participation variable by level of democracy. The first model is commented extensively, 
# all others follow the same series of commands. We then use these functions to conduct our 
# analysis and present the results (Part B). 
#-----------------------------------------------------------------------------------------

#----------------------------------
# Part A. Define relevant functions  
#----------------------------------

# Re-load data
load("pic_data.RData")
data.out <- data
# Set number of imputations
imps <- length(data.out$imputations)

# Define Function to Compute Variances from Imputed Datasets
mi.ses <- function(betas, st.errors){
  t_one <- apply(st.errors, MARGIN= 2, function(x) ((1/(length(x))) * sum(x^2)))
  t_two <- apply(betas, MARGIN= 2, function(x) sum((x - mean(x))^2 /(length(x) - 1)) *(1 + (1/length(x))))
  t_one + t_two
}

# General Function for Model 1
mod_1 <- function(DV, LAG_DV){
  # Step 1. Create empty matrices to store estimated coefficients and standard errors computed for each imputed dataset 
  betas <- matrix(NA, nrow = imps, ncol = 2) # Matrix for estimated coefficients 
  st.errors <- matrix(NA, nrow = imps, ncol = 2) # Matrix for standard errors 
  
  # Step 2. Create formula 
  IVs <- paste("active_t", substitute(LAG_DV), sep = "+") # Paste together covariates
  fmla <- formula(paste(substitute(DV), IVs, sep = "~")) # Paste on DV and convert to formula
  
  # Step 3. Estimate linear model and save standard errors for each imputed dataset
  for (i in 1:imps){  
    mod <- plm(fmla, data=data.out$imputations[[i]], index=c("COW", "YEAR"), effect ="twoways", model="within") # Compute linear panel model 
    mod <- coeftest(mod, vcovHC(mod, type = "HC3", cluster = "group")) # Compute country level cluster robust standard errors
    betas[i,] <- mod[,1] # Save estimated coefficients
    st.errors[i,] <- mod[,2] # Save clustered standard errors
  }
  
  # Step 4. Compute estimated coefficients and standard errors across all imputed datasets
  beta_estimates <- colMeans(betas) # Compute mean of estimated coefficients across imputed datasets 
  crse_estimates <- mi.ses(betas, st.errors) # Compute standard errors across imputed datasets
  
  # Step 5. Make placeholder plm object and insert correct values for estimated coefficients and standard errors so that results can be presented in a table 
  mod <- plm(fmla, data=data.out$imputations[[1]], index=c("COW", "YEAR"), effect ="twoways", model="within") # create placeholder plm object
  for(i in 1:length(colMeans(betas))){mod$coefficients[[i]] <- beta_estimates[i]} # Overwrite plm coefficients with correct coefficients
  diag(mod$vcov) <- crse_estimates # Overwrite diagonal of plm variance-covariance matrix with correct variances
  
  # Step 6. Return plm object 
  mod
}

# General Function for Model 2
mod_2 <- function(DV, LAG_DV){
  betas <- matrix(NA, nrow = imps, ncol = 3) 
  st.errors <- matrix(NA, nrow = imps, ncol = 3) 
  IVs <- paste("Dem_Part", "Auto_Part", substitute(LAG_DV), sep = "+") 
  fmla <- formula(paste(substitute(DV), IVs, sep = "~")) 
  for (i in 1:imps){  
    mod <- plm(fmla, data=data.out$imputations[[i]], index=c("COW", "YEAR"), effect ="twoways", model="within") 
    mod <- coeftest(mod, vcovHC(mod, type = "HC3", cluster = "group")) 
    betas[i,] <- mod[,1] 
    st.errors[i,] <- mod[,2] 
  }
  beta_estimates <- colMeans(betas) 
  crse_estimates <- mi.ses(betas, st.errors) 
  mod <- plm(fmla, data=data.out$imputations[[1]], index=c("COW", "YEAR"), effect ="twoways", model="within") 
  for(i in 1:length(colMeans(betas))){mod$coefficients[[i]] <- beta_estimates[i]} 
  diag(mod$vcov) <- crse_estimates 
  mod
}

# General Function for Model 3
mod_3 <- function(DV, LAG_DV){
  betas <- matrix(NA, nrow = imps, ncol = 10) 
  st.errors <- matrix(NA, nrow = imps, ncol = 10) 
  IVs <- paste("active_t", "ucdp_type3", "gtd", "trans", "polity2", "log_pop", "log_gdppc", "log_UStrade", "log_USmilaid", substitute(LAG_DV), sep = "+") 
  fmla <- formula(paste(substitute(DV), IVs, sep = "~")) 
  for (i in 1:imps){  
    mod <- plm(fmla, data=data.out$imputations[[i]], index=c("COW", "YEAR"), effect ="twoways", model="within") 
    mod <- coeftest(mod, vcovHC(mod, type = "HC3", cluster = "group")) 
    betas[i,] <- mod[,1] 
    st.errors[i,] <- mod[,2] 
  }
  beta_estimates <- colMeans(betas) 
  crse_estimates <- mi.ses(betas, st.errors) 
  mod <- plm(fmla, data=data.out$imputations[[1]], index=c("COW", "YEAR"), effect ="twoways", model="within") 
  for(i in 1:length(colMeans(betas))){mod$coefficients[[i]] <- beta_estimates[i]} 
  diag(mod$vcov) <- crse_estimates 
  mod
}

# General Function for Model 4
mod_4 <- function(DV, LAG_DV){
  betas <- matrix(NA, nrow = imps, ncol = 11) 
  st.errors <- matrix(NA, nrow = imps, ncol = 11) 
  IVs <- paste("Dem_Part", "Auto_Part", "ucdp_type3", "gtd", "trans", "polity2", "log_pop", "log_gdppc", "log_UStrade", "log_USmilaid", substitute(LAG_DV), sep = "+") 
  fmla <- formula(paste(substitute(DV), IVs, sep = "~")) 
  for (i in 1:imps){  
    mod <- plm(fmla, data=data.out$imputations[[i]], index=c("COW", "YEAR"), effect ="twoways", model="within") 
    mod <- coeftest(mod, vcovHC(mod, type = "HC3", cluster = "group")) 
    betas[i,] <- mod[,1] 
    st.errors[i,] <- mod[,2] 
  }
  beta_estimates <- colMeans(betas) 
  crse_estimates <- mi.ses(betas, st.errors) 
  mod <- plm(fmla, data=data.out$imputations[[1]], index=c("COW", "YEAR"), effect ="twoways", model="within") 
  for(i in 1:length(colMeans(betas))){mod$coefficients[[i]] <- beta_estimates[i]} 
  diag(mod$vcov) <- crse_estimates 
  mod
}

#---------------------------------
# Part B. Run and Present Analysis  
#---------------------------------

# Define standard fomatting features of tables
covariate_names <- c("Participation", "Democratic participation", "Autocratic participation", "Internal conflicts",
                     "Terrorist attacks", "Transitional state", "Polity score", "Log population",
                     "Log GDP per capita", "Log US trade", "Log US military assistance")
fe_line <- list(c("Fixed effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"))
footnote <- c("Note: All models include country and year fixed effects and a dependent variable lagged one year. Country level", 
              "cluster-robust standard errors in parentheses. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01")
  
  #c("All models include country and year fixed effects and a dependent variable lagged one year.",
   #           "Country level cluster-robust standard errors in parentheses. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01")
title1 <- "Participation in RDI program and state respect for human rights, 1992-2011"

# Run analysis and produce tables 
stargazer(mod_1(PHYSINT, lag_physint), mod_2(PHYSINT, lag_physint), mod_3(PHYSINT, lag_physint), mod_4(PHYSINT, lag_physint), 
          mod_1(latentmean, lag_latentmean), mod_2(latentmean, lag_latentmean), mod_3(latentmean, lag_latentmean), mod_4(latentmean, lag_latentmean),
          title = title1, covariate.labels = covariate_names, add.lines = fe_line, notes = footnote,
          dep.var.labels = c("Physical Integrity Score (CIRI)", "Latent Variable Model (Fariss)"),
          digits = 3, omit = c("lag_physint", "lag_latentmean"), omit.stat = c("adj.rsq", "f"), 
          notes.append = FALSE, notes.align = "l")

stargazer(mod_1(DISAP, lag_disap), mod_2(DISAP, lag_disap), mod_3(DISAP, lag_disap), mod_4(DISAP, lag_disap),  
          mod_1(POLPRIS, lag_polpris), mod_2(POLPRIS, lag_polpris), mod_3(POLPRIS, lag_polpris), mod_4(POLPRIS, lag_polpris),
          title = title1, covariate.labels = covariate_names, add.lines = fe_line, notes = footnote,
          dep.var.labels = c("Disappearance Score (CIRI)", "Political Imprisonment Score (CIRI)"),
          digits = 3, omit = c("lag_disap", "lag_polpris"), omit.stat = c("adj.rsq", "f"), 
          notes.append = FALSE, notes.align = "l")

stargazer(mod_1(Amnesty, lag_amnesty), mod_2(Amnesty, lag_amnesty), mod_3(Amnesty, lag_amnesty), mod_4(Amnesty, lag_amnesty),  
          mod_1(State, lag_state), mod_2(State, lag_state), mod_3(State, lag_state), mod_4(State, lag_state),
          title = title1, covariate.labels = covariate_names, add.lines = fe_line, notes = footnote, 
          dep.var.labels = c("Amnesty International Score (PTS)", "State Department Score (PTS)"),
          digits = 3, omit = c("lag_amnesty", "lag_state"), omit.stat = c("adj.rsq", "f"), 
          notes.append = FALSE, notes.align = "l")

stargazer(mod_1(TORT, lag_tort), mod_2(TORT, lag_tort), mod_3(TORT, lag_tort), mod_4(TORT, lag_tort),  
          mod_1(KILL, lag_kill), mod_2(KILL, lag_kill), mod_3(KILL, lag_kill), mod_4(KILL, lag_kill),
          title = title1, covariate.labels = covariate_names, add.lines = fe_line, notes = footnote,
          dep.var.labels = c("Torture Score (CIRI)", "Extrajudicial Killing Score (CIRI)"),
          digits = 3, omit = c("lag_tort", "lag_kill"), omit.stat = c("adj.rsq", "f"), 
          notes.append = FALSE, notes.align = "l")

###################################################################################
# Section 5. Replication of Analysis Using Treatment Year (t) as Time-Series Index
###################################################################################

#--------------------------------------------------------------
# Part A. Replotting with Time Series Indexed by Treatment Year
#--------------------------------------------------------------

# Compute global-year averages for democracies and non-democracies for all DVs
trends <- data %>% group_by(t, active_d) %>% 
  filter(below_polity == 1 & t > -9 & t < 9) %>% 
  summarise("Physical Integrity Score (CIRI)" = mean(PHYSINT, na.rm = TRUE),
            "Latent Variable Model Score (Fariss)" = mean(latentmean, na.rm = TRUE),
            "State Department Score (PTS)" = mean(State, na.rm = TRUE), 
            "Amnesty International Score (PTS)"  = mean(Amnesty, na.rm = TRUE),
            "Extrajudicial Killing Score (CIRI)" = mean(KILL, na.rm = TRUE),
            "Political Imprisonment Score (CIRI)" = mean(POLPRIS, na.rm = TRUE),
            "Torture Score (CIRI)" = mean(TORT, na.rm = TRUE), 
            "Disappearance Score (CIRI)" = mean(DISAP, na.rm = TRUE))

DVs <- c("Physical Integrity Score (CIRI)", "Latent Variable Model Score (Fariss)", "Disappearance Score (CIRI)", "Political Imprisonment Score (CIRI)", "Amnesty International Score (PTS)", "State Department Score (PTS)", "Extrajudicial Killing Score (CIRI)", "Torture Score (CIRI)")
graph_data <- gather(trends, type, value, DVs) # Reformat data for graphs 
graph_data$type <- factor(graph_data$type, levels = DVs) # Converts labels to factors to order plot facets 

# Relabel active_d for legend
for (i in 1:length(graph_data$active_d)){    
  if (graph_data$active_d[i] == 0){ 
    graph_data$active_d[i] <- "Other States" 
  }else if (graph_data$active_d[i] == 1){
    graph_data$active_d[i] <- "Active Participants"
  }
}

# Subset data to be graphed
graph1 <- subset(graph_data, (graph_data$type %in% DVs[1:4]))
graph2 <- subset(graph_data, (graph_data$type %in% DVs[5:8]))

# Construct matrices to call min and max values for y-axis on facet plots 
facetlims1t <- as.data.frame(cbind(t = c(rep(-8:8, times = 4, each = 2)), 
                                  active_d = c(rep(0:1, times = 68)),
                                  value = c(rep(c(0,8), 17), rep(c(-1,0.5), 17), rep(c(0,2), 17), rep(c(0,2), 17))))
facetlims2t <- as.data.frame(cbind(t = c(rep(-8:8, times = 4, each = 2)),
                                  active_d = c(rep(0:1, times = 68)),
                                  value = c(rep(c(1,5), 17), rep(c(1,5), 17), rep(c(0,2), 17), rep(c(0,2), 17))))
# Assign variable names
facetlims1t$type <- rep(DVs[1:4], times = 1, each = 34)
facetlims2t$type <- rep(DVs[5:8], times = 1, each = 34)
# Converts labels to factors to order plot facets 
facetlims1t$type <- factor(facetlims1t$type, levels = DVs) 
facetlims2t$type <- factor(facetlims2t$type, levels = DVs)

# Create Plots
ggplot(graph1, aes(x=t, y=value, group=active_d)) +   
  geom_line(aes(linetype = factor(active_d))) + theme_bw() + facet_wrap(~type, ncol=2, scales = "free") +
  geom_blank(data = facetlims1t) + 
  labs(x = "Year", y = "Lower Score = More Abuse; Higher Score = More Respect") +
  #ggtitle("Time Adjusted Plots of Non-Democracies by Participation") +
  geom_vline(xintercept = seq(0, 1, by = 0.001), colour="grey", linetype = "solid", alpha = 0.01) +
  theme(legend.position= "bottom", legend.title=element_blank(), 
        plot.title = element_text(hjust = 0.5, size=14), 
        text = element_text(size = 13),
        axis.title = element_text(size = 13), axis.text = element_text(size = 10)) 
ggsave("Figure_4.jpeg", plot = last_plot(), width = 7.5, height = 7.5, units = "in")
#dev.print(tiff, "Figure_4.tiff", res=1200, height=7.5, width=7.5, units="in")

ggplot(graph2, aes(x=t, y=value, group=active_d)) +   
  geom_line(aes(linetype = factor(active_d))) + theme_bw() + facet_wrap(~type, ncol=2, scales = "free") +
  geom_blank(data = facetlims2t) + 
  labs(x = "Year", y = "Lower Score = More Abuse; Higher Score = More Respect") +
  #ggtitle("Time Adjusted Plots of Non-Democracies by Participation") +
  geom_vline(xintercept = seq(0,1, by = 0.001), colour="grey", linetype = "solid", alpha = 0.01) +
  theme(legend.position= "bottom", legend.title=element_blank(), 
        plot.title = element_text(hjust = 0.5, size=14), 
        text = element_text(size = 13),
        axis.title = element_text(size = 13), axis.text = element_text(size = 10)) 
ggsave("Figure_4b.jpeg", plot = last_plot(), width = 7.5, height = 7.5, units = "in")
#dev.print(tiff, "Figure_4b.tiff", res=1200, height=7.5, width=7.5, units="in")

#------------------------------------------------------------------------------------
# Part B. Reproduce the statistical analysis with treatment year as time-series Index
#------------------------------------------------------------------------------------

# Update general functions so that "t" is the time series index instead of "YEAR"
mod_1t <- function(DV, LAG_DV){
  betas <- matrix(NA, nrow = imps, ncol = 2)  
  st.errors <- matrix(NA, nrow = imps, ncol = 2) 
  IVs <- paste("active_t", substitute(LAG_DV), sep = "+") 
  fmla <- formula(paste(substitute(DV), IVs, sep = "~")) 
  for (i in 1:imps){  
    mod <- plm(fmla, data=data.out$imputations[[i]], index=c("COW", "t"), effect ="twoways", model="within") # This line is updated in each function 
    mod <- coeftest(mod, vcovHC(mod, type = "HC3", cluster = "group")) 
    betas[i,] <- mod[,1] 
    st.errors[i,] <- mod[,2] 
  }
  beta_estimates <- colMeans(betas) 
  crse_estimates <- mi.ses(betas, st.errors) 
  mod <- plm(fmla, data=data.out$imputations[[1]], index=c("COW", "t"), effect ="twoways", model="within") # This line is updated in each function 
  for(i in 1:length(colMeans(betas))){mod$coefficients[[i]] <- beta_estimates[i]} 
  diag(mod$vcov) <- crse_estimates 
  mod
}

# Updated general function for model 2
mod_2t <- function(DV, LAG_DV){
  betas <- matrix(NA, nrow = imps, ncol = 3) 
  st.errors <- matrix(NA, nrow = imps, ncol = 3) 
  IVs <- paste("Dem_Part", "Auto_Part", substitute(LAG_DV), sep = "+") 
  fmla <- formula(paste(substitute(DV), IVs, sep = "~")) 
  for (i in 1:imps){  
    mod <- plm(fmla, data=data.out$imputations[[i]], index=c("COW", "t"), effect ="twoways", model="within") 
    mod <- coeftest(mod, vcovHC(mod, type = "HC3", cluster = "group")) 
    betas[i,] <- mod[,1] 
    st.errors[i,] <- mod[,2] 
  }
  beta_estimates <- colMeans(betas) 
  crse_estimates <- mi.ses(betas, st.errors) 
  mod <- plm(fmla, data=data.out$imputations[[1]], index=c("COW", "t"), effect ="twoways", model="within") 
  for(i in 1:length(colMeans(betas))){mod$coefficients[[i]] <- beta_estimates[i]} 
  diag(mod$vcov) <- crse_estimates 
  mod
}

# Updated general function for model 3
mod_3t <- function(DV, LAG_DV){
  betas <- matrix(NA, nrow = imps, ncol = 10) 
  st.errors <- matrix(NA, nrow = imps, ncol = 10) 
  IVs <- paste("active_t", "ucdp_type3", "gtd", "trans", "polity2", "log_pop", "log_gdppc", "log_UStrade", "log_USmilaid", substitute(LAG_DV), sep = "+") 
  fmla <- formula(paste(substitute(DV), IVs, sep = "~")) 
  for (i in 1:imps){  
    mod <- plm(fmla, data=data.out$imputations[[i]], index=c("COW", "t"), effect ="twoways", model="within") 
    mod <- coeftest(mod, vcovHC(mod, type = "HC3", cluster = "group")) 
    betas[i,] <- mod[,1] 
    st.errors[i,] <- mod[,2] 
  }
  beta_estimates <- colMeans(betas) 
  crse_estimates <- mi.ses(betas, st.errors) 
  mod <- plm(fmla, data=data.out$imputations[[1]], index=c("COW", "t"), effect ="twoways", model="within") 
  for(i in 1:length(colMeans(betas))){mod$coefficients[[i]] <- beta_estimates[i]} 
  diag(mod$vcov) <- crse_estimates 
  mod
}

# Updated general function for model 4
mod_4t <- function(DV, LAG_DV){
  betas <- matrix(NA, nrow = imps, ncol = 11) 
  st.errors <- matrix(NA, nrow = imps, ncol = 11) 
  IVs <- paste("Dem_Part", "Auto_Part", "ucdp_type3", "gtd", "trans", "polity2", "log_pop", "log_gdppc", "log_UStrade", "log_USmilaid", substitute(LAG_DV), sep = "+") 
  fmla <- formula(paste(substitute(DV), IVs, sep = "~")) 
  for (i in 1:imps){  
    mod <- plm(fmla, data=data.out$imputations[[i]], index=c("COW", "t"), effect ="twoways", model="within") 
    mod <- coeftest(mod, vcovHC(mod, type = "HC3", cluster = "group")) 
    betas[i,] <- mod[,1] 
    st.errors[i,] <- mod[,2] 
  }
  beta_estimates <- colMeans(betas) 
  crse_estimates <- mi.ses(betas, st.errors) 
  mod <- plm(fmla, data=data.out$imputations[[1]], index=c("COW", "t"), effect ="twoways", model="within") 
  for(i in 1:length(colMeans(betas))){mod$coefficients[[i]] <- beta_estimates[i]} 
  diag(mod$vcov) <- crse_estimates 
  mod
}

# Change table title
title1 <- "Participation in RDI program and state respect for human rights, treatment-year as time-series index"

# Run and Present Analysis 
stargazer(mod_1t(PHYSINT, lag_physint), mod_2t(PHYSINT, lag_physint), mod_3t(PHYSINT, lag_physint), mod_4t(PHYSINT, lag_physint), 
          mod_1t(latentmean, lag_latentmean), mod_2t(latentmean, lag_latentmean), mod_3t(latentmean, lag_latentmean), mod_4t(latentmean, lag_latentmean),
          title = title1, covariate.labels = covariate_names, add.lines = fe_line, notes = footnote,
          dep.var.labels = c("Physical Integrity Score (CIRI)", "Latent Variable Model (Fariss)"),
          digits = 3, omit = c("lag_physint", "lag_latentmean"), omit.stat = c("adj.rsq", "f"), 
          notes.append = FALSE, notes.align = "l")

stargazer(mod_1t(DISAP, lag_disap), mod_2t(DISAP, lag_disap), mod_3t(DISAP, lag_disap), mod_4t(DISAP, lag_disap),  
          mod_1t(POLPRIS, lag_polpris), mod_2t(POLPRIS, lag_polpris), mod_3t(POLPRIS, lag_polpris), mod_4t(POLPRIS, lag_polpris),
          title = title1, covariate.labels = covariate_names, add.lines = fe_line, notes = footnote,
          dep.var.labels = c("Disappearance Score (CIRI)", "Political Imprisonment Score (CIRI)"),
          digits = 3, omit = c("lag_disap", "lag_polpris"), omit.stat = c("adj.rsq", "f"), 
          notes.append = FALSE, notes.align = "l")

stargazer(mod_1t(Amnesty, lag_amnesty), mod_2t(Amnesty, lag_amnesty), mod_3t(Amnesty, lag_amnesty), mod_4t(Amnesty, lag_amnesty),  
          mod_1t(State, lag_state), mod_2t(State, lag_state), mod_3t(State, lag_state), mod_4t(State, lag_state),
          title = title1, covariate.labels = covariate_names, add.lines = fe_line, notes = footnote, 
          dep.var.labels = c("Amnesty International Score (PTS)", "State Department Score (PTS)"),
          digits = 3, omit = c("lag_amnesty", "lag_state"), omit.stat = c("adj.rsq", "f"), 
          notes.append = FALSE, notes.align = "l")

stargazer(mod_1t(TORT, lag_tort), mod_2t(TORT, lag_tort), mod_3t(TORT, lag_tort), mod_4t(TORT, lag_tort),  
          mod_1t(KILL, lag_kill), mod_2t(KILL, lag_kill), mod_3t(KILL, lag_kill), mod_4t(KILL, lag_kill),
          title = title1, covariate.labels = covariate_names, add.lines = fe_line, notes = footnote,
          dep.var.labels = c("Torture Score (CIRI)", "Extrajudicial Killing Score (CIRI)"),
          digits = 3, omit = c("lag_tort", "lag_kill"), omit.stat = c("adj.rsq", "f"), 
          notes.append = FALSE, notes.align = "l")


##########################################################################
# Section 7. Replicating Analysis After Dropping Problematic Country-Years
##########################################################################

#--------------------------------------------------------------------------------------------------------
# Part A. Drop country-years with human rights mentioning one or more individuals detained in RDI program
#--------------------------------------------------------------------------------------------------------

for (i in 1:imps){
  data.out$imputations[[i]] <- subset(data.out$imputations[[i]], !(data.out$imputations[[i]]$COW == 200 & (data.out$imputations[[i]]$YEAR <= 2005 & data.out$imputations[[i]]$YEAR >= 2003))) # United Kingdom 2003-2005, 3 country-years
  data.out$imputations[[i]] <- subset(data.out$imputations[[i]], !(data.out$imputations[[i]]$COW == 200 & (data.out$imputations[[i]]$YEAR <= 2010 & data.out$imputations[[i]]$YEAR >= 2007))) # United Kingdom 2007-2010, 4 country-years
  data.out$imputations[[i]] <- subset(data.out$imputations[[i]], !(data.out$imputations[[i]]$COW == 651 & (data.out$imputations[[i]]$YEAR <= 2007 & data.out$imputations[[i]]$YEAR > 2006))) # Egypt 2007, 1 country-years
  data.out$imputations[[i]] <- subset(data.out$imputations[[i]], !(data.out$imputations[[i]]$COW == 380 & (data.out$imputations[[i]]$YEAR <= 2011 & data.out$imputations[[i]]$YEAR >= 2006))) # Sweden 2006-2011, 6 country-years
  data.out$imputations[[i]] <- subset(data.out$imputations[[i]], !(data.out$imputations[[i]]$COW == 20 & (data.out$imputations[[i]]$YEAR <= 2007 & data.out$imputations[[i]]$YEAR >= 2003))) # Canada 2003-2007, 5 country-years
  data.out$imputations[[i]] <- subset(data.out$imputations[[i]], !(data.out$imputations[[i]]$COW == 663 & data.out$imputations[[i]]$YEAR == 2003)) # Jordan 2003, 1 country-year
  data.out$imputations[[i]] <- subset(data.out$imputations[[i]], !(data.out$imputations[[i]]$COW == 663 & (data.out$imputations[[i]]$YEAR <= 2008 & data.out$imputations[[i]]$YEAR >= 2007))) # Jordan 2007-2008, 2 country-years
  data.out$imputations[[i]] <- subset(data.out$imputations[[i]], !(data.out$imputations[[i]]$COW == 652 & data.out$imputations[[i]]$YEAR == 2003)) # Syria 2003, 1 country-year
  data.out$imputations[[i]] <- subset(data.out$imputations[[i]], !(data.out$imputations[[i]]$COW == 652 & data.out$imputations[[i]]$YEAR == 2005)) # Syria 2005, 1 country-year
  data.out$imputations[[i]] <- subset(data.out$imputations[[i]], !(data.out$imputations[[i]]$COW == 770 & data.out$imputations[[i]]$YEAR == 2004)) # Pakistan 2004, 1 country-year
  data.out$imputations[[i]] <- subset(data.out$imputations[[i]], !(data.out$imputations[[i]]$COW == 770 & (data.out$imputations[[i]]$YEAR <= 2008 & data.out$imputations[[i]]$YEAR >= 2007))) # Pakistan 2007-2008, 2 country-years
  data.out$imputations[[i]] <- subset(data.out$imputations[[i]], !(data.out$imputations[[i]]$COW == 325 & data.out$imputations[[i]]$YEAR == 2007)) # Italy 2007, 1 country-year
  data.out$imputations[[i]] <- subset(data.out$imputations[[i]], !(data.out$imputations[[i]]$COW == 900 & (data.out$imputations[[i]]$YEAR <= 2005 & data.out$imputations[[i]]$YEAR >= 2004))) # Australia 2004-2005, 2 country-years
  data.out$imputations[[i]] <- subset(data.out$imputations[[i]], !(data.out$imputations[[i]]$COW == 343 & (data.out$imputations[[i]]$YEAR <= 2011 & data.out$imputations[[i]]$YEAR >= 2005))) # Macedonia 2005-2011, 7 country-years
  data.out$imputations[[i]] <- subset(data.out$imputations[[i]], !(data.out$imputations[[i]]$COW == 771 & data.out$imputations[[i]]$YEAR == 2006)) # Bangladesh 2006, 1 country-year
  data.out$imputations[[i]] <- subset(data.out$imputations[[i]], !(data.out$imputations[[i]]$COW == 255 & data.out$imputations[[i]]$YEAR == 2003)) # Germany, 1 country-year
  data.out$imputations[[i]] <- subset(data.out$imputations[[i]], !(data.out$imputations[[i]]$COW == 255 & (data.out$imputations[[i]]$YEAR <= 2008 & data.out$imputations[[i]]$YEAR >= 2005))) # Germany 2005-2008, 4 country-years
  data.out$imputations[[i]] <- subset(data.out$imputations[[i]], !(data.out$imputations[[i]]$COW == 255 & (data.out$imputations[[i]]$YEAR <= 2011 & data.out$imputations[[i]]$YEAR >= 2010))) # Germany 2010-2011, 2 country-years
  data.out$imputations[[i]] <- subset(data.out$imputations[[i]], !(data.out$imputations[[i]]$COW == 600 & data.out$imputations[[i]]$YEAR == 2009)) # Morocco 2009, 1 country-year
  data.out$imputations[[i]] <- subset(data.out$imputations[[i]], !(data.out$imputations[[i]]$COW == 290 & data.out$imputations[[i]]$YEAR == 2010)) # Poland 2010, 1 country-year
  data.out$imputations[[i]] <- subset(data.out$imputations[[i]], !(data.out$imputations[[i]]$COW == 679 & data.out$imputations[[i]]$YEAR == 2004)) # Yemen 2004, 1 country-year
}

#---------------------------
# Part B. Graphical analysis 
#---------------------------

data <- (data.out$imputations[[1]] + data.out$imputations[[2]] + data.out$imputations[[3]] + data.out$imputations[[4]] + data.out$imputations[[5]] + data.out$imputations[[6]] + data.out$imputations[[7]] + data.out$imputations[[8]] + data.out$imputations[[9]])/imps # Get average values of variables across imputed datasets

# Compute global-year averages for democracies and non-democracies for all DVs
trends <- data %>% group_by(YEAR, active_d) %>% 
  filter(below_polity == 1) %>% 
  summarise("Physical Integrity Score (CIRI)" = mean(PHYSINT, na.rm = TRUE),
            "Latent Variable Model Score (Fariss)" = mean(latentmean, na.rm = TRUE),
            "State Department Score (PTS)" = mean(State, na.rm = TRUE), 
            "Amnesty International Score (PTS)"  = mean(Amnesty, na.rm = TRUE),
            "Extrajudicial Killing Score (CIRI)" = mean(KILL, na.rm = TRUE),
            "Political Imprisonment Score (CIRI)" = mean(POLPRIS, na.rm = TRUE),
            "Torture Score (CIRI)" = mean(TORT, na.rm = TRUE), 
            "Disappearance Score (CIRI)" = mean(DISAP, na.rm = TRUE))

graph_data <- gather(trends, type, value, DVs) # Reformat data for graphs 
graph_data$type <- factor(graph_data$type, levels = DVs) # Converts labels to factors to order plot facets 

# Relabel active_d for legend
for (i in 1:length(graph_data$active_d)){    
  if (graph_data$active_d[i] == 0){ 
    graph_data$active_d[i] <- "Other States" 
  }else if (graph_data$active_d[i] == 1){
    graph_data$active_d[i] <- "Active Participants"
  }
}

# Subset data to be graphed
graph1 <- subset(graph_data, (graph_data$type %in% DVs[1:4]))
graph2 <- subset(graph_data, (graph_data$type %in% DVs[5:8]))

# Create Plots
ggplot(graph1, aes(x=YEAR, y=value, group=active_d)) +   
  geom_line(aes(linetype = factor(active_d))) + theme_bw() + facet_wrap(~type, ncol=2, scales = "free") +
  geom_blank(data = facetlims1_auto) + 
  labs(x = "Year", y = "Lower Score = More Abuse; Higher Score = More Respect") +
  #ggtitle("Respect for Human Rights among Non-Democracies by Participation, 1992-2011") +
  geom_vline(xintercept = seq(2001, 2005, by = 0.001), colour="grey", linetype = "solid", alpha = 0.01) +
  theme(legend.position= "bottom", legend.title=element_blank(), 
        plot.title = element_text(hjust = 0.5, size=14), 
        text = element_text(size = 13),
        axis.title = element_text(size = 13), axis.text = element_text(size = 10)) 
# Save Figure 5 to working drive 
ggsave("Figure_5.jpeg", plot = last_plot(), width = 7.5, height = 7.5, units = "in")
#dev.print(tiff, "Figure_5.tiff", res=1200, height=7.5, width=7.5, units="in")

ggplot(graph2, aes(x=YEAR, y=value, group=active_d)) +   
  geom_line(aes(linetype = factor(active_d))) + theme_bw() + facet_wrap(~type, ncol=2, scales = "free") +
  geom_blank(data = facetlims2) + 
  labs(x = "Year", y = "Lower Score = More Abuse; Higher Score = More Respect") +
  #ggtitle("Respect for Human Rights among Non-Democracies by Participation, 1992-2011") +
  geom_vline(xintercept = seq(2001, 2005, by = 0.001), colour="grey", linetype = "solid", alpha = 0.01) +
  theme(legend.position= "bottom", legend.title=element_blank(), 
        plot.title = element_text(hjust = 0.5, size=14), 
        text = element_text(size = 13),
        axis.title = element_text(size = 13), axis.text = element_text(size = 10)) 
# Save Figure 5b to working drive 
ggsave("Figure_5b.jpeg", plot = last_plot(), width = 7.5, height = 7.5, units = "in")
#dev.print(tiff, "Figure_5b.tiff", res=1200, height=7.5, width=7.5, units="in")

#------------------------------------------------------------------------------------
# Part C. Reproduce the statistical analysis after dropping problematic country-years
#------------------------------------------------------------------------------------

# Change table title
title1 <- "Participation in RDI program and state respect for human rights, 1992-2011, excluding country-years with human rights reports containing one or more detainee names"

# Run analysis and produce tables 
stargazer(mod_1(PHYSINT, lag_physint), mod_2(PHYSINT, lag_physint), mod_3(PHYSINT, lag_physint), mod_4(PHYSINT, lag_physint), 
          mod_1(latentmean, lag_latentmean), mod_2(latentmean, lag_latentmean), mod_3(latentmean, lag_latentmean), mod_4(latentmean, lag_latentmean),
          title = title1, covariate.labels = covariate_names, add.lines = fe_line, notes = footnote,
          dep.var.labels = c("Physical Integrity Score (CIRI)", "Latent Variable Model (Fariss)"),
          digits = 3, omit = c("lag_physint", "lag_latentmean"), omit.stat = c("adj.rsq", "f"), 
          notes.append = FALSE, notes.align = "l")

stargazer(mod_1(DISAP, lag_disap), mod_2(DISAP, lag_disap), mod_3(DISAP, lag_disap), mod_4(DISAP, lag_disap),  
          mod_1(POLPRIS, lag_polpris), mod_2(POLPRIS, lag_polpris), mod_3(POLPRIS, lag_polpris), mod_4(POLPRIS, lag_polpris),
          title = title1, covariate.labels = covariate_names, add.lines = fe_line, notes = footnote,
          dep.var.labels = c("Disappearance Score (CIRI)", "Political Imprisonment Score (CIRI)"),
          digits = 3, omit = c("lag_disap", "lag_polpris"), omit.stat = c("adj.rsq", "f"), 
          notes.append = FALSE, notes.align = "l")

stargazer(mod_1(Amnesty, lag_amnesty), mod_2(Amnesty, lag_amnesty), mod_3(Amnesty, lag_amnesty), mod_4(Amnesty, lag_amnesty),  
          mod_1(State, lag_state), mod_2(State, lag_state), mod_3(State, lag_state), mod_4(State, lag_state),
          title = title1, covariate.labels = covariate_names, add.lines = fe_line, notes = footnote, 
          dep.var.labels = c("Amnesty International Score (PTS)", "State Department Score (PTS)"),
          digits = 3, omit = c("lag_amnesty", "lag_state"), omit.stat = c("adj.rsq", "f"), 
          notes.append = FALSE, notes.align = "l")

stargazer(mod_1(TORT, lag_tort), mod_2(TORT, lag_tort), mod_3(TORT, lag_tort), mod_4(TORT, lag_tort),  
          mod_1(KILL, lag_kill), mod_2(KILL, lag_kill), mod_3(KILL, lag_kill), mod_4(KILL, lag_kill),
          title = title1, covariate.labels = covariate_names, add.lines = fe_line, notes = footnote,
          dep.var.labels = c("Torture Score (CIRI)", "Extrajudicial Killing Score (CIRI)"),
          digits = 3, omit = c("lag_tort", "lag_kill"), omit.stat = c("adj.rsq", "f"), 
          notes.append = FALSE, notes.align = "l")
