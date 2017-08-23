# val_LDAResultsGlmm.R
# 
# TODO CHANGE SUMMARY 
# 
# Summary:
# Do an ANOVA type analysis with GLMMs to determine how search difficulty and 
# search type affects search behavior LD1 scores. Do ICC and correlation 
# analysis to determine how consistently subjects performed within and across 
# difficulty treatments respectively. Use model comparisons to determine if 
# included search behavior helps to explain search performance outcomes.
#
# Requires: LDA behavior data files, listed libraries
#
# Outputs: Results of Glmms, model comparisons, R squared, ICCs, and Spearman's
# correlation analysis. Also output figures detailing Glmm and correlation
# results.
# 
# Author: Olivia Guayasamin
# Date: 8/16/2017
# 
# ----- Initializing steps -----

# read and work with data files
library(reshape2)   

# glmm analysis
library(lme4)
library(lmerTest)
library(MuMIn)
library(pscl)

# additional stats functions
library(psych)
library(car)  
library(MASS)

# format figures and output
library(ggplot2)
library(grid)
library(gridExtra)  
library(broom)
library(knitr) 

# quick function for getting legend from a ggplot
get_legend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

#set working directory
getwd()
setwd("~/R/val_LDAResultsGlmm")  #TODO CHANGE 

# Save all console output to file
sink("val_LDAResultsGlmm.doc", append = FALSE)  # initialize output sink


# ----- Import and format data

# import data files containing LD1 scores
lda.GenData <- read.csv("val_LDA_Data.csv")
perf.GenData <- read.csv("val_GenResultsBlockedData.csv")

# separate fp search rows from the rest
fp.Data <- subset(lda.GenData, lda.GenData$search.Class == 'Fp')
# divide fp search rows by difficulty treatment
fp.Easy <- subset(fp.Data, fp.Data$DifTreatment == 'most')
fp.Hard <- subset(fp.Data, fp.Data$DifTreatment == 'least')


# remove fp search rows from main data set
lda.GenData <- subset(lda.GenData, lda.GenData$search.Class != 'Fp')

# convert need variables to factors
lda.GenData$PartID <- factor(lda.GenData$PartID)  
lda.GenData$search.Class <- factor(lda.GenData$search.Class)  
lda.GenData$MediaOrder <- factor(lda.GenData$MediaOrder)  
lda.GenData$BlockOrder <- factor(lda.GenData$BlockOrder)  
lda.GenData$DifTreatment <- factor(lda.GenData$DifTreatment)  


perf.GenData$PartID <- factor(perf.GenData$PartID)  
perf.GenData$ExpVer <- factor(perf.GenData$ExpVer)  
perf.GenData$BlockOrder <- factor(perf.GenData$BlockOrder)  
perf.GenData$TreatmentOrder <- factor(perf.GenData$TreatmentOrder)  


# ----- ANOVA type analysis with Glmms-----

# so that we use the right functions
require(lmerTest)

# create model
lda.Anova.Glmm <- glmer(LDA.LD1 ~ search.Class*DifTreatment + (1|BlockOrder) + (1|PartID),
                  data = lda.GenData, family = gaussian(link = identity),
                  na.action = na.exclude, 
                  REML = FALSE)

# plot results
coefs <- data.frame(coef(summary(lda.Anova.Glmm)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))

# kable output for rmkd file
print(sprintf("Results for Glmm examing LD1 Scores"))
print(summary(lda.Anova.Glmm))
print(coefs)  # full summary, with p values for fixed effects
print(confint(lda.Anova.Glmm, method = 'boot', level = 0.99, oldNames = FALSE))  # confidence intervals for everything
print(lsmeansLT(lda.Anova.Glmm, test.effs = "search.Class:DifTreatment", alpha = 0.99))  # get estimates for all levels of fixed effect
print(lsmeansLT(lda.Anova.Glmm, test.effs = "search.Class", alpha = 0.99))
print(lsmeansLT(lda.Anova.Glmm, test.effs = "DifTreatment", alpha = 0.99))
print(rand(lda.Anova.Glmm))  # p values for random effects and slopes, likelihood ratio test on random effects of a linear mixed effects model

# print conditional r squared and marginal r squared
cur.Glmm.Rsquareds <- r.squaredGLMM(lda.Anova.Glmm)
print(cur.Glmm.Rsquareds)

# for each variable plot residuals
fig.Title <- "glmmResidualPlots.LD1Score.jpg" # file name
resid.Title <- "residuals.LD1Score" # plot title
jpeg(filename <- fig.Title, units = "in", height = 6, width = 6, res = 300)
qqnorm(resid(lda.Anova.Glmm), main = resid.Title) ; qqline(resid(lda.Anova.Glmm))
dev.off()  # close and save figures
dev.off()

# analysis on FP with wilcox tests
fp.Test <- wilcox.test(fp.Easy$LDA.LD1, fp.Hard$LDA.LD1, 
                         paired = TRUE, conf.int = TRUE, 
                         na.action = na.exclude)
sprintf("Median value of Fp LD1 Scores in Easy Treatment %f", 
        median(fp.Easy$LDA.LD1, na.rm = TRUE))
sprintf("Median value of Fp LD1 Scores in Hard Treatment %f", 
        median(fp.Hard$LDA.LD1, na.rm = TRUE))
print(kable(tidy(fp.Test), 
            caption = "Fp LD1 Scores, Easy vs. Hard Treatment",
            digits = 2))


# ----- Reformat search data -----

# create string with search type names
search.Names <- cbind('Tp', 'Fn', 'Tn')

# init lists
search.Types <- list()
dat.4Corr <- list()

# subset data by search type and put into a in a list
for (i in 1:ncol(search.Names)){
  search.Types[[i]] <- subset(lda.GenData[, cbind(2:7)], 
                              lda.GenData$search.Class == search.Names[i])
  colnames(search.Types[[i]]) <- colnames(lda.GenData[, cbind(2:7)])
}
# change names
names(search.Types) <- search.Names


# ----- Correlation Analysis Across Treatments and ICC Within Treatments -----
# TN easy vs tn hard, tp easy vs tp hard easy, fn easy vs fn hard

# iterate through data columns in list to format data for ICC and correlation
# analysis
for (i in 1:length(search.Types)) {
  # select data for ICC analysis
  cur.Corr <- search.Types[[i]][, c(2, 5, 4, 6)]
  
  # current variable data
  cur.Most.Corr <- dcast(subset(cur.Corr[, c(1, 3, 4)], 
                                cur.Corr[, 2] == 'most'),
                                PartID ~ BlockOrder)
  cur.Least.Corr <- dcast(subset(cur.Corr[, c(1, 3, 4)], 
                                 cur.Corr[, 2] == 'least'), 
                                 PartID ~ BlockOrder)
  # take average of column values and convert to data frame
  cur.Data.Sum <- as.data.frame(cbind(cur.Most.Corr[1],
                                      rowMeans(cur.Most.Corr[2:5]), 
                                      rowMeans(cur.Least.Corr[2:5])))
  colnames(cur.Data.Sum) <- cbind('PartID', 'Most', 'Least')
  # add to list
  dat.4Corr[[i]] <- cur.Data.Sum
  
  # determine subject behavioral consistency within treatments
  # ICC analysis
  most.ICC <- ICC(na.omit(cur.Most.Corr[, 2:5]), alpha = 0.01)
  least.ICC <- ICC(na.omit(cur.Least.Corr[, 2:5]), alpha = 0.01)
  
  # temporary data frames for outputting results to file
  t.Easy = t(as.data.frame(c(Treatment = "Easy",
                             rbind(subset(most.ICC$results, 
                                          most.ICC$results$type == 'ICC2'),
                                   subset(most.ICC$results,
                                          most.ICC$results$type == 'ICC2k')))))
  t.Hard = t(as.data.frame(c(Treatment = "Hard",
                             rbind(subset(least.ICC$results, 
                                          least.ICC$results$type == 'ICC2'),
                                   subset(least.ICC$results,
                                          least.ICC$results$type == 'ICC2k')))))
  
  # combine into one data frame
  cur.ICC.Output <- as.data.frame(cbind(t.Easy, t.Hard))
  
  # filename, TODO kable this output later
  sprintf("%s ICC Results", search.Names[i])
  # print
  print(cur.ICC.Output)
}

# change names of list slots to variable names
names(dat.4Corr) <- search.Names

# calculate correlations for data from each experimental group
for (i in 1:length(dat.4Corr)){
  # run spearmans correlation
  corr.res <- cor.test(dat.4Corr[[i]][, 2], dat.4Corr[[i]][, 3],
                       method = "spearman", na.action = na.exclude)  
  # print with nice format
  print(sprintf("Spearman's correlation results for %s", names(dat.4Corr[i])))
  corr.4Print <- cbind(names(dat.4Corr[[i]][2]), 
                       names(dat.4Corr[[i]][3]),
                       corr.res$estimate, 
                       corr.res$p.value)
  # change names
  colnames(corr.4Print) <- cbind("Var1", "Var2", "Corr-Value", "P-Value")
  # put results into list
  # print for immediate output
  print(corr.4Print)
}


# ----- Boxplots to compare treatments -----
# use boxplots and histograms to show subject change in behavior as a result
# of difficulty treatment

# empty list
melted.Plot.Data <- list()
melted.Hist.Data <- list()

# go through data and reshape for plotting
for (i in 1:length(dat.4Corr)){
  
  # prep data frame for boxplot 1
  names(dat.4Corr[[i]]) <- cbind("SubjectID", "Easy", "Hard")
  melted.Plot.Data[[i]] <- melt(dat.4Corr[[i]], id = "SubjectID", 
                                value.name = "LD1Score",
                                variable.name = "SearchDifficulty")
  # prep data frame for histogram 1
  melted.Hist.Data[[i]] <- dat.4Corr[[i]][3] - dat.4Corr[[i]][2]
  colnames(melted.Hist.Data[[i]]) <- "Change"
}

# empty plot list
plot.List <- list()


# iterate throught each of the three search types and plot
for (i in 1:length(melted.Plot.Data)){
  plot1 <- ggplot(melted.Plot.Data[[i]], aes(x = SearchDifficulty,
                                             y = LD1Score)) + 
    geom_line(aes(group = SubjectID, colour = SubjectID), size = 1) +
    geom_point(aes(group = SubjectID, colour = SubjectID), size = 1.5) + 
    geom_boxplot(aes(fill = SearchDifficulty), width = 0.6, alpha = 0.6, lwd = 1,
                 notch = TRUE, outlier.shape = NA) +
    labs(title = "Effects of Difficulty Treatment on",  # change labels and titles
         subtitle = sprintf("%s Search LD1 Score", search.Names[i]),  
         y = "LD1 Score") +
    theme_minimal() + 
    theme(plot.title = element_text(hjust = 0.5, size = 12), 
          plot.subtitle = element_text(hjust = 0.5, size = 10)) +
    scale_fill_manual(values = c("#072bab", "#0ab2f4")) +  # boxplot fill
    guides(colour = FALSE, size = FALSE)  # remove legend for subjects & size

  # save search type histogram as jpeg image
  hist1 <- ggplot(melted.Hist.Data[[i]], aes(x = Change)) + 
    # histogram
    geom_histogram(alpha = 0.7, position = "identity", bins = 10, 
                   fill = "#0072BB") +
    # with normal dist on top
    geom_vline(xintercept = 0, linetype = "dashed", color = 'blue') +
    labs(title = expression(paste("Subject ", Delta, " Across Treatments")), 
         subtitle = sprintf("%s Search LD1 Score", search.Names[i]),
         x = expression(paste(Delta ["H - E"])), 
         y = "Count") + # axes labels and plot titles
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 12), 
          plot.subtitle = element_text(hjust = 0.5, size = 10),
          legend.position = "none")
  
  # use grid arrange to print plots and save as image
  fig.Title <- sprintf("ldaResultsBarPlots_%s.jpg", search.Names[i]) # title
  jpeg(filename <- fig.Title, units = "in", height = 4, width = 9, res = 300)
  grid.arrange(plot1, hist1, nrow = 1, ncol = 2, 
                              widths = c(0.6,0.4))
  dev.off()  # close and save figures
}
dev.off()


# ----- Scatterplots to view relationships -----

# plot scatterplots to visualize relationships 
for (i in 1:length(dat.4Corr)){
  # format data for plotting
  corr1Plot <- dat.4Corr[[i]]

  # plot scatterplots
  # use grid arrange to print plots and save as image
  fig.Title <- sprintf("ldaResultsCorrPlots_%s.jpg", search.Names[i]) # title
  # scatterplot
  corr1 <- ggplot(corr1Plot, aes(x = Easy, y = Hard)) +
    geom_point(shape = 1, size = 2, na.rm = TRUE) +
    geom_smooth(method = lm, se = TRUE) +
    labs(title = sprintf("%s LD1 Search Scores", search.Names[i], 
                         subtitle = "Across Treatments", 
                         x = sprintf("Easy"),
                         y = sprintf("Hard"))) +  # change text
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14),
          plot.subtitle = element_text(hjust = 0.5, size = 12)) 
  
  # save file
  jpeg(filename = fig.Title, units = "in", height = 5, width = 5,  res = 300)
  grid.arrange(corr1, nrow = 1, ncol = 1)
  dev.off()  # close and save figures
}
dev.off()

# ----- Clean Up -----
# 
# End Sink
sink()
closeAllConnections() # make sure sink saves

# Detach libraries
detach(package:reshape2)

detach(package:lmerTest)
detach(package:lme4)
detach(package:MuMIn)
detach(package:pscl)
detach(package:car)
detach(package:MASS)

detach(package:ggplot2)
detach(package:gridExtra)
detach(package:grid)
detach(package:knitr)

