Examining behavioral search 'profiles' during a visual search task
==================================================================

Olivia Guayasamin 9/10/2017

Introduction
------------

This code demonstrate initial analyses to determine if peoples' behavioral "profiles" during a [visual search task](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt1) vary as a function of task difficulty and search outcome. [Previously](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt3), we assigned a single behavioral "profile" score to every search that each subject completed. These scores were estimated using a model generated from LDA analysis, an approach we took to reduce the dimensionality of our data sets that contained various gaze-trajectory and behavioral measurements.

This data set comes from a study where We asked subjects to complete several rounds of a repeated visual search task where their goal was to find as many target stimuli as possible (randomly dispersed inside a large "cloud" of similar looking distractors) within a limited amount of time. Each subject completed two blocks of trials, with one block containing only Easy versions of the task and the other containing only Hard versions. The order in which these versions were presented was counterbalanced across subjects. Subject gaze positions were recorded throughout the study using Tobii eye-trackers.Task difficulty was manipulated by varying the [crypticity of targets](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt2) present in each trial of our search task, and search outcomes were determined by classying each search as: True Positives **TP** were searches where the target was successfully identified, True Negatives **TN** describe searches where a distractor was correctly left alone, False Positives **FP** were searches where a distractor was incorrectly identifed as a target, and False Negatives **FN** describe searches where a target went unidentified.

This analysis is meant to answer questions like, *"Are distinct search outcomes described by different patterns of eye movement behaviors?"*, or *"How do subjects differ in their behavior profiles during successful and unsuccessful searches?"* and *"Are these behavioral differences influenced by search difficulty?"*. The data file is organized by subject, search outcome, crypticity treatment, and the order in which the treatment was presented. The effects of task difficulty and search outcome on behavior profile scores was determined with [GLMMs](http://www.theanalysisfactor.com/advantages-of-repeated-measures-anova-as-a-mixed-model/) containing treatment and search outcome as fixed effects, and individual subject and sub-block order as random effects. Subject consistency within and across crypticity treatments was estimated using Intraclass Correlation Coefficients ([ICC](http://www.theanalysisfactor.com/the-intraclass-correlation-coefficient-in-mixed-models/))and correlation analysis with [Spearman's Rho](https://www.r-bloggers.com/spearmans-rho/) respectiviely. Summaries, comparisons, and relationships are visualized with tables, boxplots, and scatterplots.

GLMMs were conducted using the [lme4](https://cran.r-project.org/web/packages/lme4/lme4.pdf) package and further analyzed using functions from the [car](https://cran.r-project.org/web/packages/car/car.pdf), [lmerTest](https://cran.r-project.org/web/packages/lmerTest/lmerTest.pdf), [MuMIn](https://cran.r-project.org/web/packages/MuMIn/MuMIn.pdf), and [pscl](https://cran.r-project.org/web/packages/pscl/pscl.pdf) packages. ICC values were estimated using the [psych](https://cran.r-project.org/web/packages/psych/psych.pdf) package, and the *χ*<sup>2</sup> and correlation analyses were performed using the [MASS](https://cran.r-project.org/web/packages/MASS/MASS.pdf) package.

**Summary:** Use GLMMS to determine how search behaviors differ across crypticity treatments and search outcomes. Run ICC and correlation analysis to determine how consistently subjects performed within and across difficulty treatments respectively. **Requires:** Data file, mentioned R libraries.**Outputs:** GLMM, ICC, and correlation analysis. Boxplot and scatterplot figures visualizing the data as jpeg files.

Initializing Steps
==================

Load libraries from CRAN
------------------------

``` r
# read and work with data files
library(reshape2)   
library(plyr)

# glmm analysis
library(lme4)
library(lmerTest)
library(MuMIn)
library(pscl)

# stats functions
library(psych)
library(car)  
library(MASS)

# format figures and test output
library(ggplot2)
library(grid)
library(gridExtra)  
library(knitr) 
library(broom)
```

Import and format data files
----------------------------

After importing our data files, we need to make sure that all data rows containing numbers representing levels are converted to factors. Otherwise, some of our later reshaping and analyses methods may get confused and throw an error.

``` r
# import data files containing LD1 scores
lda.Data.Long <- read.csv("val_LDA_Data_Long.csv")

# convert variables to factors
lda.Data.Long$PartID <- factor(lda.Data.Long$PartID)  
lda.Data.Long$TreatmentOrder <- factor(lda.Data.Long$TreatmentOrder)  
lda.Data.Long$BlockOrder <- factor(lda.Data.Long$BlockOrder)  
```

Using GLMMs to Determine Treatment Effects
==========================================

To determine the effect of crypticity treatment (Easy vs. Hard) on our search performance and behavior measures, we will essentially be conducting a Repeated Measures ANOVA analysis using Generalized Linear Mixed Models. There are several reasons why GLMMs can be a better alternative to Repeated Measures ANOVAs. We won't spend time discussing that here, but if you are interested in knowing more, the Analysis Factor blog gives a nice overview [here](http://www.theanalysisfactor.com/advantages-of-repeated-measures-anova-as-a-mixed-model/).

Check data distributions prior to analysis
------------------------------------------

This is so that we know how to properly specify our GLMMs in our code. Here, we will visualize data distributions using histograms and compare them to a normal distribution using qq-plots.

### Check to see if data is bi-modal

``` r
# add pos and neg labels to data set
pVn <- as.data.frame(matrix(nrow = nrow(lda.Data.Long), ncol = 1))

# subset data by search type and put into a in a list
for (i in 1:nrow(lda.Data.Long)){
  # figure out type
  if ((lda.Data.Long$search.Class[i] == 'Fp') || (lda.Data.Long$search.Class[i] == 'Tp')){
    pVn[i, 1] <- 'Positive'
  } else {
    pVn[i, 1] <- 'Negative'
  }
}

# change name of new col and append to existing data set
colnames(pVn) <- 'EndType'
lda.Data.Long <- cbind(lda.Data.Long, pVn)
  
# save search type histogram as jpeg image
jpeg(filename = "ldaResults_hist1.jpg", units = "in", height = 6, width = 10, res  = 300)  
ggplot(data = lda.Data.Long, aes(x = LD1, fill = EndType)) + 
  # histogram
  geom_histogram(alpha = 0.4, position = "identity", breaks = seq(-5, 8, 0.5)) +  
  # mean lines
  labs(x = "LD1 Score", y = "Count") + # axes labels and plot titles
  ggtitle("LD1 Scores by Pos. or Neg. Search Outcome", 
          subtitle = "Pos. = Tp & Fp | Neg. = Tn & Fn") +
  theme(plot.title = element_text(hjust = 0.5, size = 16), 
        plot.subtitle = element_text(hjust = 0.5, size = 12))
```

![](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt4/blob/master/imgs/ldaResults_hist1.jpg)

**Figure 1**

stuff here

### Check the normality of each distribution

``` r
# separat by endtype
lda.Pos <- subset(lda.Data.Long, EndType == 'Positive')
lda.Neg <- subset(lda.Data.Long, EndType == 'Negative')
posNeg.list <- list(lda.Pos, lda.Neg)
names(posNeg.list) <- c('Positives', 'Negatives')

# visualize data using hists and normal qq plots
fig.Title <- sprintf("ldaResults_qqPlots.jpg")
jpeg(filename <- fig.Title, units = "in", height = 6, width = 6, res = 300)
par(mfcol = c(2, 2)) # format figure window
# iterate throughDifferent Variables
for (i in 1:2){
  # for each variable plot a histogram and qqplot 
  cur.Data <- posNeg.list[[i]]$LD1
  thisPlot.Title <- sprintf("%s", names(posNeg.list[i]))  # format and add title
  hist(cur.Data, main = thisPlot.Title)  # histogram
  qqnorm(cur.Data); qqline(cur.Data)  # qqplot with normal line
}
```
![](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt4/blob/master/imgs/ldaResults_qqPlots.jpg)

**Figure 2**

stuff here

Specificy our GLMMs
-------------------

First we are going to look at the effect of task difficulty and search outcomes (TP, FN, and FN) on behavioral profiles using GLMMs. We will essentially be conducting Repeated Measures ANOVA analyses using Generalized Linear Mixed Models. There are several reasons why GLMMs can be a better alternative to Repeated Measures ANOVAs. We won't spend time discussing that here, but if you are interested in knowing more, the Analysis Factor blog gives a nice overview [here](http://www.theanalysisfactor.com/advantages-of-repeated-measures-anova-as-a-mixed-model/).

Normally, we would check our data distributions before hand, but we don't need to do so here because because we already visualized this data set in a prior [tutorial](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt3).

``` r
# run separate glmms for pos and neg dataframes in list
for (i in 1:2){
  # extract current data frame
  cur.Data <- posNeg.list[[i]]
  # so that we use the right functions
  require(lmerTest)
  
  # create model
  lda.Anova.Glmm <- glmer(LD1 ~ search.Class*ExpVer + (1|BlockOrder) + (1|PartID),
                          data = cur.Data, family = gaussian(link = identity),
                          na.action = na.exclude, 
                          REML = FALSE)
  
  # compute coeff results
  coefs <- data.frame(coef(summary(lda.Anova.Glmm)))
  # use normal distribution to approximate p-value
  coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
}
```

talk about it mentions that the residuals look good and if you want to take a look you can find them here [residuals](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt4/blob/master/imgs).


Examine one GLMM results
------------------------

just using one as an example

| term                       |    estimate|  std.error|    statistic| group      |
|:---------------------------|-----------:|----------:|------------:|:-----------|
| (Intercept)                |  -1.5416051|  0.1125732|  -13.6942427| fixed      |
| search.ClassTn             |  -0.0334213|  0.0841119|   -0.3973427| fixed      |
| ExpVermost                 |   0.8266510|  0.0850628|    9.7181242| fixed      |
| search.ClassTn:ExpVermost  |  -0.9393917|  0.1196265|   -7.8527069| fixed      |
| sd\_(Intercept).PartID     |   0.5746563|         NA|           NA| PartID     |
| sd\_(Intercept).BlockOrder |   0.0289960|         NA|           NA| BlockOrder |
| sd\_Observation.Residual   |   0.7235581|         NA|           NA| Residual   |

| .rownames                  |      X0.5..|     X99.5..|
|:---------------------------|-----------:|-----------:|
| sd\_(Intercept)|PartID     |   0.3680675|   0.7505657|
| sd\_(Intercept)|BlockOrder |   0.0000000|   0.1118345|
| sigma                      |   0.6587191|   0.7776333|
| (Intercept)                |  -1.8388186|  -1.2099219|
| search.ClassTn             |  -0.2390326|   0.2064321|
| ExpVermost                 |   0.6156785|   1.0490708|
| search.ClassTn:ExpVermost  |  -1.2759810|  -0.5974316|

|                              |  Estimate|  Standard Error|    DF|  t-value|  Lower CI|  Upper CI|  p-value|
|------------------------------|---------:|---------------:|-----:|--------:|---------:|---------:|--------:|
| search.Class:ExpVer Fn least |   -1.5416|          0.1126|  53.4|   -13.69|   -1.7674|   -1.3159|        0|
| search.Class:ExpVer Tn least |   -1.5750|          0.1126|  53.4|   -13.99|   -1.8008|   -1.3493|        0|
| search.Class:ExpVer Fn most  |   -0.7150|          0.1133|  54.8|    -6.31|   -0.9420|   -0.4879|        0|
| search.Class:ExpVer Tn most  |   -1.6878|          0.1126|  53.4|   -14.99|   -1.9135|   -1.4620|        0|

|                 |  Estimate|  Standard Error|    DF|  t-value|  Lower CI|  Upper CI|  p-value|
|-----------------|---------:|---------------:|-----:|--------:|---------:|---------:|--------:|
| search.Class Fn |   -1.1283|          0.1046|  39.9|   -10.79|   -1.3397|   -0.9168|        0|
| search.Class Tn |   -1.6314|          0.1044|  39.6|   -15.62|   -1.8425|   -1.4203|        0|

|              |  Estimate|  Standard Error|    DF|  t-value|  Lower CI|  Upper CI|  p-value|
|--------------|---------:|---------------:|-----:|--------:|---------:|---------:|--------:|
| ExpVer least |   -1.5583|          0.1044|  39.6|   -14.92|   -1.7694|   -1.3472|        0|
| ExpVer most  |   -1.2014|          0.1046|  39.9|   -11.48|   -1.4128|   -0.9899|        0|

|            |       Chi.sq|  Chi.DF|    p.value|
|------------|------------:|-------:|----------:|
| BlockOrder |    0.0760293|       1|  0.7827523|
| PartID     |  198.3603504|       1|  0.0000000|

| names |          x|
|:------|----------:|
| R2m   |  0.1471961|
| R2c   |  0.4775683|

Discuss results

### The Effects of Difficulty Treatment on LD1 Scores during Tp Outcomes

Want to use the full Tp dataset. Now, let's separately take a look at true positives. Because we are analyzing data that contains only two levels (easy and hard) of one effect (difficulty) we don't need to use a more complicated ANOVA or Glmm. Instead, we can rely on a more simple t-test to determine if behavioral profile scores during Tp searches differ significantly between difficulty levels. Because these data were previously determined to not meet the assumptions of required for parametric t-tests, we are going ot use a non- parametric [Wilcoxon Rank-Sum Test](https://www.r-bloggers.com/wilcoxon-rank-sum-test/).

``` r
# separate fp search rows from the rest
tp.Data <- subset(lda.Data.Long, lda.Data.Long$search.Class == 'Tp')
# divide fp search rows by difficulty treatment
tp.Easy <- subset(tp.Data, tp.Data$ExpVer == 'most')
tp.Hard <- subset(tp.Data, tp.Data$ExpVer == 'least')

# put median values into a table
tp.MedScores <- as.data.frame(rbind(median(tp.Easy$LD1, na.rm = TRUE), 
                                    median(tp.Hard$LD1, na.rm = TRUE)),
                              row.names = c('Easy', 'Hard'),
                              col.names = 'LD1 Score Medians')

# run wilcox test
tp.Test <- wilcox.test(tp.Easy$LD1, tp.Hard$LD1, 
                       paired = TRUE, conf.int = TRUE, 
                       na.action = na.exclude)
```

|      |    V1|
|------|-----:|
| Easy |  2.39|
| Hard |  2.17|

|  estimate|  statistic|  p.value|  conf.low|  conf.high|
|---------:|----------:|--------:|---------:|----------:|
|      0.27|       6923|        0|      0.09|       0.46|


Looking at the median values, there is a small difference in profile score (~0.15) across the difficulty treatments, the p-value associate with this difference is barely significant, and the lower confidence interval is quite close to zero. All of these indicate that there is technically significant, but slight effect of task difficulty on FP behavior profile scores. Looking back at our [analysis](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt3) that produced the behavior profile scores, we can see that higher scores are associated with more time spent searching and sampling within the target. Translating this finding to the results of FP scores, this means that during the Easy treatment, subjects spent more time searching within FP targets than duirng the Hard treatment. This finding makes sense, considering that in the Easy treatment, targets should have been easier to spot, and confusing a non-target with an actually target should create more cognitive dissonance.

Determine within-treatment consistency of subject search profiles
-----------------------------------------------------------------

Sadly, because FP search outcomes were so rare, there is not enough data to apply additional analyses.

The intraclass correlation coefficent (ICC) that will be applied here is the ICC 2, because I am considering sub-block order (judge) as a random effect. This ICC measures subject rank consistency across samples, while controlling for the effects of varying group means. It is not an absolute measure of agreement. Search duration for TP searches will once again be used as an example.

No Fp here, because again, the sample size is so small and inconsistent.

### Reformat data for analyses and plotting

Subset the data

``` r
# create string with search type names
search.Names <- cbind('Fp', 'Tp', 'Fn', 'Tn')

# init lists
outcomes.List <- list()

# subset data by search type and put into a in a list
for (i in 1:ncol(search.Names)){
  outcomes.List[[i]] <- subset(lda.Data.Long[c(1, 2, 4, 6)], 
                               lda.Data.Long$search.Class == search.Names[i])
  }
# change names
names(outcomes.List) <- search.Names
```

Then go through it

### Conduct ICC analyses

``` r
# empty list
dat.4Corr <- list() 

# iterate through data frames in list to format data for ICC and correlation
# analysis
for (i in 2:length(outcomes.List)) {
  # select data for ICC analysis
  cur.Corr <- outcomes.List[[i]]
  
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
  icc.Name <- sprintf("%s ICC Results", search.Names[i])
  # print
  print(kable(cur.ICC.Output, caption = icc.Name, results = 'asis', digits = 2))
}
```

|             | V1        | V2        | V3           | V4           |
|-------------|:----------|:----------|:-------------|:-------------|
| Treatment   | Easy      | Easy      | Hard         | Hard         |
| type        | ICC2      | ICC2k     | ICC2         | ICC2k        |
| ICC         | 0.6623663 | 0.8869694 | 0.2860182    | 0.6157369    |
| F           | 9.009525  | 9.009525  | 2.669457     | 2.669457     |
| df1         | 36        | 36        | 35           | 35           |
| df2         | 108       | 108       | 105          | 105          |
| p           | 0         | 0         | 6.307545e-05 | 6.307545e-05 |
| lower.bound | 0.4736108 | 0.7825586 | 0.08210186   | 0.26350475   |
| upper.bound | 0.8183363 | 0.9474202 | 0.5360318    | 0.8221046    |

|             | V1          | V2          | V3           | V4           |
|-------------|:------------|:------------|:-------------|:-------------|
| Treatment   | Easy        | Easy        | Hard         | Hard         |
| type        | ICC2        | ICC2k       | ICC2         | ICC2k        |
| ICC         | 0.1190205   | 0.3508182   | 0.5034608    | 0.8022057    |
| F           | 1.525015    | 1.525015    | 5.019074     | 5.019074     |
| df1         | 31          | 31          | 36           | 36           |
| df2         | 93          | 93          | 108          | 108          |
| p           | 0.06315376  | 0.06315376  | 3.843326e-11 | 3.843326e-11 |
| lower.bound | -0.06813046 | -0.34253253 | 0.2880977    | 0.6181382    |
| upper.bound | 0.3926140   | 0.7211065   | 0.7120399    | 0.9081795    |

|             | V1           | V2           | V3           | V4           |
|-------------|:-------------|:-------------|:-------------|:-------------|
| Treatment   | Easy         | Easy         | Hard         | Hard         |
| type        | ICC2         | ICC2k        | ICC2         | ICC2k        |
| ICC         | 0.5655578    | 0.8388970    | 0.6043178    | 0.8593357    |
| F           | 6.614153     | 6.614153     | 7.115602     | 7.115602     |
| df1         | 36           | 36           | 36           | 36           |
| df2         | 108          | 108          | 108          | 108          |
| p           | 1.021405e-14 | 1.021405e-14 | 8.881784e-16 | 8.881784e-16 |
| lower.bound | 0.3564142    | 0.6889752    | 0.4022159    | 0.7290985    |
| upper.bound | 0.7555809    | 0.9251796    | 0.7813344    | 0.9346096    |

Looking at the values for the "ICC", "p", "lower.bound", and "upper.bound" rows for the Easy and Hard crypticity treatments (Table 2), we can see that subjects are significantly consistent in their behavior during both treatments. Interestingly however, it does appear that subjects were less consistent in during the Hard treatment than during the Easy treatment, suggesting that increasing target crypticity results in greater within-subject behavioral variance.

Boxplots to visualize search outcome and treatment effects
----------------------------------------------------------

To visualize treatment effects and subject variation, we will use boxplots overlaid on top of individual lines representing subject averages across treatments. Below each boxplot figure, we will include a histogram showing the distribution of within subject differences across treatments. These figures will include data from all measures, so that we can discuss results from the measures that were not used as examples for the GLMM and ICC analyses.

Show all three

![](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt4/blob/master/imgs/ldaResults_BoxPlot_Tp.jpg)

**Figure 3**

![](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt4/blob/master/imgs/ldaResults_BoxPlot_Tn.jpg)

**Figure 4**

![](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt4/blob/master/imgs/ldaResults_BoxPlot_Fn.jpg)

**Figure 5**

Discuss Results

Correlation Analysis Across Difficulty Treatments
=================================================

Using the data that we formatted in a previous section

Determine across-treatment consistency of subject search profiles
-----------------------------------------------------------------

Then we will run correlate the subject averages across treatments using Spearman's *ρ*, a rank-based correlation statistic for use with non-parametric data distributions.

``` r
# change names of list slots to variable names
names(dat.4Corr) <- search.Names

# calculate correlations for data from each experimental group
for (i in 2:length(dat.4Corr)){
  # run spearmans correlation
  corr.res <- cor.test(dat.4Corr[[i]][, 2], dat.4Corr[[i]][, 3],
                       method = "spearman", na.action = na.exclude)  
  # print with nice format
  corr.4Print <- cbind(names(dat.4Corr[[i]][2]), 
                       names(dat.4Corr[[i]][3]),
                       corr.res$estimate, 
                       corr.res$p.value)
  # change names
  colnames(corr.4Print) <- cbind("Var1", "Var2", "Corr-Value", "P-Value")
  # create table title
  tab.Cap <- sprintf("Correlation Results for %s", search.Names[i])
  # print for immediate output
  print(kable(corr.4Print, results = 'asis', caption = tab.Cap, digits = 2))
}
```

|     | Var1 | Var2  | Corr-Value        | P-Value              |
|-----|:-----|:------|:------------------|:---------------------|
| rho | Most | Least | 0.644787644787645 | 3.36533526597102e-05 |

|     | Var1 | Var2  | Corr-Value        | P-Value              |
|-----|:-----|:------|:------------------|:---------------------|
| rho | Most | Least | 0.595674486803519 | 0.000416581254664065 |

|     | Var1 | Var2  | Corr-Value        | P-Value             |
|-----|:-----|:------|:------------------|:--------------------|
| rho | Most | Least | 0.440493124703651 | 0.00681259815161805 |

Interestingly, all of the correlations are moderate to strong, positive, and significant with the exception of NumTP, the number of true-positive searches (Table 5). While all subjects did complete more TP searches in the Easy vs Hard crypticity treatment (Figure 6), subject ranking must have changed significantly across treatments. For example, the individuals who found the most targets during the Easy treatment, were not the same individuals who found the most targets during the Hard treatment.

Scatterplots to view across-treatment relationships
---------------------------------------------------

Let's visualize these correlations using scatterplots overlaid with a line of best fit.

![](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt4/blob/master/imgs/ldaResultsCorrPlots_Tp.jpg)

**Figure 6**

![](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt4/blob/master/imgs/ldaResultsCorrPlots_Tn.jpg)

**Figure 7**

![](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt4/blob/master/imgs/ldaResultsCorrPlots_Fn.jpg)

**Figure 8**

The lack of correlation for the number of true-positive searches is quite apparent.

Conclusions
===========
