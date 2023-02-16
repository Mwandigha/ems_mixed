# BACKGROUND

This page is an add on to the Essential Medical Statistics (EMS) module for Multi-level modelling for continous outcomes. To complement the Stata code demonstrated in the theory and practical session, the use of R for analyses is demonstrated with the aid of an example (used in the theory session using Stata).

# Required R packages

The following R packages should be Installed (and loaded)

-   tidyverse
-   datasets
-   multilevelTools
-   fishmethods
-   lme4
-   lmerTest
-   geepack
-   stats

You may find it easier to mass install (and load) all the R packages using the pacman R package.

```{r eval = FALSE, echo = FALSE}

### Not run

if(!(require("pacman"))){install.packages("pacman")}# will install if R package not installed already

library("pacman")

pacman::p_load("tidyverse"
               "datasets"
               "multilevelTools"
               "fishmethods"
               "lme4"
               "lmerTest"
               "geepack"
               "stats")


```

If the packages are already installed on your machine, you may mass load them without using the *R package pacman* as follows

```{r eval = FALSE, echo = FALSE}

lib.load.list <- c("tidyverse"
                    "datasets"
                    "multilevelTools"
                    "fishmethods"
                    "lme4"
                    "lmerTest"
                    "geepack"
                    "stats")


lapply(lib.load.list,
       character.only=FALSE,
       library)
```

# Data

The data to be used are available in the *R package datasets*. These consisnt of 50 chicks assigned to four different diets to evaluate which of the diets would result in the greatest weight gain over the course of the study, with weight measurements taken roughly every two days.

```{r eval = FALSE, echo = FALSE}

data.chickens <- as_tibble(datasets::ChickWeight)


###
data.chickens%>%
            head(n=5)

data.chickens <- data.chickens%>%
                  dplyr::mutate(Chick=factor(Chick, order=FALSE))

data.chickens%>%
            head(n=5)


##### Convert from long to wide (if desired)

data.chickens.wide <- data.chickens%>%
                       tidyr::pivot_wider(id_cols=c("Chick","Diet"),
                                          names_from=c("Time"),
                                          values_from ="weight",
                                          names_prefix="Wgt_t_")


data.chickens.wide
```

As the chicks have their weights taken at different time points, we may wish to check if all the chicks had an equal number of measurements taken. This means that the data structure would be balanced (very rare to have this happen).

```{r eval = FALSE, echo = FALSE}

# Checking if data are balanced?
data.chickens%>%
  dplyr::group_by(Chick)%>%
    dplyr::summarise(number_of_measurements=n())
   

## Approximate number of chickens assigned to diet
data.chickens%>%
   dplyr::count(Diet)

## Clearly, repeated measures
data.chickens%>%
  dplyr::count(Chick)%>%
      print(n=5)

```

# Exploratory Data Analysis

To explore the data, indivuidual profile plots (informally known as sphagehtti plots) may be generated using the code below

```{r eval = FALSE, echo = FALSE}

# Evolution of weights for the Chicks

subject.trend <- ggplot2::ggplot(data=data.chickens)+
                  ggplot2::geom_line(aes(x=Time,
                                         y=weight,
                                         group=Chick,
                                         colour=Diet),
                                     show.legend=FALSE)+
                       ggplot2::xlab("Time (days)")+
                        ggplot2::ylab("Weight (grams)")+
                         ggplot2::theme_bw()+
                          ggplot2::ggtitle("Subject (chick) specific trend")
                          
subject.trend

```

```{r eval = FALSE, echo = FALSE}

# Evolution of weights for the Chicks (by diet)

data.chickens.trend <- data.chickens%>%
                        dplyr::group_by(Diet,Time)%>%
                          dplyr::summarise(mean_weight=mean(weight))%>%
                           dplyr::ungroup()
   

diet.trend <- ggplot2::ggplot(data=data.chickens.trend)+
                ggplot2::geom_line(aes(x=Time,
                                       y=mean_weight,
                                       colour=Diet))+
                  ggplot2::xlab("Time (days)")+
                   ggplot2::ylab("Mean weight (grams)")+
                    ggplot2::theme_bw()+
                     ggplot2::ggtitle("Average evolution of weight by diet")


```

```{r eval = FALSE, echo = FALSE}

####
plot.grids <- gridExtra::grid.arrange(subject.trend,
                                      diet.trend,
                                      ncol=2,
                                      nrow =1)


```

![Image text](plots/diet_trends.png) *Chick specific and population average weight trend.*

## Method 1: Disregard subject (Chick) effect

As the weight measurements belonging to an individual chick are correlated, it is prudent that this aspect of the data is captured in the modelling. Failure to do this, will inadvertently inflate the type I error (false positive) rate ($\alpha$). By fitting a linear regression model, the correlation structure is disregarded.

```{r eval = FALSE, echo = FALSE}

lm(weight~Time+Diet,
   data=data.chickens)%>%
         summary()


```

```{r eval = FALSE, echo = FALSE}

Call:
lm(formula = weight ~ Time + Diet, data = data.chickens)

Residuals:
     Min       1Q   Median       3Q      Max 
-136.851  -17.151   -2.595   15.033  141.816 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  10.9244     3.3607   3.251  0.00122 ** 
Time          8.7505     0.2218  39.451  < 2e-16 ***
Diet2        16.1661     4.0858   3.957 8.56e-05 ***
Diet3        36.4994     4.0858   8.933  < 2e-16 ***
Diet4        30.2335     4.1075   7.361 6.39e-13 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 35.99 on 573 degrees of freedom
Multiple R-squared:  0.7453,	Adjusted R-squared:  0.7435 
F-statistic: 419.2 on 4 and 573 DF,  p-value: < 2.2e-16


```

### Crude approximation of correlation

The correlation (here positive due to increasing subsequent weight measurements) can be approximated prior to modelling using the functions below. It is important to note that these measure of *Intra Cluster Correlation (ICC)* is not obtained from a model, hence it is not adjusted for any important predictors. The ICC, the proportion of between variability explained due to correlation is about 0.1 (or 10%).

```{r eval = FALSE, echo = FALSE}

multilevelTools::iccMixed(dv="weight",
                          id="Chick",
                          data=data.chickens)


```

```{r eval = FALSE, echo = FALSE}

       Var     Sigma       ICC
1:    Chick  541.1698 0.1066265
2: Residual 4534.2083 0.8933735

```

To estimate confidence intervals to determine whether the estimated correlation is statisticaly signficant, the function below may be used.

```{r eval = FALSE, echo = FALSE}

# type:  1 = Equation 5.8 of Lohr (1999),
#        2 = Equation 5.10 in Lohr (1999) and
#        3 = ANOVA
#	estimate:  variance and percentiles of icc via boostrapping. 0 = No estimation (Default), 
#                                                              1 = Estimate

icc_estimate  <- fishmethods::clus.rho(popchar=data.chickens$weight,
                                       cluster =data.chickens$Chick,
                                       type = c(1,2,3), 
                                       est = 1,
                                       nboot = 1000)
                                       
icc_estimate$icc


```

The confidence intervals estimated do not include zero, so it may be concluded that the (crude) correlation, from this presumptive check, is statistically significant which suggests ignoring the correlation structure would be ill-advised.

```{r eval = FALSE, echo = FALSE}

 Value          Var      P2.5%    P97.5%
Lohr rho          0.1001972 0.0009185980 0.04390876 0.1620093
Adjusted r-square 0.1017534 0.0009160843 0.04551967 0.1634451
ANOVA rho         0.1034762 0.0009441512 0.04633481 0.1660160                                                            
```

## Method 2: Disregard subject (chick) effect

As disregarding the correlation would be inappropriate, a simpler but accurate treatment of this problem may be dealt with using cluster (here chick) level summaries.

```{r eval = FALSE, echo = FALSE}

Chick.level.summary <- data.chickens%>%
                         dplyr::group_by(Chick,Diet)%>%
                          dplyr::summarise(mean_weight=mean(weight))
                          
Chick.level.summary%>%
    dplyr::group_by(Diet)%>%
       dplyr::count(Diet)



```

This unfortunately results in loss of information as the individual level data that may be of interest are not modelled.

```{r eval = FALSE, echo = FALSE}

# A tibble: 4 × 2
# Groups:   Diet [4]
  Diet      n
  <fct> <int>
1 1        20
2 2        10
3 3        10
4 4        10 

```

```{r eval = FALSE, echo = FALSE}

lm(mean_weight~Diet,
   data=Chick.level.summary)%>%
    summary()


```

```{r eval = FALSE, echo = FALSE}

Call:
lm(formula = mean_weight ~ Diet, data = Chick.level.summary)

Residuals:
    Min      1Q  Median      3Q     Max 
-61.054 -15.088  -0.622  15.946  61.883 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)   98.054      6.283  15.607  < 2e-16 ***
Diet2         24.562     10.882   2.257 0.028795 *  
Diet3         44.896     10.882   4.126 0.000153 ***
Diet4         36.656     10.882   3.368 0.001537 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 28.1 on 46 degrees of freedom
Multiple R-squared:  0.3202,	Adjusted R-squared:  0.2759 
F-statistic: 7.222 on 3 and 46 DF,  p-value: 0.000452    

```

## Method 3 : Multilevel model

Also called *mixed effects*, *hierarchical* or *random effects* models. These models estimate correlation using a latent structure of variables called random effects. This enables the subject (or cluster) specific estimating of parameters as well as population level estimation that may also be of interest. In R, there are several R packages that are used for these models. Here, we opt for the more widely used - *lme4* (for modelling) and *lmerTest* (for assessing statistical significance).

### Random intercept model

This model would cater for the fact that upon hatching, individual chicks would have differentiated weights. This variation may be important because the starting weight may have an influence over time on weight gain irrespective of the diet. Note that the default (and unbiased) method for fitting mixed-effects model in R is *Restricted Maximum Likelihood (REML)*. The use of *Maximum Likelihood (ML)* specified by setting the argument *REML=FALSE* in the *lmer()* function. The different options available for the function may be obatined by running the command *?lme4::lmer()* or *help(package="lme4")*

```{r eval = FALSE, echo = FALSE}

lmer(weight~Time + Diet + (1|Chick),
     data=data.chickens)%>%
          summary()%>%
           print(digits=4)


```

The *ICC* can be calculated using the variance components under the *Random effects* section. For our data, the ICC is $525.4/ (525.4+799.4)=0.3966$ (or approximately 10%). Thus, the crude estimate calculated earlier was an underestimate of the correlation. This emphasises that the crude estimate is simply an exploratory figure, which by itself should not sufficient to influence modelling decisions.

```{r eval = FALSE, echo = FALSE}

Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
Formula: weight ~ Time + Diet + (1 | Chick)
   Data: data.chickens

REML criterion at convergence: 5584

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-3.0591 -0.5779 -0.1182  0.4962  3.4515 

Random effects:
 Groups   Name        Variance Std.Dev.
 Chick    (Intercept) 525.4    22.92   
 Residual             799.4    28.27   
Number of obs: 578, groups:  Chick, 50

Fixed effects:
            Estimate Std. Error       df t value Pr(>|t|)    
(Intercept)  11.2438     5.7887  57.9038   1.942 0.056960 .  
Time          8.7172     0.1755 531.4437  49.684  < 2e-16 ***
Diet2        16.2100     9.4643  46.2232   1.713 0.093461 .  
Diet3        36.5433     9.4643  46.2232   3.861 0.000349 ***
Diet4        30.0129     9.4708  46.3486   3.169 0.002709 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Correlation of Fixed Effects:
      (Intr) Time   Diet2  Diet3 
Time  -0.307                     
Diet2 -0.550 -0.015              
Diet3 -0.550 -0.015  0.339       
Diet4 -0.550 -0.011  0.339  0.339


```

### Random intercept and slope model

The random intercept model only captures the variation in the starting weights of the chicks but assumes that the the slope of incremental growth is the same for all chicks over time. This may not be reasonable. A random intercept and slope (for time) may be included in the model as follows.

```{r eval = FALSE, echo = FALSE}

lmer(weight~Time + Diet + (1 + Time|Chick),
     data=data.chickens)%>%
  summary()%>%
     print(digits=4)


```

The ICC due to random intercept and slope are now approximately 0.4642 and 0.0426 (altogether 0.5068). This model on the face of it seems to be better than random intercept model.

```{r eval = FALSE, echo = FALSE}

Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
Formula: weight ~ Time + Diet + (1 + Time | Chick)
   Data: data.chickens

REML criterion at convergence: 4803.8

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-2.7617 -0.5758 -0.0353  0.4789  3.5025 

Random effects:
 Groups   Name        Variance Std.Dev. Corr 
 Chick    (Intercept) 153.87   12.40         
          Time         14.13    3.76    -0.98
 Residual             163.45   12.78         
Number of obs: 578, groups:  Chick, 50

Fixed effects:
            Estimate Std. Error      df t value Pr(>|t|)    
(Intercept)  26.3561     2.2907 62.5937  11.506   <2e-16 ***
Time          8.4438     0.5403 48.6609  15.628   <2e-16 ***
Diet2         2.8387     2.3627 45.6620   1.201   0.2358    
Diet3         2.0045     2.3627 45.6620   0.848   0.4006    
Diet4         9.2548     2.3657 45.8853   3.912   0.0003 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Correlation of Fixed Effects:
      (Intr) Time   Diet2  Diet3 
Time  -0.796                     
Diet2 -0.351 -0.004              
Diet3 -0.351 -0.004  0.344       
Diet4 -0.350 -0.005  0.343  0.343


```

### Deciding on the random effect structure

Of the two models, we may wish to objectively determine which of them fits the data better. To do this, we would employ the use of *Likelihood Ratio Tests(LRTs)*. These testa are used to evaluate models which are nested (i.e. subsets of each other).

```{r eval = FALSE, echo = FALSE}

### Likelihood ratio test
## Step 1: Fit the random intercept model
ri <- lmer(weight~Time + Diet + (1|Chick),
            data=data.chickens)

## Step 2: Fit the random intercept and slope

corr.ri.s <- lmer(weight~Time + Diet + (1 + Time|Chick),
                  data=data.chickens)
# Test
anova(ri, corr.ri.s)


```

The pvalue suggests that there is sufficient evidence to reject the null hypotheses (no need for a random slope). Note that the random intercept model has seven parameters (6 fixed including residual variance and 1 random). The random intercept and slope model has an extra two due to the addition of a random slope and covariance parameter between the two random effects. As such, caution should be exercised specified adding random effects as the total number of parameters to be estimated may increase substantially to include all pairwise covariances between the random effects specified.

```{r eval = FALSE, echo = FALSE}

refitting model(s) with ML (instead of REML)
Data: data.chickens
Models:
ri: weight ~ Time + Diet + (1 | Chick)
corr.ri.s: weight ~ Time + Diet + (1 + Time | Chick)
          npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
ri           7 5619.2 5649.7 -2802.6   5605.2                         
corr.ri.s    9 4834.1 4873.3 -2408.0   4816.1 789.12  2  < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


```

# Final model

The resulting model determined to be useful is the one that includes a random intercept and slope. The model may be refined further by determining whether the covariance may be dropped from the model. This would be evaluated by the LRT as previously demonstrated. However, this will not be covered here. Finally, population average level (fixed effects) and chick specific predictions from the model as follows.

```{r eval = FALSE, echo = FALSE}

##

final.model <- lmer(weight~Time + Diet + (1 + Time|Chick),
                    data=data.chickens,
                    REML=FALSE)

summary(final.model)%>%
          print(digits=4)

```

```{r eval = FALSE, echo = FALSE}
Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
Formula: weight ~ Time + Diet + (1 + Time | Chick)
   Data: data.chickens

REML criterion at convergence: 4803.8

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-2.7617 -0.5758 -0.0353  0.4789  3.5025 

Random effects:
 Groups   Name        Variance Std.Dev. Corr 
 Chick    (Intercept) 153.87   12.40         
          Time         14.13    3.76    -0.98
 Residual             163.45   12.78         
Number of obs: 578, groups:  Chick, 50

Fixed effects:
            Estimate Std. Error      df t value Pr(>|t|)    
(Intercept)  26.3561     2.2907 62.5937  11.506   <2e-16 ***
Time          8.4438     0.5403 48.6609  15.628   <2e-16 ***
Diet2         2.8387     2.3627 45.6620   1.201   0.2358    
Diet3         2.0045     2.3627 45.6620   0.848   0.4006    
Diet4         9.2548     2.3657 45.8853   3.912   0.0003 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Correlation of Fixed Effects:
      (Intr) Time   Diet2  Diet3 
Time  -0.796                     
Diet2 -0.351 -0.004              
Diet3 -0.351 -0.004  0.344       
Diet4 -0.350 -0.005  0.343  0.343

```

Results for the predictions for some of the chicks are shown below

```{r eval = FALSE, echo = FALSE}

# Population average level predictions
lme4::fixef(final.model)


```

```{r eval = FALSE, echo = FALSE}

(Intercept)        Time       Diet2       Diet3       Diet4 
  26.356119    8.443778    2.838662    2.004526    9.254844

```

```{r eval = FALSE, echo = FALSE}

# Chick specific predictions (Best Linear Unbiased Predictors)
lme4::ranef(final.model)

```

```{r eval = FALSE, echo = FALSE}

$Chick
    (Intercept)        Time
18   4.20060752 -1.31340574
16  23.00717014 -7.51564270
15  20.11895134 -6.32344145
13  19.81139295 -6.32197010
9   18.09527374 -5.28896707
20  15.10313900 -4.89910284
10  13.96701879 -4.44106548
8   11.12561196 -3.19285517
17  12.27399767 -3.62483497
19  10.87806614 -3.68543653
4    7.51882977 -2.39707770
...........................
...........................

```
