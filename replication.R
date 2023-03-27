###Title: Final Assignment, Statistics for International Relations Research I
###Author: Sara Kallis (sara.kallis@graduateinstitute.ch)
###Date: 23 December 2021
setwd("/Users/sara/Documents/IHEID Academic/Semester 1/Statistics 1/Problem Sets/Final")
library(foreign)
library(tidyverse)
library(stargazer)
library(QuantPsyc)
library(interactions)
library(margins)
library(ggeffects) 
library(car)
library(maps)
library(ggplot2)
library(lmtest)

##Part 1: Data Import & Cleaning####
    #import, select variables of interest
    data_full <- read.csv("qog_std_cs_jan21.csv")
    data <- data_full %>% dplyr::select(cname, wbgi_rle, ffp_ref, ffp_ued, undp_hdi, wdi_armimp, gpi_jail) #R gets confused about select() if MASS is also downloaded
    rm(data_full)
    #rename variables more intuitively
    data <- data %>%
      rename(c(country = cname, rulelaw = wbgi_rle, refugees = ffp_ref, econ = ffp_ued, hdi = undp_hdi, arms = wdi_armimp, jail = gpi_jail))
    data$arms <- data$arms / 1000000 #convert to millions for easier interpretation

##Part 2: Descriptive Statistics####
    
    ###2.1 Table####
      #Stargazer creates table with summary statistics for all variables in dataset
    stargazer(data, type = "html", title="Descriptive Statistics", 
              digits=1, out="/Users/sara/Documents/IHEID Academic/Semester 1/Statistics 1/Problem Sets/Final/desc2.doc")

    ###2.2 Graphically: Histograms####
    ggplot(data, aes(x = rulelaw)) + 
      geom_histogram(binwidth = 0.5, center = 0.05, fill = "darkseagreen") + 
      labs(x = "Rule of Law", y = "Distribution") 
    
    ggplot(data, aes(x = econ)) + 
      geom_histogram(binwidth = 0.5, center = 0.05, fill = "darkseagreen") + 
      labs(x = "Uneven Economic Development", y = "Distribution") 
    
    ggplot(data, aes(x = refugees)) + 
      geom_histogram(binwidth = 0.4, center = 0.05, fill = "darkblue") + 
      labs(x = "Refugees and IDPs", y = "Distribution") 
    
    ggplot(data, aes(x = arms)) + 
      geom_histogram(binwidth = 800, center = 0.05, fill = "darkblue") + 
      labs(x = "Arms Imports (millions)", y = "Distribution") #Problem: too positively skewed
    
          ##Log-Transform Arms Variable
          data$arms_log <- log(data$arms)
          
          ##New plot
          ggplot(data, aes(x = arms_log)) + 
            geom_histogram(binwidth = 0.5, center = 0.05, fill = "darkblue") + 
            labs(x = "Log of Arms Imports (millions)", y = "Distribution") 
    
    ggplot(data, aes(x = jail)) + 
      geom_histogram(binwidth = 0.5, center = 0.05, fill = "darkblue") + 
      labs(x = "Incarceration", y = "Distribution") 
    
          ##Log-Transform Incarceration Variable
          data$jail_log <- log(data$jail)
          
          ##New plot
          ggplot(data, aes(x = jail_log)) + 
            geom_histogram(binwidth = 0.15, center = 0.05, fill = "darkblue") + 
            labs(x = "Log of Incarceration", y = "Distribution") 
    
    ggplot(data, aes(x = hdi)) + 
      geom_histogram(binwidth = 0.05, center = 0.05, fill = "darkblue") + 
      labs(x = "Human Development Index", y = "Distribution") 
    
    
    ###2.3 World Map of Sampled Countries####
      #Note: Carried out after removing countries with NAs, see code at 3.1
    
    countries <- data.frame(unique(data$country))
    countries$number <- 1
    world_map <- map_data("world")
    world_map <- subset(world_map, region != "Antarctica")
    
    ggplot(countries) +
      geom_map(
        dat = world_map, map = world_map, aes(map_id = region),
        fill = "white", color = "#7f7f7f", size = 0.25) +
      geom_map(map = world_map, aes(map_id = unique.data.country., fill = number), size = 0.25) +
      scale_fill_gradient(low = "#fff7bc", high = "#cc4c02", name = "Sampled Countries") +
      expand_limits(x = world_map$long, y = world_map$lat) #export and crop the ugly bit at the side :)

##Part 3: Regression Analysis####
    
    ###3.1 Bivariate####
        #Graphic representation: Scatterplot of outcome (rulelaw) and main predictor variable (econ)
        ggplot(data, aes(x = econ, y = rulelaw)) +
          geom_point() + geom_smooth(method = lm, se = FALSE) +
          labs(x = "Uneven Economic Development", y = "Rule of Law")
        
        #Regression model
        lm1 <- lm(rulelaw ~ econ, data = data)
        summary(lm1) #econ***, Adjusted R-squared:  0.6168 
    
    ###3.1 Multivariate####
        ##Some Troubleshooting with Dataset:
        #remove NAs
        data <- na.omit(data) #52 countries removed, N = 142
        
        #problem: observations 51, 66, 72, 183 have logged arms value of -Inf because arms value is 0. substitute with 0.
        data["arms_log"][data["arms_log"] == -Inf] <- 0
        
    ####Model Specification####
        
        #Backward elimination procedure, combined p-value and adjusted r-value approach
        lm2 <- lm(rulelaw ~ econ + refugees + hdi + arms_log + jail_log, data = data)
        summary(lm2) #econ***, refugees**, hdi**, arms_log, jail_log**, Adjusted R-squared:  0.694 >> remove arms_log
        
        lm3 <- lm(rulelaw ~ econ + refugees + hdi + jail_log, data = data)
        summary(lm3) #econ***, refugees**, hdi*, jail_log**, Adjusted R-squared:  0.6925
        
        ##so far: lm2 better than lm3 (ajusted R^2). let's try some interaction terms.
        
        lm4 <- lm(rulelaw ~ econ*refugees + hdi + arms_log + jail_log, data = data)
        summary(lm4) #Adjusted R-squared:  0.7045 
        
        lm5 <- lm(rulelaw ~ econ*hdi + refugees + arms_log + jail_log, data = data)
        summary(lm5) #Adjusted R-squared:  0.7371 
        
        ##lm5 is better than lm4 and lm2.
        
        ##regression table:
        stargazer(lm1, lm2, lm3, lm4, lm5,  type = "html", 
                  covariate.labels = c("Uneven Economic Development",
                                       "Refugee Strain",
                                       "HDI",
                                       "Arms Import (Log)",
                                       "Incarceration (Log)",
                                       "Uneven Econ Dev:HDI",
                                       "Uneven Econ Dev:Refugee Strain"),
                  dep.var.labels = "Rule of Law",
                  out="/Users/sara/Documents/IHEID Academic/Semester 1/Statistics 1/Problem Sets/Final/regressiontables.doc")
                  
        ####Standardised Coefficients####
        lm.beta(lm2)     
        lm.beta(lm3)
        
        ####Interaction Plots####
        interact_plot(lm5, pred = econ, modx = hdi, data = data)
        interact_plot(lm4, pred = econ, modx = refugees, data = data)
        

        ####Marginal Effects####
            #Marginal effects measure the change in the predicted value of a response variable when predictors change by one unit.
            
            #Method 1: Average 
            m_lm5 <- margins(lm5)
            m_lm4 <- margins(lm4)
            #not very sure about the interpretation - refer to forum question
           
            #Method 2: Marginal effects for econ for Model 4 (lm5)
            summary(data$hdi) #first, second, third quartile to plug into formula below
            pred_lm5 <- ggpredict(lm5, terms = c("econ", "hdi [0.585, 0.7470, 0.8468]")) 
           
            ggplot(pred_lm5, aes(x, predicted)) +
              geom_line() +
              theme_bw()
            plot(pred_lm5) #visualise marginal effects of econ on rule of law


##Part 4: Regression Diagnostics####
            #Selected Model: lm5
            plot(lm5)
            
            ##Extra tests:

            ###4.1 Normality Assumption: Assessing whether errors (residuals) are normally distributed with mean = 0 and sd = 1 ####
            residuals1 <- lm5$residuals
            residuals2 <- lm5$model$mpg - lm5$fitted.values
            table(round(residuals1,2) == round(residuals2,2) )
            hist(residuals1)
            mean(residuals1) #1.84887e-17 i.e. almost 0: OK
            sd(residuals1) #0.4973991 - not 1: Not OK
            
            shapiro.test(lm5$residuals) #Shapiro-Wilk test for normality
            #null hypothesis is that the data (here, the residuals) are normally distributed
            ##p-value is above 0.05, so accept null hypothesis :) 
            
          
            ###4.2 Homoscedasticity Assumption: Assess whether the variance of errors is constant ####
            bptest(lm5) #Breusch-Pegan test for Homoscedasticity, p-value = 0.252
            #accept the null hypothesis that the error variance is homogenous
            
            
            ###4.3 Independence Assumption: Assess whether error terms are indeed independent####
            durbinWatsonTest(lm5) # We want to not reject the hypothesis of no autocorrelation. Hence p-value should be greater than >0.05
            #p = 0.892 is more than 0.05. We can reject null hypothesis of autocorrelation.
            
            ###4.4 Multicollinearity####
            vif(lm5) # variance inflation factors. rule of thumb: VIF greater than 10 (or 5 sometimes), the multicollinearity is likely to be present
            vif(lm2) # let's also look at model 2 to see if the interaction is pushing up the figures
            
           ###4.5 Influence Points#####
            #calculate the quantities of "Studentized residuals" (to detect outliers), "hat values" (to detect leverage points),
            # and "Cook's distance" (to detect influential data points)
            influenceIndexPlot(lm5,
                               vars = c("Cook", "Studentized", "hat"))
            
            ##remove 33 (Central African Republic) and 145 (Rwanda) influence points
              data_noinfl <- data[-c(33, 145), ]   
            ##re-do regression
              lm5_noinfl <- lm(rulelaw ~ econ*hdi + refugees + arms_log + jail_log, data = data_noinfl)
              summary(lm5_noinfl) #not much changes for these results
              
              
            ##FIN! :) 
            
            

            