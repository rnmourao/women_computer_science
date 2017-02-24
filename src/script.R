# file: script.R
# authors: Guilherme N. Ramos (gnramos@unb.br)
#          Roberto N. Mourão
#
# Script for analysis of data for the Meninas na Computação Project in
# the University of Brasília (http://meninas.cic.unb.br)

# Dependencies
library(agricolae)
library(car)
library(ggplot2)
library(gridExtra)
library(multcomp)
library(readxl)
library(stringr)

# Pretty printing
options(digits=2, device='pdf', max.print=99999)
cat('\n')

# Cleaning up workspace
rm(list=ls())

#### Set working directory
# setwd('/home/mourao/Documentos/women_computer_science/src/')
plot.dir <- '../dexa/img/'

# Get raw data
poll.answers <- read_excel('../data/raw.xlsx', sheet='answers', na='')

# Cleanup data
poll.answers$Q1 <- NULL
poll.answers$Q2 <- NULL

cat('Number of respondents: ', nrow(poll.answers), '\n\n')

# Girls only
poll.answers <- subset(poll.answers, poll.answers$Gender == 'F')
poll.answers$Gender <- NULL

# Only Middle and High Schools
poll.answers <- subset(poll.answers, 
                       poll.answers$Educational_Stage != 'College' & 
                         poll.answers$Educational_Stage != 'Adult Education Program')

# Processing
poll.answers <- as.data.frame(lapply(poll.answers, as.factor))

cat('Female respondents: ', nrow(poll.answers), '\n\n')

# Results per year #############################################################
respondents.per.year <- aggregate(x=list(Total=poll.answers$Year),
                                  by=list(Year=poll.answers$Year),
                                  FUN=length)

p <- ggplot(data=respondents.per.year, aes(x='', y=Total, fill=Year)) +
     geom_bar(width=1, stat='identity') +
     coord_polar(theta='y', start=0) +
     scale_x_discrete() +
     xlab('') + ylab('') + labs(fill='') + # + labs(fill='Stage')
     # ggtitle('Respondents per Year') +
ggsave(paste0(plot.dir, 'RespondentsPerYear.pdf'), width=7, height=2)


# Results per educational stage ################################################
educational.stage <- aggregate(x=list(total=poll.answers$Educational_Stage),
                               by=list(Year=poll.answers$Year, Educational_Stage=poll.answers$Educational_Stage),
                               FUN=length)
by.year <- aggregate(x=list(Total=poll.answers$Educational_Stage),
                     by=list(Year=poll.answers$Year),
                     FUN=length)

educational.stage <- merge(educational.stage, by.year)
educational.stage$Percentage <- (educational.stage$total * 100) / educational.stage$Total

p <- ggplot(data=educational.stage, aes(x='', y=Percentage, fill=Educational_Stage)) +
     geom_bar(width=1, stat='identity') +
     facet_grid(facets=. ~ Year) +
     coord_polar(theta='y', start=0) +
     scale_x_discrete() +
     xlab('') + ylab('') + labs(fill='') + # + labs(fill='Stage')
     # ggtitle('Respondents per Educational Stage') +
     scale_fill_discrete(breaks=c('Middle School', 'High School (10th Grade)',
                                  'High School (11th Grade)', 'High School (12th Grade)'))
ggsave(paste0(plot.dir, 'EducationalStage.pdf'), width=7, height=2)

# Results per interest in undergraduate field of study #########################
field.of.interest <- aggregate(x=list(NumRespondents=poll.answers$Field_Interest),
                               by=list(Year=poll.answers$Year, FieldOfInterest=poll.answers$Field_Interest),
                               FUN=length)
by.year <- aggregate(x=list(TotalRespondents=poll.answers$Field_Interest),
                     by=list(Year=poll.answers$Year),
                     FUN=length)

field.of.interest <- merge(field.of.interest, by.year)
field.of.interest$Percentage <- (field.of.interest$NumRespondents * 100) / field.of.interest$TotalRespondents

p <- ggplot(data=field.of.interest, aes(x='', y=Percentage, fill=FieldOfInterest)) +
     geom_bar(width=1, stat='identity') +
     facet_grid(facets=. ~ Year) +
     coord_polar(theta='y', start=0) +
     scale_x_discrete() +
     xlab('') + ylab('') + labs(fill='') + # + labs(fill='Field')
     # ggtitle('Respondents per Field of Interest')
ggsave(paste0(plot.dir, 'FieldOfInterest.pdf'), width=7, height=2)

cat('All respondents:\n')
aggregate(x=list(Percentage=field.of.interest$Percentage),
          by=list(FieldOfInterest=field.of.interest$FieldOfInterest),
          FUN=mean)
cat('\n')

# Results for interest in Computer Science #####################################
interest.in.CS <- aggregate(x=list(Quantidade=poll.answers$CS_Interest),
                            by=list(Year=poll.answers$Year, CS_Interest=poll.answers$CS_Interest),
                            FUN=length)
by.year <- aggregate(x=list(Total=poll.answers$CS_Interest),
                     by=list(Year=poll.answers$Year),
                     FUN=length)

interest.in.CS <- merge(interest.in.CS, by.year)
interest.in.CS$Percentage <- (interest.in.CS$Quantidade * 100) / interest.in.CS$Total

p <- ggplot(data=interest.in.CS, aes(x='', y=Percentage, fill=CS_Interest)) +
     geom_bar(width=1, stat='identity') +
     facet_grid(facets=. ~ Year) +
     coord_polar(theta='y', start=0) +
     scale_x_discrete() +
     xlab('') + ylab('') + labs(fill='') + # labs(fill='Year')
     # ggtitle('Respondents Interested in CS')
     scale_fill_discrete(breaks=c('Yes', 'No', 'Maybe'))
ggsave(paste0(plot.dir, 'InterestInCS.pdf'), width=7, height=2)


# Variance analysis ############################################################

# For girls who intend to take a CS course
CS.choice <- data.frame(CS_Interest=levels(poll.answers$CS_Interest),
                        CS_Choice=0)
CS.choice$CS_Choice[CS.choice$CS_Interest == 'No'] <- -1
CS.choice$CS_Choice[CS.choice$CS_Interest == 'Maybe'] <- 0
CS.choice$CS_Choice[CS.choice$CS_Interest == 'Yes'] <- 1
poll.answers <- merge(poll.answers, CS.choice)
CS.choice.index <- match('CS_Choice', names(poll.answers))
CS.interest.index <- match('CS_Interest', names(poll.answers))

year.index <- match('Year', names(poll.answers))

treatments <- data.frame(treatment=NULL, f.value=NULL, max.mean=NULL, assumptions=NULL)

# Analyze individual attributes
for (i in year.index:(CS.choice.index-1)) {  # CS.choice.index is the last one
  temp <- data.frame(Treatment=poll.answers[, i],
                     CS_Choice=poll.answers[, CS.choice.index],
                     CS_Interest=poll.answers[, CS.interest.index])
  temp <- temp[!is.na(temp$Treatment),]
  attribute.name <- colnames(poll.answers)[i]
  
  f <- as.formula("CS_Choice ~ Treatment - 1")
  
  # unbalanced designs need a different library
  model <- lm(formula=f, data=temp, contrasts=list(Treatment = contr.sum))
  fit <- Anova(model, type="III")
  fit.summary <- summary(model)  
  
  ## checking anova assumptions
  
  # Are residuals normal? Since normality assumption is not a big issue when treatments are fixed
  # effects, we will test it using a rough test of Skewness and Kurtosis instead usual 
  # normality tests, e.g., Shapiro-Wilk and Kolmogorov-Smirnov.
  # using rule of thumb proposed by Bulmer, 1979.  
  symmetric <- (abs(skewness(model$residuals)) < 0.5)
  # using rule of thumb proposed by Vieira, 2006.
  not_leptokurtic <- kurtosis(model$residuals) <= 0
  
  # Are samples independent each other? Yes, because each observation belongs to one student.
  independent <- TRUE
  
  # Are variances equal?
  # using rule of thumb proposed by Dean and Voss - Design and analysis of experiments, 1999.
  v <- aggregate(x=list(variance=temp$CS_Choice), by=list(Treatment=temp$Treatment), FUN=var)
  if (max(v$variance, na.rm = TRUE) / min(v$variance, na.rm = TRUE) < 3) {
    homoscedastic <- TRUE
  } else {
    homoscedastic <- FALSE
  }
  
  # Only an indicator to ease the attribute selection. 
  # It is still necessary to check the residuals' charts.
  if (symmetric & not_leptokurtic & independent & homoscedastic) {
    a <- 'Granted'
  } else {
    a <- 'Not Granted'
  }

  # tukey-kramer post hoc test
  mcs <- summary(glht(model, linfct = mcp(Treatment="Tukey")))
  tukey <- cld(mcs, level=0.05, decreasing=TRUE)
  tukey.matrix <- as.matrix(unlist(tukey$mcletters$Letters))
  tukey.df <- data.frame(trt=row.names(tukey.matrix), M=tukey.matrix[,1])
  Mean <- aggregate(x=list(means=temp$CS_Choice), by=list(trt=temp$Treatment), FUN=mean)
  tukey.df <- merge(tukey.df, Mean)
  tukey.df <- tukey.df[, c(1, 3, 2)]
  tukey.df <- tukey.df[order(tukey.df$M),]

  # find the greatest mean value for CS_Choice which has significance.
  # initialization: max.mean begins with lower possible value for mean of CS_Choice attribute
  max.mean <- -1
  for (j in 1:nrow(tukey.df)) {
    if (tukey.df$means[j] > max.mean) {
      if (str_count(as.character(tukey.df$M[j])) == 1 & 
          str_count(paste(tukey.df$M, collapse=''), 
                    as.character(tukey.df$M[j])) == 1) {
        max.mean <- tukey.df$means[j]
        trt <- tukey.df$trt[j]
      }
    }
  }

  df <-  data.frame(treatment=paste0(attribute.name, "=", trt),
                    f.value=fit$`F value`[1],
                    max.mean=max.mean,
                    assumptions=a)
  treatments <- rbind(treatments, df)
  
  ## Charts
  # barplot to show differences between treatments
  temp2 <- aggregate(x=list(Quantity=temp$CS_Interest),
                     by=list(CS_Interest=temp$CS_Interest, Treatment=temp$Treatment),
                     FUN=length)
  p <- ggplot(temp2, aes(fill=Treatment, y=Quantity, x=CS_Interest)) +
    geom_bar(position="dodge", stat="identity") +
    scale_fill_discrete(name='Treatment')
  ggsave(paste0(plot.dir, 'plot_', attribute.name, '.pdf'), width=5, height=3)
  
  # anova table
  pdf(paste0(plot.dir, 'anova_', attribute.name, '.pdf'), height=1, width=8)
    grid.table(fit)
  ignore <- dev.off()
  
  # Residuals charts
  pdf(paste0(plot.dir, 'conditions_', attribute.name, '.pdf'))
    # A warning here about the exclusion of the lack of fit chart. 
    # This chart is not essencial to perform residuals analysis.
    options(warn=-1)
      residualPlots(model, layout= c(2, 1))
    options(warn=0)
    qqPlot(model)
  ignore <- dev.off()
  
  # height and width adjustments based on trial and error
  h <- 1.5 + 0.1 * length(unique(temp$Treatment))
  w <- 1.5 + 0.1 * max(nchar(as.character(levels(temp$Treatment))))
  pdf(paste0(plot.dir, 'tukey_', attribute.name, '.pdf'), height=h, width=w)
    grid.table(tukey.df, rows=NULL)
  ignore <- dev.off()
}

## Attribute Selection using Factorial Analysis

# Removing not-significant attributes using a top-down approach
not_significant <- c(CS.interest.index, year.index)
temp <- poll.answers[, -not_significant]
gotcha <- FALSE
while(!gotcha | ncol(temp) <= 1) {
  # checking which treatments are significants
  model <- lm(formula='CS_Choice ~ . - 1', data=temp, contrasts=contr.sum)
  # Type II Anova analyzes treatments ignoring interacions
  fit <- Anova(model, type="II")
  
  # Creating a data frame for p-values
  values <- as.data.frame(fit)
  values$Attribute <- trimws(rownames(values))
  rownames(values) <- seq(1, nrow(values))
  values <- values[!is.na(values$`Pr(>F)`),]
  values <- values[order(values$`Pr(>F)`, decreasing=TRUE),]

  # remove attribute with greater p-value
  if (values$`Pr(>F)`[1] >= 0.05) {
    index <- match(values$Attribute[1], names(poll.answers))
    not_significant <- c(not_significant, index)
    temp <- poll.answers[, -not_significant]
  } else {
      pdf(paste0(plot.dir, 'attribute_selection.pdf'), height=6, width=9)
        grid.table(fit)
      ignore <- dev.off()
      break
  }
}

### Test interactions between Family_Approval and others attributes
CS.interest.index <- match('CS_Interest', names(poll.answers))
not_significant <- setdiff(not_significant, CS.interest.index)
temp <- poll.answers[, -not_significant]
Family.index <- match('Family_Approval', names(temp))
temp <- temp[, c(setdiff(1:ncol(temp), c(Family.index, CS.interest.index)), Family.index, CS.interest.index)]
Family.index <- match('Family_Approval', names(temp))
CS.choice.index <- match('CS_Choice', names(temp))
CS.interest.index <- match('CS_Interest', names(temp))
for (i in 1:(CS.choice.index-1)) {
  temp2 <- temp[, c(Family.index, i, CS.choice.index, CS.interest.index)]
  FactorB.name <- names(temp2)[2]
  
  Interaction.name <- paste0(names(temp2)[1], '_x_', names(temp2)[2])
  
  f <- as.formula(paste("CS_Choice ~ Family_Approval", "*", FactorB.name, "- 1"))

  # unbalanced designs need a different library to analyze interactions  
  model <- lm(formula=f, data=temp2, contrasts=contr.sum)
  fit <- Anova(model, type="III")
  fit.summary <- summary(model)
  
  # if the interaction has significance (p-value < .05), proceed
  p_value <- fit$`Pr(>F)`[3]
  if (p_value < .05) {
    ## construct a new model to test only the interaction
    temp2$I <- interaction(temp2$Family_Approval, temp2[, 2], drop = TRUE, sep = "_x_")
    
    model <- lm(formula='CS_Choice ~ I', data=temp2, contrasts=contr.sum)
    fit <- Anova(model, type="III")
    fit.summary <- summary(model)
    ## checking anova assumptions
    
    # Are residuals normal? Since normality assumption is not a big issue when treatments are fixed
    # effects, we will test it using a rough test of Skewness and Kurtosis instead usual 
    # normality tests, e.g., Shapiro-Wilk and Kolmogorov-Smirnov.
    # using rule of thumb proposed by Bulmer, 1979.  
    symmetric <- (abs(skewness(model$residuals)) < 0.5)
    # using rule of thumb proposed by Vieira, 2006.
    not_leptokurtic <- kurtosis(model$residuals) <= 0
    
    # Are samples independent each other? Yes, because each observation belongs to one student.
    independent <- TRUE
    
    # Are variances equal?
    # using rule of thumb proposed by Dean and Voss - Design and analysis of experiments, 1999.
    v <- aggregate(x=list(variance=temp2$CS_Choice), 
                   by=list(A=temp2$I), FUN=var)
    if (max(v$variance, na.rm = TRUE) / min(v$variance, na.rm = TRUE) < 3) {
      homoscedastic <- TRUE
    } else {
      homoscedastic <- FALSE
    }
    
    if (symmetric & not_leptokurtic & independent & homoscedastic) {
      a <- 'Granted'
    } else {
      a <- 'Not Granted'
    }
    
    # tukey-kramer post hoc test
    mcs <- summary(glht(model, linfct = mcp(I="Tukey")))
    tukey <- cld(mcs, level=0.05, decreasing=TRUE)
    tukey.matrix <- as.matrix(unlist(tukey$mcletters$Letters))
    tukey.df <- data.frame(trt=row.names(tukey.matrix), M=tukey.matrix[,1])
    Mean <- aggregate(x=list(means=temp2$CS_Choice), by=list(trt=temp2$I), FUN=mean)
    tukey.df <- merge(tukey.df, Mean)
    tukey.df <- tukey.df[, c(1, 3, 2)]
    tukey.df <- tukey.df[order(tukey.df$M),]
    
    # find the greatest mean value for CS_Choice which has significance.
    # initialization: max.mean begins with lower possible value for mean of CS_Choice attribute
    max.mean <- -1
    for (j in 1:nrow(tukey.df)) {
      if (tukey.df$means[j] > max.mean) {
        if (str_count(as.character(tukey.df$M[j])) == 1 & 
            str_count(paste(tukey.df$M, collapse=''), 
                      as.character(tukey.df$M[j])) == 1) {
          max.mean <- tukey.df$means[j]
          trt <- tukey.df$trt[j]
        }
      }
    }
    
    # adding significant interactions in control table
    df <- data.frame(treatment=Interaction.name,
                     f.value=p_value,
                     max.mean=max.mean,
                     assumptions=a)
    treatments <- rbind(treatments, df)
    
    ## Charts
    # barplot to show differences between treatments
    temp3 <- aggregate(x=list(Quantity=temp2$CS_Interest),
                       by=list(CS_Interest=temp2$CS_Interest, Interaction=temp2$I),
                       FUN=length)
    p <- ggplot(temp3, aes(fill=Interaction, y=Quantity, x=CS_Interest)) +
      geom_bar(position="dodge", stat="identity") +
      scale_fill_discrete(name='Interaction')
    ggsave(paste0(plot.dir, 'plot_', Interaction.name, '.pdf'), width=8, height=5)
    
    # anova table
    pdf(paste0(plot.dir, 'anova_', Interaction.name, '.pdf'), height=2, width=10)
      grid.table(fit)
    ignore <- dev.off()
    
    # Residuals charts
    pdf(paste0(plot.dir, 'conditions_', Interaction.name, '.pdf'))
      # A warning here about the exclusion of the lack of fit chart. 
      # This chart is not essencial to perform residuals analysis.
      options(warn=-1)
        residualPlots(model, layout= c(2, 1))
      options(warn=-1)
      qqPlot(model)
    ignore <- dev.off()
    
    # height and width adjustments based on trial and error
    h <- 2.5 + 0.1 * length(unique(temp2$I))
    w <- 1.5 + 0.1 * max(nchar(as.character(levels(temp2$I))))
    pdf(paste0(plot.dir, 'tukey_', Interaction.name, '.pdf'), height=h, width=w)
      grid.table(tukey.df, rows=NULL)
    ignore <- dev.off()
    
    pdf(paste0(plot.dir, 'interactions_', Interaction.name, '.pdf'))
      interaction.plot(temp2[, 1], temp2[, 2], temp2[, 3], type='b')
    ignore <- dev.off()
  }
}

treatments <- treatments[order(treatments$max.mean, decreasing=TRUE),]
write.table(treatments, '../data/anova.csv', row.names=FALSE, dec=',', sep=';')
