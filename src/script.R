# file: script.R
# authors: Guilherme N. Ramos (gnramos@unb.br)
#          Roberto N. Mourão
#
# Script for analysis of data for the Meninas na Computação Project in
# the University of Brasília (http://meninas.cic.unb.br)

# Dependencies
library(agricolae)
library(arules)
library(arulesViz)
library(ggplot2)
library(gridExtra)
library(readxl)
library(reshape)
library(stringr)

# Pretty printing
options(digits=2, device='pdf', max.print=99999)
cat('\n')

# Cleaning up workspace
rm(list=ls())

#### Set working directory
# setwd('/home/f8676628/Documentos/women_computer_science/src/')
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
                                  'High School (11th Grade)', 'High School (12th Grade)',
                                  'Adult Education Program', 'College'))
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
CS.response.index <- match('CS_Interest', names(poll.answers))

year.index <- match('Year', names(poll.answers))

treatments <- data.frame(treatment=NULL, f.value=NULL, max.mean=NULL, sup=NULL)

# Analyze individual attributes
for (i in year.index:(CS.choice.index-1)) {  # CS.choice.index is the last one
  temp <- data.frame(Treatment=poll.answers[, i],
                     CS_Choice=poll.answers[, CS.choice.index],
                     CS_Interest=poll.answers[, CS.response.index])
  attribute.name <- colnames(poll.answers)[i]

  f <- as.formula('CS_Choice ~ Treatment')
  fit <- aov(f, data=temp)
  fit.summary <- summary(fit)
  tukey <- HSD.test(fit, 'Treatment')

  temp2 <- aggregate(x=list(Quantity=temp$CS_Interest),
                     by=list(CS_Interest=temp$CS_Interest, Treatment=temp$Treatment),
                     FUN=length)
  p <- ggplot(temp2, aes(fill=Treatment, y=Quantity, x=CS_Interest)) +
       geom_bar(position="dodge", stat="identity") +
       scale_fill_discrete(name='Treatment')
  ggsave(paste0(plot.dir, 'plot_', attribute.name, '.pdf'), width=5, height=3)

  # Qual a origem destes números mágicos (6 e 2.5)?
  w <- 6 + 2.5 * nchar(as.character(fit.summary[[1]][['Pr(>F)']][1])) / 20
  pdf(paste0(plot.dir, 'anova_table_', attribute.name, '.pdf'), height=1, width=w)
    grid.table(anova(fit))
  ignore <- dev.off()

  pdf(paste0(plot.dir, 'anova_chart_', attribute.name, '.pdf'))
    par(mfrow=c(2,2))
    plot(fit)
  ignore <- dev.off()

  # Qual a origem destes números mágicos (1.5 e 0.1)?
  h <- 1.5 + 0.1 * length(unique(temp$Treatment))
  w <- 1.5 + 0.1 * max(nchar(as.character(levels(temp$Treatment))))
  pdf(paste0(plot.dir, 'tukey_', attribute.name, '.pdf'), height=h, width=w)
    grid.table(tukey$groups, rows=NULL)
  ignore <- dev.off()

  groups <- tukey$groups
  means <- tukey$means
  sum.means <- sum(means$r)
  # Qual a origem deste número mágico (9)?
  max.mean <- -9
  s <- 0
  for (j in 1:nrow(groups)) {
    if (groups$means[j] > max.mean) {
      if (str_count(paste(groups$M, collapse=''), as.character(groups$M[j])) == 1) {
        max.mean <- groups$means[j]
        s <- means$r[row.names(means) == trimws(groups$trt[j])] / sum.means
      }
    }
  }

  df <-  data.frame(treatment=attribute.name,
                    f.value=fit.summary[[1]][['F value']][[1]],
                    max.mean=max.mean,
                    sup=s)
  treatments <- rbind(treatments, df)
}

## Factorial Analysis

# Removing non-significant attributes using a top-down approach
not_significant <- c(CS.response.index, year.index)
temp <- poll.answers[, -not_significant]
gotcha <- FALSE
while(!gotcha | ncol(temp) <= 1) {
  fit <- aov(CS_Choice ~ ., data=temp)

  # Creating a data frame for p-values
  values <- as.data.frame(summary(fit)[[1]][4:5])
  names(values)[1] <- 'f'
  names(values)[2] <- 'p'
  values$attribute <- trimws(rownames(values))
  rownames(values) <- seq(1, nrow(values))
  values <- values[!is.na(values$p),]
  values <- values[order(values$p, decreasing=TRUE),]

  # remove attribute with greater p-value
  if (values$p[1] >= 0.05) {
    index <- match(values$attribute[1], names(poll.answers))
    not_significant <- c(not_significant, index)
    temp <- poll.answers[, -not_significant]
  } else {
    print(summary(fit))
    break
  }
}

### Test interactions
CS.choice.index <- match('CS_Choice', names(temp))
combinations <- t(combn(1:(ncol(temp) - 1), 2))
for (i in 1:nrow(combinations)) {
  temp2 <- temp[, c(combinations[i, 1], combinations[i, 2], CS.choice.index)]
  temp2 <- temp2[!is.na(temp2[, 1]) & !is.na(temp2[, 2]),]
  temp2$interaction <- as.factor(paste0(temp2[, 1], '_x_', temp2[, 2]))
  trt <- paste0(names(temp2)[1], '_x_', names(temp2)[2])
  names(temp2)[4] <- trt

  f <- as.formula(paste("CS_Choice ~", trt))
  fit <- aov(f, data=temp2)
  fit.summary <- summary(fit)
  tukey <- HSD.test(fit, trt)

  # Qual a origem destes números mágicos (4 e 2.6)?
  w <- 4 + 2.6 *(nchar(trt) + nchar(as.character(summary(fit)[[1]][['Pr(>F)']][1])))/ 20
  pdf(paste0(plot.dir, 'anova_table_', trt, '.pdf'), height=1, width=w)
    grid.table(anova(fit))
  ignore <- dev.off()

  pdf(paste0(plot.dir, 'anova_chart_', trt, '.pdf'))
    interaction.plot(temp2[, 1], temp2[, 2], temp2[, 3], type='b')
  ignore <- dev.off()

  # Qual a origem destes números mágicos (1.5 e 0.3 e 0.1)?
  h <- 1.5 + 0.3 * length(levels(temp2[, 4]))
  w <- 1.5 + 0.1 * max(nchar(as.character(levels(temp2[, 4]))))
  pdf(paste0(plot.dir, 'tukey_', trt, '.pdf'), height=h, width=w)
    grid.table(HSD.test(fit, trt)$groups, rows=NULL)
  ignore <- dev.off()

  groups <- tukey$groups
  means <- tukey$means
  sum.means <- sum(means$r)
  # Qual a origem deste número mágico?
  max.mean <- -9
  s <- 0
  for (j in 1:nrow(groups)) {
    if (groups$means[j] > max.mean) {
      if (str_count(paste(groups$M, collapse=''), as.character(groups$M[j])) == 1) {
        max.mean <- groups$means[j]
        s <- means$r[row.names(means) == trimws(groups$trt[j])] / sum.means
      }
    }
  }

  df <- data.frame(treatment=trt,
                   f.value=fit.summary[[1]][['F value']][[1]],
                   max.mean=max.mean,
                   sup=s)
  treatments <- rbind(treatments, df)
}

# Qual a origem deste número mágico? (0.1)
treatments <- treatments[treatments$sup >= 0.1,]
treatments <- treatments[order(treatments$max.mean, decreasing=TRUE),]
write.table(treatments, '../data/anova.csv', row.names=FALSE, dec=',', sep=';')

# Association rules will be done in future works
quit()
################################################################################
################################################################################
################################################################################
################################################################################









# Association Rule Mining #############################################################

# Restoring data frame to its original format
CS.choice.index <- match('CS_Choice', names(poll.answers))
year.index <- match('Year', names(poll.answers))
temp <- poll.answers[, -c(CS.choice.index, year.index)]

# Mining rules

# APRIORI
cs_rules = apriori(data = temp,
                   parameter = list(confidence=0.5, maxtime=300, maxlen=3),
                   appearance = list(rhs="CS_Interest=Yes", default="lhs"))
cs_rules_ordered <- sort(cs_rules, by='lift')

# plot
pdf(paste0(plot.dir, 'apriori.pdf'))
  plot(cs_rules_ordered)
ignore <- dev.off()

# Saving rules on disk
write(cs_rules_ordered, file='../data/apriori.csv', sep=';', row.names=FALSE)

# # ECLAT
# cs_rules_eclat <- eclat(data=temp, parameter=list(maxlen=3, support=0.01))
# cs_rules_eclat <- sort(cs_rules_eclat, by='support')
#
# # Checking complementary rules
# df <- data.frame(itemset=labels(cs_rules_eclat), support=cs_rules_eclat@quality)
# df$itemset <- gsub("\\{|\\}", "", df$itemset)
# itemsets <- as.data.frame(str_split_fixed(gsub("\\{|\\}", "", df$itemset), ",", 3))
# names(itemsets) <- c("item1", "item2", "item3")
# itemsets[grepl("CS_Interest", itemsets$item2, fixed=TRUE), c("item1", "item2")] <- itemsets[grepl("CS_Interest", itemsets$item2, fixed=TRUE), c("item2", "item1")]
# itemsets[grepl("CS_Interest", itemsets$item3, fixed=TRUE), c("item1", "item3")] <- itemsets[grepl("CS_Interest", itemsets$item3, fixed=TRUE), c("item3", "item1")]
# itemsets <- cbind(itemsets, value=df$support)
# itemsets <- itemsets[order(itemsets$item1, itemsets$item2, itemsets$item3),]
# # complementary <- cast(data=itemsets[grepl("CS_Interest", itemsets$item1, fixed=TRUE),], formula="item2+item3~item1", mean, fill=NA)
# uniques <- itemsets[itemsets$item2 == "" & itemsets$item3 == "",]
# CS_plus_other <- itemsets[itemsets$item1 == "CS_Interest=Yes" & itemsets$item2 != "" & itemsets$item3 == "",]
#
# ## Rules A => B
# # confidence (A => B) = support(A U B) / support(A)
# # lift (A => B) = support(A U B) / support(A) * support(B)
#
# CS_plus_other$confidence <- NA
# CS_plus_other$lift <- NA
# for (i in 1:nrow(CS_plus_other)) {
#   item <- CS_plus_other$item2[i]
#   A <- uniques$value[match(item, uniques$item1)]
#   B <- uniques$value[match("CS_Interest=Yes", uniques$item1)]
#   CS_plus_other$confidence[i] <- CS_plus_other$value[i] / A
#   CS_plus_other$lift[i] <- CS_plus_other$value[i] / (A * B)
# }
#
# # Saving rules on disk
# # write(complementary, file='../data/eclat.txt')
#
# # Finding interactions...
