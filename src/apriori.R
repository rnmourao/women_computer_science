# file: script.R
# authors: Guilherme N. Ramos (gnramos@unb.br)
#          Roberto N. Mourão
#
# Script for analysis of data for the Meninas na Computação Project in
# the University of Brasília (http://meninas.cic.unb.br)

################################## Environment ######################################################

## Adapt this command to script's path
# setwd('/home/mourao/Documentos/women_computer_science/src/')

# Dependencies
library(arules)
library(arulesViz)
library(ggplot2)
library(gridExtra)
library(readxl)

# Pretty printing
options(digits=2, device='pdf', max.print=99999)
cat('\n')

# Cleaning up workspace
rm(list=ls())

#### Set working directory
plot.dir <- '../dexa/img/'


################################# Data Preparation ##########################################

# Get raw data
poll.answers <- read_excel('../data/raw.xlsx', sheet='answers', col_types=rep("text", 38), na='')

# Cleanup data
poll.answers$Q1 <- NULL
poll.answers$Q2 <- NULL

cat('Number of respondents: ', nrow(poll.answers), '\n\n')

# Girls only
poll.answers <- subset(poll.answers, poll.answers$Gender == 'F')
poll.answers$Gender <- NULL

# Remove 2011's questionnaires
poll.answers <- subset(poll.answers, poll.answers$Year != 2011)

# Only Middle and High Schools
poll.answers <- subset(poll.answers, 
                       poll.answers$Educational.Stage != 'College' & 
                         poll.answers$Educational.Stage != 'Adult Education Program')

# Remove NA from CS.Interest
poll.answers <- subset(poll.answers, !is.na(poll.answers$CS.Interest))

# transform variables type to factor
poll.answers <- as.data.frame(lapply(poll.answers, as.factor))
poll.answers$CS.Interest <- factor(poll.answers$CS.Interest, 
                                   levels=c("No", "Maybe", "Yes"))

# Ordering columns
poll.answers <- poll.answers[, c('CS.Interest', setdiff(names(poll.answers), 'CS.Interest'))]

cat('Female respondents: ', nrow(poll.answers), '\n\n')

cat('Female interested in CS: ', nrow(poll.answers[poll.answers$CS.Interest=='Yes',]), '\n\n')

############################# Association Rule Mining #################################

# Mining rules

# APRIORI
cs.rules = apriori(data = poll.answers, 
                   parameter = list(confidence = 0.5, maxtime = 300, maxlen=15), 
                   appearance = list(rhs = list("CS.Interest=Yes",
                                                "CS.Interest=No"), default = "lhs"))

cs.rules.ordered <- sort(cs.rules, by = 'lift')

rules.df <- cbind(as(cs.rules.ordered, "data.frame"))
rules.df$lhs <- substr(rules.df$rules, 
                       1, 
                       regexpr(' =>', as.character(rules.df$rules), fixed=TRUE))
rules.df$rhs <- substr(rules.df$rules, 
                       regexpr('=>', as.character(rules.df$rules), fixed=TRUE) + 3, 
                       nchar(as.character(rules.df$rules)))
rules.df$rules <- NULL
rules.df <- rules.df[, c(4, 5, 1, 2, 3)]

# Saving rules on disk
write(cs.rules.ordered, file='../data/apriori.csv', sep=";", row.names = FALSE)

# plot
pdf(paste0(plot.dir, 'plot.apriori.pdf'))
  plot(cs.rules.ordered)
ignore <- dev.off()

# df
pdf(paste0(plot.dir, 'apriori.pdf'), width=10, height=4)
  grid.table(rules.df, rows=NULL)
ignore <- dev.off()

############################################## Charts ###############################################
### Results per year 
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


### Results per educational stage 
educational.stage <- aggregate(x=list(total=poll.answers$Educational.Stage),
                               by=list(Year=poll.answers$Year, Educational.Stage=poll.answers$Educational.Stage),
                               FUN=length)
by.year <- aggregate(x=list(Total=poll.answers$Educational.Stage),
                     by=list(Year=poll.answers$Year),
                     FUN=length)
educational.stage <- merge(educational.stage, by.year)
educational.stage$Percentage <- (educational.stage$total * 100) / educational.stage$Total

p <- ggplot(data=educational.stage, aes(x='', y=Percentage, fill=Educational.Stage)) +
  geom_bar(width=1, stat='identity') +
  facet_grid(facets=. ~ Year) +
  coord_polar(theta='y', start=0) +
  scale_x_discrete() +
  xlab('') + ylab('') + labs(fill='') +
  scale_fill_discrete(breaks=c('Middle School', 'High School (10th Grade)',
                               'High School (11th Grade)', 'High School (12th Grade)'))
ggsave(paste0(plot.dir, 'EducationalStage.pdf'), width=7, height=2)

### Results per interest in undergraduate field of study 
field.of.interest <- aggregate(x=list(NumRespondents=poll.answers$Field.Interest),
                               by=list(Year=poll.answers$Year, FieldOfInterest=poll.answers$Field.Interest),
                               FUN=length)
by.year <- aggregate(x=list(TotalRespondents=poll.answers$Field.Interest),
                     by=list(Year=poll.answers$Year),
                     FUN=length)
field.of.interest <- merge(field.of.interest, by.year)
field.of.interest$Percentage <- (field.of.interest$NumRespondents * 100) / field.of.interest$TotalRespondents

p <- ggplot(data=field.of.interest, aes(x='', y=Percentage, fill=FieldOfInterest)) +
  geom_bar(width=1, stat='identity') +
  facet_grid(facets=. ~ Year) +
  coord_polar(theta='y', start=0) +
  scale_x_discrete() +
  xlab('') + ylab('') + labs(fill='')
ggsave(paste0(plot.dir, 'FieldOfInterest.pdf'), width=7, height=2)

cat('All respondents:\n')
aggregate(x=list(Percentage=field.of.interest$Percentage),
          by=list(FieldOfInterest=field.of.interest$FieldOfInterest),
          FUN=mean)
cat('\n')

### Results for interest in Computer Science
interest.in.CS <- aggregate(x=list(Quantidade=poll.answers$CS.Interest),
                            by=list(Year=poll.answers$Year, CS.Interest=poll.answers$CS.Interest),
                            FUN=length)
by.year <- aggregate(x=list(Total=poll.answers$CS.Interest),
                     by=list(Year=poll.answers$Year),
                     FUN=length)
interest.in.CS <- merge(interest.in.CS, by.year)
interest.in.CS$Percentage <- (interest.in.CS$Quantidade * 100) / interest.in.CS$Total

p <- ggplot(data=interest.in.CS, aes(x='', y=Percentage, fill=CS.Interest)) +
  geom_bar(width=1, stat='identity') +
  facet_grid(facets=. ~ Year) +
  coord_polar(theta='y', start=0) +
  scale_x_discrete() +
  xlab('') + ylab('') + labs(fill='') + # labs(fill='Year')
  # ggtitle('Respondents Interested in CS')
  scale_fill_discrete(breaks=c('Yes', 'No', 'Maybe'))
ggsave(paste0(plot.dir, 'InterestInCS.pdf'), width=7, height=2)


### Show relation between CS.Interest and others Variables
for (i in 2:ncol(poll.answers)) {
  attribute.name <- names(poll.answers)[i]
  temp <- aggregate(x=list(Quantity=poll.answers$CS.Interest),
                    by=list(CS.Interest=poll.answers$CS.Interest, Treatment=poll.answers[, i]),
                    FUN=length)
  p <- ggplot(temp, aes(fill=Treatment, y=Quantity, x=CS.Interest)) +
    geom_bar(position="dodge", stat="identity") +
    scale_fill_discrete(name=attribute.name)
  ggsave(paste0(plot.dir, 'plot.', attribute.name, '.pdf'), width=5, height=3)
}

### Show relation between CS.Interest and others Variables
for (i in 2:ncol(poll.answers)) {
  attribute.name <- names(poll.answers)[i]
  temp <- aggregate(x=list(Quantity=poll.answers$CS.Interest),
                    by=list(CS.Interest=poll.answers$CS.Interest, Treatment=poll.answers[, i]),
                    FUN=length)
  p <- ggplot(temp, aes(fill=Treatment, y=Quantity, x=CS.Interest)) +
    geom_bar(position="dodge", stat="identity") +
    scale_fill_discrete(name=attribute.name)
  ggsave(paste0(plot.dir, 'plot.', attribute.name, '.pdf'), width=5, height=3)
}
