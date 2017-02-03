# file: script.R
# authors: Guilherme N. Ramos (gnramos@unb.br)
#          Roberto Mourão
#
# Script for analysis of data for the Meninas na Computação Project in
# the University of Brasília (http://meninas.cic.unb.br)

# Dependencies
for (pkg in c('readxl', 'readr', 'arules', 'ggplot2', 'reshape2',
              'gridExtra', 'stringr', 'knitr', 'agricolae'))
  if (!require(pkg, character.only=TRUE))
    ignore <- install.packages(pkg)

# Pretty printing
options('scipen'=100, 'digits'=2)

# Cleaning up workspace
rm(list = ls())

# Set working directory
# setwd("/home/mourao/Documentos/women_computer_science/")

# Get raw data
poll.answers <- read_excel('raw.xlsx', sheet='unificado', na='')

# Cleanup data
poll.answers$Q1 <- NULL
poll.answers$Q2 <- NULL
poll.answers$Serie <- stringr::str_trim(poll.answers$Serie, side='both')

# Girls only
poll.answers <- subset(poll.answers, poll.answers$Sexo == 'F')
poll.answers$Sexo <- NULL

# Translation
poll.answers$Serie[poll.answers$Serie=='Ensino Fundamental'] <- 'Middle School'
poll.answers$Serie[poll.answers$Serie=='Primeiro Ano'] <- 'High School (10th Grade)'
poll.answers$Serie[poll.answers$Serie=='Segundo Ano'] <- 'High School (11th Grade)'
poll.answers$Serie[poll.answers$Serie=='Terceiro Ano'] <- 'High School (12th Grade)'
poll.answers$Serie[poll.answers$Serie=='Supletivo'] <- 'Adult Education Program'
poll.answers$Serie[poll.answers$Serie=='Ensino Superior'] <- 'College'
poll.answers$Fara_Curso_Superior[poll.answers$Fara_Curso_Superior=='Biologicas e Saude'] <- 'Biology-Health Sciences'
poll.answers$Fara_Curso_Superior[poll.answers$Fara_Curso_Superior=='Humanas'] <- 'Human Sciences'
poll.answers$Fara_Curso_Superior[poll.answers$Fara_Curso_Superior=='Exatas'] <- 'Exact Sciences'
poll.answers$Fara_Computacao[poll.answers$Fara_Computacao=='Sim'] <- 'Yes'
poll.answers$Fara_Computacao[poll.answers$Fara_Computacao=='Nao'] <- 'No'
poll.answers$Fara_Computacao[poll.answers$Fara_Computacao=='Nao sei ainda'] <- 'Maybe'

# Processing
poll.answers <- as.data.frame(lapply(poll.answers, as.factor))

# Results per educational stage ################################################
educational.stage <- aggregate(x=list(total=poll.answers$Serie),
                               by=list(Year=poll.answers$Ano, Serie=poll.answers$Serie),
                               FUN=length)
by.year <- aggregate(x=list(Total=poll.answers$Serie),
                     by=list(Year=poll.answers$Ano),
                     FUN=length)

educational.stage <- merge(educational.stage, by.year)
educational.stage$Percentual <- (educational.stage$total * 100) / educational.stage$Total

p <- ggplot(data=educational.stage, aes(x='', y=Percentual, fill=Serie)) +
     geom_bar(width=1, stat='identity') +
     facet_grid(facets=. ~ Year) +
     coord_polar(theta='y', start=0) +
     scale_x_discrete() +
     xlab('') + ylab('') + labs(fill='') + # + labs(fill='Stage')
     ggtitle('Interviewees per Educational Stage') +
     scale_fill_discrete(breaks=c('Middle School', 'High School (10th Grade)', 'High School (11th Grade)', 'High School (12th Grade)', 'Adult Education Program', 'College'))
ggsave('EducationalStage.pdf', width=7, height=2)


# Results per interest in undergraduate field of study #########################
field.of.interest <- aggregate(x=list(total=poll.answers$Fara_Curso_Superior),
                               by=list(Year=poll.answers$Ano, Fara_Curso_Superior=poll.answers$Fara_Curso_Superior),
                               FUN=length)
by.year <- aggregate(x=list(Total=poll.answers$Fara_Curso_Superior),
                     by=list(Year=poll.answers$Ano),
                     FUN=length)

field.of.interest <- merge(field.of.interest, by.year)
field.of.interest$Percentual <- (field.of.interest$total * 100) / field.of.interest$Total

p <- ggplot(data=field.of.interest, aes(x='', y=Percentual, fill=Fara_Curso_Superior)) +
     geom_bar(width=1, stat='identity') +
     facet_grid(facets=. ~ Year) +
     coord_polar(theta='y', start=0) +
     scale_x_discrete() +
     xlab('') + ylab('') + labs(fill='') + # + labs(fill='Field')
     ggtitle('Interviewees per Field of Interest')
ggsave('FieldOfInterest.pdf', width=7, height=2)

# Results for interest in Computer Science #####################################
interest.in.CS <- aggregate(x=list(Quantidade=poll.answers$Fara_Computacao),
                            by=list(Year=poll.answers$Ano, Fara_Computacao=poll.answers$Fara_Computacao),
                            FUN=length)
by.year <- aggregate(x=list(Total=poll.answers$Fara_Computacao),
                     by=list(Year=poll.answers$Ano),
                     FUN=length)

interest.in.CS <- merge(interest.in.CS, by.year)
interest.in.CS$Percentual <- (interest.in.CS$Quantidade * 100) / interest.in.CS$Total

p <- ggplot(data=interest.in.CS, aes(x='', y=Percentual, fill=Fara_Computacao)) +
     geom_bar(width=1, stat='identity') +
     facet_grid(facets=. ~ Year) +
     coord_polar(theta='y', start=0) +
     scale_x_discrete() +
     xlab('') + ylab('') + labs(fill='') + # labs(fill='Ano')
     ggtitle('Interviewees Interested in CS')
ggsave('InterestInCS.pdf', width=7, height=2)

# Variance analysis ############################################################

# For girls who chose to
CS.choice <- data.frame(Fara_Computacao=levels(poll.answers$Fara_Computacao),
                        CS_Choice=0)
CS.choice$CS_Choice[CS.choice$Fara_Computacao == "No"] <- -1
CS.choice$CS_Choice[CS.choice$Fara_Computacao == "Maybe"] <- 0
CS.choice$CS_Choice[CS.choice$Fara_Computacao == "Yes"] <- 1
poll.answers <- merge(poll.answers, CS.choice)
CS.choice.index <- match('CS_Choice', names(poll.answers))

print('CS.choice.index')
print(CS.choice.index)

year.index <- match('Ano', names(poll.answers))
temp <- data.frame(Treatment=poll.answers[, year.index],
                   CS_Choice=poll.answers[, CS.choice.index])

fit <- aov(CS_Choice ~ Treatment, data=temp)
pdf("anova_table_Ano.pdf", height=1, width=9)
  grid.table(anova(fit))
dev.off()

print(HSD.test(fit, 'Treatment'))


pdf('anova_chart_Ano.pdf')
  par(mfrow=c(2,2))
  plot(fit)
dev.off()

# Other attributes
pdf('Variance.pdf')
for (i in (year.index + 1):(CS.choice.index-1)) {  # CS.choice.index is the last one
  temp <- data.frame(Treatment=poll.answers[, i], CS_Choice=poll.answers[, CS.choice.index])

  nome <- colnames(poll.answers)[i]
  cat(paste('\nTreatment:', nome, '\n'))
  cat('\n')

  fit <- aov(CS_Choice ~ Treatment, data=temp)
  print(anova(fit))
  cat('\n')

  print(HSD.test(fit, 'Treatment'))

  par(mfrow=c(2,2))
  plot(fit)
  
}
dev.off()

# Grouping ANOVA treatments
multiple_anova <- function (temp, index) {
  number = sprintf("%02d", index)
  fit <- aov(CS_Choice ~ ., data = temp)
  pdf(paste("anova_table_multiple_", number, ".pdf", sep = ""), height = 10, width = 18)
    grid.table(anova(fit))
  dev.off()
  
  pdf(paste("anova_chart_multiple_", number, ".pdf", sep = ""))
    par(mfrow=c(2,2))
    plot(fit)
  dev.off()
  
  return(index + 1)
}

temp <- poll.answers[, c(2:ncol(poll.answers))]
i = 0

i <- multiple_anova(temp, i)

temp$Usa_Computador_Biblioteca <- NULL
i <- multiple_anova(temp, i)

temp$Usa_Email <- NULL
i <- multiple_anova(temp, i)

temp$Usa_Editor_Texto <- NULL
i <- multiple_anova(temp, i)

temp$Usa_Outros_Softwares <- NULL
i <- multiple_anova(temp, i)

temp$Usa_Criatividade <- NULL
i <- multiple_anova(temp, i)

temp$Usa_Computador_Trabalho <- NULL
i <- multiple_anova(temp, i)

temp$Usa_Redes_Sociais <- NULL
i <- multiple_anova(temp, i)

temp$Usa_Computador_Casa_Parentes <- NULL
i <- multiple_anova(temp, i)

temp$Usa_Editor_Imagem <- NULL
i <- multiple_anova(temp, i)

temp$Usa_Internet <- NULL
i <- multiple_anova(temp, i)

temp$Pouco_Lazer <- NULL
i <- multiple_anova(temp, i)

temp$Desenvolve_Paginas_Web <- NULL
i <- multiple_anova(temp, i)

temp$Usa_Planilha_Eletrônica <- NULL
i <- multiple_anova(temp, i)

temp$Usa_Computador_Casa_Amigos <- NULL
i <- multiple_anova(temp, i)

temp$Usa_Jogos <- NULL
i <- multiple_anova(temp, i)