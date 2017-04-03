# arquivo: apriori-pt-br.R
# autores: Guilherme N. Ramos (gnramos@unb.br)
#          Roberto N. Mourão
#
# Script para análise de dados do projeto "Meninas na Computação" da UnB.

################################## Ambiente ######################################################

## Adapte este comando para a localização do script
# setwd('/home/mourao/Documentos/women_computer_science/src/')

# Bibliotecas
library(arules)
library(arulesViz)
library(ggplot2)
library(gridExtra)
library(methods)
library(readxl)

# Formatação dos números, escolha do arquivo para impressão
options(digits=2, device='pdf', max.print=99999)
cat('\n')

# Efetuando limpeza do Workspace
rm(list=ls())

#### Definindo pasta para gravação gráficos e tabelas
plot.dir <- '../dexa/img/'

#### Funções

# tabela de contingencia
freq.table <- function(...) {
  columns <- list(...)
  column.names <- names(list(...))
  mytable <- table(A=columns[[1]], B=columns[[2]])
  Total <- margin.table(mytable, 1)
  mytable <- cbind(mytable, Total)
  Total <- margin.table(mytable, 2)
  mytable <- rbind(mytable, Total)
  mytable <- cbind(matrix(ncol=1, nrow=nrow(mytable)), mytable)
  mytable <- rbind(matrix(nrow=1, ncol=ncol(mytable)), mytable)
  rownames(mytable)[1] <- column.names[1]
  colnames(mytable)[1] <- column.names[2]
  mytable <- as.data.frame(mytable)
  mytable[is.na(mytable)] <- " "
  return(mytable)
}

################################# Preparação dos Dados ##########################################

# Recuperando dados de uma planilha.
respostas <- read_excel('../data/answers.xlsx', sheet='unificado', col_types=rep("text", 38), na='')

# Eliminando colunas desnecessarias
respostas$Q1 <- NULL
respostas$Q2 <- NULL

cat('--------------------------------------------------------------------\n')
cat('                               Número total de entrevistados: |', nrow(respostas), '\n')

# Removendo questionários de 2011
respostas <- subset(respostas, respostas$Ano != 2011)

cat('                  Número de entrevistados entre 2012 to 2014: |', nrow(respostas), '\n')

# Removendo entrevistados do sexo masculino
respostas <- subset(respostas, respostas$Sexo == 'F')
respostas$Sexo <- NULL

cat('                         Número de meninas entre 2012 e 2014: |', nrow(respostas), '\n')


# Mantendo somente Ensino Fundamental e Médio
respostas <- subset(respostas,
                       respostas$Serie != 'Ensino Superior' &
                         respostas$Serie != 'Supletivo')

cat('                     Meninas nos Ensinos Fundamental e Médio: |', nrow(respostas), '\n')

# Removendo NA do atributo alvo Fara.Computacao
respostas <- subset(respostas, !is.na(respostas$Fara.Computacao))
cat('Meninas que responderam se fariam vestibular para Computação: |', nrow(respostas), '\n')

# Transforma tipo dos atributos para Factor
respostas <- as.data.frame(lapply(respostas, as.factor))

# Ordena fatores
# for (i in 1:ncol(respostas)) {
#   if (length(levels(respostas[, i])) == 3 &
#       names(respostas)[i] != 'Ano' &
#       names(respostas)[i] != 'Fara.Curso.Superior') {
#     respostas[, i] <- factor(respostas[, i],
#                                        levels=c("No", "Maybe", "Yes"))
#   }
# }

# Ordenando colunas
respostas <- respostas[, c('Fara.Computacao', setdiff(names(respostas), 'Fara.Computacao'))]

cat('                               Meninas que fariam Computacao: |', nrow(respostas[respostas$Fara.Computacao=='Sim',]), '\n')
cat('--------------------------------------------------------------------\n')


## Tabelas de contingencia
for (i in 2:ncol(respostas)) {
  nome.atributo <- names(respostas)[i]

  ft <- freq.table(a=respostas[, i], Fara.Computacao=respostas$Fara.Computacao)
  rownames(ft)[1] <- nome.atributo

  # turn bold rownames' font
  t1 <- ttheme_minimal(rowhead=list(fg_params=list(fontface=c(rep("bold", nrow(ft))))))

  # size of image
  w <- 1 * ncol(ft) + .1 * max(nchar(row.names(ft)))
  h <- 1 + nrow(ft) * .2

  pdf(paste0(plot.dir, 'freq.', nome.atributo, '.pdf'), width=w, height=h)
    grid.table(ft, theme=t1)
  ignore <- dev.off()
}

############################# Mineração de Regras de Associação #################################
# APRIORI
regras   = apriori(data = respostas,
                   parameter = list(confidence = 0.5, maxtime = 300, maxlen=5),
                   appearance = list(rhs = list("Fara.Computacao=Sim",
                                                "Fara.Computacao=Nao sei ainda",
                                                "Fara.Computacao=Nao"), default = "lhs"))

regras.ordenadas <- sort(regras, by = 'lift')

# Salvando regras
write(regras.ordenadas, file='../data/apriori.csv', sep=";", row.names=FALSE)

# Gráfico
pdf(paste0(plot.dir, 'grafico.regras.pdf'))
  plot(regras.ordenadas)
ignore <- dev.off()

regras.df <- cbind(as(regras.ordenadas, "data.frame"))

regras.df$lhs <- substr(regras.df$rules,
                       1,
                       regexpr(' =>', as.character(regras.df$rules), fixed=TRUE))
regras.df$lhs <- gsub(',', ',\n', regras.df$lhs)

regras.df$rhs <- substr(regras.df$rules,
                       regexpr('=>', as.character(regras.df$rules), fixed=TRUE) + 3,
                       nchar(as.character(regras.df$rules)))
regras.df$rules <- NULL
regras.df <- regras.df[, c(4, 5, 1, 2, 3)]
regras.df <- subset(regras.df, regras.df$lift >= 1.5) # maior que 50%

# Tabela
pdf(paste0(plot.dir, 'tabela.regras.pdf'), width=8, height=6)
  grid.table(regras.df, rows=NULL)
ignore <- dev.off()


############################################## Gráficos ###############################################
### Resultados por ano
entrevistados.por.Ano <- aggregate(x=list(Total=respostas$Ano),
                                  by=list(Ano=respostas$Ano),
                                  FUN=length)

p <- ggplot(data=entrevistados.por.Ano, aes(x='', y=Total, fill=Ano)) +
  geom_bar(width=1, stat='identity') +
  coord_polar(theta='y', start=0) +
  scale_x_discrete() +
  xlab('') + ylab('') + labs(fill='') + # + labs(fill='Stage')
  # ggtitle('Entrevistados por Ano') +
  ggsave(paste0(plot.dir, 'EntrevistadosPorAno.pdf'), width=7, height=2)

### Resultados por Serie
Serie <- aggregate(x=list(total=respostas$Serie),
                               by=list(Ano=respostas$Ano, Serie=respostas$Serie),
                               FUN=length)
por.Ano <- aggregate(x=list(Total=respostas$Serie),
                     by=list(Ano=respostas$Ano),
                     FUN=length)
Serie <- merge(Serie, por.Ano)
Serie$Percentual <- (Serie$total * 100) / Serie$Total

p <- ggplot(data=Serie, aes(x='', y=Percentual, fill=Serie)) +
  geom_bar(width=1, stat='identity') +
  facet_grid(facets=. ~ Ano) +
  coord_polar(theta='y', start=0) +
  scale_x_discrete() +
  xlab('') + ylab('') + labs(fill='') +
  scale_fill_discrete(breaks=c('Ensino Fundamental', 'Primeiro Ano',
                               'Segundo Ano', 'Terceiro Ano'))
ggsave(paste0(plot.dir, 'Serie.pdf'), width=7, height=2)

### Resultados por Área de Interesse
Fara.Curso.Superior <- aggregate(x=list(NumEntrevistados=respostas$Fara.Curso.Superior),
                               by=list(Ano=respostas$Ano, FaraCursoSuperior=respostas$Fara.Curso.Superior),
                               FUN=length)
by.Year <- aggregate(x=list(TotalEntrevistados=respostas$Fara.Curso.Superior),
                     by=list(Ano=respostas$Ano),
                     FUN=length)
Fara.Curso.Superior <- merge(Fara.Curso.Superior, by.Year)
Fara.Curso.Superior$Percentual <- (Fara.Curso.Superior$NumEntrevistados * 100) / Fara.Curso.Superior$TotalEntrevistados

p <- ggplot(data=Fara.Curso.Superior, aes(x='', y=Percentual, fill=FaraCursoSuperior)) +
  geom_bar(width=1, stat='identity') +
  facet_grid(facets=. ~ Ano) +
  coord_polar(theta='y', start=0) +
  scale_x_discrete() +
  xlab('') + ylab('') + labs(fill='')
ggsave(paste0(plot.dir, 'FaraCursoSuperior.pdf'), width=7, height=2)

cat('Todos os Entrevistados:\n')
aggregate(x=list(Percentual=Fara.Curso.Superior$Percentual),
          by=list(FieldOfInterest=Fara.Curso.Superior$FaraCursoSuperior),
          FUN=mean)
cat('\n')

### Resultados por interesse em Ciência da Computação
Fara.Computacao <- aggregate(x=list(Quantidade=respostas$Fara.Computacao),
                            by=list(Ano=respostas$Ano, Fara.Computacao=respostas$Fara.Computacao),
                            FUN=length)
por.Ano <- aggregate(x=list(Total=respostas$Fara.Computacao),
                     by=list(Ano=respostas$Ano),
                     FUN=length)
Fara.Computacao <- merge(Fara.Computacao, por.Ano)
Fara.Computacao$Percentual <- (Fara.Computacao$Quantidade * 100) / Fara.Computacao$Total

p <- ggplot(data=Fara.Computacao, aes(x='', y=Percentual, fill=Fara.Computacao)) +
  geom_bar(width=1, stat='identity') +
  facet_grid(facets=. ~ Ano) +
  coord_polar(theta='y', start=0) +
  scale_x_discrete() +
  xlab('') + ylab('') + labs(fill='') + # labs(fill='Ano')
  scale_fill_discrete(breaks=c('Sim', 'Nao', 'Nao sei ainda'))
ggsave(paste0(plot.dir, 'FaraComputacao.pdf'), width=7, height=2)


### Mostra a relação entre Fara.Computacao e outros atributos
for (i in 2:ncol(respostas)) {
  nome.atributo <- names(respostas)[i]
  temp <- aggregate(x=list(Quantidade=respostas$Fara.Computacao),
                    by=list(Fara.Computacao=respostas$Fara.Computacao, Tratamento=respostas[, i]),
                    FUN=length)
  p <- ggplot(temp, aes(fill=Tratamento, y=Quantidade, x=Fara.Computacao)) +
    geom_bar(position="dodge", stat="identity") +
    scale_fill_discrete(name=nome.atributo)
  ggsave(paste0(plot.dir, 'plot.', nome.atributo, '.pdf'), width=5, height=3)
}
