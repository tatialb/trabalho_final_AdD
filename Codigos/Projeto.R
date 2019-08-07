##########################################
#### TRABALHO FINAL- Analise de Dados ####
########## Prof: Davi Moreira ############
####### Aluna Tatiane Albuquerque ########
##########################################


# importar dados
library(readxl)
library(readr)

fao = read_excel("Dados/fao_fome.xlsx")
fao2 = read.csv("Dados/fao2.csv", sep= ',')
bm = read_excel("Dados/banco_mundial.xlsx")

#=================================================#
# tratamento de dados #
#=================================================#
# install.packages("reshape2")
# selecionar colunas de interesse
fao = fao[,c(6, 7, 13:29)]


#### FAO ####

# transformar colunas em linhas por ano

library(reshape2)

fao = melt(fao, measure.vars = 3:19, variable.name="year", value.name="undernourishment")

# remover '<'

library(stringr)

fao$undernourishment = str_replace(fao$undernourishment, '<', '')

# remover '"' e substituir

fao$GeoAreaCode = str_replace_all(fao$GeoAreaCode, '"','')

fao$GeoAreaName = str_replace_all(fao$GeoAreaName, '"','')

# transformar em numerico

fao$undernourishment = as.numeric(fao$undernourishment)

# visualizar casos faltantes

# install.packages("Amelia")

library(Amelia)
missmap(fao)

# remover casos faltantes

fao = fao[complete.cases(fao$undernourishment),]


# FAO 2

# selecionar variaveis de interesse

fao2 = fao2[,c(3,4,6,8,10,12)]

# pivotar tabela

fao2 <- dcast(fao2, Area + Year ~ Element + Item, value.var = "Value")

#### BANCO MUNDIAL ####

# selecionar nomes das colunas

names = colnames(bm)

names = gsub("\\s*\\[[^\\)]+\\]","",(names))

# substituicao do nome das colunas

colnames(bm) = names

bm = bm[,-c(4)]

# pivotando tabela

bmm = melt(bm, measure.vars = 4:28, variable.names = "year", value.name= "value")

# transformar os pontos em valores nulos

bmm$value = as.numeric(bmm$value)

# remover missing

bmm = bmm[complete.cases(bmm$`Country Name`),]

# transformar coluna 'Series Name' em variaveis 

bm <- dcast(bmm, `Country Name` + `Country Code` + `variable` ~ `Series Name`, value.var = "value")


## distribuicao da producao de cereal

library(ggplot2)

ggplot(data=bm, aes(bm$`Agricultural land (% of land area)`))+
         geom_histogram()+
         labs(x='Terra para agricultura', y = 'Frequência')

## distribuicao das terras por pais

# remover casos faltantes

bm = bm[complete.cases(bm$`Agricultural land (% of land area)`),]

# agregar valores por pais

agc = aggregate(bm$`Agricultural land (% of land area)`, by = list(bm$`Country Name`), sum)

# ordenar casos

agc$Group.1 = factor(agc$Group.1, levels = agc$Group.1[order(agc$x, decreasing = TRUE)])


# visualizar grafico

ggplot(data=agc, aes(x = agc$Group.1, y= agc$x))+
  geom_col()+
  labs(x='', y= 'Total de Terras para agricultura')+
  theme_minimal()+
  theme(axis.text.x = element_text(size=10, angle = 60, hjust = 1, vjust = 1))


# distribuicao da terra para agricultura na America latina por ano

# agregar valores por ano

agc = aggregate(bm$`Agricultural land (% of land area)`, by = list(bm$variable), sum)

# visualizar grafico

ggplot(data=agc, aes(x = agc$Group.1, y= agc$x, group=1))+
  geom_line(color='#702963')+
  labs(x='',y= ' Total de terras para agricultura')+
  theme_minimal()+
  theme(axis.text.x = element_text(size=10, angle = 60, hjust = 1, vjust = 1))


# combinacao dos bancos de dados
library(stringi)
fao$nome_pais = stri_trans_general(fao$GeoAreaName, "latin-ascii")
fao2$nome_pais = stri_trans_general(fao2$Area, "latin-ascii")
bm$nome_pais = stri_trans_general(bm$`Country Name`, "latin-ascii")

# passar para caixa baixa
fao$nome_pais = tolower(fao$nome_pais)
fao2$nome_pais = tolower(fao2$nome_pais)
bm$nome_pais = tolower(bm$nome_pais)

# remover texto entre parenteses

fao$nome_pais = gsub("\\s*\\([^\\)]+\\)","",as.character(fao$nome_pais))
fao2$nome_pais = gsub ("\\s*\\([^\\)]+\\)","",as.character(fao2$nome_pais))

# remover ', rb' no nome da venezuela 
library(stringr)
bm$nome_pais = str_replace(bm$nome_pais,', rb', '')

bm$year = bm$variable

# criar coluna padronizada year

fao2$year = fao2$Year

### combinar dados
dataset = merge(bm, fao2, by=c('nome_pais', 'year'))

datasetx = merge(dataset, fao, by=c ('nome_pais', 'year'))

write.csv(datasetx, 'dataset.csv', row.names = F)
write_excel_csv(datasetx, 'dataset2.csv')
