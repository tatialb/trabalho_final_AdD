##########################################
##### Projeto - Analise de Dados #########
####### aluna Tatiane Albuquerque ########
########## Prof Davi Moreira #############
############# 09/08/2019 #################
##########################################


# importar dados
library(readxl)
library(readr)

setwd("C:/Users/tatia/Desktop/trabalho dados/trabalho_final_AdD")

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


#------------- continuacao-------------#

# Residuos

res = rstandard(lm(data$undernourishment ~ data$food_exports+data$agricultural_land+data$food_production));res

# Teste de normalidade (Shapiro-Wilk)

shapiro.test(res)

# verificando residuos do modelo escolhido, pelo teste de normalidade, verificamos que 
# não houve violação da normalidade dos res�?duos.  Tendo distribuição normal.
# data:  res
# W = 0.92405, p-value = 2.358e-12


# Teste de variancia
library(car)
ncvTest(lm(data$undernourishment~data$food_exports + data$food_production + data$agricultural_land))

# Teste de Hausman

phtest(model,model1)

library(lmtest)

# Ver resultado em tabela

resultado = stargazer(data, type = "html")
write(resultado, "res1.html")

# visualizar grafico

datayear = aggregate(data$`food_exports`, by = list(data$year), sum)

ggplot(data=datayear, aes(x = datayear$Group.1, y= datayear$x, group=1))+
  geom_line(color='#702963')+
  labs(x='',y= 'Exports in Latin America per year')+
  theme_minimal()+
  theme(axis.text.x = element_text(size=10, angle = 60, hjust = 1, vjust = 1))

## Agregar variaveis por pa�?s em forma de coluna

# Subnutricao

datacountry = aggregate(data$`undernourishment`, by = list(data$country_name), sum)

ggplot(data=datacountry, aes(x = datacountry$Group.1, y= datacountry$x, group=1))+
  geom_col(fill = "lightblue")+
  labs(x='',y= 'Undernourishment per country')+
  theme_minimal()+
  theme(axis.text.x = element_text(size=15, angle = 50, hjust = 1, vjust = 1))

# Exportacao de alimentos

datacountry = aggregate(data$`food_exports`, by = list(data$country_name), sum)

ggplot(data=datacountry, aes(x = datacountry$Group.1, y= datacountry$x, group=1))+
  geom_col(fill = "lightblue")+
  labs(x='',y= 'Food Exports per country')+
  theme_minimal()+
  theme(axis.text.x = element_text(size=15, angle = 50, hjust = 1, vjust = 1))

# Producao de alimentos

datacountry = aggregate(data$`food_production`, by = list(data$country_name), sum)

ggplot(data=datacountry, aes(x = datacountry$Group.1, y= datacountry$x, group=1))+
  geom_col(fill = "lightblue")+
  labs(x='',y= 'Food Production per country')+
  theme_minimal()+
  theme(axis.text.x = element_text(size=15, angle = 50, hjust = 1, vjust = 1))

# Terras agricultaveis

datacountry = aggregate(data$`agricultural_land`, by = list(data$country_name), sum)

ggplot(data=datacountry, aes(x = datacountry$Group.1, y= datacountry$x, group=1))+
  geom_col(fill = "lightblue")+
  labs(x='',y= 'Agricultural Land per country')+
  theme_minimal()+
  theme(axis.text.x = element_text(size=15, angle = 50, hjust = 1, vjust = 1))

### grafico de dispersao com reta de regressao em painel

ggplot(data = data, aes(data$undernourishment, data$agricultural_land))+
  geom_point() +
  stat_smooth(method = "lm", col = "blue")  


stargazer(model,
          type = "text",
          header = FALSE,
          title = "Table of Regression Results",
          digits = 2,
          style = "apsr")

library(stargazer)
data(mtcars)

tex_write(model, file = "model.tex", headers = TRUE)


# Para checar dados com gráfico de pontos

plot(data$undernourishment,data$food_exports)


# ## 2 OLS models
linear.1 <- lm(undernourishment~ food_exports + food_production + agricultural_land,
               data=data)
linear.2 <- lm(undernourishment~ food_exports + food_production + agricultural_land,
               data=data)

## Criar um indicador para a variavel dependente | Rodar um modelo probit

# "dcolumn" LATEX, tentar no Rmd <<<<

data$highundernourishment <- (data$undernourishment > 20)
probit.model <- glm(highundernourishment ~ food_exports + food_production + agricultural_land,
                    data=data,
                    family = binomial(link = "probit"))
stargazer(linear.1, linear.2, probit.model, title="Results", align=TRUE)

### MODELAGEM ###

#--------- importar e selecionar dados

# carregar pacotes
library(ggplot2); library(reshape2); library(plm)

# importar dados
dataset = read.csv("dataset2.csv")

# selecionar variaveis de interesse
data = dataset[, c(1,2,3,7,14,16,46)]

# renomear colunas
colnames(data) = c('nome_pais', 'year', 'country_name', 'agricultural_land', 'food_exports',
                   'food_production', 'undernourishment')

# remover casos faltantes 

data = data[complete.cases(data),]

### ------- visualizar distribuição das variaveis 

# undernourishment
ggplot(data=data, aes(data$undernourishment))+
  geom_histogram()+
  labs(x='Undernourishment', y= 'Frequência')
ggsave('grafico1.png', width = 4, height = 6, units = "in")

## agricultural_land
ggplot(data= data, aes(data$agricultural_land))+
  geom_histogram()+
  labs(x= 'agricultural_land', y= 'Frequência')
ggsave('grafico2.png', width = 4, height = 6, units ="in")


###------------------- graficos de correlacao

### matrix de correlacao

# gerar matrix
cormat <- round(cor(data[,4:7]),2)

# configurar matriz para visualizacao
melted_cormat = melt(cormat)
head(melted_cormat)

# visualizar matrix com o ggplot2
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile()+
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank()
  )

### grafico de dispersao com reta de regressao

ggplot(data = data, aes(data$undernourishment, data$agricultural_land))+
  geom_point() +
  stat_smooth(method = "lm", col = "blue")  

#### Modelagem linear ####

model1 = lm(undernourishment ~
              agricultural_land +
              food_exports + food_production,
            data = data)

# visualizar modelo
summary(model1)

# visualizar modelo
install.packages('stargazer')
library(stargazer)

stargazer(model1,
          type = "text",
          header = FALSE,
          title= "Table of Regression Results",
          digits = 2,
          style = "apsr")



# Modelagem em painel

# executar modelo painel
model = plm(undernourishment ~
              agricultural_land +
              food_exports + food_production,
            index = c("nome_pais", "year"),
            model= "within",
            effect= "twoways",
            data=data)

# visualizacao modelo
summary(model)

# visualizacao modelo com Stargazer

stargazer(model,
          type = "text",
          header = FALSE,
          title = "Table of Regression Results",
          digits = 2,
          style = "apsr")

# grafico de hipotese interativa

install.packages("lmtest") 
install.packages(c('sjPlot','sjmisc'))
library(sjPlot)
library(sjmisc)

plot_model(model1,type="pred",terms =c("food_exports", "food_production", "agricultural_land"))

