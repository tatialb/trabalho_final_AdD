##########################################
##### Projeto - Analise de Dados #########
####### aluna Tatiane Albuquerque ########
########## Prof Davi Moreira #############
############# 09/08/2019 #################
##########################################


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
