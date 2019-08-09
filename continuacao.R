##########################################
#### TRABALHO FINAL- Analise de Dados ####
########## Prof: Davi Moreira ############
####### Aluna Tatiane Albuquerque ########
########### Gráficos e testes ############
##########################################


#------------- continuação-------------#

# Residuos

res = rstandard(lm(data$undernourishment ~ data$food_exports+data$agricultural_land+data$food_production));res

# Teste de normalidade (Shapiro-Wilk)

shapiro.test(res)

# verificando residuos do modelo escolhido, pelo teste de normalidade, verificamos que 
# não houve violação da normalidade dos resíduos.  Tendo distribuição normal.
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

## Agregar variaveis por país em forma de coluna

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

