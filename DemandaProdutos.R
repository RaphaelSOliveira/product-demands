# Este trabalho tem o objetivo de desenvolver um modelo para prever com precisão a demanda de estoque com base em dados históricos de vendas.

# O conjunto de dados que trabalhado consiste em 7 semanas de transações de vendas no México. Toda semana, existem caminhões de entrega que
# entregam produtos aos fornecedores. Cada transação consiste em vendas e devoluções. Os retornos são os produtos que não foram vendidos e expiraram.
# A demanda por um produto em uma determinada semana é definida como as vendas desta semana subtraídas pelo retorno na próxima semana.

# Carregando bibliotecas
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(viridis))
suppressMessages(library(gridExtra))
suppressMessages(library(randomForest))
library(readr)
library(dplyr)
library(ggplot2)
library(viridis)
library(gridExtra)
library(randomForest)

# Lendo o dataset de treino
train_dataset <- read_csv("train.csv")
town_state <- read_csv("town_state.csv")
clients_table <- read_csv("cliente_tabla.csv")
products_table <- read_csv("producto_tabla.csv")

# Verificando dimensões do dataset de treino
dim(train_dataset)

# Verificando a quantidade de dados por semana
qtd_val_semana <- table(train_dataset$Semana)
prop.table(qtd_val_semana)

# Retirando amostra do dataset de treino
set.seed(1234)
train_sample_index <- sample(1:nrow(train_dataset), 100000)
train_sample <- train_dataset[train_sample_index,]

# Verificando a proporção de dados por semana para confirmar se a amostragem foi homogênea
# para as semanas
table(train_sample$Semana)
prop.table(table(train_sample$Semana))

# Visualizando tabelas
View(train_sample)
View(town_state)
View(clients_table)
View(products_table)

# Informações úteis sobre as tabelas
dim(train_sample)
dim(town_state)         
# 790 cidades cadastradas
dim(clients_table)      
# 935362 clientes cadastrados
dim(products_table)    
# 2592 produtos cadastrados

#informações sobre as variáveis dos datasets
str(train_sample)
str(town_state)         
str(clients_table)    
str(products_table)

# Análise exploratória

# Quantidade de valores únicos por variável
unique_values <- as.data.frame(lapply(train_sample, function(x)length(unique(x))))
unique_values

# Verificando valores missing
sapply(train_sample, function(x)sum(is.na(x)))
# Observado que não existem valores missing para serem tratados

# Analisando a variável de demanda ajustada
summary(train_sample$Demanda_uni_equil)
sd(train_sample$Demanda_uni_equil)

# Aqui temos uma ideia da quantidade de demanda das agências.
# Apenas 25% tem demanda maior que 6.000 unidade.

quantile(train_sample$Demanda_uni_equil, 0.9)

# Criando função que remove os outliers
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

# Removendo os outliers
train_sample_outlier <- remove_outliers(train_sample$Demanda_uni_equil)

# Visualizando a variável sem outliers
summary(train_sample_outlier)
boxplot(train_sample_outlier, ylab= "Demanda", main="Boxplot Demanda")

# Analisando a variável de devolução semanal de produtos em unidades
summary(train_sample$Dev_uni_proxima)
sd(train_sample$Dev_uni_proxima)
# Vemos que muitas agencias possuem uma devolução muito baixa, porém observamos
# apresença de outliers, chegando a 1.148 unidade devolvidas.

# A seguir farei uma séria de plotagens para responder a perguntas específicas.

# Quais as agências que possuem a maior demanda ajustada?
h1 <- train_sample %>% group_by(Agencia_ID) %>% summarise(Demand = sum(Demanda_uni_equil)) %>% arrange(desc(Demand)) %>%
  head(10) %>% mutate(Agencia_ID = as.character(Agencia_ID)) %>%
  ggplot(aes(x=reorder(Agencia_ID, Demand), y=Demand,fill=Agencia_ID)) + geom_bar(stat='identity') +
  ggtitle("Top 10 Agencias") + labs(x="Agencias") + scale_fill_hue(l=30, c=35) + coord_flip()  + 
  theme(legend.position = "none")

# Quais os clientes que possuem a maior demanda ajustada?
h2 <- train_sample %>% group_by(Cliente_ID) %>% summarise(Demand = sum(Demanda_uni_equil)) %>% arrange(desc(Demand)) %>%
  head(10) %>% mutate(Cliente_ID = as.character(Cliente_ID)) %>%
  ggplot(aes(x=reorder(Cliente_ID, Demand), y=Demand, fill=Cliente_ID)) + geom_bar(stat='identity') +
  ggtitle("Top 10 Clientes") + labs(x="Clientes") + scale_fill_hue(l=30, c=35) + coord_flip() +
  theme(legend.position = "none")

# Quais os produtos que possuem a maior demanda ajustada?
h3 <- train_sample %>% group_by(Producto_ID) %>% summarise(Demand = sum(Demanda_uni_equil)) %>% arrange(desc(Demand)) %>%
  head(15) %>% mutate(Producto_ID = as.character(Producto_ID)) %>%
  ggplot(aes(x=reorder(Producto_ID, Demand), y=Demand, fill=Producto_ID)) + geom_bar(stat='identity') +
  ggtitle("Top 15 Produtos") + labs(x="Produtos")  + scale_fill_hue(l=30, c=35) + coord_flip() +
  theme(legend.position = "none")

# Quais os canais que possuem a maior demanda ajustada?
h4 <- train_sample %>% group_by(Canal_ID) %>% summarise(Demand = sum(Demanda_uni_equil)) %>% arrange(desc(Demand)) %>%
  head(10) %>% mutate(Canal_ID = as.character(Canal_ID)) %>%
  ggplot(aes(x=reorder(Canal_ID, Demand), y=Demand, fill=Canal_ID)) + geom_bar(stat='identity') +
  ggtitle("Top 10 Canais") + labs(x="Canais")  + scale_fill_hue(l=30, c=35) + coord_flip() +
  theme(legend.position = "none")

grid.arrange(h1, h2, h3, h4, nrow=2)

# Dados dos IDs usados na montagem dos gráfico de barras acima.
# Cidade e Estado das agências 
as.data.frame(town_state %>% filter(Agencia_ID == 1911 | Agencia_ID == 1142 | Agencia_ID == 1347 | Agencia_ID == 1470
                                       | Agencia_ID == 1912 | Agencia_ID == 1129 | Agencia_ID == 1312 | Agencia_ID == 2034
                                       | Agencia_ID == 1114 | Agencia_ID == 2013))

# Nome clientes
as.data.frame(clients_table %>% filter(Cliente_ID == 653378 | Cliente_ID == 459391 | Cliente_ID == 653039 | Cliente_ID == 19189
                         | Cliente_ID == 787455 | Cliente_ID == 4514757 | Cliente_ID == 19237 | Cliente_ID == 4285606
                         | Cliente_ID == 56498 | Cliente_ID == 2418007))

# Nome produtos
as.data.frame(products_table %>% filter(Producto_ID == 2425 | Producto_ID == 1278 | Producto_ID == 1284 | Producto_ID == 43285
                                       | Producto_ID == 1250 | Producto_ID == 36610 | Producto_ID == 1240 | Producto_ID == 2233
                                       | Producto_ID == 1242 | Producto_ID == 45143 | Producto_ID == 1125 | Producto_ID == 35651
                                       | Producto_ID == 43206 | Producto_ID == 37058 | Producto_ID == 30532))



# Temos um cliente que consome disparadamente em relação aos outros, em qual agencia esse cliente consome?
h5 <- train_sample %>% filter(Cliente_ID==653378) %>% group_by(Agencia_ID) %>% summarise(Demand = sum(Demanda_uni_equil)) %>%
  arrange(desc(Demand)) %>% head(10) %>% mutate(Agencia_ID = as.character(Agencia_ID)) %>% 
  ggplot(aes(x=reorder(Agencia_ID, Demand), y=Demand, fill=Agencia_ID)) + geom_bar(stat='identity') +
  ggtitle("Top 10 Agencias para o TOP cliente - PUEBLA REMISION") + labs(x="Agencias") + scale_fill_hue(l=30, c=35) + 
  coord_flip() + theme(legend.position = "none")

# Temos um canal por onde passa que gera a maior parte da demana, para quais agencias a mercadoria vai?
h6 <- train_sample %>% filter(Canal_ID==1) %>% group_by(Agencia_ID) %>% summarise(Demand = sum(Demanda_uni_equil)) %>%
  arrange(desc(Demand)) %>% head(10) %>% mutate(Agencia_ID = as.character(Agencia_ID)) %>% 
  ggplot(aes(x=reorder(Agencia_ID, Demand), y=Demand, fill=Agencia_ID)) + geom_bar(stat='identity') +
  ggtitle("Top 10 Agencias para o TOP canal") + labs(x="Agencies") + scale_fill_hue(l=30, c=35) + 
  coord_flip() + theme(legend.position = "none")

grid.arrange(h5, h6, nrow=1)

# Cidade e Estado das agências 
as.data.frame(town_state %>% filter(Agencia_ID == 1255 | Agencia_ID == 1228 | Agencia_ID == 4082 | Agencia_ID == 1253
                                    | Agencia_ID == 1978 | Agencia_ID == 1250 | Agencia_ID == 2243 | Agencia_ID == 1976
                                    | Agencia_ID == 1252 | Agencia_ID == 4086))
# Cidade e Estado das agências 
as.data.frame(town_state %>% filter(Agencia_ID == 1911 | Agencia_ID == 1347 | Agencia_ID == 1470 | Agencia_ID == 1912
                                    | Agencia_ID == 1312 | Agencia_ID == 2034 | Agencia_ID == 2013 | Agencia_ID == 1945
                                    | Agencia_ID == 1123 | Agencia_ID == 1315))

# Quais clientes possuem a maior quantidade de produtos devolvidos(unidade)?
h7 <- train_sample %>% group_by(Cliente_ID) %>% summarise(Devolution = sum(Dev_uni_proxima)) %>% arrange(desc(Devolution)) %>%
  head(10) %>% mutate(Cliente_ID = as.character(Cliente_ID)) %>%
  ggplot(aes(x=reorder(Cliente_ID, Devolution), y=Devolution,fill=Cliente_ID)) + geom_bar(stat='identity') +
  ggtitle("Top 10 Clientes") + labs(x="Clientes") + scale_fill_hue(l=30, c=35) + coord_flip()  + 
  theme(legend.position = "none")

# Quais produtos possuem o maior valor de devolução?
h8 <- train_sample %>% group_by(Producto_ID) %>% summarise(Devolution = sum(Dev_uni_proxima)) %>% arrange(desc(Devolution)) %>%
  head(10) %>% mutate(Producto_ID = as.character(Producto_ID)) %>%
  ggplot(aes(x=reorder(Producto_ID, Devolution), y=Devolution,fill=Producto_ID)) + geom_bar(stat='identity') +
  ggtitle("Top 10 Produtos") + labs(x="Devolução") + scale_fill_hue(l=30, c=35) + coord_flip()  + 
  theme(legend.position = "none")

grid.arrange(h7, h8, nrow=1)

# Aqui temos alguma observações interessantes. O produto 1278 (Nito 1p 62g BIM 1278) é o segundo produto com maior demanda ajustada 
# e também é o segundo produto com maior devolução. Outros produtos se encaixam nesse perfil: 36610, 1240, 1125 e 37058.
# Seria interessante investigar estas questões mais a fundo

# Nome clientes
as.data.frame(clients_table %>% filter(Cliente_ID == 1061373 | Cliente_ID == 653378 | Cliente_ID == 4597742 | Cliente_ID == 4283860
                                       | Cliente_ID == 1961946 | Cliente_ID == 303718 | Cliente_ID == 4389154 | Cliente_ID == 225590
                                       | Cliente_ID == 1083916 | Cliente_ID == 652850))
# Nome produtos
as.data.frame(products_table %>% filter(Producto_ID == 30571 | Producto_ID == 1278 | Producto_ID == 36610 | Producto_ID == 37273
                                        | Producto_ID == 1240 | Producto_ID == 30574 | Producto_ID == 35651 | Producto_ID == 1125
                                        | Producto_ID == 37058 | Producto_ID == 41938))

# Outras questões poderiam ser levantadas como: quais os principais produtos que passam pela principal rota?
# Porém, não é objetivo deste trabalho explorar exaustivamente os dados pois as possibilidade são muitas.
# Queremos ter uma visão geral, caso a empresa necessitasse de uma informação específica, seria possível
# ir atrás de mais informações e insights.

# Machine Learning

# Transfomarndo as variáveis Semana e Canal_ID em fator
train_sample$Canal_ID <- as.factor(train_sample$Canal_ID)

# Removendo a variável semana do dataset
#train_sample <-train_sample[-1]

# Dividindo data set em treino e teste
smp_size <- floor(0.70 * nrow(train_sample)) 
train_ind <- sample(seq_len(nrow(train_sample)), size = smp_size)

train <- train_sample[train_ind,]
test <- train_sample[-train_ind,]

# Utilizando o algorítimo randomforest como ferramenta para averiguar a importância das vairiáveis
# para modelo preditivo
modelo_rf1 <- randomForest(Demanda_uni_equil ~ .,
                           data=train,
                           importance=TRUE)

varImpPlot(modelo_rf1,main="Importânica Variávies")

# Treinando modelo preditivo com o algorítimo de regressão linear
modelo_lm <- lm(Demanda_uni_equil ~ Venta_uni_hoy +
                           Venta_hoy +
                           Canal_ID +
                           Producto_ID +
                           Ruta_SAK +
                           Cliente_ID +
                           Agencia_ID,
                           data=train)
summary(modelo_lm)

par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(modelo_lm)

# Avaliando o modelo preditivo 
scores <- data.frame(actual = test$Demanda_uni_equil,
                     predicted = predict(modelo_lm, newdata = test[-11]))

# Calculando resíduos e salvando no DataFrame
scores <-  mutate(scores, resids = predicted - actual)

# Plotando os resíduos
ggplot(scores, aes(x = resids)) + 
  geom_histogram(binwidth = 1, fill = "white", color = "black") +
  ggtitle("Distribuição de Resíduos")
