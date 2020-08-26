install.packages("caret")
install.packages("ISLR")
install.packages("forecast")
install.packages("seasonal")
install.packages("fpp2")
library(fpp2)
library(seasonal)
library(forecast)
library("ISLR")
library(caret)
head(Wage)

class(Wage)
class(ativos)
Wage

dim(ativos)
0.7*543
543-380
380+163

base <- createTimeSlices(y = ativos$LAME4, initialWindow = 380, horizon = 163)
base
ativos2 <- as.zoo(window(ativos$LAME4, end = (2018-08-27)))

library(readxl)
url = 'https://bit.ly/2N9vtOh'
download.file(url, 'primario.xlsx', mode='wb')
data = read_excel('primario.xlsx', sheet='1.1', skip=4,
                  col_types = c('text', rep('numeric', 264)))
previdencia = t((data[c(14,36),-1]))

#Deflator da previdência
library(sidrar)
ipca = get_sidra(api='/t/1737/n1/all/v/2266/p/all/d/v2266%2013')
ipca = ts(ipca$Valor, start=c(1979,12), freq=12)
ipca = window(ipca, start=c(2003,02), end=c(2019,01))
ipca <- as.xts(ipca)

#############################################################################
Previdencia_receita <- read.csv(file.choose(), sep = ";", header = TRUE, dec = ",")

#Carregando dados da previdência
receita <- read.csv("C:/Users/gabri/OneDrive/Área de Trabalho/receita1.csv",
                   header = TRUE, sep = ';', dec = ",")
despesa <- read.csv("C:/Users/gabri/OneDrive/Área de Trabalho/GASTO.csv",
                    header = TRUE, sep = ";", dec = ",")

prev <- cbind(receita, despesa)
prev <- ts(prev, start = c(2003,02), end = c(2019,01), freq = 12 )
prev <- prev[,-1]
prev <- as.xts(prev)
View(prev)
head(prev)
class(prev)
dim(prev)

library(ggplot2)
head(fortify(prev))
ggplot(data = prev, aes(x = Index)) +
  geom_line(aes(y = RECEITA/1000, colour = "Receita"), size = 0.2) +
  geom_line(aes(y = ï..GASTO/1000, colour = "Despesa"), size = 0.2) +
  xlab("Anos") +
  theme(legend.position = "top") +
  ylab("R$ Bilhões") + labs(title = "Previdência Social: Receira x Despesa", 
                            caption = "Fonte: Criação própria com dados IPEADATA")

#Teste de estacionáriedade das séries
library(urca)
estac_receita <- ur.df(prev$RECEITA, type = "none", selectlags = "AIC")
summary(estac_receita)

estac_despesa <- ur.df(prev$ï..GASTO, type = "none", selectlags = "AIC")
summary(estac_despesa)

#Correlação de Pearson
cor(prev$RECEITA, prev$ï..GASTO)

#Regressão linear
prev_reg <- as.zoo(prev)
prev_beta <- lm(prev_reg$ï..GASTO~prev_reg$RECEITA)
summary(prev_beta)

#Regressão linear graficamente
ggplot(data = prev_reg, aes(x = RECEITA/1000, y = ï..GASTO/1000)) +
  geom_point(colour = "red", size = 1, shape = 1) +
  geom_smooth(method = "lm")+
  xlab("Receita")+
  ylab("Despesa")+
  labs(title = "Regressão Linear")

#Resíduos
prev_res <- as.zoo(prev_beta$residuals)
summary(prev_res)
plot(prev_res)

#Autocorrelação
ggAcf(prev_res)
ggPacf(prev_res)
dwtest(prev_beta)

#Teste de estacionáriedade dos resíduos
estac_res <- ur.df(prev_res, type = "none", selectlags = "AIC")
summary(estac_res)

#Separação dos dados para aprendizado de modelos auto regressivos e de Médias Móveis

prev_previsao <- (prev$ï..GASTO)
prev_d_test <- prev_previsao[1:134,]
prev_d_valid <- prev_previsao[135:192,]

dif_prev <- diff(prev_d_test)
ggAcf(dif_prev)
ggPacf(dif_prev)

#treinamento
ma_treino <- ma(prev_d_test, order = 57)
summary(ma_treino)

ggAcf(prev_d_test)
ggPacf(prev_d_test)
arima_treino <- arima(prev_d_test, order = c(0,0,0))
summary(arima_treino)

autoarima_treino <- auto.arima(prev_d_test)
summary(autoarima_treino)
plot(autoarima_treino$residuals)
### Holt's linear trends method
Htrend_treino <- holt(prev_d_test, h = 58)
summary(Htrend_treino)

#Predição 
predict_despesaMA <- forecast(ma_treino, h = 58)
predict_despesaMA
plot(predict_despesaMA)
rMA <- predict_despesaMA$residuals
ggAcf(rMA)
ggPacf(rMA)

predict_despesaARIMA <- forecast(arima_treino, h = 58)
summary(predict_despesaARIMA)
predict_despesaARIMA
plot(predict_despesaARIMA)

rARIMA <- predict_despesaARIMA$residuals
ggAcf(rARIMA)
ggPacf(rARIMA)

predict_despesaAUTOARIMA <- forecast(autoarima_treino, h = 58)
summary(predict_despesaAUTOARIMA)
predict_despesaAUTOARIMA
plot(predict_despesaAUTOARIMA)
rAUTOARIMA <- predict_despesaAUTOARIMA$residuals
ggAcf(rAUTOARIMA)
ggPacf(rAUTOARIMA)
View(predict_despesaAUTOARIMA$lower)
View(predict_despesaAUTOARIMA$upper)

predict_Htrend <- forecast(Htrend_treino, h = 58)
summary(predict_Htrend)
plot(predict_Htrend)
rHtrend <- predict_Htrend$residuals
ggAcf(predict_Htrend)
ggPacf(predict_Htrend)
View(predict_Htrend$lower)
View(predict_Htrend$upper)

#Variação
auto.arima_lower80 <- predict_despesaAUTOARIMA$lower[,1]
auto.arima_lower95 <- predict_despesaAUTOARIMA$lower[,2]

Htrend_lower80 <- predict_Htrend$lower[,1]
Htrend_lower95 <- predict_Htrend$lower[,2]

#Dados completos para previsão 2019
    #Estimação dos modelos com dados completos

predictor_despesas <- prev$ï..GASTO
class(predictor_despesas)

ma_despesa2019 <- ma(predictor_despesas, order = 11)
summary(ma_despesa2019)

arima_despesa2019 <- arima(predictor_despesas, order = c(0,0,0))
summary(arima_despesa2019)

autoarima_despesa2019 <- auto.arima(predictor_despesas)
summary(autoarima_despesa2019)

Htrend_despesa2019 <- holt(predictor_despesas, h = 11)
summary(Htrend_despesa2019)

#Predict 2019
predictMA <- forecast(ma_despesa2019, h = 11)
summary(predictMA)
plot(predictMA, main = "")

predictARIMA <- forecast(arima_despesa2019, h = 11)
summary(predictARIMA)
plot(predictARIMA, main = "")

predictAUTOARIMA <- forecast(autoarima_despesa2019, h = 11)
summary(predictAUTOARIMA)
plot(predictAUTOARIMA, main = "")
plot(predictAUTOARIMA$residuals)
View(predictAUTOARIMA$upper)

Htrend_1 <- holt(Htrend_despesa2019, h = 11)
summary(Htrend_despesa2019)
plot(Htrend_despesa2019, main = "")

#################
?auto.arima
