###############################################
# ЗАНЯТИЕ 3

#Путь к файлам с данными
setwd("E:/ML 2025/DataSets/DataSets")

#Читаем файл данных 
data1<-read.csv("DataSet1_3.csv", header =T, sep = ",") # Курс акций Google
data2<-read.csv("DataSet1_4.csv", header =T, sep = ",") # Заболеваемость COVID-19 по странам

#Читаем файл данных 
data3<-read.csv("DataSet3_1.csv", header =T, sep = ",") # Выработка электроэнергии
data4<-read.csv("DataSet3_2.csv", header =T, sep = ";") # Курс валюты и цена барреля нефти

n0<-1
n1<-101
n2<-300
data<-data.frame(x=as.numeric(as.POSIXct(data1$Date[n1:n2], format="%Y-%m-%d"))-as.numeric(as.POSIXct(data1$Date[n1], format="%Y-%m-%d"))+1, y=data1$Close[n1:n2]) 
# Время приведено к первому периоду (=1 для первого наблюдения) 

t<-round((n2-n1+1)*0.8)
data_train<-data[1:t,] # обучающая подвыборка; строим ("обучаем") модель по ней
data_test<-data[(t+1):(n2-n1+1),] # Тестовая подвыборка; проверяем модель по ней 

##################################
# -=[ МНК (Метод Наименьших Квадратов) / OLS (Ordinary Least Squares) ]=-

# Подбираем коэффициенты регрессионной модели

# Вообще в рамках эконометрики много возможностей для оценки регрессий в R, например: 
lm1 <- lm(y~x+I(x^2), data=data_train) # спец. команды и доп. пакеты, lm() - самая базовая команда
summary(lm1) # Результаты оценки модели + ключевые характеристики модели

# Используем нелинейный метод наименьших квадратов (НЛМНК)...
res_mod1<-nls(y~a+b*x+c*x^2, data=data_train, start=list(a=0.01, b=0.01, c=0.01)) # В НЛМНК оценка параметров находится численными методами (сеткой)
summary(res_mod1) # Оценка; тут в отличие от lm() напрямую нет качества модели, но нам оно пока не важно
c<-coef(res_mod1) # Получаем значения коэффициентов

#Строим регрессионную функцию для тестового участка
x1<-data$x[1:t]
y1<-Cf[1]+Cf[2]*x1+Cf[3]*x1^2

#Строим регрессионную функцию для прогнозного участка
x2<-data$x[(t+1):(n2-n1+1)]
y2<-Cf[1]+Cf[2]*x2+Cf[3]*x2^2

#Графики
plot(data$x, data$y, type= "l", col="black", panel.first=grid(),  xlab = "Время", ylab = "Объем купли/продаж", ylim=c(0.95*min(y1, y2), 1.05*max(y1, y2)) ) #type=b
lines(data$x, data$y, pch=20, type="p", cex = 1.5)
lines(data_train$x, y1, type= "l", col="blue", lwd = 2.0)
lines(data_test$x, y2, type= "l", col="red", lwd= 2.0)

##################################
# -=[ kNN (k-nearest neighbors algorithm) - метод ближайших соседей ]=-

library(caret)

mod2 <- train(y ~ x, data_train, method = "knn")
mod2
res_mod2 <- predict(mod2, data_test)

# Графики
plot(data$x, data$y, type= "l", col="black", panel.first=grid(),  xlab = "Время", ylab = "Объем купли/продаж") #type=b
lines(data$x, data$y, pch=20, type="p", cex = 1.5)
lines(data_test$x, res_mod2, type= "l", col="red", lwd = 2.0)

res_mod2a <- predict(mod2, data_train)
lines(data_train$x, res_mod2a, type= "l", col="blue", lwd = 2.0)

##################################
# -=[ ARIMA ]=-

library(forecast)

mod3<-auto.arima(data_train$y) # Подбор "наилучшей" формы модели
res_mod3<-forecast(mod3, h=(n2-n1-t+1)) # Прогноз на длину тестовой выборки
summary(res_mod3)

# Графики
plot(res_mod3, lwd = 2.0) # Прогноз с доверит. интервалом (готовый график)

plot(data$x, data$y, type= "l", col="dark blue", panel.first=grid(), lwd = 2.0, xlab = "Время", ylab = "Цена закрытия", ylim=c(180,275)) # Факт
lines(data$x, data$y, pch=20, type="p", col="dark blue", cex = 1.5) # Факт
lines(data_train$x, mod3$fitted, type= "l", col="dark red", lwd = 2.0) #  + подогнанное значение
lines(data_test$x,res_mod3$mean, type= "l", lty = "dotted", col="black", lwd = 3.0) # Прогноз
# Хорошо бы ещё наложить области с доверит. интервалами...

##################################
# -=[ SVM (Support Vector Machine) / Метод опорных векторов ]=-

mod4 <- train(y ~ x, data_train, method = "svmLinear")
res_mod4 <- predict(mod4, data_test)

#Графики
plot(data$x, data$y, type= "l", col="black", panel.first=grid(), lwd = 2.0,  xlab = "Время", ylab = "Цена закрытия", ylim=c(min(data$y, res_mod4), max(data$y, res_mod4)) )
lines(data$x, data$y, pch=20, type="p", cex = 1.5)
lines(data_test$x, res_mod4, type= "l", col="red", lwd = 2.0)

res_mod4a <- predict(mod4, data_train)
lines(data_train$x, res_mod4a, type= "l", col="blue", lwd = 2.0)