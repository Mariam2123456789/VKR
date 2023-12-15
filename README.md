# VKR
library(AER)
library(foreign)
library(sandwich)
library(lmtest)
library(gplots)
library(knitr)
library(texreg)
library(tidyverse)
library(plm)
library(ggplot2) 
library(car) 
library(dplyr)
library(corrplot)
library(readxl)
library(mice)
library(VIM)
library(psych)
library(reshape)
library(MASS)
library(Ecdat)
library(factoextra)
library(missMDA)
library(FactoMineR)
library(tidyr)
library(sandwich)
library(leaps)
library(glmnet)
library(caret)
library(pscl)
library(InformationValue)
library(openxlsx)
library(texreg)
### РАЗБОР ПРОПУЩЕННЫХ ПЕРЕМЕННЫХ
Data <- read_excel("/Users/mariammaloan/Desktop/данные.xlsx")
View(Data)
Data0 <- pdata.frame(Data, index = c("Country", "Year"), row.names = TRUE)
View(Data0)
colnames(Data0)
Data1 <- dplyr::select(Data0, - Country, - Year)
View(Data1)
corrplot(cor(Data1), method="circle", col = NULL) 
#для корректировки размера рисунка
par(mar = c(1,1,1,1))
#количество рисунков в экране
par(mfrow=c(1,1))

#строки без пропущенных значений
Data1[complete.cases(Data1),]

#строки с пропущенными значениями
Data1[!complete.cases(Data1),]
sum(is.na(Data1$GDP))

#таблица и рисунки всякие с пропущенными переменными
aggr(Data1, prop=FALSE, numbers=TRUE)

#Удаление всех строк с пропущенными данными
newdata <- na.omit(Data1)
View(newdata)
#построим корреляцию и модель с данными без пропусков (newdata) (24 наблюдения)
options(digits=1)
cor(na.omit(newdata))
corrplot(cor(na.omit(newdata)), method="circle", col = NULL)
mod1 <- lm(Life_exp ~ GDP + Mortality_infant + Health_expenditure_per_capita + Urban_pop + Prevalence_HIV, data = newdata)
summary(mod1)

#попробуем попарное удаление, хотя говорят, что это плохой подход
cor(Data1, use="pairwise.complete.obs")
corrplot(cor(Data1, use="pairwise.complete.obs"), method="circle", col = NULL)

#Простое (нестохастическое) восстановление данных я использовать не буду, тк для наших данных оно не подойдет

#попробуем удалить переменные, которые не коррелируют с продолжительностью жизни и у которых много пропусков
Data2 <- dplyr::select(Data1, -Alcohol, -Investment, -School_female, -School_male, -Immunization_tetanus, -Air_pollution, - Food, -CO2)
aggr(Data2, prop=FALSE, numbers=TRUE)
#Удаление всех строк с пропущенными данными
newdata2 <- na.omit(Data2)
View(newdata2)

#построим корреляцию и модель с данными без пропусков (newdata2) (833 наблюдений)
cor(na.omit(newdata2))
corrplot(cor(na.omit(newdata2)), method="circle", col = NULL)
mod2 <- lm(Life_exp ~ ., data = newdata2)
summary(mod2)
crPlots(mod2) 
plot(mod2, which = 3)
vif(mod2)
resettest(mod2)
bptest(mod2)
write.xlsx(mod2, file = "/Users/mariammaloan/Desktop/модель2.xlsx")
#попробуем удалить выбросы
par(mar=c(5.1 ,4.1 ,4.1 ,2.1))
Cook <- cooks.distance(mod2)
plot(Cook, type = "h")
influencePlot(mod2)
N <- which(Cook > 0.01)
Data3 <- newdata2[-N,]
View(Data3)
# осталось 816 наблюдений
summary(Data3)
describe(Data3)
write.xlsx(describe(Data3), "/Users/mariammaloan/Desktop/describe.xlsx")
#построим модель
mod3 <- lm(Life_exp ~ ., data = Data3)
summary(mod3)
crPlots(mod3) 
plot(mod3, which = 3)

mod4 <- update(mod3, .~. -Immunization_HepB3 -Immunization_DPT - Urban_pop -Immunization_measles)
summary(mod4)
crPlots(mod4) 
plot(mod4, which =3)
boxCox(mod4)
vif(mod4)
bptest(mod4)
# есть м/к в Health_expenditure_per_capita, есть г/к

V <- vcovHC(mod4)
coeftest(mod4, vcov. = V)

mod5 <- update(mod4, .~. -Health_expenditure_per_capita)
summary(mod5)
crPlots(mod5) 
plot(mod5, which = 3)
boxCox(mod5)
vif(mod5)
bptest(mod5)
#м/к г/к нет
#попробуем степени
mod6 <- update(mod5, .~. +I(Health_expenditure_from_GDP^2) - GDP_growth - Exports)
summary(mod6)
crPlots(mod6) 
resettest(mod6)
AIC(mod6, mod5)
vif(mod6)
bptest(mod6)
#mod6 лучше
write.xlsx(mod6, file = "/Users/mariammaloan/Desktop/модель6.xlsx")
mod7 <- lm(Life_exp ~ Fertility_rate, data = newdata2)
summary(mod7)


m.pooled <- plm(Life_exp~GDP+Inflation+Mortality_infant+Agriculture+Prevalence_HIV+Tuberculosis
                  +Health_expenditure_from_GDP + Fertility_rate + Exports + I(Health_expenditure_from_GDP^2),  data = Data3, model = "pooling")
m.re <- plm(Life_exp~GDP+Inflation+Mortality_infant+Agriculture+Prevalence_HIV+Tuberculosis
            +Health_expenditure_from_GDP + Fertility_rate + Exports + I(Health_expenditure_from_GDP^2),  data = Data3, model = "random")
m.fe <- plm(Life_exp~GDP+Inflation+Mortality_infant+Agriculture+Prevalence_HIV+Tuberculosis
            +Health_expenditure_from_GDP + Fertility_rate + Exports + I(Health_expenditure_from_GDP^2),  data = Data3, model = "within")
summary(m.pooled)
summary(m.re)
summary(m.fe)
pFtest(m.fe, m.pooled) 
#Выбираем FE
phtest(m.fe, m.re)
#Выбираем FE
plmtest(m.re, type = "bp")
#Выбираем RE
#модель с фикс эффектами
summary(m.fe)
htmlreg(list(m.pooled, m.re, m.fe), custom.model.names = c("Pooling", "RE", "FE"))
htmlreg(list(m.pooled, m.re, m.fe), file = "htmlreg_table.xlsx", single.row = 1)
stargazer(m.pooled, m.re, m.fe, out = "stargazer_table.html", single.row = TRUE)


###Модель с данными за 2015 год
#чистим данные
Data5 <- (Data, Year == 2015)
View(Data5)
Data6 <- dplyr::select(Data5,-Year, -Alcohol)
View(Data6)
aggr(Data6, prop=FALSE, numbers=TRUE)
corrplot(cor(na.omit(Data6[,-1])), method="circle", col = NULL)
colnames(Data6)
Data7 <- dplyr::select(Data6, Country, Life_exp, GDP, Inflation, Agriculture, Prevalence_HIV, Tuberculosis, Health_expenditure_from_GDP, Exports, Fertility_rate, Mortality_infant)
View(Data7)
aggr(Data7, prop=FALSE, numbers=TRUE)
corrplot(cor(na.omit(Data7)), method="circle", col = NULL)
Data8 <- na.omit(Data7)
View(Data8)
#выборка из 124 наблюдений

#строим ту же модель, что мы строили по всем данным, но только за 2015 год, так как R^2 подозрительно высокий
mod10 <- lm(Life_exp ~ ., data = Data8[,-1])
summary(mod10)
crPlots(mod10, wich=2)
boxCox(mod10)
mod11 <- lm(Life_exp~.+I(Health_expenditure_from_GDP^2) - Exports ,data = Data8[,-1])
summary(mod11)
crPlots(mod11, wich=2)
boxCox(mod11)
resettest(mod11)
vif(mod11)
bptest(mod11)
ggplot(Data3, aes(Fertility_rate,Life_exp))+geom_point()
write.xlsx(mod11, file = "/Users/mariammaloan/Desktop/модель11.xlsx")
#картинка показывает какой R^2 дают разные наборы переменных  

leaps <-regsubsets(Life_exp ~ ., data=Data8[,-1], nbest=4)
plot(leaps, scale="adjr2", col = rainbow(50))

###кластеризация для 2015 года
data_scale <- scale(Data8[,-1])
data_scale
data_dist <- dist(data_scale)
data_dist
clastering <- hclust(data_dist)
plot(clastering, ann = FALSE, labels = FALSE, hang = -1)
#разделим на 2 кластера
rect.hclust(clastering, k=2)
cluster <- cutree(clastering, k=2)
Data8$cluster <- cluster
Data_group <- group_by(Data8, cluster)
Data_group_mean <- summarise_all(Data_group, mean)
View(Data_group_mean)
data_chart <- as.data.frame(Data8)
ggplot(data_chart, aes(Life_exp)) + geom_density(fill = "#258054", alpha = 0.5) + facet_grid(~ cluster)
View(Data8)
#метод главных компонент
Data9 <- dplyr::select(Data8, - cluster)
Data9.1 <- data.frame(Data9)
View(Data9.1)
Data9.2 <- dplyr::select(Data9.1, - Country)
row.names(Data9.2) <- Data9$Country
row.names(Data9.2)[62] <- "Latin America & Caribbean*"
row.names(Data9.2)[75] <- "Middle East & North Africa*"
row.names(Data9.2)[106] <- "Sub-Saharan Africa*"

mod12 <- PCA(Data9.2, graph = FALSE)
fviz_pca_biplot(mod12)
fviz_eig(mod12, addlabels = TRUE)
corrplot(mod12$var$coord, is.corr =FALSE)
corrplot(mod12$var$cos2, is.corr = FALSE)
fviz_pca_var(mod12, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

Data_var <- as.data.frame(mod12$var$coord)
Data_var
mod_pa <- HCPC(mod12, graph = FALSE)
fviz_cluster(mod_pa, palette = "jco", repel = TRUE)
fviz_dend(mod_pa, cex = 0.7, palette = "jco", rect = TRUE, rect_fill = TRUE, rect_border = "jco", labels_track_height = 0.8)
View(mod_pa)
#как вывести средние по кластерам???
library(openxlsx)

mod_pa$desc.var
Data_clust1 <- filter(mod_pa$data.clust, clust == 1)
Data_clust2 <- filter(mod_pa$data.clust, clust == 2)
Data_clust3 <- filter(mod_pa$data.clust, clust == 3)
Data_clust4 <- filter(mod_pa$data.clust, clust == 4)
Data_cluster <- mod_pa$data.clust %>% group_by(clust) %>% summarise_all(mean)
View(Data_cluster)
write.xlsx(Data_cluster, "/Users/mariammaloan/Desktop/кластер.xlsx")
### попробуем через логистическую регрессию
View(Data9)
Data10 <- data.frame(Data9)
mean(Data10$Life_exp)
row.names(Data10) <- Data10$Country
Data10$Life_exp <- ifelse(Data10$Life_exp < 70, 0, 1)
View(Data10)
Data10 <- dplyr::select(Data10, - Country)
table(Data10$Life_exp)
mod20 <- glm(Life_exp ~ ., data = Data10, family = binomial, maxit = 500)
summary(mod20)
exp(coef(mod20))
s <- optimalCutoff(Data10$Life_exp, fitted(mod20))
hitmiss(mod20, k = s)
plotROC(Data10$Life_exp, fitted(mod20))
table(Data10$Life_exp)
#Мы получили 100% прогноз продолжительности жизни
##########
#рассмотрим по отдельности каждую переменную
regsubsets.out <- regsubsets(Life_exp ~ ., data = Data10, nbest = 1, force.out = NULL, method = "exhaustive")
plot(regsubsets.out, scale = "adjr2", main = "Adjusted R^2")

mod21 <- glm(Life_exp ~ Fertility_rate, data = Data10, family = binomial, maxit = 500)
summary(mod21)
exp(coef(mod21))
s1 <- optimalCutoff(Data10$Life_exp, fitted(mod21))
hitmiss(mod21, k = s1)
plotROC(Data10$Life_exp, fitted(mod21))
#зная только Fertility_rate, мы уже можем сделать на 91% точный прогноз продолжительности жизни

mod22 <- glm(Life_exp ~ Tuberculosis, data = Data10, family = binomial, maxit = 500)
s2 <- optimalCutoff(Data10$Life_exp, fitted(mod21))
hitmiss(mod22, k = s2)
plotROC(Data10$Life_exp, fitted(mod22))
#зная только заболеваемость туберкулезом, мы можем сделать на 78% точный прогноз

mod23 <- glm(Life_exp ~ Mortality_infant, data = Data10, family = binomial, maxit = 500)
s3 <- optimalCutoff(Data10$Life_exp, fitted(mod23))
hitmiss(mod23, k = s3)
plotROC(Data10$Life_exp, fitted(mod23))
#зная только младенческую смертность, мы можем сделать на 96% точный прогноз

mod24 <- glm(Life_exp ~ Prevalence_HIV, data = Data10, family = binomial, maxit = 500)
s4 <- optimalCutoff(Data10$Life_exp, fitted(mod24))
hitmiss(mod24, k = s4)
plotROC(Data10$Life_exp, fitted(mod24))
#86%

mod25 <- glm(Life_exp ~ Agriculture, data = Data10, family = binomial, maxit = 500)
s5 <- optimalCutoff(Data10$Life_exp, fitted(mod25))
hitmiss(mod25, k = s5)
plotROC(Data10$Life_exp, fitted(mod25))
#86%
mod26 <- glm(Life_exp ~ Health_expenditure_from_GDP, data = Data10, family = binomial, maxit = 500)
s6 <- optimalCutoff(Data10$Life_exp, fitted(mod26))
hitmiss(mod26, k = s6)
plotROC(Data10$Life_exp, fitted(mod26))
#71%, так себе, можем убрать
mod27 <- glm(Life_exp ~ GDP, data = Data10, family = binomial, maxit = 500)
s7 <- optimalCutoff(Data10$Life_exp, fitted(mod27))
hitmiss(mod27, k = s7)
plotROC(Data10$Life_exp, fitted(mod27))
#89%
mod28 <- glm(Life_exp ~ Inflation, data = Data10, family = binomial, maxit = 500)
s8 <- optimalCutoff(Data10$Life_exp, fitted(mod28))
hitmiss(mod28, k = s8)
plotROC(Data10$Life_exp, fitted(mod28))
#66%, так себе, можем убрать
mod29 <- glm(Life_exp ~ Exports, data = Data10, family = binomial, maxit = 500)
s9 <- optimalCutoff(Data10$Life_exp, fitted(mod29))
hitmiss(mod29, k = s9)
plotROC(Data10$Life_exp, fitted(mod29))
#68%, так себе, можем убрать
#прогнозы склонны завышать ожидаемую продолжительность жизни 
mod30 <- glm(Life_exp ~ .-Exports -Inflation - Health_expenditure_from_GDP-Tuberculosis, data = Data10, family = binomial, maxit = 500)
s10 <- optimalCutoff(Data10$Life_exp, fitted(mod30))
hitmiss(mod30, k = s10)
plotROC(Data10$Life_exp, fitted(mod30))
summary(mod30)
#######ЛАССО#######
#лассо регрессия (Data11 - это только регрессоры)
Data11 <- dplyr::select(Data9, - Life_exp, - Country)
model <- glmnet(Data11, Data9$Life_exp, alpha = 1)
summary(model)
View(Data11)
plot(model, label = TRUE, xvar="lambda")
coef(model)
model
data12 <- as.matrix(Data11)
model1 <- cv.glmnet(data12, Data9$Life_exp, alpha = 1)
#подберем лямбду
model1$lambda.min
model1$lambda.1se
model3 <- glmnet(Data11, Data8$Life_exp, alpha = 1, lambda = model1$lambda.min)
coef(model3)

#подберем альфу
TR <- trainControl(method = "repeatedcv", repeats = 5)
model4 <- train(Life_exp~., method = "glmnet", trControl = TR, data=Data9)
model4
model4$bestTune$lambda
#построим модель с подобраннами альфой и лямбдой
model5 <- glmnet(Data11, Data8$Life_exp, alpha = 1, lambda = 1)
coef(model5)
