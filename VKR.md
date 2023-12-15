#### ДАННЫЕ ####
data_norm <- read_excel("Desktop/data_norm.xlsx")
colnames(data_norm)
panel <- pdata.frame(data_norm, index = c("Country", "Year"), row.names = TRUE)
aggr(panel, prop=FALSE, numbers=TRUE)
panel1 <- dplyr::select(data_norm, Country, Year, Population, GDP, CO2, LE,health_expenditure_per_capita, Population, Inflation_GDP_deflator, GDP_per_capita)
data20 <- filter(panel1, Year >= 2000, Year <= 2020)
aggr(data20, prop=FALSE, numbers=TRUE)
colnames(data20)
a <- function(x){
  sum(is.na(x))
}
data21 <- group_by(data20, Country) %>% summarise_all(a)
b <- data21 %>% filter(GDP >5 | LE> 5 | health_expenditure_per_capita> 5 | CO2 >5 | Population >5)
data22 <- filter(data20, !Country %in% b$Country)
data23 <- pdata.frame(data22, index = c("Country", "Year"), row.names = TRUE)
lehfjw <- data.frame(cbind(log(data23$Population), log(data23$GDP), log(data23$CO2), log(data23$LE), log(data23$health_expenditure_per_capita), log(data23$GDP_per_capita)))
colnames(lehfjw) <-  c("Population", "GDP", "CO2", "LE", "health_expenditure_per_capita", "GDP_per_capita")
corrplot(cor(lehfjw, use="pairwise.complete.obs"), method="circle", col = NULL)
corrplot(cor(data23[, -c(1, 2,8)], use="pairwise.complete.obs"), method="circle", col = NULL)
panel2 <- na.omit(panel1)
panel3 <- pdata.frame(panel2, index = c("Country", "Year"), row.names = TRUE)
tab <- table(panel3$Country)
tab2 <- names(tab)[tab==20]
pan <- panel3 %>% filter(Country %in% tab2)
table(pan$Country)
panel3 <- pan

data40 <- filter(panel1, Year >= 1980, Year <= 2020)
data41 <- group_by(data40, Country) %>% summarise_all(a)
b <- data41 %>% filter(GDP >5 | LE> 5 | GDP_per_capita > 5 | Population >5)
data42 <- filter(data40, !Country %in% b$Country)
data42 <- filter(data42, Country !=  2, Country != 211,Country != 129, Country != 118, Country != 164, Country != 94, Country != 67, Country != 132)
data43 <- dplyr::select(data42,Country, Year, GDP, LE, GDP_per_capita, Population)
#### МОДЕЛИ ПАНЕЛЬНЫХ ДАННЫХ ####

#тесты на стационарность 
adf1 <- adf.test(panel3$GDP)
adf2 <- adf.test(panel3$Population)
adf3 <- adf.test(panel3$health_expenditure_per_capita)
adf4 <- adf.test(panel3$LE)
adf1 <- data.frame(adf1$type1)
adf2 <- data.frame(adf2$type1)
adf3 <- data.frame(adf3$type1)
adf4 <- data.frame(adf4$type1)
jnlkmkm <- cbind(adf1, adf2$ADF, adf2$p.value, adf3$ADF, adf3$p.value, adf4$ADF, adf4$p.value)
colnames(jnlkmkm) <- c("lag", "ADF_GDP", "p.value1", "ADF_POP", "p.value2", "ADF_health_exp", "p.value3", "ADF_LE", "p.value4")
round(jnlkmkm, 2) %>% gt()
#первая разность для переменных
LE_ts <- ts(panel3$LE, start = 1980, end = 2020)
delta_LE_ts <- diff(LE_ts, difference = 1)
diff(panel3$LE, 1)
View(delta_LE_ts)
#ardl модели 
ardl1 <- ardlDlm(LE ~ lag(LE) + log(delta_POP) + log(delta_GDP) + delta_CO2 + delta_Health_exp + delta_inflation + delta_GDP_per_cap, data = panel3)
summary(ardl1)

ardl2 <- ardlDlm(health_expenditure_per_capita ~  log(delta_GDP) + log(CO2), data = panel3)
summary(ardl2)

ardl3 <- ardlDlm(health_expenditure_per_capita ~  log(GDP) + log(abs(CO2)) + log(Population), data = panel3)
summary(ardl3)
# модели пула, случ и фикс эффектов
m.pooled <- plm(delta_LE ~ log_delta_CO2 + log_delta_GDP + log_delta_POP + log_delta_Health_exp, data = panel3, model = "pooling")
summary(m.pooled)
m.re <- plm(delta_LE ~ log_delta_CO2 + log_delta_GDP + log_delta_POP + log_delta_Health_exp, data = panel3, model = "random")
summary(m.re)
m.fe <- plm(delta_LE ~ log_delta_CO2 + log_delta_GDP + log_delta_POP + log_delta_Health_exp, data = panel3, model = "within")
summary(m.fe)
stargazer(m.re, m.fe, m.pooled, title="Regression Results", align=TRUE, dep.var.labels=c("LE"), column.labels=c("m.re", "m.fe", "m.pooled"),  no.space=TRUE, type = "text", digits = 2)

#Проведем тесты и выберем лучшую
pFtest(m.fe, m.pooled) 
#Выбираем FE
phtest(m.fe, m.re)
#Выбираем FE
plmtest(m.re, type = "bp")

Population1 <- diff(Population)
mod1<-lm(LE1 ~ lag(LE1) + lag(LE1, 2) + lag(LE1, 3) + lag(GDP1) + lag(GDP1,2) + lag(GDP1,3)+ lag(Population1) + lag(Population1,2) + lag(Population1,3))
summary(mod1)

ardl22 <- ardlDlm(delta_LE ~  delta_GDP + delta_CO2, data = panel3)
summary(ardl22)

ardl32 <- ardlDlm(health_expenditure_per_capita ~  log(GDP) + log(abs(CO2)) + log(Population), data = panel3)
summary(ardl32)

###МАЛАЙЗИЯ удалить###
{
  Malaysia <- dplyr::select(data_norm, Country, Year, GDP_per_capita, CO2, Fertility_rate, health_expenditure_per_capita, MR, LE)
  Malaysia1 <- filter(Malaysia, Malaysia$Country == 121)
  Malaysia1$ln_HE <- log(Malaysia1$health_expenditure_per_capita)
  View(Malaysia1)
  mal <- na.omit(Malaysia1)
  View(mal)
  mal <- pdata.frame(mal, index = c("Country", "Year"), row.names = TRUE)
  ardl30 <- ardlDlm(ln_HE ~  log(GDP_per_capita) + log(CO2) + log(Fertility_rate) + log(MR), data = mal, p = 1, q = 1)
  summary(ardl30)
  ardl31 <- ardlDlm(LE ~  log(GDP_per_capita) + log(CO2) + log(Fertility_rate) + log(MR), data = mal, p = 1, q = 1)
  summary(ardl31)
  
  mal$deltaLE <- mal$LE - lag(mal$LE)
  mal$deltaGDP_per_capita <- mal$GDP_per_capita - lag(mal$GDP_per_capita)
  mal$deltaCO2 <- mal$CO2 - lag(mal$CO2)
  mal$deltaMR <- mal$MR - lag(mal$MR)
  mal$deltaFertility_rate <- mal$Fertility_rate - lag(mal$Fertility_rate)
  mal$deltahealth_expenditure_per_capita <- mal$health_expenditure_per_capita - lag(mal$health_expenditure_per_capita)
  
  ardl32 <- ardlDlm(deltahealth_expenditure_per_capita ~  deltaLE + deltaGDP_per_capita + deltaCO2 + deltaMR + deltaFertility_rate, data = mal)
  summary(ardl32)
  
  ardl33 <- ardlDlm(health_expenditure_per_capita ~  deltahealth_expenditure_per_capita + deltaLE + deltaGDP_per_capita + deltaCO2 + deltaMR + deltaFertility_rate, data = mal)
  summary(ardl33)
  
  mod3 <- lm(ln_HE ~  log(GDP_per_capita) + log(CO2) + log(Fertility_rate) + log(MR), data = na.omit(Malaysia1))
  summary(mod3)
  
  mod3 <- plm(LE ~  log(GDP_per_capita) + log(CO2) + log(Fertility_rate) + log(MR), data = mal, model = "within")
  summary(mod3)
  
  #модель коррекции ошибок на малайзии
  X <- cbind(Malaysia1$GDP_per_capita, Malaysia1$Fertility_rate, Malaysia1$MR, Malaysia1$CO2)
  Malaysia2 <- dplyr::select(Malaysia1, GDP_per_capita, Fertility_rate, MR, CO2, health_expenditure_per_capita)
  Malaysia2 <- na.omit(Malaysia2)
  mod_ecm1 <- ecm(Malaysia2$health_expenditure_per_capita, as.matrix(Malaysia2[,-5]))
}
#### МНОЖЕСТВЕННАЯ РЕГРЕССИЯ 2019 год ####
Data <- read_excel("/Users/mariammaloan/Desktop/данные.xlsx")
View(Data1)
Data <- filter(Data, Year == 2019)
Data0 <- pdata.frame(Data, index = c("Country", "Year"), row.names = TRUE)
Data1 <- dplyr::select(Data0, - Country, - Year)
corrplot(cor(Data1), method="circle", col = NULL) 

Data2 <- dplyr::select(Data1, -Alcohol,-GDP_growth,  -Investment, - Health_expenditure_per_capita, -School_female, -School_male, - Exports, - Inflation, -Immunization_tetanus, -Air_pollution, - Food, - CO2, - Immunization_HepB3, - Immunization_DPT, - Urban_pop, - Mortality_infant)
newdata2 <- na.omit(Data2)
View(newdata2)

corrplot(cor(na.omit(newdata2)), method="circle", col = NULL, addCoef.col = TRUE, pch.cex = 1)
mod1 <- lm(log(Life_exp) ~ log(GDP), data = newdata2)
summary(mod1)

mod2 <- lm(log(Life_exp) ~ log(GDP) + log(Health_expenditure_from_GDP), data = newdata2)
summary(mod2)

mod3 <- lm(log(Life_exp) ~ log(GDP) + log(Health_expenditure_from_GDP) + Agriculture, data = newdata2)
summary(mod3)

mod4 <- lm(log(Life_exp) ~ log(GDP) + log(Health_expenditure_from_GDP) + Agriculture  + log(Prevalence_HIV), data = newdata2)
summary(mod4)

mod5 <- lm(log(Life_exp) ~ log(GDP) + log(Health_expenditure_from_GDP) + Agriculture + log(Prevalence_HIV) + Tuberculosis, data = newdata2)
summary(mod5)

mod6 <- lm(log(Life_exp) ~ log(GDP) + log(Health_expenditure_from_GDP) + Agriculture + log(Prevalence_HIV) + Tuberculosis + log(Immunization_measles), data = newdata2)
summary(mod6)

crPlots(mod6)
plot(mod6, which = 3)
vif(mod6)
resettest(mod6)
bptest(mod6)
stargazer::stargazer(mod1, mod2, mod3, mod4, mod5, mod6, type = "html", out = "mytable.htm")

#### ГРУППЫ СТРАН ПО ПРОДОЛЖИТЕЛЬНОСТИ ЖИЗНИ ####
#3 группы стран
not_hight <- filter(filter(data43, Year == 2020), LE <= quantile(data43$LE, 0.75))
hight_LE <- filter(data43, !Country %in% not_hight$Country)
hight_LE$GDP <- log(hight_LE$GDP)
hight_LE$LE <- log(hight_LE$LE)
hight_LE$GDP_per_capita <- log(hight_LE$GDP_per_capita)
hight_LE$Population <- log(hight_LE$Population)
summary(low_LE$GDP)
summary(low_LE$LE)
summary(low_LE$Population)
summary(low_LE$GDP_per_capita)
stargazer::stargazer(summary(hight_LE))
not_low <- filter(filter(data43, Year == 2020), LE >= quantile(data43$LE, 0.25))
low_LE <- filter(data43, !Country %in% not_low$Country)
low_LE$GDP <- log(low_LE$GDP)
low_LE$LE <- log(low_LE$LE)
low_LE$GDP_per_capita <- log(low_LE$GDP_per_capita)
low_LE$Population <- log(low_LE$Population)

middle_LE <- filter(data43, !Country %in% hight_LE$Country, !Country %in% low_LE$Country)
middle_LE$GDP <- log(middle_LE$GDP)
middle_LE$LE <- log(middle_LE$LE)
middle_LE$GDP_per_capita <- log(middle_LE$GDP_per_capita)
middle_LE$Population <- log(middle_LE$Population)

#### ECM by country: УДАЛИТЬ   log(GDP per capita) ~ log(LE) + log(POP) ####

#данные data43
data44 <- read_excel("Desktop/data43.xlsx")
View(data44)
country_list <- unique(data44$Country)
table_1 <- as.table(rep(1, times = 6), ncol = 6, byrow = true)
data44$GDP <- log(data44$GDP)
data44$Population <- log(data44$Population)
data44$GDP_per_capita <- log(data44$GDP_per_capita)
data44$LE <- log(data44$LE)
for (i in 1:138) {
  print(i)
  data45 <- filter(data44, Countnom == i)
  mod_ecm_by_country <- ecm(data45$GDP_per_capita, as.matrix(data45[, -c(1, 2, 3, 4, 6)]))
  table_1 <- rbind(table_1, c(mod_ecm_by_country$coefficients[,1], mod_ecm_by_country$coefficients[,4]))
}
table_1 <- table_1[-1,]
colnames(table_1) <- c("LE", "POP", "ECM", "pvalue1", "pvalue2", "pvalue3")
cbind(table_1, country_list) 
View(table_3)
table_2 <- as.data.frame(cbind(table_1, country_list))
table_3 <- table_2[c("country_list", "LE", "pvalue1","POP", "pvalue2","ECM", "pvalue3")]

ECM_gdp_per_cap <- mutate(table_3, pvalue1 = case_when(pvalue1 < 0.01 ~ 3, pvalue1 < 0.05 ~ 2, pvalue1 < 0.1 ~ 1, TRUE ~ 0),
                          pvalue2 = case_when(pvalue2 < 0.01 ~ 3, pvalue2 < 0.05 ~ 2, pvalue2 < 0.1 ~ 1, TRUE ~ 0), 
                          pvalue3 = case_when(pvalue3 < 0.01 ~ 3, pvalue3 < 0.05 ~ 2, pvalue3 < 0.1 ~ 1, TRUE ~ 0))
View(round(ECM_gdp_per_cap, 2))

data44 <- read_excel("Desktop/data44.xlsx")
ECM_gdp_per_cap1 <- merge(ECM_gdp_per_cap, unique(dplyr::select(data44,c_id, Country_name )), by.x = "country_list", by.y = "c_id", join ="right")
pval1 <- ifelse(ECM_gdp_per_cap1$pvalue1 == 3, "***", ifelse(ECM_gdp_per_cap1$pvalue1 == 2, "**", ifelse(ECM_gdp_per_cap1$pvalue1 == 1, "*", "")))
pval2 <- ifelse(ECM_gdp_per_cap1$pvalue2 == 3, "***", ifelse(ECM_gdp_per_cap1$pvalue2 == 2, "**", ifelse(ECM_gdp_per_cap1$pvalue2 == 1, "*", "")))
pval3 <- ifelse(ECM_gdp_per_cap1$pvalue3 == 3, "***", ifelse(ECM_gdp_per_cap1$pvalue3 == 2, "**", ifelse(ECM_gdp_per_cap1$pvalue3 == 1, "*", "")))
ECM_gdp_per_cap1 <- data.frame(cbind(ECM_gdp_per_cap1$Country_name, ECM_gdp_per_cap1$LE, pval1, ECM_gdp_per_cap1$POP, pval2, ECM_gdp_per_cap1$ECM, pval3))
colnames(ECM_gdp_per_cap1) <- c("Country", "LE", "pvalue","POP", "pvalue","ECM", "pvalue")
ECM_gdp_per_cap1 %>% gt() 
View(ECM_gdp_per_cap1)
write.xlsx(ECM_gdp_per_cap1, file = "/Users/mariammaloan/Desktop/модельecm.xlsx")
#### ECM by country:   log(GDP total) ~ log(LE) + log(POP) ####
#данные data43
data44 <- read_excel("Desktop/data43.xlsx")
country_list <- unique(data44$Country)
table_1 <- as.table(rep(1, times = 4), ncol = 4, byrow = true)
data44$GDP <- log(data44$GDP)
data44$GDP_per_capita <- log(data44$GDP_per_capita)
data44$LE <- log(data44$LE)
names(data44)
for (i in 1:138) {
  print(i)
  data45 <- filter(data44, Countnom == i)
  mod_ecm_by_country <- ecm(data45$GDP, as.matrix(data45[, -c(1, 2, 3, 4, 6, 7)]), nlags)
  table_1 <- rbind(table_1, c(mod_ecm_by_country$coefficients[,1], mod_ecm_by_country$coefficients[,4]))
}
table_1 <- table_1[-1,]
colnames(table_1) <- c("LE", "ECM", "pvalue1", "pvalue2")
cbind(table_1, country_list) 
View(table_3)
table_2 <- as.data.frame(cbind(table_1, country_list))
table_3 <- table_2[c("country_list", "LE", "pvalue1", "pvalue2","ECM")]

ECM_GDP <- mutate(table_3, pvalue1 = case_when(pvalue1 < 0.01 ~ 3, pvalue1 < 0.05 ~ 2, pvalue1 < 0.1 ~ 1, TRUE ~ 0),
                  pvalue2 = case_when(pvalue2 < 0.01 ~ 3, pvalue2 < 0.05 ~ 2, pvalue2 < 0.1 ~ 1, TRUE ~ 0))

data44 <- read_excel("Desktop/data44.xlsx")
ECM_GDP1 <- merge(ECM_GDP, unique(dplyr::select(data44,c_id, Country_name )), by.x = "country_list", by.y = "c_id", join ="left")
ECM_GDP$pvalue1 <- ifelse(ECM_GDP$pvalue1 == 3, "***", ifelse(ECM_GDP$pvalue1 == 2, "**", ifelse(ECM_GDP$pvalue1 == 1, "*", "")))
ECM_GDP$pvalue2 <- ifelse(ECM_GDP$pvalue2 == 3, "***", ifelse(ECM_GDP$pvalue2 == 2, "**", ifelse(ECM_GDP$pvalue2 == 1, "*", "")))
ECM_GDP1 <- data.frame(cbind(ECM_GDP1$Country_name, ECM_GDP1$LE, pval1, ECM_GDP1$ECM, pval2))
colnames(ECM_GDP1) <- c("Country", "LE", "pvalue","ECM", "pvalue")

write.xlsx(ECM_GDP, file = "/Users/mariammaloan/Desktop/модельecm_передел.xlsx")


#### ECM by country:   log(LE) ~ log(GDP total) + log(POP) ####

#данные data43
data44 <- read_excel("Desktop/data43.xlsx")
country_list <- unique(data44$Country)
table_1 <- as.table(rep(1, times = 4), ncol = 4, byrow = true)
data44$GDP <- log(data44$GDP)
data44$GDP_per_capita <- log(data44$GDP_per_capita)
data44$LE <- log(data44$LE)
for (i in 1:138) {
  print(i)
  data45 <- filter(data44, Countnom == i)
  mod_ecm_by_country <- ecm(data45$LE, as.matrix(data45[, -c(1, 2, 3, 5, 6, 7)]))
  table_1 <- rbind(table_1, c(mod_ecm_by_country$coefficients[,1], mod_ecm_by_country$coefficients[,4]))
}
table_1 <- table_1[-1,]
colnames(table_1) <- c("GDP", "ECM", "pvalue1", "pvalue2")
cbind(table_1, country_list) 
table_2 <- as.data.frame(cbind(table_1, country_list))
table_3 <- table_2[c("country_list", "GDP", "pvalue1","ECM", "pvalue2")]

ECM_LE1 <- mutate(table_3, pvalue1 = case_when(pvalue1 < 0.01 ~ 3, pvalue1 < 0.05 ~ 2, pvalue1 < 0.1 ~ 1, TRUE ~ 0),
                  pvalue2 = case_when(pvalue2 < 0.01 ~ 3, pvalue2 < 0.05 ~ 2, pvalue2 < 0.1 ~ 1, TRUE ~ 0))
data44 <- read_excel("Desktop/data44.xlsx")
ECM_LE11 <- merge(ECM_LE1, unique(dplyr::select(data44,c_id, Country_name )), by.x = "country_list", by.y = "c_id", join ="right")
ECM_LE1$pvalue1 <- ifelse(ECM_LE1$pvalue1 == 3, "***", ifelse(ECM_LE1$pvalue1 == 2, "**", ifelse(ECM_LE1$pvalue1 == 1, "*", "")))
ECM_LE1$pvalue2 <- ifelse(ECM_LE1$pvalue2 == 3, "***", ifelse(ECM_LE1$pvalue2 == 2, "**", ifelse(ECM_LE1$pvalue2 == 1, "*", "")))
ECM_LE11 <- data.frame(cbind(ECM_LE11$Country_name, ECM_LE11$GDP, pval1, ECM_LE11$ECM, pval2))
colnames(ECM_LE11) <- c("Country", "GDP", "pvalue","ECM", "pvalue")

write.xlsx(ECM_LE1, file = "/Users/mariammaloan/Desktop/модельecmпередел 2.xlsx")


#### ECM by country:   log(LE) ~ log(GDP per capita) + log(POP) ####

#данные data43
data44 <- read_excel("Desktop/data43.xlsx")
country_list <- unique(data44$Country)
table_1 <- as.table(rep(1, times = 6), ncol = 6, byrow = true)
data44$GDP <- log(data44$GDP)
data44$Population <- log(data44$Population)
data44$GDP_per_capita <- log(data44$GDP_per_capita)
data44$LE <- log(data44$LE)
for (i in 1:138) {
  print(i)
  data45 <- filter(data44, Countnom == i)
  mod_ecm_by_country <- ecm(data45$LE, as.matrix(data45[, -c(1, 2, 3, 4, 5)]))
  table_1 <- rbind(table_1, c(mod_ecm_by_country$coefficients[,1], mod_ecm_by_country$coefficients[,4]))
}
table_1 <- table_1[-1,]
colnames(table_1) <- c("GDP_per_capita", "POP", "ECM", "pvalue1", "pvalue2", "pvalue3")
cbind(table_1, country_list) 
table_2 <- as.data.frame(cbind(table_1, country_list))
table_3 <- table_2[c("country_list", "GDP_per_capita", "pvalue1","POP", "pvalue2","ECM", "pvalue3")]


ECM_LE_2 <- mutate(table_3, pvalue1 = case_when(pvalue1 < 0.01 ~ 3, pvalue1 < 0.05 ~ 2, pvalue1 < 0.1 ~ 1, TRUE ~ 0),
                   pvalue2 = case_when(pvalue2 < 0.01 ~ 3, pvalue2 < 0.05 ~ 2, pvalue2 < 0.1 ~ 1, TRUE ~ 0), 
                   pvalue3 = case_when(pvalue3 < 0.01 ~ 3, pvalue3 < 0.05 ~ 2, pvalue3 < 0.1 ~ 1, TRUE ~ 0))
View(ECM_LE_2)
ECM_LE11 <- merge(ECM_LE1, unique(dplyr::select(data44,c_id, Country_name )), by.x = "country_list", by.y = "c_id", join ="right")
pval1 <- ifelse(ECM_LE11$pvalue1 == 3, "***", ifelse(ECM_LE11$pvalue1 == 2, "**", ifelse(ECM_LE11$pvalue1 == 1, "*", "")))
pval2 <- ifelse(ECM_LE11$pvalue2 == 3, "***", ifelse(ECM_LE11$pvalue2 == 2, "**", ifelse(ECM_LE11$pvalue2 == 1, "*", "")))
pval3 <- ifelse(ECM_LE11$pvalue3 == 3, "***", ifelse(ECM_LE11$pvalue3 == 2, "**", ifelse(ECM_LE11$pvalue3 == 1, "*", "")))
ECM_LE11 <- data.frame(cbind(ECM_LE11$Country_name, ECM_LE11$GDP, pval1, ECM_LE11$POP, pval2, ECM_LE11$ECM, pval3))
colnames(ECM_LE11) <- c("Country", "GDP", "pvalue","POP", "pvalue","ECM", "pvalue")

write.xlsx(ECM_LE11, file = "/Users/mariammaloan/Desktop/модельecm.xlsx")


#### ГРАФИКИ ####

hight_LE
plot(hight_LE$LE, hight_LE$GDP, col = hight_LE$Country)
plot(middle_LE$LE, middle_LE$GDP, col = middle_LE$Country)
plot(low_LE$LE, low_LE$GDP, col = low_LE$Country)
plot(data44$LE, data44$GDP_per_capita, col = data44$Country)
hight_LE$LE <- log(hight_LE$LE)
#график для стран - долгожителей
ggplot(data = hight_LE, aes( x = LE, y = GDP)) + geom_point(aes(col = as.factor(Country)), show.legend = FALSE) + theme_bw()
#график для стран со средней ОПЖ (отсеили лишние точки)
k <- filter(middle_LE, LE > 3)
middle_LE$LE <- log(middle_LE$LE)
ggplot(data = k, aes( x = LE, y = GDP)) + geom_point(aes(col = as.factor(Country)), show.legend = FALSE) + theme_bw()
#график для стран с никзой ОПЖ
low_LE$LE <- log(low_LE$LE)
ggplot(data = low_LE, aes( x = LE, y = GDP)) + geom_point(aes(col = as.factor(Country)),  show.legend = FALSE) + theme_bw()

#график для стран - долгожителей
ggplot(data = hight_LE, aes( x = LE, y = GDP)) + geom_point(aes(col = as.factor(Country)))
#график для стран со средней ОПЖ (отсеили лишние точки)
ggplot(data = k, aes(x = LE, y = GDP)) + geom_point(aes(col = as.factor(Country)))
#график для стран со средней ОПЖ
ggplot(data = low_LE, aes(x = LE, y = GDP)) + geom_point(aes(col = as.factor(Country)))
GDP_ts <- ts(hight_LE$GDP, start = 1980, frequency = 1, end = 2020)
hight_LE_ts <- ts(hight_LE$LE, start = 1980, frequency = 1, end = 2020)

ggplot(hight_LE, aes(x = LE, y=GDP, size = Population, colour = Country)) +
  geom_point(show.legend = TRUE, alpha = 0.7) +
  scale_size() +
  scale_x_log10() +
  labs(title = 'Relation between GDP and Life Expectancy in hight LE group', subtitle = 'Year: {frame_time}', x = 'Life Expectancy', y = 'GDP', caption = 'Data Source: www.gapminder.org/data') +
  transition_time(as.numeric(hight_LE$Year)) +
  ease_aes('linear')

ggplot(k, aes(x = LE, y=GDP, size = Population, colour = Country)) +
  geom_point(show.legend = TRUE, alpha = 0.7) +
  scale_size() +
  scale_x_log10() +
  labs(title = 'Relation between GDP and Life Expectancy in middle LE group', subtitle = 'Year: {frame_time}', x = 'Life Expectancy', y = 'GDP', caption = 'Data Source: www.gapminder.org/data') +
  transition_time(as.numeric(hight_LE$Year)) +
  ease_aes('linear')

ggplot(low_LE, aes(x = LE, y=GDP, size = Population, colour = Country)) +
  geom_point(show.legend = TRUE, alpha = 0.7) +
  scale_size() +
  scale_x_log10() +
  labs(title = 'Relation between GDP and Life Expectancy in low LE group', subtitle = 'Year: {frame_time}', x = 'Life Expectancy', y = 'GDP', caption = 'Data Source: www.gapminder.org/data') +
  transition_time(as.numeric(hight_LE$Year)) +
  ease_aes('linear')

#### кластеризация для средние 2010 - 2019 года ####
Data <- read_excel("/Users/mariammaloan/Desktop/mean 2013-2020.xlsx")
View(Data)
Data7 <- data.frame(Data)
Data7 <- Data7[,-10]
Data8 <- na.omit(Data7)
colnames(Data8)
View(Data8)
row.names(Data8) <- Data8$Country
Data8 <- Data8[,-1]
#логарифмируем данные 
Data8$GDP <- log(Data8$GDP)
Data8$Life_exp <- log(Data8$Life_exp)
Data8$Tuberculosis <- log(Data8$Tuberculosis)
Data8$CO2 <- log(Data8$CO2)
Data8$Urban_pop <- log(Data8$Urban_pop)
Data8$Health_expenditure_from_GDP <- log(Data8$Health_expenditure_from_GDP)
Data8$Population <- log(Data8$Population)
Data8$School.prim <- log(Data8$School.prim)
Data8$GDP.per.capita <- log(Data8$GDP.per.capita)
data_scale <- scale(Data8)
data_dist <- dist(data_scale) 
clastering <- hclust(data_dist)
plot(clastering, ann = FALSE, labels = FALSE, hang = -1)

#обычная кластеризация k-means
rect.hclust(clastering, k=4)
cluster <- cutree(clastering, k=4)
Data8$cluster <- cluster
Data_group <- group_by(Data8, cluster)
Data_group_mean <- summarise_all(Data_group, mean)
View(Data_group_mean)
data_chart <- as.data.frame(Data8)
ggplot(data_chart, aes(Life_exp)) + geom_density(fill = "#258054", alpha = 0.5) + facet_grid(~ cluster)
View(Data8)

#### метод главных компонент ####
Data <- read_excel("/Users/mariammaloan/Desktop/mean 2013-2020.xlsx")
Data7 <- data.frame(Data)
rownames(Data7) <- Data7$Country
Data7 <- Data7[,-10]
Data8 <- na.omit(Data7)
#логарифмируем данные 
Data8$GDP <- log(Data8$GDP)
Data8$Life_exp <- log(Data8$Life_exp)
Data8$Tuberculosis <- log(Data8$Tuberculosis)
Data8$Agriculture <- log(Data8$Agriculture)
Data8$CO2 <- log(Data8$CO2)
Data8$Urban_pop <- log(Data8$Urban_pop)
Data8$Health_expenditure_from_GDP <- log(Data8$Health_expenditure_from_GDP)
Data9.1 <- data.frame(Data8)
Data9.2 <- dplyr::select(Data9.1, - Country)
mod12 <- PCA(Data9.2, graph = FALSE)
fviz_pca_biplot(mod12,addlabels = TRUE )
fviz_eig(mod12, addlabels = TRUE)
corrplot(mod12$var$coord, is.corr = FALSE)
corrplot(mod12$var$cos2, is.corr = FALSE)
fviz_pca_var(mod12, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
Data_var <- as.data.frame(mod12$var$coord)
Data_var
mod_pa <- HCPC(mod12, graph = FALSE, nb.clust = 4)
fviz_cluster(mod_pa, palette = "jco", repel = TRUE)
fviz_dend(mod_pa, cex = 0.7, palette = "jco", rect = TRUE, rect_fill = TRUE, rect_border = "jco", labels_track_height = 0.8)
mod_pa$desc.var
Data_clust1 <- dplyr::filter(mod_pa$data.clust, clust == 1)
Data_clust2 <- dplyr::filter(mod_pa$data.clust, clust == 2)
Data_clust3 <- dplyr::filter(mod_pa$data.clust, clust == 3)
Data_clust4 <- dplyr::filter(mod_pa$data.clust, clust == 4)
Data_cluster <- mod_pa$data.clust %>% group_by(clust) %>% summarise_all(mean) 
View(Data_cluster)
#таблица какая страна к какому кластеру относится 
View(mod_pa$data.clust$clust)
clasters_countries <- cbind(row.names(mod_pa$data.clust), mod_pa$data.clust$clust)
data.frame(clasters_countries)
write.xlsx(data.frame(clasters_countries), file = "/Users/mariammaloan/Desktop/clasters_countries.xlsx")
View(clasters_countries)
#### MG ####
data_panel <- pdata.frame(data44, index = c("Country_name", "Year"), row.names = TRUE)
View(data44)
data_panel$delta_LE <- diff(data_panel$LE)
data_panel$delta_GDP <- diff(data_panel$GDP)
data_panel$delta_Pop <- diff(data_panel$Population)
data_panel$delta_GDP_per_capita <- diff(data_panel$GDP_per_capita)
pmg <- pmg(delta_LE ~ data_panel$delta_GDP_per_capita + lag(delta_Pop) + delta_Pop +  lag(data_panel$delta_GDP_per_capita) + lag(delta_LE), data = data_panel, model = "mg")
summary(pmg)
vif(pmg)
pool_model <- plm(delta_LE ~ delta_GDP + delta_Pop + lag(delta_GDP) + lag(delta_Pop) + lag(delta_LE), data = data_panel, model = "pooling")
hausman_test <- phtest(pmg, pool_model)
cor(na.omit(delta_Pop),na.omit(delta_LE))
print(hausman_test)

#MG для долгожителей (ко всем показателям уже применен логарифм)
data_panel_hight_LE <- pdata.frame(hight_LE, index = c("Country", "Year"), row.names = TRUE)
data_panel_hight_LE$delta_LE <- diff(data_panel_hight_LE$LE)
data_panel_hight_LE$delta_GDP <- diff(data_panel_hight_LE$GDP)
data_panel_hight_LE$delta_Pop <- diff(data_panel_hight_LE$Population)
data_panel_hight_LE$delta_GDP_per_capita <- diff(data_panel_hight_LE$GDP_per_capita)
pmg1 <- pmg(delta_LE ~ GDP_per_capita + delta_Pop + lag(GDP_per_capita) + lag(delta_LE), data = data_panel_hight_LE, model = "mg")
summary(pmg1)
vif(pmg1)
#MG для middle (ко всем показателям уже применен логарифм)
data_panel_middle_LE <- pdata.frame(middle_LE, index = c("Country", "Year"), row.names = TRUE)
data_panel_middle_LE$delta_LE <- diff(data_panel_middle_LE$LE)
data_panel_middle_LE$delta_GDP <- diff(data_panel_middle_LE$GDP)
data_panel_middle_LE$delta_Pop <- diff(data_panel_middle_LE$Population)

pmg2 <- pmg(delta_LE ~ delta_GDP + delta_Pop + lag(delta_GDP)  + lag(delta_LE), data = data_panel_middle_LE, model = "mg")
summary(pmg2)
vif(pmg2)
pmg1$indcoef

#MG для low (ко всем показателям уже применен логарифм)
data_panel_low_LE <- pdata.frame(low_LE, index = c("Country", "Year"), row.names = TRUE)
data_panel_low_LE$delta_LE <- diff(data_panel_low_LE$LE)
data_panel_low_LE$delta_GDP <- diff(data_panel_low_LE$GDP)
data_panel_low_LE$delta_Pop <- diff(data_panel_low_LE$Population)
pmg3 <- pmg(delta_LE ~ delta_GDP + delta_Pop + lag(delta_GDP)  + lag(delta_LE), data = data_panel_low_LE, model = "mg")
summary(pmg3)
vif(pmg3)
pmg2$indcoef

stargazer(pmg, pmg1, pmg2, pmg3, title="PMG", align=TRUE, dep.var.labels=c("LE"), column.labels=c("ALL", "HIGH", "MIDDLE", "LOW"),  no.space=TRUE,  type = "html", out = "mytable.htm", digits = 2)


#### VECM все переменные в уравнении ниже прологарифмированы ####
# GDP ~ ECT + Intercept + GDP_ts-1 + Population_ts-1 + LE_ts-1 + GDP_ts-2 + Population_ts-2 + LE_ts-2 + GDP_ts-3 + Population_ts-3 + LE_ts-3 + GDP_ts-4 + Population_ts-4 + LE_ts-4

data44 <- read_excel("Desktop/data44.xlsx")
country_list <- unique(data44$Country)
table_1 <- as.table(rep(1, times = 20), ncol = 20, byrow = true)
con_with_coint <- integer(0)
con_with_coint <- NULL
for (i in 1:138){
  print(i)
  data45 <- filter(data44, Country_name ==
                     unique(data44$Country_name)[i])
  GDP_ts <- ts(data45$GDP, start = 1980, frequency = 1, end = 2020)
  Population_ts <- ts(data45$Population, start = 1980, frequency = 1, end = 2020)
  LE_ts <-  ts(data45$LE, start = 1980, frequency = 1, end = 2020)
  dset <- cbind(GDP_ts, LE_ts)
  vkgh <- coint.test(LE_ts, as.matrix(data.frame(GDP_ts)), d = 0, nlag = 4, output = TRUE)
  if (vkgh[1,3] < 0.1) {
    con_with_coint <- c(con_with_coint, i)} else {}
}
#страны с коинтеграцией
con_with_coint
data50 <- filter(data44, c_id %in% con_with_coint)
write.xlsx(data50, file = "/Users/mariammaloan/Desktop/data50.xlsx")
data50 <- read_excel("Desktop/data50.xlsx")
#строим векм
for (i in 1:45) {
  data51 <- filter(data50, c_id2 == i)
  GDP_ts <- ts(data51$GDP, start = 1980, frequency = 1, end = 2020)
  Population_ts <- ts(data51$Population, start = 1980, frequency = 1, end = 2020)
  LE_ts <-  ts(data51$LE, start = 1980, frequency = 1, end = 2020)
  dset <- cbind(GDP_ts, LE_ts)
  model1 <- VECM(dset, lag = 4)
  t <- summary(model1)
  table_1 <- rbind(table_1, c(t$coefficients[1,], t$Pvalues[1,]))
}
table_1 <- table_1[-1,]
View(table_2)
colnames(table_1) <- c("ECT", "Intercept", "GDP_ts-1", "LE_ts-1", "GDP_ts-2","LE_ts-2", "GDP_ts-3", "LE_ts-3", "GDP_ts-4", "LE_ts-4", "pvalue1", "pvalue2", "pvalue3", "pvalue4", "pvalue5", "pvalue6", "pvalue7", "pvalue8", "pvalue9", "pvalue10")
table_2 <- as.data.frame(cbind(table_1, country_list))
table_3 <- table_2[c("country_list", "ECT", "pvalue1", "Intercept", "pvalue2", "GDP_ts-1", "pvalue3",  "LE_ts-1", "pvalue4", "GDP_ts-2","pvalue5",  "LE_ts-2","pvalue6" ,  "GDP_ts-3", "pvalue7", "LE_ts-3", "pvalue8", "GDP_ts-4","pvalue9", "LE_ts-4", "pvalue10")]
View(table_2)
VECM_GDP <- mutate(table_3, pvalue1 = case_when(pvalue1 < 0.01 ~ 3, pvalue1 < 0.05 ~ 2, pvalue1 < 0.1 ~ 1, TRUE ~ 0),
                   pvalue2 = case_when(pvalue2 < 0.01 ~ 3, pvalue2 < 0.05 ~ 2, pvalue2 < 0.1 ~ 1, TRUE ~ 0),
                   pvalue3 = case_when(pvalue3 < 0.01 ~ 3, pvalue3 < 0.05 ~ 2, pvalue3 < 0.1 ~ 1, TRUE ~ 0), 
                   pvalue4 = case_when(pvalue4 < 0.01 ~ 3, pvalue4 < 0.05 ~ 2, pvalue4 < 0.1 ~ 1, TRUE ~ 0), 
                   pvalue5 = case_when(pvalue5 < 0.01 ~ 3, pvalue5 < 0.05 ~ 2, pvalue5 < 0.1 ~ 1, TRUE ~ 0), 
                   pvalue6 = case_when(pvalue6 < 0.01 ~ 3, pvalue6 < 0.05 ~ 2, pvalue6 < 0.1 ~ 1, TRUE ~ 0), 
                   pvalue7 = case_when(pvalue7 < 0.01 ~ 3, pvalue7 < 0.05 ~ 2, pvalue7 < 0.1 ~ 1, TRUE ~ 0), 
                   pvalue8 = case_when(pvalue8 < 0.01 ~ 3, pvalue8 < 0.05 ~ 2, pvalue8 < 0.1 ~ 1, TRUE ~ 0), 
                   pvalue9 = case_when(pvalue9 < 0.01 ~ 3, pvalue9 < 0.05 ~ 2, pvalue9 < 0.1 ~ 1, TRUE ~ 0), 
                   pvalue10 = case_when(pvalue10 < 0.01 ~ 3, pvalue10 < 0.05 ~ 2, pvalue10 < 0.1 ~ 1, TRUE ~ 0), 
                   pvalue11 = case_when(pvalue11 < 0.01 ~ 3, pvalue11 < 0.05 ~ 2, pvalue11 < 0.1 ~ 1, TRUE ~ 0))
View(VECM_GDP)
write.xlsx(round(VECM_GDP, 2), file = "/Users/mariammaloan/Desktop/модельecm.xlsx")


#### VECM все переменные в уравнении ниже прологарифмированы ####
# LE_ts ~ ECT + Intercept + GDP_ts-1 + LE_ts-1 + GDP_ts-2 + LE_ts-2 + GDP_ts-3 +  LE_ts-3 + GDP_ts-4 + Population_ts + LE_ts-4

data44
country_list <- unique(data44$Country)
table_1 <- as.table(rep(1, times = 22), ncol = 22, byrow = true)
for (i in 1:138) {
  print(i)
  data45 <- filter(data44, Countnom == i)
  GDP_ts <- ts(data45$GDP, start = 1980, frequency = 1, end = 2020)
  Population_ts <- ts(data45$Population, start = 1980, frequency = 1, end = 2020)
  LE_ts <-  ts(data45$LE, start = 1980, frequency = 1, end = 2020)
  dset <- cbind(GDP_ts, LE_ts)
  r_coef <- ifelse(o@teststat[1]>o@cval[1,1], 2, ifelse(o@teststat[2] > o@cval[2,1], 1, 0))
  model1 <- VECM(dset, 4, r = r_coef, estim = "2OLS",  exogen = data.frame(Population_ts))
  t <- summary(model1)
  table_1 <- rbind(table_1, c(t$coefficients[2,], t$Pvalues[2,]))
}
table_1 <- table_1[-1,]
colnames(table_1) <- c("ECT", "Intercept", "GDP_ts-1", "LE_ts-1", "GDP_ts-2", "LE_ts-2", "GDP_ts-3","LE_ts-3", "GDP_ts-4", "LE_ts-4", "Population_ts", "pvalue1", "pvalue2", "pvalue3", "pvalue4", "pvalue5", "pvalue6", "pvalue7", "pvalue8", "pvalue9", "pvalue10","pvalue11")
table_2 <- as.data.frame(cbind(table_1, country_list))
table_3 <- table_2[c("country_list", "ECT", "pvalue1", "Intercept", "pvalue2", "GDP_ts-1", "pvalue3",  "LE_ts-1", "pvalue4", "GDP_ts-2","pvalue5", "LE_ts-2","pvalue6","GDP_ts-3", "pvalue7", "LE_ts-3", "pvalue8", "GDP_ts-4", "pvalue9",  "LE_ts-4","pvalue10","Population_ts", "pvalue11")]
VECM_LE1 <- mutate(table_3, pvalue1 = case_when(pvalue1 < 0.01 ~ 3, pvalue1 < 0.05 ~ 2, pvalue1 < 0.1 ~ 1, TRUE ~ 0),
                   pvalue2 = case_when(pvalue2 < 0.01 ~ 3, pvalue2 < 0.05 ~ 2, pvalue2 < 0.1 ~ 1, TRUE ~ 0),
                   pvalue3 = case_when(pvalue3 < 0.01 ~ 3, pvalue3 < 0.05 ~ 2, pvalue3 < 0.1 ~ 1, TRUE ~ 0), 
                   pvalue4 = case_when(pvalue4 < 0.01 ~ 3, pvalue4 < 0.05 ~ 2, pvalue4 < 0.1 ~ 1, TRUE ~ 0), 
                   pvalue5 = case_when(pvalue5 < 0.01 ~ 3, pvalue5 < 0.05 ~ 2, pvalue5 < 0.1 ~ 1, TRUE ~ 0), 
                   pvalue6 = case_when(pvalue6 < 0.01 ~ 3, pvalue6 < 0.05 ~ 2, pvalue6 < 0.1 ~ 1, TRUE ~ 0), 
                   pvalue7 = case_when(pvalue7 < 0.01 ~ 3, pvalue7 < 0.05 ~ 2, pvalue7 < 0.1 ~ 1, TRUE ~ 0), 
                   pvalue8 = case_when(pvalue8 < 0.01 ~ 3, pvalue8 < 0.05 ~ 2, pvalue8 < 0.1 ~ 1, TRUE ~ 0), 
                   pvalue9 = case_when(pvalue9 < 0.01 ~ 3, pvalue9 < 0.05 ~ 2, pvalue9 < 0.1 ~ 1, TRUE ~ 0), 
                   pvalue10 = case_when(pvalue10 < 0.01 ~ 3, pvalue10 < 0.05 ~ 2, pvalue10 < 0.1 ~ 1, TRUE ~ 0), 
                   pvalue11 = case_when(pvalue11 < 0.01 ~ 3, pvalue11 < 0.05 ~ 2, pvalue11 < 0.1 ~ 1, TRUE ~ 0))
View(VECM_LE1)

#### VECM все переменные в уравнении ниже прологарифмированы ####
# GDP_per_capita ~ ECT + Intercept + GDP_per_capita-1 + LE_ts-1 + GDP_per_capita-2 + LE_ts-2 + GDP_per_capita-3 +  LE_ts-3 + GDP_per_capita-4 + LE_ts-4 + Population_ts 

data44
country_list <- unique(data44$Country)
table_1 <- as.table(rep(1, times = 22), ncol = 22, byrow = true)
for (i in 1:138) {
  print(i)
  data45 <- filter(data44, Countnom == i)
  GDP_per_capita_ts <- ts(data45$GDP_per_capita, start = 1980, frequency = 1, end = 2020)
  Population_ts <- ts(data45$Population, start = 1980, frequency = 1, end = 2020)
  LE_ts <-  ts(data45$LE, start = 1980, frequency = 1, end = 2020)
  dset <- cbind(GDP_per_capita_ts, LE_ts)
  r_coef <- ifelse(o@teststat[1]>o@cval[1,1], 2, ifelse(o@teststat[2] > o@cval[2,1], 1, 0))
  model1 <- VECM(dset, 4, r = r_coef, estim = "2OLS", exogen = data.frame(Population_ts))
  t <- summary(model1)
  table_1 <- rbind(table_1, c(t$coefficients[1,], t$Pvalues[1,]))
}
table_1 <- table_1[-1,]
colnames(table_1) <- c("ECT", "Intercept", "GDP_per_capita_ts-1", "LE_ts-1", "GDP_per_capita_ts-2", "LE_ts-2", "GDP_per_capita_ts-3", "LE_ts-3", "GDP_per_capita_ts-4", "LE_ts-4", "Population_ts",  "pvalue1", "pvalue2", "pvalue3", "pvalue4", "pvalue5", "pvalue6", "pvalue7", "pvalue8", "pvalue9", "pvalue10","pvalue11")
table_2 <- as.data.frame(cbind(table_1, country_list))
table_3 <- table_2[c("country_list", "ECT", "pvalue1", "Intercept", "pvalue2", "GDP_per_capita_ts-1", "pvalue3",  "LE_ts-1", "pvalue4", "GDP_per_capita_ts-2","pvalue5",  "LE_ts-2","pvalue6", "GDP_per_capita_ts-3","pvalue7", "LE_ts-3", "pvalue8",  "GDP_per_capita_ts-4","pvalue9",  "LE_ts-4", "pvalue10","Population_ts","pvalue11")]
VECM_GDP_per_cap <- mutate(table_3, pvalue1 = case_when(pvalue1 < 0.01 ~ 3, pvalue1 < 0.05 ~ 2, pvalue1 < 0.1 ~ 1, TRUE ~ 0),
                           pvalue2 = case_when(pvalue2 < 0.01 ~ 3, pvalue2 < 0.05 ~ 2, pvalue2 < 0.1 ~ 1, TRUE ~ 0),
                           pvalue3 = case_when(pvalue3 < 0.01 ~ 3, pvalue3 < 0.05 ~ 2, pvalue3 < 0.1 ~ 1, TRUE ~ 0), 
                           pvalue4 = case_when(pvalue4 < 0.01 ~ 3, pvalue4 < 0.05 ~ 2, pvalue4 < 0.1 ~ 1, TRUE ~ 0), 
                           pvalue5 = case_when(pvalue5 < 0.01 ~ 3, pvalue5 < 0.05 ~ 2, pvalue5 < 0.1 ~ 1, TRUE ~ 0), 
                           pvalue6 = case_when(pvalue6 < 0.01 ~ 3, pvalue6 < 0.05 ~ 2, pvalue6 < 0.1 ~ 1, TRUE ~ 0), 
                           pvalue7 = case_when(pvalue7 < 0.01 ~ 3, pvalue7 < 0.05 ~ 2, pvalue7 < 0.1 ~ 1, TRUE ~ 0), 
                           pvalue8 = case_when(pvalue8 < 0.01 ~ 3, pvalue8 < 0.05 ~ 2, pvalue8 < 0.1 ~ 1, TRUE ~ 0), 
                           pvalue9 = case_when(pvalue9 < 0.01 ~ 3, pvalue9 < 0.05 ~ 2, pvalue9 < 0.1 ~ 1, TRUE ~ 0), 
                           pvalue10 = case_when(pvalue10 < 0.01 ~ 3, pvalue10 < 0.05 ~ 2, pvalue10 < 0.1 ~ 1, TRUE ~ 0), 
                           pvalue11 = case_when(pvalue11 < 0.01 ~ 3, pvalue11 < 0.05 ~ 2, pvalue11 < 0.1 ~ 1, TRUE ~ 0))
View(VECM_GDP_per_cap)


#### VECM все переменные в уравнении ниже прологарифмированы ####
# LE_ts ~ ECT + Intercept + GDP_per_capita-1 + LE_ts-1 + GDP_per_capita-2 + LE_ts-2 + GDP_per_capita-3 + LE_ts-3 + GDP_per_capita-4 + LE_ts-4 + Population_ts 

data44
country_list <- unique(data44$Country)
table_1 <- as.table(rep(1, times = 22), ncol = 22, byrow = true)
for (i in 1:138) {
  print(i)
  data45 <- filter(data44, Countnom == i)
  GDP_per_capita_ts <- ts(data45$GDP_per_capita, start = 1980, frequency = 1, end = 2020)
  Population_ts <- ts(data45$Population, start = 1980, frequency = 1, end = 2020)
  LE_ts <-  ts(data45$LE, start = 1980, frequency = 1, end = 2020)
  dset <- cbind(GDP_per_capita_ts, LE_ts)
  r_coef <- ifelse(o@teststat[1]>o@cval[1,1], 2, ifelse(o@teststat[2] > o@cval[2,1], 1, 0))
  model1 <- VECM(dset, 4, r = r_coef, estim = "2OLS")
  t <- summary(model1)
  table_1 <- rbind(table_1, c(t$coefficients[2,], t$Pvalues[2,]))
}
table_1 <- table_1[-1,]
colnames(table_1) <- c("ECT", "Intercept", "GDP_per_capita_ts-1", "LE_ts-1", "GDP_per_capita_ts-2", "LE_ts-2", "GDP_per_capita_ts-3", "LE_ts-3", "GDP_per_capita_ts-4", "LE_ts-4", "Population_ts", "pvalue1", "pvalue2", "pvalue3", "pvalue4", "pvalue5", "pvalue6", "pvalue7", "pvalue8", "pvalue9", "pvalue10","pvalue11")
table_2 <- as.data.frame(cbind(table_1, country_list))
table_3 <- table_2[c("country_list", "ECT", "pvalue1", "Intercept", "pvalue2", "GDP_per_capita_ts-1", "pvalue3",  "LE_ts-1", "pvalue4", "GDP_per_capita_ts-2","pvalue5",  "LE_ts-2","pvalue6" ,  "GDP_per_capita_ts-3", "pvalue7", "LE_ts-3", "pvalue8", "GDP_per_capita_ts-4","pvalue9",   "LE_ts-4", "pvalue10", "Population_ts", "pvalue11")]
VECM_LE2 <- mutate(table_3, pvalue1 = case_when(pvalue1 < 0.01 ~ 3, pvalue1 < 0.05 ~ 2, pvalue1 < 0.1 ~ 1, TRUE ~ 0),
                   pvalue2 = case_when(pvalue2 < 0.01 ~ 3, pvalue2 < 0.05 ~ 2, pvalue2 < 0.1 ~ 1, TRUE ~ 0),
                   pvalue3 = case_when(pvalue3 < 0.01 ~ 3, pvalue3 < 0.05 ~ 2, pvalue3 < 0.1 ~ 1, TRUE ~ 0), 
                   pvalue4 = case_when(pvalue4 < 0.01 ~ 3, pvalue4 < 0.05 ~ 2, pvalue4 < 0.1 ~ 1, TRUE ~ 0), 
                   pvalue5 = case_when(pvalue5 < 0.01 ~ 3, pvalue5 < 0.05 ~ 2, pvalue5 < 0.1 ~ 1, TRUE ~ 0), 
                   pvalue6 = case_when(pvalue6 < 0.01 ~ 3, pvalue6 < 0.05 ~ 2, pvalue6 < 0.1 ~ 1, TRUE ~ 0), 
                   pvalue7 = case_when(pvalue7 < 0.01 ~ 3, pvalue7 < 0.05 ~ 2, pvalue7 < 0.1 ~ 1, TRUE ~ 0), 
                   pvalue8 = case_when(pvalue8 < 0.01 ~ 3, pvalue8 < 0.05 ~ 2, pvalue8 < 0.1 ~ 1, TRUE ~ 0), 
                   pvalue9 = case_when(pvalue9 < 0.01 ~ 3, pvalue9 < 0.05 ~ 2, pvalue9 < 0.1 ~ 1, TRUE ~ 0), 
                   pvalue10 = case_when(pvalue10 < 0.01 ~ 3, pvalue10 < 0.05 ~ 2, pvalue10 < 0.1 ~ 1, TRUE ~ 0), 
                   pvalue11 = case_when(pvalue11 < 0.01 ~ 3, pvalue11 < 0.05 ~ 2, pvalue11 < 0.1 ~ 1, TRUE ~ 0))
View(VECM_LE2)




#### коинтеграция + векм с другим пакетом ####
data44 <- read_excel("Desktop/data44.xlsx")
View(data44)
con_with_coint <- NULL
qwer <- NULL
for (i in 1:126){
  data45 <- filter(data44, Country_name == unique(data44$Country_name)[i])
  GDP_ts <- ts(log(data45$GDP), start = 1980, frequency = 1, end = 2020)
  LE_ts <-  ts(log(data45$LE), start = 1980, frequency = 1, end = 2020)
  vkgh <- coint.test(LE_ts, as.matrix(data.frame(GDP_ts)), nlag = 1, output = TRUE)
  qwer <- c(vkgh[1,3], qwer)
  if (vkgh[1,3] <= 0.05) {
    con_with_coint <- c(con_with_coint, i)} else {}
}
qwer
sum(qwer<=0.01)
#страны с коинтеграцией: поиск коинтеграции на другом листе, тк мы сравнивали 7 тестов
data50 <- data.frame(read_excel("Desktop/data44.xlsx"))
groups_of_con <- data.frame(read_excel("Desktop/countries with cointegration 3 groups.xlsx"))
View(groups_of_con)
data50$group1 <- ifelse(data50$Country %in% groups_of_con$countries.with.cointegration.1.group, 1, 0)
{
#график для стран с коинтеграцией 
ggplot(filter(data50, data50$group1 ==1), aes(x = log(LE), y=log(GDP), size = Population, colour = Country_name)) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_size() +
  scale_x_log10() +
  labs(title = 'Relation between GDP and Life Expectancy in countries with cointegration', subtitle = 'Year: {frame_time}', x = 'Life Expectancy', y = 'GDP') +
  transition_time(as.numeric(data50$Year)) +
  ease_aes('linear')
View(data50)
ggplot(data = filter(data50, data50$group1 ==1), aes( x = log(LE), y = log(GDP))) + geom_point(aes(col = as.factor(Country_name)), show.legend = FALSE)
ggplot(data = filter(data50, data50$group1 ==0), aes( x = log(LE), y = log(GDP))) + geom_point(aes(col = as.factor(Country_name)), show.legend = TRUE)
}

#график для стран без коинтеграцией 
ggplot(filter(data50, data50$group1 ==0), aes(x = log(LE), y=log(GDP), size = Population, colour = Country_name)) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_size() +
  scale_x_log10() +
  labs(title = 'Relation between GDP and Life Expectancy in countries with no cointegration', subtitle = 'Year: {frame_time}', x = 'Life Expectancy', y = 'GDP') +
  transition_time(as.numeric(data51$Year)) +
  ease_aes('linear')

#### Тест Грейнджера ####
c1 <-  NULL
c2 <-  NULL
ecm_coef <-  NULL
ecm_resid <-  NULL
ecm_coef2 <-  NULL
ecm_resid2 <-  NULL
lr_mult_gdp <-  NULL
lr_mult_gdp_pvalue <-  NULL
lr_mult_le <-  NULL
lr_mult_le_pvalue <-  NULL
ecm_coef_GDP  <-  NULL
ecm_resid_GDP  <-  NULL
ecm_coef2_LE  <-  NULL
ecm_resid2_LE <-  NULL
unique(data50$Country_name)

for (i in 1:137){
  data1 <- filter(data50, Country_name == unique(data50$Country_name)[i])
  #лучшую модель определим через ecm
  mod <- auto_ardl(LE ~ GDP, data1, max_order = 5)
  mod2 <- auto_ardl(GDP ~ LE, data1, max_order = 5)
  mod_recm <- recm(mod$best_model, case = 2)
  mod_recm2 <- recm(mod2$best_model, case = 2)
  ecmmod <- summary(mod_recm)
  ecmmod2 <- summary(mod_recm2)
  ecm_coef <- c(ecm_coef, ecmmod$coefficients[dim(ecmmod$coefficients)[1]])
  ecm_resid <- c(ecm_resid, ecmmod$coefficients[dim(ecmmod$coefficients)[1], 4])
  ecm_coef2 <- c(ecm_coef2, ecmmod2$coefficients[dim(ecmmod2$coefficients)[1]])
  ecm_resid2 <- c(ecm_resid2, ecmmod2$coefficients[dim(ecmmod2$coefficients)[1], 4])
  # # ifelse(ecmmod$coefficients[dim(ecmmod$coefficients)[1] - 1] != NA,
  #        ecm_coef_GDP <- c(ecm_coef_GDP, ecmmod$coefficients[dim(ecmmod$coefficients)[1] - 1]), 
  #        ecm_coef_GDP <- c(ecm_coef_GDP, 0))
  # # ifelse(ecmmod$coefficients[dim(ecmmod$coefficients)[1]-1, 4] != NA,
  #        ecm_resid_GDP <- c(ecm_resid_GDP, ecmmod$coefficients[dim(ecmmod$coefficients)[1]-1, 4]), 
  #        ecm_resid_GDP <- c(ecm_resid_GDP, 0))
  # ifelse(ecmmod2$coefficients[dim(ecmmod2$coefficients)[1] - 1] != NA,
  #        ecm_coef2_LE <- c(ecm_coef2_LE, ecmmod2$coefficients[dim(ecmmod2$coefficients)[1] - 1]), 
  #        ecm_coef2_LE <- c(ecm_coef2_LE, 0))
  # ifelse(ecmmod2$coefficients[dim(ecmmod2$coefficients)[1] - 1, 4] != NA,
  #        ecm_resid2_LE <- c(ecm_resid2_LE, ecmmod2$coefficients[dim(ecmmod2$coefficients)[1] - 1, 4]), 
  #        ecm_resid2_LE <- c(ecm_resid2_LE, 0))
  #тест Грейнджера
  grang1 <- grangertest(LE ~ GDP, order = mod$best_order[1], data = data1)
  grang1$`Pr(>F)`[2]
  grang2 <- grangertest(GDP ~ LE, order = mod2$best_order[1], data = data1)
  grang2$`Pr(>F)`[2]
  c1 <- c(c1, grang1$`Pr(>F)`[2])
  c2 <- c(c2, grang2$`Pr(>F)`[2])
  #долгосрочный мультипликтор
  lr_mult <- multipliers(mod$best_model, type = "lr")
  lr_mult2 <- multipliers(mod2$best_model, type = "lr")
  lr_mult_gdp <- c(lr_mult_gdp, lr_mult[2, 2])
  lr_mult_gdp_pvalue <- c(lr_mult_gdp_pvalue, lr_mult[2, 5])
  lr_mult_le <- c(lr_mult_le, lr_mult2[2, 2])
  lr_mult_le_pvalue <- c(lr_mult_le_pvalue, lr_mult2[2, 5])
}
ubhkjn <- unique(data50$Country_name)
jwifeokl <- data.frame(cbind(ubhkjn, c1, c2, ecm_coef, ecm_resid, ecm_coef2, ecm_resid2,
                             lr_mult_gdp, lr_mult_gdp_pvalue, lr_mult_le, lr_mult_le_pvalue))
table_with_coef <- merge(jwifeokl, clasters_countries, join = "left", by.x = "V1", by.y = "V1")

View(jwifeokl)
data1 <- filter(data50, Country_name == unique(data50$Country_name)[6])
#лучшую модель определим через ecm
mod <- auto_ardl(LE ~ GDP, data1, max_order = 5)
mod2 <- auto_ardl(GDP ~ LE, data1, max_order = 5)
mod_recm <- recm(mod$best_model, case = 2)
mod_recm2 <- recm(mod2$best_model, case = 2)
ecmmod <- summary(mod_recm)
ecmmod2 <- summary(mod_recm2)
summary(mod_recm2)


write.xlsx(jwifeokl, file = "/Users/mariammaloan/Desktop/table_with_coef.xlsx")
View(jwifeokl)
for (i in 1:45) {
  #порядок интегрирования
  data2 <- filter(data50, Country_name == unique(data50$Country_name)[i])
  LE_ts <-  ts(data1$LE, start = 1980, frequency = 1, end = 2020)
  GDP_ts <- ts(data1$GDP, start = 1980, frequency = 1, end = 2020)
  adf.test(data1$LE)
  adf.test(data1$GDP_per_capita)
  #оба не стационарны
  
  #### VECM ####
  dset <- cbind(GDP_ts, LE_ts)
  lagselect <- VARselect(dset, lag.max = 5, type = "const")
  lagselect$selection[3]
  data_vecm <- dplyr::select(data2, LE_df, GDP_df)
  mod_vecm <- VECM(data_vecm, lag = lagselect$selection[3])
  model_vecm <- summary(mod_vecm)
}
table_1
#### ecm - 3 переменных ####
H <- NULL
for(i in 1:138) {
  data1 <- filter(data44, Country_name ==
                    unique(data44$Country_name)[i])
  # #шаг 1: порядок интегрирования
  # adf.test(data1$LE)
  # adf.test(data1$GDP_per_capita)
  # adf.test(data1$GDP)
  # #все не стационарны
  
  #шаг 2: коинтеграция
  data_x <- cbind(data1$GDP_per_capita, data1$GDP)
  s <- coint.test(data1$LE, data_x)
  H[i] <- s[1,3]}
sum(H < 0.1)
#у некоторых стран есть коинтеграция

#### Для двух параметров ####
H <- NULL
for(i in 1:138) {
  data1 <- filter(data44, Country_name ==
                    unique(data44$Country_name)[i])
  #шаг 2: коинтеграция
  data_x <- cbind(data1$GDP_per_capita)
  s <- coint.test(data1$LE, data_x, nlag = 1)
  H[i] <- s[1,3]}
sum(H < 0.1)

####################


#### Для первого кластера PVECM####
id <- NULL
i <- NULL
for (j in 1:41) {
  i <-  rep(j, 41) 
  id <- c(i, id)
}
sort(id)

data44 <- read_excel("Desktop/data44.xlsx")
data44$LE <- log(data44$LE)
data44$GDP <- log(data44$GDP)
Data_clust1
View(data_cluster1)
cluster1 <- filter(clasters_countries, clasters_countries$X2  == 1)
cluster1
data_cluster1 <- cbind(dplyr::filter(data44, Country_name %in% row.names(Data_clust1)), id)
data45 <- data.frame(data_cluster1$GDP, data_cluster1$LE, id = id, time = 1980:2020)

###pvecm
PVEC <- pvecm(right_hand_side = data45[1], 
              left_hand_side = data45[2],
              cross_sections = data45[3], 
              time = data45[4], vecm_lags = 1,
              deterministic_long = "none",
              deterministic_short = "drift",
              method = "FM")
PVEC
table <- data.frame(cbind(PVEC$VECMS$data_cluster1.LE$coefficients, 
                          PVEC$VECMS$data_cluster1.GDP$coefficients)) 
colnames(table) <- c("LE", "GDP")
rownames(table) <- c("Intercept", "lag LE - 1", "lag GDP - 1", "lag(ECT)" )
table %% gt()
#видимо, это тест на проверку гипотезы о влиянии А на Б
cause_pvecm(PVEC)
#это коэффициенты долгосрочного равновесия в каждом VECM
PVEC$individual.coefficients
#это t-value для каждого 
#коэффициента долгосрочного равновесия в каждом VECM
PVEC$individual.coefficients.t.statistics
#общий усредненный
PVEC$long.run.vector
#все VECM
PVEC$individual.vecm
#нулевая гипотеза на то, что приведено в первой строке не влияет регрессор
library(tidyr)
X <- data45 %>% dplyr::mutate(id = as.factor(id)) %>% 
  dplyr::select(- data_cluster1.LE) %>% spread(key = id, value = data_cluster1.GDP)
Y <- data45 %>% dplyr::mutate(id = as.factor(id)) %>% 
  dplyr::select(-data_cluster1.GDP) %>% spread(key = id, value = data_cluster1.LE)
X <- X[, - c(1)] %>% as.matrix()
Y <- Y[, - c(1)] %>% as.matrix()
pedroni99(X, Y)
library("devtools")
####есть Для второго кластера PVECM####
id <- NULL
i <- NULL
for (j in 1:18) {
  i <-  rep(j, 41) 
  id <- c(i, id)
}
sort(id)

data44 <- read_excel("Desktop/data44.xlsx")
data44$LE <- log(data44$LE)
data44$GDP <- log(data44$GDP)
Data_clust2
data_cluster2 <- cbind(dplyr::filter(data44, Country_name %in% row.names(Data_clust2)), id)
data45 <- data.frame(data_cluster2$GDP, data_cluster2$LE, id = id, time = 1980:2020)

###pvecm
PVEC <- pvecm(right_hand_side = data45[1], 
              left_hand_side = data45[2],
              cross_sections = data45[3], 
              time = data45[4], vecm_lags = 1,
              deterministic_long = "none",
              deterministic_short = "drift",
              method = "FM")
PVEC
table <- data.frame(cbind(PVEC$VECMS$data_cluster2.LE$coefficients, 
                          PVEC$VECMS$data_cluster2.GDP$coefficients)) 
colnames(table) <- c("LE", "GDP")
rownames(table) <- c("Intercept", "lag LE - 1", "lag GDP - 1", "lag(ECT)" )
View(table)
table %% gt()
#видимо, это тест на проверку гипотезы о влиянии А на Б
cause_pvecm(PVEC)
#это коэффициенты долгосрочного равновесия в каждом VECM
PVEC$individual.coefficients
#это t-value для каждого 
#коэффициента долгосрочного равновесия в каждом VECM
PVEC$individual.coefficients.t.statistics
#общий усредненный
PVEC$long.run.vector
#все VECM
PVEC$individual.vecm
#так можно достать один VECM - не работает
PVEC$individual.vecm[[1]]$y
#видимо, это тест на проверку гипотезы о влиянии А на Б
cause_pvecm(PVEC)
#нулевая гипотеза на то, что приведено в первой строке не влияет регрессор

X <- data45 %>% dplyr::mutate(id = as.factor(id)) %>% 
  dplyr::select(- data_cluster2.LE) %>% spread(key = id, value = data_cluster2.GDP)
Y <- data45 %>% dplyr::mutate(id = as.factor(id)) %>% 
  dplyr::select(-data_cluster2.GDP) %>% spread(key = id, value = data_cluster2.LE)
X <- X[, - c(1)] %>% as.matrix()
Y <- Y[, - c(1)] %>% as.matrix()
pedroni99(X, Y)


#### Для третьего кластера PVECM####
id <- NULL
i <- NULL
for (j in 1:27) {
  i <-  rep(j, 41) 
  id <- c(i, id)
}
sort(id)

data44 <- read_excel("Desktop/data44.xlsx")
data44$LE <- log(data44$LE)
data44$GDP <- log(data44$GDP)
Data_clust3
data_cluster3 <- cbind(dplyr::filter(data44, Country_name %in% row.names(Data_clust3)), id)
data45 <- data.frame(data_cluster3$GDP, data_cluster3$LE, id = id, time = 1980:2020)

###pvecm
PVEC <- pvecm(right_hand_side = data45[1], 
              left_hand_side = data45[2],
              cross_sections = data45[3], 
              time = data45[4], vecm_lags = 1,
              deterministic_long = "none",
              deterministic_short = "drift",
              method = "FM")
PVEC
table <- data.frame(cbind(PVEC$VECMS$data_cluster3.LE$coefficients, 
                          PVEC$VECMS$data_cluster3.GDP$coefficients)) 
colnames(table) <- c("LE", "GDP")
View(table)
#видимо, это тест на проверку гипотезы о влиянии А на Б
cause_pvecm(PVEC)
#это коэффициенты долгосрочного равновесия в каждом VECM
PVEC$individual.coefficients
#это t-value для каждого 
#коэффициента долгосрочного равновесия в каждом VECM
PVEC$individual.coefficients.t.statistics
#общий усредненный
PVEC$long.run.vector
#все VECM
PVEC$individual.vecm
#так можно достать один VECM - не работает
PVEC$individual.vecm[[1]]$y
#видимо, это тест на проверку гипотезы о влиянии А на Б
cause_pvecm(PVEC)
#нулевая гипотеза на то, что приведено в первой строке не влияет регрессор

X <- data45 %>% dplyr::mutate(id = as.factor(id)) %>% 
  dplyr::select(- data_cluster3.LE) %>% spread(key = id, value = data_cluster3.GDP)
Y <- data45 %>% dplyr::mutate(id = as.factor(id)) %>% 
  dplyr::select(-data_cluster3.GDP) %>% spread(key = id, value = data_cluster3.LE)
X <- X[, - c(1)] %>% as.matrix()
Y <- Y[, - c(1)] %>% as.matrix()
pedroni99(X, Y)
####есть Для четвертого кластера PVECM####
id <- NULL
i <- NULL
for (j in 1:40) {
  i <-  rep(j, 41) 
  id <- c(i, id)
}
sort(id)

data44 <- read_excel("Desktop/data44.xlsx")
data44$LE <- log(data44$LE)
data44$GDP <- log(data44$GDP)
Data_clust4
data_cluster4 <- cbind(dplyr::filter(data44, Country_name %in% row.names(Data_clust4)), id)
data45 <- data.frame(data_cluster4$GDP, data_cluster4$LE, id = id, time = 1980:2020)

###pvecm
PVEC <- pvecm(right_hand_side = data45[1], 
              left_hand_side = data45[2],
              cross_sections = data45[3], 
              time = data45[4], vecm_lags = 1,
              deterministic_long = "none",
              deterministic_short = "drift",
              method = "FM")
PVEC
table <- data.frame(cbind(PVEC$VECMS$data_cluster4.LE$coefficients, 
                          PVEC$VECMS$data_cluster4.GDP$coefficients)) 
colnames(table) <- c("LE", "GDP")
rownames(table) <- c("Intercept", "lag LE - 1", "lag GDP - 1", "lag(ECT)" )
View(table)
#видимо, это тест на проверку гипотезы о влиянии А на Б
cause_pvecm(PVEC)
#это коэффициенты долгосрочного равновесия в каждом VECM
PVEC$individual.coefficients
#это t-value для каждого 
#коэффициента долгосрочного равновесия в каждом VECM
PVEC$individual.coefficients.t.statistics
#общий усредненный
PVEC$long.run.vector
#все VECM
PVEC$individual.vecm
#так можно достать один VECM - не работает
PVEC$individual.vecm[[1]]$y
#видимо, это тест на проверку гипотезы о влиянии А на Б
cause_pvecm(PVEC)
#нулевая гипотеза на то, что приведено в первой строке не влияет регрессор

X <- data45 %>% dplyr::mutate(id = as.factor(id)) %>% 
  dplyr::select(- data_cluster4.LE) %>% spread(key = id, value = data_cluster4.GDP)
Y <- data45 %>% dplyr::mutate(id = as.factor(id)) %>% 
  dplyr::select(-data_cluster4.GDP) %>% spread(key = id, value = data_cluster4.LE)
X <- X[, - c(1)] %>% as.matrix()
Y <- Y[, - c(1)] %>% as.matrix()
pedroni99(X, Y)


#### кластеризация только по LE и GDP ####
Data <- read_excel("/Users/mariammaloan/Desktop/mean 2013-2020.xlsx")
Data7 <- data.frame(Data)
rownames(Data7) <- Data7$Country
Data7 <- dplyr::select(Data7, Life_exp, GDP)
Data8 <- na.omit(Data7)
#логарифмируем данные 
Data8$GDP <- log(Data8$GDP)
Data8$Life_exp <- log(Data8$Life_exp)
Data9 <- data.frame(Data8)
data_scale <- scale(Data9[,-1])
data_dist <- dist(data_scale) 
clastering <- hclust(data_dist)
plot(clastering, ann = FALSE, labels = FALSE, hang = -1)
#обычная кластеризация k-means
rect.hclust(clastering, k=4)
cluster <- cutree(clastering, k=4)
Data9$cluster <- cluster
Data_group <- group_by(Data9, cluster)
Data_group_mean <- summarise_all(Data_group, mean)
data_chart <- as.data.frame(Data9)
ggplot(data_chart, aes(Life_exp)) + geom_density(fill = "#258054", alpha = 0.5) + facet_grid(~ cluster)

mod12 <- PCA(Data9, graph = FALSE)
fviz_pca_biplot(mod12)
mod_pa <- HCPC(mod12, graph = FALSE, nb.clust = 4)
fviz_cluster(mod_pa, palette = "jco", repel = TRUE)
fviz_dend(mod_pa, cex = 0.7, palette = "jco", rect = TRUE, rect_fill = TRUE, rect_border = "jco", labels_track_height = 0.8)
View(mod_pa)
mod_pa$desc.var

View(Data9)

#строим PVECM по новым кластерам
####нет Для первого кластера PVECM####
id <- NULL
i <- NULL
for (j in 1:15) {
  i <-  rep(j, 41) 
  id <- c(i, id)
}
sort(id)
data44 <- read_excel("Desktop/data44.xlsx")
data44$LE <- log(data44$LE)
data44$GDP <- log(data44$GDP)
cluster1 <- filter(Data9, cluster == 1)
cluster1
data_cluster1 <- cbind(dplyr::filter(data44, Country_name %in% row.names(cluster1)), id)
data45 <- data.frame(data_cluster1$GDP, data_cluster1$LE, id = id, time = 1980:2020)

###pvecm
PVEC <- pvecm(right_hand_side = data45[1], 
              left_hand_side = data45[2],
              cross_sections = data45[3], 
              time = data45[4], vecm_lags = 1,
              deterministic_long = "none",
              deterministic_short = "drift",
              method = "FM")
PVEC
table <- data.frame(cbind(PVEC$VECMS$data_cluster1.LE$coefficients, 
                          PVEC$VECMS$data_cluster1.GDP$coefficients)) 
colnames(table) <- c("LE", "GDP")
View(table)
#видимо, это тест на проверку гипотезы о влиянии А на Б
cause_pvecm(PVEC)
#это коэффициенты долгосрочного равновесия в каждом VECM
PVEC$individual.coefficients
#это t-value для каждого 
#коэффициента долгосрочного равновесия в каждом VECM
PVEC$individual.coefficients.t.statistics
#общий усредненный
PVEC$long.run.vector
#все VECM
PVEC$individual.vecm
#так можно достать один VECM - не работает
PVEC$individual.vecm[[1]]$y
#видимо, это тест на проверку гипотезы о влиянии А на Б
cause_pvecm(PVEC)
View(data45)
#нулевая гипотеза на то, что приведено в первой строке не влияет регрессор
X <- data45 %>% dplyr::mutate(id = as.factor(id)) %>% 
  dplyr::select(- data_cluster1.LE) %>% spread(key = id, value = data_cluster1.GDP)
Y <- data45 %>% dplyr::mutate(id = as.factor(id)) %>% 
  dplyr::select(-data_cluster1.GDP) %>% spread(key = id, value = data_cluster1.LE)
X <- X[, - c(1)] %>% as.matrix()
Y <- Y[, - c(1)] %>% as.matrix()
pedroni99(X, Y)
#больше 1.96 значит проходит
####есть Для второго кластера PVECM####
id <- NULL
i <- NULL
for (j in 1:40) {
  i <-  rep(j, 41) 
  id <- c(i, id)
}
sort(id)

data44 <- read_excel("Desktop/data44.xlsx")
data44$LE <- log(data44$LE)
data44$GDP <- log(data44$GDP)
cluster2 <- filter(Data9, cluster == 2)
cluster2
data_cluster2 <- cbind(dplyr::filter(data44, Country_name %in% row.names(cluster2)), id)
data45 <- data.frame(data_cluster2$GDP, data_cluster2$LE, id = id, time = 1980:2020)

###pvecm
PVEC <- pvecm(right_hand_side = data45[1], 
              left_hand_side = data45[2],
              cross_sections = data45[3], 
              time = data45[4], vecm_lags = 1,
              deterministic_long = "none",
              deterministic_short = "drift",
              method = "FM")
PVEC
table <- data.frame(cbind(PVEC$VECMS$data_cluster2.LE$coefficients, 
                          PVEC$VECMS$data_cluster2.GDP$coefficients)) 
colnames(table) <- c("LE", "GDP")
View(table)
#видимо, это тест на проверку гипотезы о влиянии А на Б
cause_pvecm(PVEC)
#это коэффициенты долгосрочного равновесия в каждом VECM
PVEC$individual.coefficients
#это t-value для каждого 
#коэффициента долгосрочного равновесия в каждом VECM
PVEC$individual.coefficients.t.statistics
#общий усредненный
PVEC$long.run.vector
#все VECM
PVEC$individual.vecm
#так можно достать один VECM - не работает
PVEC$individual.vecm[[1]]$y
#видимо, это тест на проверку гипотезы о влиянии А на Б
cause_pvecm(PVEC)
#нулевая гипотеза на то, что приведено в первой строке не влияет регрессор
X <- data45 %>% dplyr::mutate(id = as.factor(id)) %>% 
  dplyr::select(- data_cluster2.LE) %>% spread(key = id, value = data_cluster2.GDP)
Y <- data45 %>% dplyr::mutate(id = as.factor(id)) %>% 
  dplyr::select(-data_cluster2.GDP) %>% spread(key = id, value = data_cluster2.LE)
X <- X[, - c(1)] %>% as.matrix()
Y <- Y[, - c(1)] %>% as.matrix()
pedroni99(X, Y)


####есть Для третьего кластера PVECM####
id <- NULL
i <- NULL
for (j in 1:46) {
  i <-  rep(j, 41) 
  id <- c(i, id)
}
sort(id)

data44 <- read_excel("Desktop/data44.xlsx")
data44$LE <- log(data44$LE)
data44$GDP <- log(data44$GDP)
cluster3 <- filter(Data9, cluster == 3)
cluster3
data_cluster3 <- cbind(dplyr::filter(data44, Country_name %in% row.names(cluster3)), id)
data45 <- data.frame(data_cluster3$GDP, data_cluster3$LE, id = id, time = 1980:2020)

###pvecm
PVEC <- pvecm(right_hand_side = data45[1], 
              left_hand_side = data45[2],
              cross_sections = data45[3], 
              time = data45[4], vecm_lags = 1,
              deterministic_long = "none",
              deterministic_short = "drift",
              method = "FM")
PVEC
table <- data.frame(cbind(PVEC$VECMS$data_cluster3.LE$coefficients, 
                          PVEC$VECMS$data_cluster3.GDP$coefficients)) 
colnames(table) <- c("LE", "GDP")
View(table)
#видимо, это тест на проверку гипотезы о влиянии А на Б
cause_pvecm(PVEC)
#это коэффициенты долгосрочного равновесия в каждом VECM
PVEC$individual.coefficients
#это t-value для каждого 
#коэффициента долгосрочного равновесия в каждом VECM
PVEC$individual.coefficients.t.statistics
#общий усредненный
PVEC$long.run.vector
#все VECM
PVEC$individual.vecm
#так можно достать один VECM - не работает
PVEC$individual.vecm[[1]]$y
#нулевая гипотеза на то, что приведено в первой строке не влияет регрессор

X <- data45 %>% dplyr::mutate(id = as.factor(id)) %>% 
  dplyr::select(- data_cluster3.LE) %>% spread(key = id, value = data_cluster3.GDP)
Y <- data45 %>% dplyr::mutate(id = as.factor(id)) %>% 
  dplyr::select(-data_cluster3.GDP) %>% spread(key = id, value = data_cluster3.LE)
X <- X[, - c(1)] %>% as.matrix()
Y <- Y[, - c(1)] %>% as.matrix()
pedroni99(X, Y)
####есть Для четвертого кластера PVECM####
id <- NULL
i <- NULL
for (j in 1:33) {
  i <-  rep(j, 41) 
  id <- c(i, id)
}
sort(id)
data44 <- read_excel("Desktop/data44.xlsx")
data44$LE <- log(data44$LE)
data44$GDP <- log(data44$GDP)
cluster4 <- filter(Data9, cluster == 4)
cluster4
data_cluster4 <- cbind(dplyr::filter(data44, Country_name %in% row.names(cluster4)), id)
data45 <- data.frame(data_cluster4$GDP, data_cluster4$LE, id = id, time = 1980:2020)

###pvecm
PVEC <- pvecm(right_hand_side = data45[1], 
              left_hand_side = data45[2],
              cross_sections = data45[3], 
              time = data45[4], vecm_lags = 1,
              deterministic_long = "none",
              deterministic_short = "drift",
              method = "FM")
PVEC
table <- data.frame(cbind(PVEC$VECMS$data_cluster4.LE$coefficients, 
                          PVEC$VECMS$data_cluster4.GDP$coefficients)) 
colnames(table) <- c("LE", "GDP")
View(table)
#видимо, это тест на проверку гипотезы о влиянии А на Б
cause_pvecm(PVEC)
#это коэффициенты долгосрочного равновесия в каждом VECM
PVEC$individual.coefficients
#это t-value для каждого 
#коэффициента долгосрочного равновесия в каждом VECM
PVEC$individual.coefficients.t.statistics
#общий усредненный
PVEC$long.run.vector
#все VECM
PVEC$individual.vecm
#так можно достать один VECM - не работает
PVEC$individual.vecm[[1]]$y
#нулевая гипотеза на то, что приведено в первой строке не влияет регрессор

X <- data45 %>% dplyr::mutate(id = as.factor(id)) %>% 
  dplyr::select(- data_cluster4.LE) %>% spread(key = id, value = data_cluster4.GDP)
Y <- data45 %>% dplyr::mutate(id = as.factor(id)) %>% 
  dplyr::select(-data_cluster4.GDP) %>% spread(key = id, value = data_cluster4.LE)
X <- X[, - c(1)] %>% as.matrix()
Y <- Y[, - c(1)] %>% as.matrix()
pedroni99(X, Y)


#### кластеризация по всем параметрам + коэф джинни ####
par(mfrow = c(1,1))
Data1 <- read_excel("/Users/mariammaloan/Desktop/mean 2013-2020.xlsx")
Data7 <- data.frame(Data1)
Data7 <- dplyr::select(Data7, - GDP_growth)
Data8 <- na.omit(Data7)
View(Data8)
#логарифмируем данные 
Data8$GDP <- log(Data8$GDP)
Data8$Life_exp <- log(Data8$Life_exp)
Data8$Agriculture <- log(Data8$Agriculture)
Data8$Urban_pop <- log(Data8$Urban_pop)
Data8$Tuberculosis <- log(Data8$Tuberculosis)
Data8$CO2 <- log(Data8$CO2)
Data8$GDP.per.capita <- log(Data8$GDP.per.capita)
Data8$Health_expenditure_from_GDP <- log(Data8$Health_expenditure_from_GDP)
Data8$ginni <- log(Data8$ginni)
Data8$School.prim <- log(Data8$School.prim)
Data8$Population <- log(Data8$Population)
Data9 <- data.frame(Data8)
row.names(Data9) <- Data8$Country
Data9 <- dplyr::select(Data9, - Country)
data_scale <- scale(Data9)
data_dist <- dist(data_scale) 
clastering <- hclust(data_dist)

#обычная кластеризация k-means
rect.hclust(clastering, k=4)
cluster <- cutree(clastering, k=4)
Data9$cluster <- cluster
Data_group <- group_by(Data9, cluster)
Data_group_mean <- summarise_all(Data_group, mean)
View(Data_group_mean)
data_chart <- as.data.frame(Data9)
ggplot(data_chart, aes(Life_exp)) + geom_density(fill = "#258054", alpha = 0.5) + facet_grid(~ cluster)
View(Data9)
mod12 <- PCA(Data9[,-1], graph = FALSE)
fviz_pca_biplot(mod12)
mod_pa <- HCPC(mod12, graph = FALSE, nb.clust = 4)
fviz_cluster(mod_pa, palette = "jco", repel = TRUE)
fviz_dend(mod_pa, cex = 0.7, palette = "jco", rect = TRUE, rect_fill = TRUE, rect_border = "jco", labels_track_height = 0.8)
View(mod_pa)
mod_pa$desc.var
#строим PVECM по новым кластерам
####есть немного Для первого кластера PVECM####
id <- NULL
i <- NULL
for (j in 1:16) {
  i <-  rep(j, 41) 
  id <- c(i, id)
}
sort(id)
data44 <- read_excel("Desktop/data44.xlsx")
data44$LE <- log(data44$LE)
data44$GDP <- log(data44$GDP)
cluster1 <- filter(Data9, cluster == 1)
cluster1
data_cluster1 <- cbind(dplyr::filter(data44, Country_name %in% row.names(cluster1)), id)
data45 <- data.frame(data_cluster1$GDP, data_cluster1$LE, id = id, time = 1980:2020)

###pvecm
PVEC <- pvecm(right_hand_side = data45[1], 
              left_hand_side = data45[2],
              cross_sections = data45[3], 
              time = data45[4], vecm_lags = 1,
              deterministic_long = "none",
              deterministic_short = "drift",
              method = "FM")
PVEC
#видимо, это тест на проверку гипотезы о влиянии А на Б
cause_pvecm(PVEC)
#это коэффициенты долгосрочного равновесия в каждом VECM
PVEC$individual.coefficients
#это t-value для каждого 
#коэффициента долгосрочного равновесия в каждом VECM
PVEC$individual.coefficients.t.statistics
#общий усредненный - коэф коинтеграции
PVEC$long.run.vector
#все VECM
PVEC$individual.vecm
#так можно достать один VECM - не работает
PVEC$individual.vecm[[1]]$y
#видимо, это тест на проверку гипотезы о влиянии А на Б
cause_pvecm(PVEC)
View(data45)
#нулевая гипотеза на то, что приведено в первой строке не влияет регрессор
library(tidyr)
X <- data45 %>% dplyr::mutate(id = as.factor(id)) %>% 
  dplyr::select(- data_cluster1.LE) %>% spread(key = id, value = data_cluster1.GDP)
Y <- data45 %>% dplyr::mutate(id = as.factor(id)) %>% 
  dplyr::select(-data_cluster1.GDP) %>% spread(key = id, value = data_cluster1.LE)
X <- X[, - c(1)] %>% as.matrix()
Y <- Y[, - c(1)] %>% as.matrix()
pedroni99(X, Y)
#больше 1.96 значит проходит
####есть Для второго кластера PVECM####
id <- NULL
i <- NULL
for (j in 1:49) {
  i <-  rep(j, 41) 
  id <- c(i, id)
}
sort(id)

data44 <- read_excel("Desktop/data44.xlsx")
data44$LE <- log(data44$LE)
data44$GDP <- log(data44$GDP)
cluster2 <- filter(Data9, cluster == 2)
cluster2
data_cluster2 <- cbind(dplyr::filter(data44, Country_name %in% row.names(cluster2)), id)
data45 <- data.frame(data_cluster2$GDP, data_cluster2$LE, id = id, time = 1980:2020)

###pvecm
PVEC <- pvecm(right_hand_side = data45[1], 
              left_hand_side = data45[2],
              cross_sections = data45[3], 
              time = data45[4], vecm_lags = 1,
              deterministic_long = "none",
              deterministic_short = "drift",
              method = "FM")
PVEC
#видимо, это тест на проверку гипотезы о влиянии А на Б
cause_pvecm(PVEC)
#это коэффициенты долгосрочного равновесия в каждом VECM
PVEC$individual.coefficients
#это t-value для каждого 
#коэффициента долгосрочного равновесия в каждом VECM
PVEC$individual.coefficients.t.statistics
#общий усредненный
PVEC$long.run.vector
#все VECM
PVEC$individual.vecm
#так можно достать один VECM - не работает
PVEC$individual.vecm[[1]]$y
#видимо, это тест на проверку гипотезы о влиянии А на Б
cause_pvecm(PVEC)
#нулевая гипотеза на то, что приведено в первой строке не влияет регрессор

X <- data45 %>% dplyr::mutate(id = as.factor(id)) %>% 
  dplyr::select(- data_cluster2.LE) %>% spread(key = id, value = data_cluster2.GDP)
Y <- data45 %>% dplyr::mutate(id = as.factor(id)) %>% 
  dplyr::select(-data_cluster2.GDP) %>% spread(key = id, value = data_cluster2.LE)
X <- X[, - c(1)] %>% as.matrix()
Y <- Y[, - c(1)] %>% as.matrix()
pedroni99(X, Y)


####есть Для третьего кластера PVECM####
id <- NULL
i <- NULL
for (j in 1:8) {
  i <-  rep(j, 41) 
  id <- c(i, id)
}
sort(id)

data44 <- read_excel("Desktop/data44.xlsx")
data44$LE <- log(data44$LE)
data44$GDP <- log(data44$GDP)
cluster3 <- filter(Data9, cluster == 3)
cluster3
data_cluster3 <- cbind(dplyr::filter(data44, Country_name %in% row.names(cluster3)), id)
data45 <- data.frame(data_cluster3$GDP, data_cluster3$LE, id = id, time = 1980:2020)

###pvecm
PVEC <- pvecm(right_hand_side = data45[1], 
              left_hand_side = data45[2],
              cross_sections = data45[3], 
              time = data45[4], vecm_lags = 2,
              deterministic_long = "none",
              deterministic_short = "drift",
              method = "FM")
PVEC
#видимо, это тест на проверку гипотезы о влиянии А на Б
cause_pvecm(PVEC)
#это коэффициенты долгосрочного равновесия в каждом VECM
PVEC$individual.coefficients
#это t-value для каждого 
#коэффициента долгосрочного равновесия в каждом VECM
PVEC$individual.coefficients.t.statistics
#общий усредненный
PVEC$long.run.vector
#все VECM
PVEC$individual.vecm
#так можно достать один VECM - не работает
PVEC$individual.vecm[[1]]$y
#видимо, это тест на проверку гипотезы о влиянии А на Б
cause_pvecm(PVEC)
#нулевая гипотеза на то, что приведено в первой строке не влияет регрессор

X <- data45 %>% dplyr::mutate(id = as.factor(id)) %>% 
  dplyr::select(- data_cluster3.LE) %>% spread(key = id, value = data_cluster3.GDP)
Y <- data45 %>% dplyr::mutate(id = as.factor(id)) %>% 
  dplyr::select(-data_cluster3.GDP) %>% spread(key = id, value = data_cluster3.LE)
X <- X[, - c(1)] %>% as.matrix()
Y <- Y[, - c(1)] %>% as.matrix()
pedroni99(X, Y)
####есть немного Для четвертого кластера PVECM####
id <- NULL
i <- NULL
for (j in 1:32) {
  i <-  rep(j, 41) 
  id <- c(i, id)
}
sort(id)
data44 <- read_excel("Desktop/data44.xlsx")
data44$LE <- log(data44$LE)
data44$GDP <- log(data44$GDP)
cluster4 <- filter(Data9, cluster == 4)
cluster4
data_cluster4 <- cbind(dplyr::filter(data44, Country_name %in% row.names(cluster4)), id)
data45 <- data.frame(data_cluster4$GDP, data_cluster4$LE, id = id, time = 1980:2020)

###pvecm
PVEC <- pvecm(right_hand_side = data45[1], 
              left_hand_side = data45[2],
              cross_sections = data45[3], 
              time = data45[4], vecm_lags = 2,
              deterministic_long = "none",
              deterministic_short = "drift",
              method = "FM")
PVEC
#видимо, это тест на проверку гипотезы о влиянии А на Б
cause_pvecm(PVEC)
#это коэффициенты долгосрочного равновесия в каждом VECM
PVEC$individual.coefficients
#это t-value для каждого 
#коэффициента долгосрочного равновесия в каждом VECM
PVEC$individual.coefficients.t.statistics
#общий усредненный
PVEC$long.run.vector
#все VECM
PVEC$individual.vecm
#так можно достать один VECM - не работает
PVEC$individual.vecm[[1]]$y
#видимо, это тест на проверку гипотезы о влиянии А на Б
cause_pvecm(PVEC)
#нулевая гипотеза на то, что приведено в первой строке не влияет регрессор

X <- data45 %>% dplyr::mutate(id = as.factor(id)) %>% 
  dplyr::select(- data_cluster4.LE) %>% spread(key = id, value = data_cluster4.GDP)
Y <- data45 %>% dplyr::mutate(id = as.factor(id)) %>% 
  dplyr::select(-data_cluster4.GDP) %>% spread(key = id, value = data_cluster4.LE)
X <- X[, - c(1)] %>% as.matrix()
Y <- Y[, - c(1)] %>% as.matrix()
pedroni99(X, Y)

























#### кластеризация сельхоз туберкулез и джинни ####
Data1 <- read_excel("/Users/mariammaloan/Desktop/mean 2013-2020.xlsx")
Data7 <- data.frame(Data1)
rownames(Data7) <- Data7$Country
colnames(Data7)
Data7 <- dplyr::select(Data7, Agriculture, Tuberculosis, ginni)
Data8 <- na.omit(Data7)
View(Data8)
#логарифмируем данные 
Data8$Agriculture <- log(Data8$Agriculture)
Data8$Tuberculosis <- log(Data8$Tuberculosis)
Data8$ginni <- log(Data8$ginni)
Data9 <- data.frame(Data8)
View(Data9)
data_scale <- scale(Data9[,-1])
data_dist <- dist(data_scale) 
clastering <- hclust(data_dist)
plot(clastering, ann = FALSE, labels = FALSE, hang = -1)
View(Data9)
#обычная кластеризация k-means
rect.hclust(clastering, k=4)
cluster <- cutree(clastering, k=4)
Data9$cluster <- cluster
Data_group <- group_by(Data9, cluster)
Data_group_mean <- summarise_all(Data_group, mean)
View(Data_group_mean)
data_chart <- as.data.frame(Data9)

mod12 <- PCA(Data9, graph = FALSE)
fviz_pca_biplot(mod12)
mod_pa <- HCPC(mod12, graph = FALSE, nb.clust = 4)
fviz_cluster(mod_pa, palette = "jco", repel = TRUE)
fviz_dend(mod_pa, cex = 0.7, palette = "jco", rect = TRUE, rect_fill = TRUE, rect_border = "jco", labels_track_height = 0.8)
View(mod_pa)
mod_pa$desc.var
#строим PVECM по новым кластерам
####есть немного Для первого кластера PVECM####
id <- NULL
i <- NULL
for (j in 1:57) {
  i <-  rep(j, 41) 
  id <- c(i, id)
}
sort(id)
data44 <- read_excel("Desktop/data44.xlsx")
data44$LE <- log(data44$LE)
data44$GDP <- log(data44$GDP)
cluster1 <- filter(Data9, cluster == 1)
cluster1
data_cluster1 <- cbind(dplyr::filter(data44, Country_name %in% row.names(cluster1)), id)
data45 <- data.frame(data_cluster1$GDP, data_cluster1$LE, id = id, time = 1980:2020)

###pvecm
PVEC <- pvecm(right_hand_side = data45[1], 
              left_hand_side = data45[2],
              cross_sections = data45[3], 
              time = data45[4], vecm_lags = 1,
              deterministic_long = "none",
              deterministic_short = "drift",
              method = "FM")
PVEC

#видимо, это тест на проверку гипотезы о влиянии А на Б
cause_pvecm(PVEC)
#это коэффициенты долгосрочного равновесия в каждом VECM
PVEC$individual.coefficients
#это t-value для каждого 
#коэффициента долгосрочного равновесия в каждом VECM
PVEC$individual.coefficients.t.statistics
#общий усредненный - коэф коинтеграции
PVEC$long.run.vector
#все VECM
PVEC$individual.vecm
#так можно достать один VECM - не работает
PVEC$individual.vecm[[1]]$y
#видимо, это тест на проверку гипотезы о влиянии А на Б
cause_pvecm(PVEC)
View(data45)
#нулевая гипотеза на то, что приведено в первой строке не влияет регрессор
library(tidyr)
X <- data45 %>% dplyr::mutate(id = as.factor(id)) %>% 
  dplyr::select(- data_cluster1.LE) %>% spread(key = id, value = data_cluster1.GDP)
Y <- data45 %>% dplyr::mutate(id = as.factor(id)) %>% 
  dplyr::select(-data_cluster1.GDP) %>% spread(key = id, value = data_cluster1.LE)
X <- X[, - c(1)] %>% as.matrix()
Y <- Y[, - c(1)] %>% as.matrix()
pedroni99(X, Y)
#больше 1.96 значит проходит
####нет Для второго кластера PVECM####
id <- NULL
i <- NULL
for (j in 1:5) {
  i <-  rep(j, 41) 
  id <- c(i, id)
}
sort(id)

data44 <- read_excel("Desktop/data44.xlsx")
data44$LE <- log(data44$LE)
data44$GDP <- log(data44$GDP)
cluster2 <- filter(Data9, cluster == 2)
cluster2
data_cluster2 <- cbind(dplyr::filter(data44, Country_name %in% row.names(cluster2)), id)
data45 <- data.frame(data_cluster2$GDP, data_cluster2$LE, id = id, time = 1980:2020)

###pvecm
PVEC <- pvecm(right_hand_side = data45[1], 
              left_hand_side = data45[2],
              cross_sections = data45[3], 
              time = data45[4], vecm_lags = 2,
              deterministic_long = "none",
              deterministic_short = "drift",
              method = "FM")
PVEC
#видимо, это тест на проверку гипотезы о влиянии А на Б
cause_pvecm(PVEC)
#это коэффициенты долгосрочного равновесия в каждом VECM
PVEC$individual.coefficients
#это t-value для каждого 
#коэффициента долгосрочного равновесия в каждом VECM
PVEC$individual.coefficients.t.statistics
#общий усредненный
PVEC$long.run.vector
#все VECM
PVEC$individual.vecm
#так можно достать один VECM - не работает
PVEC$individual.vecm[[1]]$y
#видимо, это тест на проверку гипотезы о влиянии А на Б
cause_pvecm(PVEC)
#нулевая гипотеза на то, что приведено в первой строке не влияет регрессор

X <- data45 %>% dplyr::mutate(id = as.factor(id)) %>% 
  dplyr::select(- data_cluster2.LE) %>% spread(key = id, value = data_cluster2.GDP)
Y <- data45 %>% dplyr::mutate(id = as.factor(id)) %>% 
  dplyr::select(-data_cluster2.GDP) %>% spread(key = id, value = data_cluster2.LE)
X <- X[, - c(1)] %>% as.matrix()
Y <- Y[, - c(1)] %>% as.matrix()
pedroni99(X, Y)


####нет Для третьего кластера PVECM####
id <- NULL
i <- NULL
for (j in 1:35) {
  i <-  rep(j, 41) 
  id <- c(i, id)
}
sort(id)

data44 <- read_excel("Desktop/data44.xlsx")
data44$LE <- log(data44$LE)
data44$GDP <- log(data44$GDP)
cluster3 <- filter(Data9, cluster == 3)
cluster3
data_cluster3 <- cbind(dplyr::filter(data44, Country_name %in% row.names(cluster3)), id)
data45 <- data.frame(data_cluster3$GDP, data_cluster3$LE, id = id, time = 1980:2020)

###pvecm
PVEC <- pvecm(right_hand_side = data45[1], 
              left_hand_side = data45[2],
              cross_sections = data45[3], 
              time = data45[4], vecm_lags = 1,
              deterministic_long = "none",
              deterministic_short = "drift",
              method = "FM")
PVEC
#видимо, это тест на проверку гипотезы о влиянии А на Б
cause_pvecm(PVEC)
#это коэффициенты долгосрочного равновесия в каждом VECM
PVEC$individual.coefficients
#это t-value для каждого 
#коэффициента долгосрочного равновесия в каждом VECM
PVEC$individual.coefficients.t.statistics
#общий усредненный
PVEC$long.run.vector
#все VECM
PVEC$individual.vecm
#так можно достать один VECM - не работает
PVEC$individual.vecm[[1]]$y
#видимо, это тест на проверку гипотезы о влиянии А на Б
cause_pvecm(PVEC)
#нулевая гипотеза на то, что приведено в первой строке не влияет регрессор

X <- data45 %>% dplyr::mutate(id = as.factor(id)) %>% 
  dplyr::select(- data_cluster3.LE) %>% spread(key = id, value = data_cluster3.GDP)
Y <- data45 %>% dplyr::mutate(id = as.factor(id)) %>% 
  dplyr::select(-data_cluster3.GDP) %>% spread(key = id, value = data_cluster3.LE)
X <- X[, - c(1)] %>% as.matrix()
Y <- Y[, - c(1)] %>% as.matrix()
pedroni99(X, Y)
####есть немного Для четвертого кластера PVECM####
id <- NULL
i <- NULL
for (j in 1:8) {
  i <-  rep(j, 41) 
  id <- c(i, id)
}
sort(id)
data44 <- read_excel("Desktop/data44.xlsx")
data44$LE <- log(data44$LE)
data44$GDP <- log(data44$GDP)
cluster4 <- filter(Data9, cluster == 4)
cluster4
data_cluster4 <- cbind(dplyr::filter(data44, Country_name %in% row.names(cluster4)), id)
data45 <- data.frame(data_cluster4$GDP, data_cluster4$LE, id = id, time = 1980:2020)

###pvecm
PVEC <- pvecm(right_hand_side = data45[1], 
              left_hand_side = data45[2],
              cross_sections = data45[3], 
              time = data45[4], vecm_lags = 1,
              deterministic_long = "none",
              deterministic_short = "drift",
              method = "FM")
PVEC
table <- data.frame(cbind(PVEC$VECMS$data_cluster4.LE$coefficients, 
                          PVEC$VECMS$data_cluster4.GDP$coefficients)) 
colnames(table) <- c("LE", "GDP")
View(table)
#видимо, это тест на проверку гипотезы о влиянии А на Б
cause_pvecm(PVEC)
#это коэффициенты долгосрочного равновесия в каждом VECM
PVEC$individual.coefficients
#это t-value для каждого 
#коэффициента долгосрочного равновесия в каждом VECM
PVEC$individual.coefficients.t.statistics
#общий усредненный
PVEC$long.run.vector
#все VECM
PVEC$individual.vecm
#так можно достать один VECM - не работает
PVEC$individual.vecm[[1]]$y
#видимо, это тест на проверку гипотезы о влиянии А на Б
cause_pvecm(PVEC)
#нулевая гипотеза на то, что приведено в первой строке не влияет регрессор

X <- data45 %>% dplyr::mutate(id = as.factor(id)) %>% 
  dplyr::select(- data_cluster4.LE) %>% spread(key = id, value = data_cluster4.GDP)
Y <- data45 %>% dplyr::mutate(id = as.factor(id)) %>% 
  dplyr::select(-data_cluster4.GDP) %>% spread(key = id, value = data_cluster4.LE)
X <- X[, - c(1)] %>% as.matrix()
Y <- Y[, - c(1)] %>% as.matrix()
pedroni99(X, Y)
