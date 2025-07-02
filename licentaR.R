library(ggplot2)
library(reshape2)
library(urca)
library(tseries)
library(lubridate)
library(patchwork)
library(corrplot)
library(lmtest)
library(car)
library(sandwich)
library(lmtest)
library(dplyr)
library(forecast)
library(vars)
library(stargazer)
library(psych)
library(Metrics)
setwd("C:\\Users\\danru\\Desktop")
getwd()



################ Citire ------------------------------------------------------------------

preturi <- read.csv("pret_inchidere.csv",header = T,sep=",",dec = ".")
pretSP <- read.csv("pret_sp500.csv",header = T,sep = ",",dec = ".")



rataDobanzii <- read.csv("RataDobanzii.csv",header = T,sep = ",",dec = ".")
rataSomajului <- read.csv("RataSomajului.csv",header = T,sep = ",",dec = ".")
ipc <- read.csv("IPC.csv",header = T,sep = ",",dec = ".")
titei <- read.csv("titei_pret_inchidere.csv",header = T,sep = ",",dec = ".")
aur <- read.csv("aur_pret_inchidere.csv",header = T,sep = ",",dec = ".")

aur$Date <- as.Date(aur$Date, format = "%m/%d/%Y")
titei$Date <- as.Date(titei$Date, format = "%m/%d/%Y")

rataDobanzii$Date <- as.Date(paste0(rataDobanzii$Perioada,"-01"),format = "%Y-%b-%d" )
rataDobanzii$Perioada <- NULL
rataDobanzii <- rataDobanzii[,c("Date","RataDobanzii")]

rataSomajului$Date <- as.Date(paste0(rataSomajului$Perioada,"-01"),format = "%Y-%b-%d" )
rataSomajului$Perioada <- NULL
rataSomajului <- rataSomajului[,c("Date","RataSomajului")]

ipc$Date <- parse_date_time(ipc$Periaoda, orders = "Y-b")
ipc$Date <- as.Date(paste0(format(ipc$Date, "%Y-%m"), "-01"))
ipc$Perioada <- NULL
ipc <- ipc[,c("Date","IPC")]

pretSP$Date <- as.Date(pretSP$Date, format = "%m/%d/%Y")
preturi$Date <- as.Date(preturi$Date, format = "%m/%d/%Y")


pret_tehnologie <- preturi[,c("Date","MSFT", "AAPL", "NVDA", "ORCL")]
pret_tehnologie$Media_Tehnologie <- rowMeans(pret_tehnologie[,-1],na.rm = TRUE)
pret_financiar <- preturi[,c("Date","BRK.B", "JPM", "BAC", "WFC")]
pret_financiar$Media_Financiar <- rowMeans(pret_financiar[,-1],na.rm = TRUE)
pret_sanatate <-  preturi[,c("Date","LLY", "JNJ", "UNH", "ABT")]
pret_sanatate$Media_Sanatate <- rowMeans(pret_sanatate[,-1],na.rm = TRUE)
pret_industrial <-  preturi[,c("Date","GE", "RTX", "CAT", "HON")]
pret_industrial$Media_Industrial <- rowMeans(pret_industrial[,-1],na.rm = TRUE)
pret_consumDeBaza <-  preturi[,c("Date","WMT", "COST", "PG", "KO")]
pret_consumDeBaza$Media_ConsumDeBaza <- rowMeans(pret_consumDeBaza[,-1],na.rm = TRUE)
pret_energie <-  preturi[,c("Date","XOM", "CVX", "COP", "WMB")]
pret_energie$Media_Energie <- rowMeans(pret_energie[,-1],na.rm = TRUE)



################### Data frame uri impreuna -------------------------------------------------
variabile_macro1 <- rataDobanzii
variabile_macro1 <- merge(variabile_macro1,rataSomajului, by="Date")
variabile_macro1 <- merge(variabile_macro1, ipc, by = "Date")
variabile_macro1 <- merge(variabile_macro1,titei, by="Date")
variabile_macro1 <- merge(variabile_macro1,aur, by="Date")
variabile_macro1 <- merge(variabile_macro1,pretSP,by="Date")



df_medii1 <- data.frame(
  Date = pret_tehnologie$Date,
  Sectorul_Tehnologiei = pret_tehnologie$Media_Tehnologie,
  Sectorul_Financiar = pret_financiar$Media_Financiar,
  Sectorul_Sanatatii = pret_sanatate$Media_Sanatate,
  Sectorul_Industrial = pret_industrial$Media_Industrial,
  Sectorul_BunurilorDeConsumDeBaza = pret_consumDeBaza$Media_ConsumDeBaza,
  Sectorul_Energetic = pret_energie$Media_Energie
)




df_toate <- data.frame(
  Date = pret_tehnologie$Date,
  Sectorul_Tehnologiei = pret_tehnologie$Media_Tehnologie,
  Sectorul_Financiar = pret_financiar$Media_Financiar,
  Sectorul_Sanatatii = pret_sanatate$Media_Sanatate,
  Sectorul_Industrial = pret_industrial$Media_Industrial,
  Sectorul_BunurilorDeConsumDeBaza = pret_consumDeBaza$Media_ConsumDeBaza,
  Sectorul_Energetic = pret_energie$Media_Energie
)
df_toate <- merge(df_toate,rataDobanzii, by="Date")
df_toate <- merge(df_toate,rataSomajului, by="Date")
df_toate <- merge(df_toate, ipc, by = "Date")
df_toate <- merge(df_toate,titei, by="Date")
df_toate <- merge(df_toate,aur, by="Date")
df_toate <- merge(df_toate,pretSP,by="Date")


################### Grafice -----------------------------------------------------------------
grafic_macro1 <- melt(variabile_macro1, id.vars = "Date")


ggplot(grafic_macro1, aes(x = Date, y = value, color = variable)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~variable, scales = "free_y", ncol = 2) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  labs(title = "Evoluția Variabilelor Macroeconomice şi S&P500",
       x = "Data", y = "Valoare") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))




grafic_medii1 <- melt(df_medii1, id.vars = "Date")


ggplot(grafic_medii1, aes(x = Date, y = value, color = variable)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~variable, scales = "free_y", ncol = 2) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  labs(title = " Evoluția prețurilor medii lunare pe sectoare bursiere",
       x = "Data", y = "Valoare") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))







############### Corelatii ---------------------------------------------------------------
cor_variabile_macro1 <- cor(variabile_macro1[,-1],use = "complete.obs")
corrplot(cor_variabile_macro1,method = "number",type = "upper",tl.cex = 0.8,
         col = colorRampPalette(c("red", "white", "blue"))(200))



cor_medii1 <- cor(df_medii1[,-1],use = "complete.obs")
corrplot(cor_medii1,method = "number",type = "upper",tl.cex = 0.8,
         col = colorRampPalette(c("red", "white", "blue"))(200))



#################Stationaritate pentru Variabilele Macroeconomice si SP ------------------------

dobanda_ts <- ts(rataDobanzii$RataDobanzii,start = c(2005,1),frequency = 12)
somaj_ts <- ts(rataSomajului$RataSomajului,start = c(2005,1),frequency = 12)
ipc_ts <- ts(ipc$IPC,start = c(2005,1),frequency = 12)
aur_ts1 <- ts(aur$aur,start = c(2005,1),frequency = 12)
titei_ts1 <- ts(titei$titei,start = c(2005,1),frequency = 12)
sp_ts1 <- ts(pretSP$SP,start = c(2005,1),frequency = 12)

#ADF
# H0 seria nu este stationara p-value > 0.05
# H1 seria este stationatra p-value < 0.05
adf_dobanda <- ur.df(dobanda_ts,type = "trend",selectlags = c("AIC"))
summary(adf_dobanda)
#t statistic < toate critical values -> Serie nestationara

adf_somaj <- ur.df(somaj_ts,type = "trend",selectlags = c("AIC"))
summary(adf_somaj)
#t statistic < toate critical values -> Serie nestationara

adf_ipc <- ur.df(ipc_ts,type = "trend",selectlags = c("AIC"))
summary(adf_ipc)
#t statistic > toate critical values -> Serie stationara

adf_aur <- ur.df(aur_ts1,type = "trend",selectlags = c("AIC"))
summary(adf_aur)
#t statistic < toate critical values -> Serie nestationara

adf_titei <- ur.df(titei_ts1,type = "trend",selectlags = c("AIC"))
summary(adf_titei)
#t statistic < toate critical values (> la 10%) -> Serie nestationara


adf_sp <- ur.df(sp_ts1,type = "trend",selectlags = c("AIC"))
summary(adf_sp)
#t statistic < toate critical values -> Serie nestationara


################## Diferentiere variabile macro si SP --------------------------------------
dobanda_diff <- diff(dobanda_ts)
somaj_diff <- diff(somaj_ts)

rentabilitate_log <- function(x){
  return(diff(log(x))*100)
}
rentabilitate_aur <- rentabilitate_log(aur$aur)
rentabilitate_titei <- rentabilitate_log(titei$titei)
rentabilitate_sp <- rentabilitate_log(pretSP$SP)

dobanda_diff_df <- data.frame(
  Date = rataDobanzii$Date[-1],
  Dobanda_diff = diff(dobanda_ts)
)

somaj_diff_df <- data.frame(
  Date = rataSomajului$Date[-1],
  Somaj_diff = diff(somaj_ts)
)
rentabilitate_aur <- data.frame(
  Date = aur$Date[-1],  
  Rentabilitate_Aur = rentabilitate_log(aur$aur)
)

rentabilitate_titei <- data.frame(
  Date = titei$Date[-1],
  Rentabilitate_Titei = rentabilitate_log(titei$titei)
)

rentabilitateSP <- data.frame(
  Date = pretSP$Date[-1],
  Rentabilitate_sp = rentabilitate_log(pretSP$SP)
)
aur_ts <- ts(rentabilitate_aur$Rentabilitate_Aur,start = c(2005,1),frequency = 12)
titei_ts <- ts(rentabilitate_titei$Rentabilitate_Titei,start = c(2005,1),frequency = 12)
sp_ts <- ts(rentabilitateSP$Rentabilitate_sp,start = c(2005,1),frequency = 12)



adf_dobanda_diff <- ur.df(dobanda_diff,type = "trend",selectlags = c("AIC"))
summary(adf_dobanda_diff)
#t statistic > toate critical values -> Serie stationara

adf_somaj_diff <- ur.df(somaj_diff,type = "trend",selectlags = c("AIC"))
summary(adf_somaj_diff)
#t statistic > toate critical values -> Serie stationara

adf_aur_diff <- ur.df(aur_ts,type = "trend",selectlags = c("AIC"))
summary(adf_aur_diff)
#t statistic > toate critical values -> Serie stationara

adf_titei_diff <- ur.df(titei_ts,type = "trend",selectlags = c("AIC"))
summary(adf_titei_diff)
#t statistic > toate critical values  -> Serie stationara

adf_sp_diff <- ur.df(rentabilitate_sp,type = "trend",selectlags = c("AIC"))
summary(adf_sp_diff)
#t statistic > toate critical values -> Serie nestationara


variabile_macro <- dobanda_diff_df
variabile_macro <- merge(variabile_macro,somaj_diff_df,by="Date")
variabile_macro <- merge(variabile_macro, ipc, by = "Date")
variabile_macro <- merge(variabile_macro,rentabilitate_titei, by="Date")
variabile_macro <- merge(variabile_macro,rentabilitate_aur, by="Date")
variabile_macro <- merge(variabile_macro,rentabilitateSP,by="Date")


grafic_macro <- melt(variabile_macro, id.vars = "Date")


ggplot(grafic_macro, aes(x = Date, y = value, color = variable)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~variable, scales = "free_y", ncol = 2) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  labs(title = "Evoluția Variabilelor Macroeconomice şi S&P500",
       x = "Data", y = "Valoare") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))





############## Stationaritate pentru Preturi -------------------------------------------

tehnologie_ts1 <- ts(pret_tehnologie$Media_Tehnologie,start = c(2005,1),frequency = 12)
financiar_ts1 <- ts(pret_financiar$Media_Financiar,start = c(2005,1),frequency = 12)
sanatate_ts1 <- ts(pret_sanatate$Media_Sanatate,start = c(2005,1),frequency = 12)
industrial_ts1 <- ts(pret_industrial$Media_Industrial,start = c(2005,1),frequency = 12)
consumDeBaza_ts1 <- ts(pret_consumDeBaza$Media_ConsumDeBaza,start = c(2005,1),frequency = 12)
energetic_ts1 <- ts(pret_energie$Media_Energie,start = c(2005,1),frequency = 12)

#ADF
adf_tehnologie1 <- ur.df(tehnologie_ts1,type = "trend",selectlags = c("AIC"))
summary(adf_tehnologie1)
#t statistic < toate critical values -> Serie nestationara

adf_financiar1 <- ur.df(financiar_ts1,type = "trend",selectlags = c("AIC"))
summary(adf_financiar1)
#t statistic < toate critical values -> Serie nestationara

adf_sanatate1 <- ur.df(sanatate_ts1,type = "trend",selectlags = c("AIC"))
summary(adf_sanatate1)
#t statistic < toate critical values -> Serie nestationara

adf_industrial1 <- ur.df(industrial_ts1,type = "trend",selectlags = c("AIC"))
summary(adf_industrial1)
#t statistic < toate critical values -> Serie nestationara

adf_consumDeBaza1 <- ur.df(consumDeBaza_ts1,type = "trend",selectlags = c("AIC"))
summary(adf_consumDeBaza1)
#t statistic < toate critical values -> Serie nestationara

adf_energetic1 <- ur.df(energetic_ts1,type = "trend",selectlags = c("AIC"))
summary(adf_energetic1)
#t statistic < toate critical values -> Serie nestationara


# #########Diferentiere preturi -------------------------------------------

rentabilitate_tehnologie <- rentabilitate_log(pret_tehnologie$Media_Tehnologie)
rentabilitate_financiar <- rentabilitate_log(pret_financiar$Media_Financiar)
rentabilitate_sanatate <- rentabilitate_log(pret_sanatate$Media_Sanatate)
rentabilitate_industrial <- rentabilitate_log(pret_industrial$Media_Industrial)
rentabilitate_consumDeBaza <- rentabilitate_log(pret_consumDeBaza$Media_ConsumDeBaza)
rentabilitate_energetic <- rentabilitate_log(pret_energie$Media_Energie)

#ADF
adf_tehnologie <- ur.df(rentabilitate_tehnologie,type = "trend",selectlags = c("AIC"))
summary(adf_tehnologie)
#t statistic > toate critical values -> Serie stationara

adf_financiar <- ur.df(rentabilitate_financiar,type = "trend",selectlags = c("AIC"))
summary(adf_financiar)
#t statistic > toate critical values -> Serie stationara

adf_sanatate <- ur.df(rentabilitate_sanatate,type = "trend",selectlags = c("AIC"))
summary(adf_sanatate)
#t statistic > toate critical values -> Serie stationara

adf_industrial <- ur.df(rentabilitate_industrial,type = "trend",selectlags = c("AIC"))
summary(adf_industrial)
#t statistic > toate critical values -> Serie stationara

adf_consumDeBaza <- ur.df(rentabilitate_consumDeBaza,type = "trend",selectlags = c("AIC"))
summary(adf_consumDeBaza)
#t statistic > toate critical values -> Serie stationara

adf_energetic <- ur.df(rentabilitate_energetic,type = "trend",selectlags = c("AIC"))
summary(adf_energetic)
#t statistic > toate critical values -> Serie stationara



rentabilitate_tehnologie_df <- data.frame(
  Date = pret_tehnologie$Date[-1],
  Rentabilitate_Tehnologie = rentabilitate_log(pret_tehnologie$Media_Tehnologie)
)

rentabilitate_financiar_df <- data.frame(
  Date = pret_financiar$Date[-1],
  Rentabilitate_Financiar = rentabilitate_log(pret_financiar$Media_Financiar)
)

rentabilitate_sanatate_df <- data.frame(
  Date = pret_sanatate$Date[-1],
  Rentabilitate_Sanatate = rentabilitate_log(pret_sanatate$Media_Sanatate)
)

rentabilitate_industrial_df <- data.frame(
  Date = pret_industrial$Date[-1],
  Rentabilitate_Industrial = rentabilitate_log(pret_industrial$Media_Industrial)
)

rentabilitate_consumDeBaza_df <- data.frame(
  Date = pret_consumDeBaza$Date[-1],
  Rentabilitate_ConsumDeBaza = rentabilitate_log(pret_consumDeBaza$Media_ConsumDeBaza)
)

rentabilitate_energetic_df <- data.frame(
  Date = pret_energie$Date[-1],
  Rentabilitate_Energetic = rentabilitate_log(pret_energie$Media_Energie)
)


df_medii <- data.frame(
  Date = rentabilitate_tehnologie_df$Date,
  Sectorul_Tehnologiei = rentabilitate_tehnologie_df$Rentabilitate_Tehnologie,
  Sectorul_Financiar = rentabilitate_financiar_df$Rentabilitate_Financiar,
  Sectorul_Sanatatii = rentabilitate_sanatate_df$Rentabilitate_Sanatate,
  Sectorul_Industrial = rentabilitate_industrial_df$Rentabilitate_Industrial,
  Sectorul_BunurilorDeConsumDeBaza = rentabilitate_consumDeBaza_df$Rentabilitate_ConsumDeBaza,
  Sectorul_Energetic = rentabilitate_energetic_df$Rentabilitate_Energetic
)


grafic_medii <- melt(df_medii, id.vars = "Date")


ggplot(grafic_medii, aes(x = Date, y = value, color = variable)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~variable, scales = "free_y", ncol = 2) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  labs(title = "Evoluția Rentabilităţilor medii sectoriale",
       x = "Data", y = "Valoare") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# ##########Grafice Serii Stationare-------------------------------------------------------

grafic_macro <- melt(variabile_macro, id.vars = "Date")


ggplot(grafic_macro, aes(x = Date, y = value, color = variable)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~variable, scales = "free_y", ncol = 2) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Evoluția Variabilelor Macroeconomice şi S&P500",
       x = "Data", y = "Valoare") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))



grafic_medii <- melt(df_medii, id.vars = "Date")


ggplot(grafic_medii, aes(x = Date, y = value, color = variable)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~variable, scales = "free_y", ncol = 2) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Evoluția Rentabilităţilor medii sectoriale",
       x = "Data", y = "Valoare") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))



# ###########DF toate variabilele -----------------------------------------
df_toate1 <- data.frame(
  Date = rentabilitate_tehnologie_df$Date,
  Sectorul_Tehnologiei = rentabilitate_tehnologie_df$Rentabilitate_Tehnologie,
  Sectorul_Financiar = rentabilitate_financiar_df$Rentabilitate_Financiar,
  Sectorul_Sanatatii = rentabilitate_sanatate_df$Rentabilitate_Sanatate,
  Sectorul_Industrial = rentabilitate_industrial_df$Rentabilitate_Industrial,
  Sectorul_BunurilorDeConsumDeBaza = rentabilitate_consumDeBaza_df$Rentabilitate_ConsumDeBaza,
  Sectorul_Energetic = rentabilitate_energetic_df$Rentabilitate_Energetic
)
df_toate1 <- merge(df_toate1,dobanda_diff_df,by="Date")
df_toate1 <- merge(df_toate1,somaj_diff_df,by="Date")
df_toate1 <- merge(df_toate1, ipc, by = "Date")
df_toate1 <- merge(df_toate1,rentabilitate_titei, by="Date")
df_toate1 <- merge(df_toate1,rentabilitate_aur, by="Date")
df_toate1 <- merge(df_toate1,rentabilitateSP,by="Date")

# ###########Statistici Descriptive Variabile Macro -----------------------

summary(variabile_macro)
describe(variabile_macro[,-1])

cor_variabile_macro <- cor(variabile_macro[,-1],use = "complete.obs")
corrplot(cor_variabile_macro,method = "number",type = "upper",tl.cex = 0.8,
         col = colorRampPalette(c("red", "white", "blue"))(200))



# ########Statistici descriptive rentabilitati ----------------------------


summary(df_medii)
describe(df_medii[,-1])


cor_medii <- cor(df_medii[,-1],use = "complete.obs")
corrplot(cor_medii,method = "number",type = "upper",tl.cex = 0.8,
         col = colorRampPalette(c("red", "white", "blue"))(200))





Rf <- mean(dobanda_ts, na.rm = TRUE) / 100  
sharpe_ratio <- function(rentabilitate, rata_fara_risc) {
  rent_exces <- rentabilitate - rata_fara_risc
  mean(rent_exces, na.rm = TRUE) / sd(rent_exces, na.rm = TRUE)
}


sharpe_tehnologie <- sharpe_ratio(df_medii$Sectorul_Tehnologiei, Rf)
sharpe_financiar <- sharpe_ratio(df_medii$Sectorul_Financiar, Rf)
sharpe_sanatate <- sharpe_ratio(df_medii$Sectorul_Sanatatii, Rf)
sharpe_industrial <- sharpe_ratio(df_medii$Sectorul_Industrial, Rf)
sharpe_bunuri_baza <- sharpe_ratio(df_medii$Sectorul_BunurilorDeConsumDeBaza, Rf)
sharpe_energetic <- sharpe_ratio(df_medii$Sectorul_Energetic, Rf)


sharpe_table <- data.frame(
  Sector = c("Tehnologie", "Financiar", "Sănătate", "Industrial", "Consum de bază", "Energie"),
  Sharpe_Ratio = c(
    sharpe_tehnologie,
    sharpe_financiar,
    sharpe_sanatate,
    sharpe_industrial,
    sharpe_bunuri_baza,
    sharpe_energetic
  )
)

print(sharpe_table)
#Consum de baza este cel mai stabil





sharpe_df <- data.frame(
  Sector = c("Tehnologie", "Financiar", "Sănătate", "Industrial", "Consum de bază", "Energie"),
  Sharpe_Ratio = c(0.20959872, 0.11177389, 0.20769324, 0.06855375, 0.21810746, 0.06273474)
)


sharpe_df$Sector <- factor(sharpe_df$Sector, levels = sharpe_df$Sector[order(sharpe_df$Sharpe_Ratio)])

# Plotul
ggplot(sharpe_df, aes(x = Sector, y = Sharpe_Ratio)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(Sharpe_Ratio, 3)), vjust = -0.5, size = 4) +
  labs(
    title = "Coeficientul Sharpe pe sectoare",
    x = "Sector",
    y = "Sharpe Ratio"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


# ######COR toate ---------------------------------------------------------

cor_toate <- cor(df_toate1[,-1],use = "complete.obs")
corrplot(cor_toate,method = "number",type = "upper",tl.cex = 0.8,
         col = colorRampPalette(c("red", "white", "blue"))(200))


# ###############REGRESII -------------------------------------------------

df_regresie_tehnologie <- merge(df_medii[, c("Date", "Sectorul_Tehnologiei")], variabile_macro, by = "Date")
df_regresie_financiar <- merge(df_medii[, c("Date", "Sectorul_Financiar")], variabile_macro, by = "Date")
df_regresie_sanatate <- merge(df_medii[, c("Date", "Sectorul_Sanatatii")], variabile_macro, by = "Date")
df_regresie_industrial <- merge(df_medii[, c("Date", "Sectorul_Industrial")], variabile_macro, by = "Date")
df_regresie_consumBaza <- merge(df_medii[, c("Date", "Sectorul_BunurilorDeConsumDeBaza")], variabile_macro, by = "Date")
df_regresie_energetic <- merge(df_medii[, c("Date", "Sectorul_Energetic")], variabile_macro, by = "Date")






regresie_tehnologie <- lm(Sectorul_Tehnologiei~Dobanda_diff+Somaj_diff+IPC+Rentabilitate_Aur+Rentabilitate_Titei+Rentabilitate_sp,data = df_regresie_tehnologie)
summary(regresie_tehnologie)
nobs(regresie_tehnologie)
#Rentabilitate_sp 1.146   p < 2e-16	Foarte semnificativ – randamentul S&P 500 influențează pozitiv sectorul Tehnologic:
#o creștere cu 1 punct procentual a S&P500 implică o creștere de aproximativ 1.15% a sectorului tehnologic.

#R² = 0.6411 → modelul explică 64,11% din variația rentabilității sectorului tehnologic.
#Adjusted R² = 0.6301 → după ajustarea pentru numărul de variabile, modelul rămâne puternic explicativ.
#F-statistic: 58.34, p < 2.2e-16 → modelul este semnificativ per ansamblu.

#Normalitatea reziduurilor
#H₀: Reziduurile sunt distribuite normal.
#H₁: Reziduurile NU sunt distribuite normal.

jarque.bera.test(regresie_tehnologie$residuals)
#p-value = 0.04864 <0.05 Nu sunt normal distribuite

#Homoscedasticitate (varianță constantă a erorilor)
#H₀: Varianța reziduurilor este constantă (nu există heteroscedasticitate).
#H₁: Varianța reziduurilor este variabilă.
bptest(regresie_tehnologie)
#p-value = 0.462 > 0.05 Nu exista heteroscedasticitate

#Autocorelarea erorilor (relevantă doar pentru date în serie temporală)
#H₀: Nu există autocorelare între erori.
#H₁: Erorile sunt autocorelate.
dwtest(regresie_tehnologie)
#p-value = 0.8553 >0.05 nu exista autocorelare


#Multicoliniaritate între variabile explicative
vif(regresie_tehnologie) #<5
#nu exista multicolinaritate



regresie_financiar <- lm(Sectorul_Financiar~Dobanda_diff+Somaj_diff+IPC+Rentabilitate_Aur+Rentabilitate_Titei+Rentabilitate_sp,data = df_regresie_financiar)
summary(regresie_financiar)
#Dobanda_diff	4.504	p=0.000113 ***	Foarte semnificativ →
#o creștere cu 1 punct procentual a dobânzii conduce la o creștere de 4.5% a rentabilității sectorului financiar.
#Are sens economic: băncile câștigă mai mult când dobânzile cresc.
#Rentabilitate_sp	0.935 p	< 2e-16	Foarte semnificativ → 
#randamentul S&P 500 are efect direct asupra sectorului financiar, corelat pozitiv.

#R² = 0.5502 → modelul explică 55,02% din variația rentabilității sectorului financiar.
#Adjusted R² = 0.5364 → bună explicativitate după ajustarea numărului de variabile.
#F-statistic: 39.96, p < 2.2e-16 → modelul este semnificativ per ansamblu.

#Normalitatea reziduurilor
#H₀: Reziduurile sunt distribuite normal.
#H₁: Reziduurile NU sunt distribuite normal.

jarque.bera.test(regresie_financiar$residuals)
#p-value < 2.2e-16 Nu sunt normal distribuite

#Homoscedasticitate (varianță constantă a erorilor)
#H₀: Varianța reziduurilor este constantă (nu există heteroscedasticitate).
#H₁: Varianța reziduurilor este variabilă.
bptest(regresie_financiar)
#p-value = 0.02502 < 0.05  exista heteroscedasticitate

#Autocorelarea erorilor (relevantă doar pentru date în serie temporală)
#H₀: Nu există autocorelare între erori.
#H₁: Erorile sunt autocorelate.
dwtest(regresie_financiar)
#p-value = 0.8436 >0.05 nu exista autocorelare


#Multicoliniaritate între variabile explicative
vif(regresie_financiar) #<5
#nu exista multicolinaritate


regresie_sanatate <- lm(Sectorul_Sanatatii~Dobanda_diff+Somaj_diff+IPC+Rentabilitate_Aur+Rentabilitate_Titei+Rentabilitate_sp,data = df_regresie_sanatate)
summary(regresie_sanatate)
#(Intercept)	0.529	p=0.0398	 Semnificativă la 5% → rentabilitate medie de ~0.53% când ceilalți factori sunt zero.
#Rentabilitate_sp	0.604	p<2e-16	Foarte semnificativă → o creștere cu 1% în S&P 500 implică o creștere de 0.6% în sectorul sănătății

#R² = 0.396 → ~40% din variația rentabilității este explicată de model.
#Adjusted R² = 0.377 → încă acceptabil pentru un model de regresie cu mai mulți predictori.
#F-statistic = 21.41, p < 2.2e-16 → modelul este semnificativ per ansamblu.


#Normalitatea reziduurilor
#H₀: Reziduurile sunt distribuite normal.
#H₁: Reziduurile NU sunt distribuite normal.

jarque.bera.test(regresie_sanatate$residuals)
#p-value = 0.4274 >0.05  sunt normal distribuite

#Homoscedasticitate (varianță constantă a erorilor)
#H₀: Varianța reziduurilor este constantă (nu există heteroscedasticitate).
#H₁: Varianța reziduurilor este variabilă.
bptest(regresie_sanatate)
#p-value = 0.7442 > 0.05 Nu exista heteroscedasticitate

#Autocorelarea erorilor (relevantă doar pentru date în serie temporală)
#H₀: Nu există autocorelare între erori.
#H₁: Erorile sunt autocorelate.
dwtest(regresie_sanatate)
#p-value = 0.8783 >0.05 nu exista autocorelare


#Multicoliniaritate între variabile explicative
vif(regresie_sanatate) #<5
#nu exista multicolinaritate


regresie_industrial <- lm(Sectorul_Industrial~Dobanda_diff+Somaj_diff+IPC+Rentabilitate_Aur+Rentabilitate_Titei+Rentabilitate_sp,data = df_regresie_industrial)
summary(regresie_industrial)
#Rentabilitate_Titei	0.045	p=0.084	️ Aproape semnificativ la 10% (poate avea un efect marginal pozitiv asupra sectorului industrial)
#Rentabilitate_sp	1.208	p<2e-16	 Foarte semnificativă – o creștere cu 1% în S&P 500 se asociază cu o creștere de 1.2% în sectorul industrial

#R² = 0.7295 → Modelul explică ~73% din variația rentabilității sectorului industrial.
#Adjusted R² = 0.7212 → Putere explicativă excelentă, mai mare decât în alte sectoare.
#F-statistic = 88.09, p < 2.2e-16 → Modelul este foarte semnificativ per ansamblu.


#Normalitatea reziduurilor
#H₀: Reziduurile sunt distribuite normal.
#H₁: Reziduurile NU sunt distribuite normal.

jarque.bera.test(regresie_industrial$residuals)
#p-value = 0.006599 <0.05 nu sunt normal distribuite

#Homoscedasticitate (varianță constantă a erorilor)
#H₀: Varianța reziduurilor este constantă (nu există heteroscedasticitate).
#H₁: Varianța reziduurilor este variabilă.
bptest(regresie_industrial)
#p-value = 0.05115 > 0.05 Nu exista heteroscedasticitate

#Autocorelarea erorilor (relevantă doar pentru date în serie temporală)
#H₀: Nu există autocorelare între erori.
#H₁: Erorile sunt autocorelate.
dwtest(regresie_industrial)
#p-value = 0.9456 >0.05 nu exista autocorelare


#Multicoliniaritate între variabile explicative
vif(regresie_industrial) #<5
#nu exista multicolinaritate


regresie_consumDeBaza <- lm(Sectorul_BunurilorDeConsumDeBaza~Dobanda_diff+Somaj_diff+IPC+Rentabilitate_Aur+Rentabilitate_Titei+Rentabilitate_sp,data = df_regresie_consumBaza)
summary(regresie_consumDeBaza)#(Intercept)	0.419	p=0.077	️ Marginal semnificativ (nivel 10%)
#Rentabilitate_Titei	-0.092	p=7.6e-05	 Semnificativ (p < 0.001) → efect negativ
#Rentabilitate_sp	0.753	p<2e-16	Foarte semnificativ (p < 0.001) → efect pozitiv

#R² = 0.5265 → Modelul explică ~52.6% din variația rentabilității sectorului de consum de bază.
#Adjusted R² = 0.512 → Putere explicativă bună.
#F-statistic = 36.33, p < 2.2e-16 → Modelul este foarte semnificativ per ansamblu.


#Normalitatea reziduurilor
#H₀: Reziduurile sunt distribuite normal.
#H₁: Reziduurile NU sunt distribuite normal.

jarque.bera.test(regresie_consumDeBaza$residuals)
#p-value = 0.8739 > 0.05  sunt normal distribuite

#Homoscedasticitate (varianță constantă a erorilor)
#H₀: Varianța reziduurilor este constantă (nu există heteroscedasticitate).
#H₁: Varianța reziduurilor este variabilă.
bptest(regresie_consumDeBaza)
#p-value = 0.1256 > 0.05 Nu exista heteroscedasticitate

#Autocorelarea erorilor (relevantă doar pentru date în serie temporală)
#H₀: Nu există autocorelare între erori.
#H₁: Erorile sunt autocorelate.
dwtest(regresie_consumDeBaza)
#p-value = 0.4673 >0.05 nu exista autocorelare


#Multicoliniaritate între variabile explicative
vif(regresie_consumDeBaza) #<5
#nu exista multicolinaritate




regresie_energetic <- lm(Sectorul_Energetic~Dobanda_diff+Somaj_diff+IPC+Rentabilitate_Aur+Rentabilitate_Titei+Rentabilitate_sp,data = df_regresie_energetic)
summary(regresie_energetic)
#Somaj_diff	3.178	0.006	 Semnificativ (p < 0.01) → efect pozitiv
#Rentabilitate_Titei	0.221	 p=1.61e-08	 Foarte semnificativ → efect pozitiv
# Rentabilitate_sp	0.658 	p=1.19e-12  Foarte semnificativ → efect pozitiv

#R² = 0.4494 → ~45% din variația rentabilității sectorului energetic este explicată de model.
#Adjusted R² = 0.4325 → Modelul este destul de solid.
#F-statistic = 26.66, p < 2.2e-16 → Modelul este semnificativ global.


#Concluzii
#Rentabilitatea S&P 500 este semnificativă în toate sectoarele, fiind principalul predictor al randamentelor sectoriale.
#️ Rentabilitatea țițeiului influențează sectoarele energiei, industrial și consum de bază.
# Dobânda este semnificativă doar pentru sectorul financiar (pozitiv).
#Aurul și IPC-ul nu au fost semnificativi în niciun model.
# Puterea explicativă (R²) este ridicată în tehnologie și industrie, dar mai scăzută în sănătate și energie.



#Normalitatea reziduurilor
#H₀: Reziduurile sunt distribuite normal.
#H₁: Reziduurile NU sunt distribuite normal.

jarque.bera.test(regresie_energetic$residuals)
#p-value = 1.437e-06 < 0.05 nu sunt normal distribuite

#Homoscedasticitate (varianță constantă a erorilor)
#H₀: Varianța reziduurilor este constantă (nu există heteroscedasticitate).
#H₁: Varianța reziduurilor este variabilă.
bptest(regresie_energetic)
#p-value = 0.07756 > 0.05 Nu exista heteroscedasticitate

#Autocorelarea erorilor (relevantă doar pentru date în serie temporală)
#H₀: Nu există autocorelare între erori.
#H₁: Erorile sunt autocorelate.
dwtest(regresie_energetic)
#p-value = 0.3355 >0.05 nu exista autocorelare


#Multicoliniaritate între variabile explicative
vif(regresie_energetic) #<5
#nu exista multicolinaritate





summary(regresie_tehnologie)
summary(regresie_financiar)
summary(regresie_sanatate)
summary(regresie_industrial)
summary(regresie_consumDeBaza)
summary(regresie_energetic)
# ###########VAR Tehnologie----------------------------------------------------------


max_date <- max(df_regresie_tehnologie$Date)


cut_date <- seq(max_date, length = 2, by = "-6 months")[2]

# Separare în train și test
data_train_tehnologie <- subset(df_regresie_tehnologie, Date < cut_date)
data_test_tehnologie  <- subset(df_regresie_tehnologie, Date >= cut_date)



train_ts_tehnologie <- ts(data_train_tehnologie[,-1],
                          start = c(year(min(data_train_tehnologie$Date)), month(min(data_train_tehnologie$Date))),
                          frequency = 12)

test_ts_tehnologie <- ts(data_test_tehnologie[,-1],
                         start = c(year(min(data_test_tehnologie$Date)), month(min(data_test_tehnologie$Date))),
                         frequency = 12)



lags_tehnologie <- VARselect(train_ts_tehnologie,lag.max = 12,type = "const")
lags_tehnologie$selection#Conform tuturor criteriilor alegem lagul 1
lags_tehnologie2 <- VARselect(train_ts_tehnologie,lag.max = 8,type = "const")
lags_tehnologie2$selection


model_var_train_tehnologie <- VAR(train_ts_tehnologie,p=1,type = "const")
summary(model_var_train_tehnologie)
#Roots of the characteristic polynomial:
#  0.4674 0.4674 0.4546 0.1492 0.1492 0.0667 0.0667 <1  Modelul este stabil
#Dobanda_diff.l1                -3.071* 
#Pe termen scurt, dobânda pare a avea un impact negativ asupra sectorului tehnologic, 
#dar efectul nu este puternic semnificativ.
#R2                              0.051
#Adjusted R2                     0.016
#F Statistic (df = 7; 188)       1.450
#Corelatia dintre Sector si Rentabilitate_SP este 0.79
stargazer(model_var_train_tehnologie[['varresult']],type = 'text')

#Autocorelarea reziduurilor - Portmanteau Test
#H0: Nu există autocorelare (reziduurile sunt zgomot alb)
#H1: Există autocorelare
serial.test(model_var_train_tehnologie, lags.pt = 6, type = "PT.asymptotic")
#p-value = p-value = 0.01026 < 0.05 Reziduurile  sunt autocorelate
#                   

#Test de heteroscedasticitate - ARCH Test
#H0: Varianța reziduurilor este constantă (nu există heteroscedasticitate)
#H1: Varianța reziduurilor este variabilă (efecte ARCH)
arch.test(model_var_train_tehnologie, lags.multi = 5, multivariate.only = TRUE)
#p-value = 9.847e-06 <0.05  Exista efecte ARCH
                  
#Normalitatea reziduurilor - Jarque-Bera test multivariat
#H0: Reziduurile sunt distribuite normal
#H1: Reziduurile nu sunt distribuite normal
normality.test(model_var_train_tehnologie, multivariate.only = TRUE)
#p-value < 2.2e-16 Reziduurile nu sunt normal distribuite
#Skewness p-value < 2.2e-16
#Kurtosis p-value < 2.2e-16


# Ipotezele cauzalitatii Granger
# H0: valorile cu lag ale lui X, nu explica variatia in Y 
# H1: valorile cu lag ale lui X, explica variatia in Y

# H0: variabila X nu prezinta cauzalitate Granger pentru variabila Y
# H1: Variabila X prezinta cauzalitate Granger pentru variabila Y
Granger_Tehnologie <- causality(model_var_train_tehnologie,cause = "Sectorul_Tehnologiei")
Granger_Tehnologie$Granger
#Granger (lag)	p = 0.1982	Nu există cauzalitate Granger → Sectorul Tehnologiei nu prezice celelalte
causalitate_dobanda_tehnologie <- causality(model_var_train_tehnologie, cause = "Dobanda_diff")$Granger
causalitate_somaj_tehnologie <- causality(model_var_train_tehnologie, cause = "Somaj_diff")
causalitate_ipc_tehnologie <- causality(model_var_train_tehnologie, cause = "IPC")
causalitate_titei_tehnologie <- causality(model_var_train_tehnologie, cause = "Rentabilitate_Titei")
causalitate_aur_tehnologie <- causality(model_var_train_tehnologie, cause = "Rentabilitate_Aur")
causalitate_sp_tehnologie <- causality(model_var_train_tehnologie, cause = "Rentabilitate_sp")

causalitate_dobanda_tehnologie$Granger
#p-value = 0.1658 Nu exista
causalitate_somaj_tehnologie$Granger
#p-value = 0.8782 Nu exista
causalitate_ipc_tehnologie$Granger
#p-value = 0.02059 Exista
causalitate_titei_tehnologie$Granger
#p-value = 0.000194 Exista
causalitate_aur_tehnologie$Granger
#p-value = 0.02503 Exista
causalitate_sp_tehnologie$Granger
#-value = 0.1037 Nu exusta



VARselect(train_ts_tehnologie,lag.max = 12,type = "const")
model_var_dob_tech1 <- VAR(train_ts_tehnologie[, c("Sectorul_Tehnologiei", "Dobanda_diff")], p = 1, type = "const")
causality(model_var_dob_tech, cause = "Dobanda_diff")$Granger

model_var_dob_tech2 <- VAR(train_ts_tehnologie[, c("Sectorul_Tehnologiei", "Somaj_diff")], p = 1, type = "const")
causality(model_var_dob_tech2, cause = "Somaj_diff")$Granger

model_var_dob_tech3 <- VAR(train_ts_tehnologie[, c("Sectorul_Tehnologiei", "IPC")], p = 1, type = "const")
causality(model_var_dob_tech3, cause = "IPC")$Granger


model_var_dob_tech4 <- VAR(train_ts_tehnologie[, c("Sectorul_Tehnologiei", "Rentabilitate_Titei")], p = 1, type = "const")
causality(model_var_dob_tech4, cause = "Rentabilitate_Titei")$Granger

model_var_dob_tech5 <- VAR(train_ts_tehnologie[, c("Sectorul_Tehnologiei", "Rentabilitate_Aur")], p = 1, type = "const")
causality(model_var_dob_tech5, cause = "Rentabilitate_Aur")$Granger

model_var_dob_tech6 <- VAR(train_ts_tehnologie[, c("Sectorul_Tehnologiei", "Rentabilitate_sp")], p = 1, type = "const")
causality(model_var_dob_tech6, cause = "Rentabilitate_sp")$Granger









# IRF pentru toate variabilele asupra Sectorului_Tehnologiei
irf_tehnologie_all <- irf(model_var_train_tehnologie,
                          impulse = colnames(train_ts_tehnologie)[-1], 
                          response = "Sectorul_Tehnologiei",
                          n.ahead = 12, # n.ahead = perioade in viitor
                          boot = TRUE, # boot = TRUE pentru a crea intervale de incredere
                          ci = 0.95) # ci - nivelul de semnificatie

irf_tehnologie_all <- irf(model_var_train_tehnologie,
                          impulse = "Dobanda_diff", 
                          response = "Sectorul_Tehnologiei",
                          n.ahead = 12, # n.ahead = perioade in viitor
                          boot = TRUE, # boot = TRUE pentru a crea intervale de incredere
                          ci = 0.95) 

# Grafic IRF pentru toate variabilele
plot(irf_tehnologie_all,
     main = "Funcții de răspuns la impuls (IRF) – Sector Tehnologie")



# FEVD pentru Sectorul_Tehnologiei
fevd_tehnologie <- fevd(model_var_train_tehnologie, n.ahead = 12)

# Grafic FEVD
plot(fevd_tehnologie, names = "Sectorul_Tehnologiei",
     main = "Decompozitia erorii de prognoza (FEVD) – Sector Tehnologie")







h <- nrow(data_test_tehnologie)
forecast_var_tehnologie <- predict(model_var_train_tehnologie, n.ahead = h, ci = 0.95)
forecast_tehnologie <- forecast_var_tehnologie$fcst$Sectorul_Tehnologiei

plot(forecast_var_tehnologie,name="Sectorul_Tehnologiei")

# Date forecast într-un data frame
df_forecast <- data.frame(
  Date = data_test_tehnologie$Date,
  Forecast = forecast_tehnologie[, "fcst"],
  Lower = forecast_tehnologie[, "lower"],
  Upper = forecast_tehnologie[, "upper"]
)

# Grafic prognoză VAR simplu
ggplot(df_forecast, aes(x = Date, y = Forecast)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "blue", alpha = 0.2) +
  labs(
    title = "Prognoza Sectorului Tehnologic (VAR)",
    y = "Rentabilitate prognozată",
    x = "Dată"
  ) +
  theme_minimal()

# Pregătim datele pentru comparație cu realul
forecast_df <- data.frame(
  Data = data_test_tehnologie$Date,
  Real = data_test_tehnologie$Sectorul_Tehnologiei,
  Previziune = forecast_tehnologie[, "fcst"],
  Low_95 = forecast_tehnologie[, "lower"],
  High_95 = forecast_tehnologie[, "upper"]
)

# Grafic comparativ: Previziune vs Real
ggplot(forecast_df, aes(x = Data)) +
  geom_line(aes(y = Real), color = "black", size = 1.2) +
  geom_line(aes(y = Previziune), color = "blue", linetype = "dashed", size = 1) +
  geom_ribbon(aes(ymin = Low_95, ymax = High_95), fill = "blue", alpha = 0.2) +
  labs(title = "Prognoză vs Valori Reale – Sectorul Tehnologic",
       y = "Rentabilitate",
       x = "Dată") +
  theme_minimal()




# Extragem valorile reale și prognozate
real_values <- forecast_df$Real
predicted_values <- forecast_df$Previziune

# Calculăm erorile
rmse_value <- rmse(real_values, predicted_values)
mae_value <- mae(real_values, predicted_values)

# Afișăm rezultatele
cat("RMSE (Root Mean Squared Error):", round(rmse_value, 4), "\n")
cat("MAE (Mean Absolute Error):", round(mae_value, 4), "\n")



# #####VAR  Financiar---------------------------------------------------------------

max_date_financiar <- max(df_regresie_financiar$Date)
cut_date_financiar <- seq(max_date, length = 2, by = "-6 months")[2]

data_train_financiar <- subset(df_regresie_financiar, Date < cut_date_financiar)
data_test_financiar  <- subset(df_regresie_financiar, Date >= cut_date_financiar)

train_ts_financiar <- ts(data_train_financiar[,-1],
                         start = c(year(min(data_train_financiar$Date)), month(min(data_train_financiar$Date))),
                         frequency = 12)

test_ts_financiar <- ts(data_test_financiar[,-1],
                        start = c(year(min(data_test_financiar$Date)), month(min(data_test_financiar$Date))),
                        frequency = 12)

# 3. Selectare lag optim
lags_financiar <- VARselect(train_ts_financiar, lag.max = 12, type = "const")
lags_financiar$selection #lag1

model_var_train_financiar <- VAR(train_ts_financiar, p = 1, type = "const")
summary(model_var_train_financiar)

stargazer(model_var_train_financiar[['varresult']],type = 'text')
#Roots of the characteristic polynomial:
# 0.4714 0.4491 0.4491 0.1852 0.1852 0.04083 0.00385 <1  Modelul este stabil
#Sectorul_Financiar.l1          -0.217**
#Rentabilitate_Aur.l1           -0.218***
#Rentabilitate_sp.l1             0.266*

#R2                              0.077
#Adjusted R2                     0.043
#F Statistic (df = 7; 188)       2.256**
#Corelatia dintre Sector si Rentabilitate_SP este 0.69

#Autocorelarea reziduurilor - Portmanteau Test
#H0: Nu există autocorelare (reziduurile sunt zgomot alb)
#H1: Există autocorelare
serial.test(model_var_train_financiar, lags.pt = 6, type = "PT.asymptotic")
#p-value = 0.0002992 < 0.05 Reziduurile  sunt autocorelate

#Test de heteroscedasticitate - ARCH Test
#H0: Varianța reziduurilor este constantă (nu există heteroscedasticitate)
#H1: Varianța reziduurilor este variabilă (efecte ARCH)
arch.test(model_var_train_financiar, lags.multi = 5, multivariate.only = TRUE)
#p-value = 1.402e-07 <0.05  Exista efecte ARCH

#Normalitatea reziduurilor - Jarque-Bera test multivariat
#H0: Reziduurile sunt distribuite normal
#H1: Reziduurile nu sunt distribuite normal
normality.test(model_var_train_financiar, multivariate.only = TRUE)
#p-value < 2.2e-16 Reziduurile nu sunt normal distribuite
#Skewness p-value < 2.2e-16
#Kurtosis p-value < 2.2e-16


# Ipotezele cauzalitatii Granger
# H0: valorile cu lag ale lui X, nu explica variatia in Y 
# H1: valorile cu lag ale lui X, explica variatia in Y

# H0: variabila X nu prezinta cauzalitate Granger pentru variabila Y
# H1: Variabila X prezinta cauzalitate Granger pentru variabila Y
Granger_Financiar <- causality(model_var_train_financiar,cause = "Sectorul_Financiar")
Granger_Financiar$Granger
#Granger (lag)	p-value = 0.1152	Nu există cauzalitate Granger 
causalitate_dobanda_financiar <- causality(model_var_train_financiar, cause = "Dobanda_diff")
causalitate_somaj_financiar <- causality(model_var_train_financiar, cause = "Somaj_diff")
causalitate_ipc_financiar <- causality(model_var_train_financiar, cause = "IPC")
causalitate_titei_financiar <- causality(model_var_train_financiar, cause = "Rentabilitate_Titei")
causalitate_aur_financiar <- causality(model_var_train_financiar, cause = "Rentabilitate_Aur")
causalitate_sp_financiar <- causality(model_var_train_financiar, cause = "Rentabilitate_sp")

causalitate_dobanda_financiar$Granger
#p-value = 0.3063 Nu exista
causalitate_somaj_financiar$Granger
#p-value = 0.7146 Nu exista
causalitate_ipc_financiar$Granger
#p-value = 0.02523 Exista
causalitate_titei_financiar$Granger
#p-value = 6.294e-05 Exista
causalitate_aur_financiar$Granger
#p-value = 0.02183 Exista
causalitate_sp_financiar$Granger
#p-value = 0.1779 Nu exusta



VARselect(train_ts_financiar,lag.max = 12,type = "const")
model_var_financiar1 <- VAR(train_ts_financiar[, c("Sectorul_Financiar", "Dobanda_diff")], p = 1, type = "const")
causality(model_var_financiar1, cause = "Dobanda_diff")$Granger

model_var_financiar2 <- VAR(train_ts_financiar[, c("Sectorul_Financiar", "Somaj_diff")], p = 1, type = "const")
causality(model_var_financiar2, cause = "Somaj_diff")$Granger

model_var_financiar3 <- VAR(train_ts_financiar[, c("Sectorul_Financiar", "IPC")], p = 1, type = "const")
causality(model_var_financiar3, cause = "IPC")$Granger


model_var_financiar4 <- VAR(train_ts_financiar[, c("Sectorul_Financiar", "Rentabilitate_Titei")], p = 1, type = "const")
causality(model_var_financiar4, cause = "Rentabilitate_Titei")$Granger

model_var_financiar5 <- VAR(train_ts_financiar[, c("Sectorul_Financiar", "Rentabilitate_Aur")], p = 1, type = "const")
causality(model_var_financiar5, cause = "Rentabilitate_Aur")$Granger

model_var_financiar6 <- VAR(train_ts_financiar[, c("Sectorul_Financiar", "Rentabilitate_sp")], p = 1, type = "const")
causality(model_var_financiar6, cause = "Rentabilitate_sp")$Granger





h <- nrow(data_test_financiar)
forecast_var_financiar <- predict(model_var_train_financiar, n.ahead = h, ci = 0.95)
forecast_financiar <- forecast_var_financiar$fcst$Sectorul_Financiar

plot(forecast_var_financiar,name="Sectorul_Financiar")

df_forecast_financiar <- data.frame(
  Date = data_test_financiar$Date,
  Forecast = forecast_financiar[, "fcst"],
  Lower = forecast_financiar[, "lower"],
  Upper = forecast_financiar[, "upper"]
)


# Grafic forecast simplu
ggplot(df_forecast_financiar, aes(x = Date, y = Forecast)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "darkgreen", alpha = 0.2) +
  labs(title = "Prognoza Sectorului Financiar (VAR)",
       y = "Rentabilitate prognozată",
       x = "Dată") +
  theme_minimal()

# Comparație cu valori reale
forecast_df_financiar <- data.frame(
  Data = data_test_financiar$Date,
  Real = data_test_financiar$Sectorul_Financiar,
  Previziune = forecast_financiar[, "fcst"],
  Low_95 = forecast_financiar[, "lower"],
  High_95 = forecast_financiar[, "upper"]
)

ggplot(forecast_df_financiar, aes(x = Data)) +
  geom_line(aes(y = Real), color = "black", size = 1.2) +
  geom_line(aes(y = Previziune), color = "darkgreen", linetype = "dashed", size = 1) +
  geom_ribbon(aes(ymin = Low_95, ymax = High_95), fill = "darkgreen", alpha = 0.2) +
  labs(title = "Prognoză vs Valori reale – Sectorul Financiar",
       y = "Rentabilitate",
       x = "Dată") +
  theme_minimal()

# Erori forecast
real_values_fin <- forecast_df_financiar$Real
predicted_values_fin <- forecast_df_financiar$Previziune

rmse_fin <- rmse(real_values_fin, predicted_values_fin)
mae_fin <- mae(real_values_fin, predicted_values_fin)

cat("RMSE:", round(rmse_fin, 4), "\n")
cat("MAE:", round(mae_fin, 4), "\n")



# ###### VAR SANATATE -----------------------------------------------------

max_date_sanatate <- max(df_regresie_sanatate$Date)
cut_date_sanatate <- seq(max_date_sanatate, length = 2, by = "-6 months")[2]

# 2. Împărțirea în seturi de training și test
data_train_sanatate <- subset(df_regresie_sanatate, Date < cut_date_sanatate)
data_test_sanatate  <- subset(df_regresie_sanatate, Date >= cut_date_sanatate)

# 3. Conversie în obiecte ts
train_ts_sanatate <- ts(data_train_sanatate[,-1],
                        start = c(year(min(data_train_sanatate$Date)), month(min(data_train_sanatate$Date))),
                        frequency = 12)

test_ts_sanatate <- ts(data_test_sanatate[,-1],
                       start = c(year(min(data_test_sanatate$Date)), month(min(data_test_sanatate$Date))),
                       frequency = 12)

lags_sanatate <- VARselect(train_ts_sanatate, lag.max = 12, type = "const")
lags_sanatate$selection

# 5. Estimare model VAR
model_var_train_sanatate <- VAR(train_ts_sanatate, p = 1, type = "const")
summary(model_var_train_sanatate)
stargazer(model_var_train_sanatate[['varresult']],type = 'text')
#Roots of the characteristic polynomial:
# 0.4561 0.4497 0.4497 0.1554 0.1554 0.1474 0.1474 <1  Modelul este stabil
#Dobanda_diff.l1                -2.283**
#Rentabilitate_Titei.l1          0.052*
#Rentabilitate_Aur.l1           -0.135**

#R2                              0.073
#Adjusted R2                     0.039
#F Statistic (df = 7; 188)      2.121**
#Corelatia dintre Sector si Rentabilitate_SP este 0.600989



#Autocorelarea reziduurilor - Portmanteau Test
#H0: Nu există autocorelare (reziduurile sunt zgomot alb)
#H1: Există autocorelare
serial.test(model_var_train_sanatate, lags.pt = 6, type = "PT.asymptotic")
# p-value = 0.0007572 < 0.05 Reziduurile  sunt autocorelate

#Test de heteroscedasticitate - ARCH Test
#H0: Varianța reziduurilor este constantă (nu există heteroscedasticitate)
#H1: Varianța reziduurilor este variabilă (efecte ARCH)
arch.test(model_var_train_sanatate, lags.multi = 5, multivariate.only = TRUE)
#p-value = 1.92e-08 <0.05  Exista efecte ARCH

#Normalitatea reziduurilor - Jarque-Bera test multivariat
#H0: Reziduurile sunt distribuite normal
#H1: Reziduurile nu sunt distribuite normal
normality.test(model_var_train_sanatate, multivariate.only = TRUE)
#p-value < 2.2e-16 Reziduurile nu sunt normal distribuite
#Skewness p-value < 2.2e-16
#Kurtosis p-value < 2.2e-16


# Ipotezele cauzalitatii Granger
# H0: valorile cu lag ale lui X, nu explica variatia in Y 
# H1: valorile cu lag ale lui X, explica variatia in Y

# H0: variabila X nu prezinta cauzalitate Granger pentru variabila Y
# H1: Variabila X prezinta cauzalitate Granger pentru variabila Y
Granger_Sanatate <- causality(model_var_train_sanatate,cause = "Sectorul_Sanatatii")
Granger_Sanatate$Granger
#Granger (lag)	p-value = 0.1725	Nu există cauzalitate Granger 
causalitate_dobanda_sanatate <- causality(model_var_train_sanatate, cause = "Dobanda_diff")
causalitate_somaj_sanatate <- causality(model_var_train_sanatate, cause = "Somaj_diff")
causalitate_ipc_sanatate <- causality(model_var_train_sanatate, cause = "IPC")
causalitate_titei_sanatate <- causality(model_var_train_sanatate, cause = "Rentabilitate_Titei")
causalitate_aur_sanatate <- causality(model_var_train_sanatate, cause = "Rentabilitate_Aur")
causalitate_sp_sanatate <- causality(model_var_train_sanatate, cause = "Rentabilitate_sp")

causalitate_dobanda_sanatate$Granger
#p-value = 0.1448 Nu exista
causalitate_somaj_sanatate$Granger
#p-value = 0.802 Nu exista
causalitate_ipc_sanatate$Granger
#p-value = 0.04074 Exista
causalitate_titei_sanatate$Granger
#p-value = 8.67e-05 Exista
causalitate_aur_sanatate$Granger
#p-value = 0.02477 Exista
causalitate_sp_sanatate$Granger
#p-value = 0.8051 Nu exusta



VARselect(train_ts_sanatate,lag.max = 12,type = "const")
model_var_sanatate1 <- VAR(train_ts_sanatate[, c("Sectorul_Sanatatii", "Dobanda_diff")], p = 1, type = "const")
causality(model_var_sanatate1, cause = "Dobanda_diff")$Granger

model_var_sanatate2 <- VAR(train_ts_sanatate[, c("Sectorul_Sanatatii", "Somaj_diff")], p = 1, type = "const")
causality(model_var_sanatate2, cause = "Somaj_diff")$Granger

model_var_sanatate3 <- VAR(train_ts_sanatate[, c("Sectorul_Sanatatii", "IPC")], p = 1, type = "const")
causality(model_var_sanatate3, cause = "IPC")$Granger


model_var_sanatate4 <- VAR(train_ts_sanatate[, c("Sectorul_Sanatatii", "Rentabilitate_Titei")], p = 1, type = "const")
causality(model_var_sanatate4, cause = "Rentabilitate_Titei")$Granger

model_var_sanatate5 <- VAR(train_ts_sanatate[, c("Sectorul_Sanatatii", "Rentabilitate_Aur")], p = 1, type = "const")
causality(model_var_sanatate5, cause = "Rentabilitate_Aur")$Granger

model_var_sanatate6 <- VAR(train_ts_sanatate[, c("Sectorul_Sanatatii", "Rentabilitate_sp")], p = 1, type = "const")
causality(model_var_sanatate6, cause = "Rentabilitate_sp")$Granger




h <- nrow(data_test_sanatate)
forecast_var_sanatate <- predict(model_var_train_sanatate, n.ahead = h, ci = 0.95)
forecast_sanatate <- forecast_var_sanatate$fcst$Sectorul_Sanatatii

plot(forecast_var_sanatate,name="Sectorul_Sanatatii")

# Data frame pentru forecast
df_forecast_sanatate <- data.frame(
  Date = data_test_sanatate$Date,
  Forecast = forecast_sanatate[, "fcst"],
  Lower = forecast_sanatate[, "lower"],
  Upper = forecast_sanatate[, "upper"]
)

# Grafic forecast
ggplot(df_forecast_sanatate, aes(x = Date, y = Forecast)) +
  geom_line(color = "purple", size = 1) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "purple", alpha = 0.2) +
  labs(title = "Prognoza Sectorului Sănătății (VAR)",
       y = "Rentabilitate prognozată",
       x = "Dată") +
  theme_minimal()

# Comparație cu valori reale
forecast_df_sanatate <- data.frame(
  Data = data_test_sanatate$Date,
  Real = data_test_sanatate$Sectorul_Sanatatii,
  Previziune = forecast_sanatate[, "fcst"],
  Low_95 = forecast_sanatate[, "lower"],
  High_95 = forecast_sanatate[, "upper"]
)

ggplot(forecast_df_sanatate, aes(x = Data)) +
  geom_line(aes(y = Real), color = "black", size = 1.2) +
  geom_line(aes(y = Previziune), color = "purple", linetype = "dashed", size = 1) +
  geom_ribbon(aes(ymin = Low_95, ymax = High_95), fill = "purple", alpha = 0.2) +
  labs(title = "Prognoză vs Valori reale – Sectorul Sănătății",
       y = "Rentabilitate",
       x = "Dată") +
  theme_minimal()

# Erori forecast
real_values_san <- forecast_df_sanatate$Real
predicted_values_san <- forecast_df_sanatate$Previziune

rmse_san <- rmse(real_values_san, predicted_values_san)
mae_san <- mae(real_values_san, predicted_values_san)

cat("RMSE:", round(rmse_san, 4), "\n")
cat("MAE:", round(mae_san, 4), "\n")



# ##### VAR INDUSTRIAL---------------------------------------------------------------

max_date_industrial <- max(df_regresie_industrial$Date)
cut_date_industrial <- seq(max_date_industrial, length = 2, by = "-6 months")[2]

# Împărțirea în seturi
data_train_industrial <- subset(df_regresie_industrial, Date < cut_date_industrial)
data_test_industrial  <- subset(df_regresie_industrial, Date >= cut_date_industrial)

# Conversie la time series
train_ts_industrial <- ts(data_train_industrial[,-1],
                          start = c(year(min(data_train_industrial$Date)), month(min(data_train_industrial$Date))),
                          frequency = 12)

test_ts_industrial <- ts(data_test_industrial[,-1],
                         start = c(year(min(data_test_industrial$Date)), month(min(data_test_industrial$Date))),
                         frequency = 12)


lags_industrial <- VARselect(train_ts_industrial, lag.max = 12, type = "const")
lags_industrial$selection  

# Model VAR
model_var_train_industrial <- VAR(train_ts_industrial, p = 1, type = "const")
summary(model_var_train_industrial)
stargazer(model_var_train_industrial[['varresult']],type = 'text')
#Roots of the characteristic polynomial:
#0.4555 0.4555 0.4543 0.1337 0.1337 0.08417 0.02893 <1  Modelul este stabil
#Dobanda_diff.l1                -3.514*
#Rentabilitate_Aur.l1           -0.189**

#R2                              0.045
#Adjusted R2                     0.010
#F Statistic (df = 7; 188)       1.275
#Corelatia dintre Sector si Rentabilitate_SP este 0.847964


#Autocorelarea reziduurilor - Portmanteau Test
#H0: Nu există autocorelare (reziduurile sunt zgomot alb)
#H1: Există autocorelare
serial.test(model_var_train_industrial, lags.pt = 6, type = "PT.asymptotic")
# p-value = 7.9e-05 < 0.05 Reziduurile  sunt autocorelate

#Test de heteroscedasticitate - ARCH Test
#H0: Varianța reziduurilor este constantă (nu există heteroscedasticitate)
#H1: Varianța reziduurilor este variabilă (efecte ARCH)
arch.test(model_var_train_industrial, lags.multi = 5, multivariate.only = TRUE)
#p-value = 2.162e-09 <0.05  Exista efecte ARCH

#Normalitatea reziduurilor - Jarque-Bera test multivariat
#H0: Reziduurile sunt distribuite normal
#H1: Reziduurile nu sunt distribuite normal
normality.test(model_var_train_industrial, multivariate.only = TRUE)
#p-value < 2.2e-16 Reziduurile nu sunt normal distribuite
#Skewness p-value < 2.2e-16
#Kurtosis p-value < 2.2e-16


# Ipotezele cauzalitatii Granger
# H0: valorile cu lag ale lui X, nu explica variatia in Y 
# H1: valorile cu lag ale lui X, explica variatia in Y

# H0: variabila X nu prezinta cauzalitate Granger pentru variabila Y
# H1: Variabila X prezinta cauzalitate Granger pentru variabila Y
Granger_Industrial <- causality(model_var_train_industrial,cause = "Sectorul_Industrial")
Granger_Industrial$Granger
#Granger (lag)	p-value = 0.1717	Nu există cauzalitate Granger 
causalitate_dobanda_industrial <- causality(model_var_train_industrial, cause = "Dobanda_diff")
causalitate_somaj_industrial <- causality(model_var_train_industrial, cause = "Somaj_diff")
causalitate_ipc_industrial <- causality(model_var_train_industrial, cause = "IPC")
causalitate_titei_industrial <- causality(model_var_train_industrial, cause = "Rentabilitate_Titei")
causalitate_aur_industrial <- causality(model_var_train_industrial, cause = "Rentabilitate_Aur")
causalitate_sp_industrial <- causality(model_var_train_industrial, cause = "Rentabilitate_sp")

causalitate_dobanda_industrial$Granger
#p-value = 0.144 Nu exista
causalitate_somaj_industrial$Granger
#p-value = 0.9167 Nu exista
causalitate_ipc_industrial$Granger
#p-value = 0.02539 Exista
causalitate_titei_industrial$Granger
#p-value = 0.0002673 Exista
causalitate_aur_industrial$Granger
#p-value = 0.03149 Exista
causalitate_sp_industrial$Granger
#p-value = 0.4569 Nu exista




VARselect(train_ts_industrial,lag.max = 12,type = "const")
model_var_industrial1 <- VAR(train_ts_industrial[, c("Sectorul_Industrial", "Dobanda_diff")], p = 1, type = "const")
causality(model_var_industrial1, cause = "Dobanda_diff")$Granger

model_var_industrial2 <- VAR(train_ts_industrial[, c("Sectorul_Industrial", "Somaj_diff")], p = 1, type = "const")
causality(model_var_industrial2, cause = "Somaj_diff")$Granger

model_var_industrial3 <- VAR(train_ts_industrial[, c("Sectorul_Industrial", "IPC")], p = 1, type = "const")
causality(model_var_industrial3, cause = "IPC")$Granger


model_var_industrial4 <- VAR(train_ts_industrial[, c("Sectorul_Industrial", "Rentabilitate_Titei")], p = 1, type = "const")
causality(model_var_industrial4, cause = "Rentabilitate_Titei")$Granger

model_var_industrial5 <- VAR(train_ts_industrial[, c("Sectorul_Industrial", "Rentabilitate_Aur")], p = 1, type = "const")
causality(model_var_industrial5, cause = "Rentabilitate_Aur")$Granger

model_var_industrial6 <- VAR(train_ts_industrial[, c("Sectorul_Industrial", "Rentabilitate_sp")], p = 1, type = "const")
causality(model_var_industrial6, cause = "Rentabilitate_sp")$Granger






h <- nrow(data_test_industrial)
forecast_var_industrial <- predict(model_var_train_industrial, n.ahead = h, ci = 0.95)
forecast_industrial <- forecast_var_industrial$fcst$Sectorul_Industrial
plot(forecast_var_industrial,name="Sectorul_Industrial")

# 7. Data frame forecast
df_forecast_industrial <- data.frame(
  Date = data_test_industrial$Date,
  Forecast = forecast_industrial[, "fcst"],
  Lower = forecast_industrial[, "lower"],
  Upper = forecast_industrial[, "upper"]
)

# 8. Plot forecast
ggplot(df_forecast_industrial, aes(x = Date, y = Forecast)) +
  geom_line(color = "orange", size = 1) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "orange", alpha = 0.2) +
  labs(title = "Prognoza Sectorului Industrial (VAR)",
       y = "Rentabilitate prognozată",
       x = "Dată") +
  theme_minimal()

# 9. Comparație cu valori reale
forecast_df_industrial <- data.frame(
  Data = data_test_industrial$Date,
  Real = data_test_industrial$Sectorul_Industrial,
  Previziune = forecast_industrial[, "fcst"],
  Low_95 = forecast_industrial[, "lower"],
  High_95 = forecast_industrial[, "upper"]
)

ggplot(forecast_df_industrial, aes(x = Data)) +
  geom_line(aes(y = Real), color = "black", size = 1.2) +
  geom_line(aes(y = Previziune), color = "orange", linetype = "dashed", size = 1) +
  geom_ribbon(aes(ymin = Low_95, ymax = High_95), fill = "orange", alpha = 0.2) +
  labs(title = "Prognoză vs Valori reale – Sectorul Industrial",
       y = "Rentabilitate",
       x = "Dată") +
  theme_minimal()

# 10. Erori forecast
real_values_ind <- forecast_df_industrial$Real
predicted_values_ind <- forecast_df_industrial$Previziune

rmse_ind <- rmse(real_values_ind, predicted_values_ind)
mae_ind <- mae(real_values_ind, predicted_values_ind)

cat("RMSE:", round(rmse_ind, 4), "\n")
cat("MAE:", round(mae_ind, 4), "\n")



# #####VAR Consum De Baza -------------------------------------------------

max_date_consum <- max(df_regresie_consumBaza$Date)
cut_date_consum <- seq(max_date_consum, length = 2, by = "-6 months")[2]

data_train_consum <- subset(df_regresie_consumBaza, Date < cut_date_consum)
data_test_consum  <- subset(df_regresie_consumBaza, Date >= cut_date_consum)

train_ts_consum <- ts(data_train_consum[,-1],
                      start = c(year(min(data_train_consum$Date)), month(min(data_train_consum$Date))),
                      frequency = 12)

test_ts_consum <- ts(data_test_consum[,-1],
                     start = c(year(min(data_test_consum$Date)), month(min(data_test_consum$Date))),
                     frequency = 12)

# 2. Selectare lag și estimare VAR
lags_consum <- VARselect(train_ts_consum, lag.max = 12, type = "const")
lags_consum$selection
model_var_train_consum <- VAR(train_ts_consum, p = 1, type = "const")
summary(model_var_train_consum)
stargazer(model_var_train_consum[['varresult']],type = 'text')
#Roots of the characteristic polynomial:
#0.4808 0.4808 0.4578 0.1526 0.1526 0.0972 0.02379 <1  Modelul este stabil
#Dobanda_diff.l1                      -1.962*
#IPC.l1                              2.202***
#Rentabilitate_Aur.l1                -0.152***

#R2                                    0.097
#Adjusted R2                           0.064
#F Statistic (df = 7; 188)           2.896***
#Corelatia dintre Sector si Rentabilitate_SP este 0.678565



#Autocorelarea reziduurilor - Portmanteau Test
#H0: Nu există autocorelare (reziduurile sunt zgomot alb)
#H1: Există autocorelare
serial.test(model_var_train_consum, lags.pt = 6, type = "PT.asymptotic")
# p-value = 0.001647 < 0.05 Reziduurile  sunt autocorelate

#Test de heteroscedasticitate - ARCH Test
#H0: Varianța reziduurilor este constantă (nu există heteroscedasticitate)
#H1: Varianța reziduurilor este variabilă (efecte ARCH)
arch.test(model_var_train_consum, lags.multi = 5, multivariate.only = TRUE)
#p-value = 2.587e-06 <0.05  Exista efecte ARCH

#Normalitatea reziduurilor - Jarque-Bera test multivariat
#H0: Reziduurile sunt distribuite normal
#H1: Reziduurile nu sunt distribuite normal
normality.test(model_var_train_consum, multivariate.only = TRUE)
#p-value < 2.2e-16 Reziduurile nu sunt normal distribuite
#Skewness p-value < 2.2e-16
#Kurtosis p-value < 2.2e-16


# Ipotezele cauzalitatii Granger
# H0: valorile cu lag ale lui X, nu explica variatia in Y 
# H1: valorile cu lag ale lui X, explica variatia in Y

# H0: variabila X nu prezinta cauzalitate Granger pentru variabila Y
# H1: Variabila X prezinta cauzalitate Granger pentru variabila Y
Granger_ConsumBaza <- causality(model_var_train_consum,cause = "Sectorul_BunurilorDeConsumDeBaza")
Granger_ConsumBaza$Granger
#Granger (lag)	p-value = 0.1281	Nu există cauzalitate Granger 
causalitate_dobanda_consum <- causality(model_var_train_consum, cause = "Dobanda_diff")
causalitate_somaj_consum <- causality(model_var_train_consum, cause = "Somaj_diff")
causalitate_ipc_consum <- causality(model_var_train_consum, cause = "IPC")
causalitate_titei_consum <- causality(model_var_train_consum, cause = "Rentabilitate_Titei")
causalitate_aur_consum <- causality(model_var_train_consum, cause = "Rentabilitate_Aur")
causalitate_sp_consum <- causality(model_var_train_consum, cause = "Rentabilitate_sp")

causalitate_dobanda_consum$Granger
#p-value = 0.1878 Nu exista
causalitate_somaj_consum$Granger
#p-value = 0.7372 Nu exista
causalitate_ipc_consum$Granger
#p-value = 0.0004206 Exista
causalitate_titei_consum$Granger
#p-value = 0.0007014 Exista
causalitate_aur_consum$Granger
#p-value = 0.02667 Exista
causalitate_sp_consum$Granger
#p-value = 0.4599 Nu exista



VARselect(train_ts_consum,lag.max = 12,type = "const")
model_var_consum1 <- VAR(train_ts_consum[, c("Sectorul_BunurilorDeConsumDeBaza", "Dobanda_diff")], p = 1, type = "const")
causality(model_var_consum1, cause = "Dobanda_diff")$Granger

model_var_consum2 <- VAR(train_ts_consum[, c("Sectorul_BunurilorDeConsumDeBaza", "Somaj_diff")], p = 1, type = "const")
causality(model_var_consum2, cause = "Somaj_diff")$Granger

model_var_consum3 <- VAR(train_ts_consum[, c("Sectorul_BunurilorDeConsumDeBaza", "IPC")], p = 1, type = "const")
causality(model_var_consum3, cause = "IPC")$Granger


model_var_consum4 <- VAR(train_ts_consum[, c("Sectorul_BunurilorDeConsumDeBaza", "Rentabilitate_Titei")], p = 1, type = "const")
causality(model_var_consum4, cause = "Rentabilitate_Titei")$Granger

model_var_consum5 <- VAR(train_ts_consum[, c("Sectorul_BunurilorDeConsumDeBaza", "Rentabilitate_Aur")], p = 1, type = "const")
causality(model_var_consum5, cause = "Rentabilitate_Aur")$Granger

model_var_consum6 <- VAR(train_ts_consum[, c("Sectorul_BunurilorDeConsumDeBaza", "Rentabilitate_sp")], p = 1, type = "const")
causality(model_var_consum6, cause = "Rentabilitate_sp")$Granger









h <- nrow(data_test_consum)
forecast_var_consum <- predict(model_var_train_consum, n.ahead = h, ci = 0.95)
forecast_consum <- forecast_var_consum$fcst$Sectorul_BunurilorDeConsumDeBaza

plot(forecast_var_consum,name="Sectorul_BunurilorDeConsumDeBaza")

# Data frame forecast
df_forecast_consum <- data.frame(
  Date = data_test_consum$Date,
  Forecast = forecast_consum[, "fcst"],
  Lower = forecast_consum[, "lower"],
  Upper = forecast_consum[, "upper"]
)

# Plot forecast
ggplot(df_forecast_consum, aes(x = Date, y = Forecast)) +
  geom_line(color = "brown", size = 1) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "brown", alpha = 0.2) +
  labs(title = "Prognoza Sectorului Bunurilor de Consum de Bază (VAR)",
       y = "Rentabilitate prognozată",
       x = "Dată") +
  theme_minimal()

# Comparație cu valorile reale
forecast_df_consum <- data.frame(
  Data = data_test_consum$Date,
  Real = data_test_consum$Sectorul_BunurilorDeConsumDeBaza,
  Previziune = forecast_consum[, "fcst"],
  Low_95 = forecast_consum[, "lower"],
  High_95 = forecast_consum[, "upper"]
)

ggplot(forecast_df_consum, aes(x = Data)) +
  geom_line(aes(y = Real), color = "black", size = 1.2) +
  geom_line(aes(y = Previziune), color = "brown", linetype = "dashed", size = 1) +
  geom_ribbon(aes(ymin = Low_95, ymax = High_95), fill = "brown", alpha = 0.2) +
  labs(title = "Prognoză vs Valori reale – Sectorul Bunurilor de Consum de Bază",
       y = "Rentabilitate",
       x = "Dată") +
  theme_minimal()

# Erori forecast
real_values_consum <- forecast_df_consum$Real
predicted_values_consum <- forecast_df_consum$Previziune

rmse_consum <- rmse(real_values_consum, predicted_values_consum)
mae_consum <- mae(real_values_consum, predicted_values_consum)

cat("RMSE:", round(rmse_consum, 4), "\n")
cat("MAE:", round(mae_consum, 4), "\n")




# ######VAR Sectorul Energetic --------------------------------------------

max_date_energie <- max(df_regresie_energetic$Date)
cut_date_energie <- seq(max_date_energie, length = 2, by = "-6 months")[2]

data_train_energie <- subset(df_regresie_energetic, Date < cut_date_energie)
data_test_energie  <- subset(df_regresie_energetic, Date >= cut_date_energie)

train_ts_energie <- ts(data_train_energie[,-1],
                       start = c(year(min(data_train_energie$Date)), month(min(data_train_energie$Date))),
                       frequency = 12)

test_ts_energie <- ts(data_test_energie[,-1],
                      start = c(year(min(data_test_energie$Date)), month(min(data_test_energie$Date))),
                      frequency = 12)

# 2. Selectare lag și estimare model VAR
lags_energie <- VARselect(train_ts_energie, lag.max = 12, type = "const")
lags_energie$selection
model_var_train_energie <- VAR(train_ts_energie, p = 1, type = "const")
summary(model_var_train_energie)
stargazer(model_var_train_energie[['varresult']],type = 'text')
#Roots of the characteristic polynomial:
#0.4803 0.4637 0.4637 0.194 0.194 0.05647 0.05647 <1  Modelul este stabil
#Rentabilitate_Aur.l1           -0.202**

#R2                              0.051
#Adjusted R2                     0.015
#F Statistic (df = 7; 188)       1.436
#Corelatia dintre Sector si Rentabilitate_SP este 0.585160



#Autocorelarea reziduurilor - Portmanteau Test
#H0: Nu există autocorelare (reziduurile sunt zgomot alb)
#H1: Există autocorelare
serial.test(model_var_train_energie, lags.pt = 6, type = "PT.asymptotic")
# p-value = 0.0008725 < 0.05 Reziduurile  sunt autocorelate

#Test de heteroscedasticitate - ARCH Test
#H0: Varianța reziduurilor este constantă (nu există heteroscedasticitate)
#H1: Varianța reziduurilor este variabilă (efecte ARCH)
arch.test(model_var_train_energie, lags.multi = 5, multivariate.only = TRUE)
#p-value = 3.806e-10 <0.05  Exista efecte ARCH

#Normalitatea reziduurilor - Jarque-Bera test multivariat
#H0: Reziduurile sunt distribuite normal
#H1: Reziduurile nu sunt distribuite normal
normality.test(model_var_train_energie, multivariate.only = TRUE)
#p-value < 2.2e-16 Reziduurile nu sunt normal distribuite
#Skewness p-value < 2.2e-16
#Kurtosis p-value < 2.2e-16


# Ipotezele cauzalitatii Granger
# H0: valorile cu lag ale lui X, nu explica variatia in Y 
# H1: valorile cu lag ale lui X, explica variatia in Y

# H0: variabila X nu prezinta cauzalitate Granger pentru variabila Y
# H1: Variabila X prezinta cauzalitate Granger pentru variabila Y
Granger_Energie <- causality(model_var_train_energie,cause = "Sectorul_Energetic")
Granger_Energie$Granger
#Granger (lag)	p-value = 0.001423	 există cauzalitate Granger 
causalitate_dobanda_energie <- causality(model_var_train_energie, cause = "Dobanda_diff")
causalitate_somaj_energie <- causality(model_var_train_energie, cause = "Somaj_diff")
causalitate_ipc_energie <- causality(model_var_train_energie, cause = "IPC")
causalitate_titei_energie <- causality(model_var_train_energie, cause = "Rentabilitate_Titei")
causalitate_aur_energie <- causality(model_var_train_energie, cause = "Rentabilitate_Aur")
causalitate_sp_energie <- causality(model_var_train_energie, cause = "Rentabilitate_sp")



causalitate_dobanda_energie$Granger
#p-value = 0.04184  exista
causalitate_somaj_energie$Granger
#p-value = 0.4387 Nu exista
causalitate_ipc_energie$Granger
#p-value = 0.04219 Exista
causalitate_titei_energie$Granger
#p-value = 0.0003906 Exista
causalitate_aur_energie$Granger
#p-value = 0.01866 Exista
causalitate_sp_energie$Granger
#p-value = 0.6698 Nu exista




VARselect(train_ts_energie,lag.max = 12,type = "const")
model_var_energie1 <- VAR(train_ts_energie[, c("Sectorul_Energetic", "Dobanda_diff")], p = 1, type = "const")
causality(model_var_energie1, cause = "Dobanda_diff")$Granger

model_var_energie2 <- VAR(train_ts_energie[, c("Sectorul_Energetic", "Somaj_diff")], p = 1, type = "const")
causality(model_var_energie2, cause = "Somaj_diff")$Granger

model_var_energie3 <- VAR(train_ts_energie[, c("Sectorul_Energetic", "IPC")], p = 1, type = "const")
causality(model_var_energie3, cause = "IPC")$Granger


model_var_energie4 <- VAR(train_ts_energie[, c("Sectorul_Energetic", "Rentabilitate_Titei")], p = 1, type = "const")
causality(model_var_energie4, cause = "Rentabilitate_Titei")$Granger

model_var_energie5 <- VAR(train_ts_energie[, c("Sectorul_Energetic", "Rentabilitate_Aur")], p = 1, type = "const")
causality(model_var_energie5, cause = "Rentabilitate_Aur")$Granger

model_var_energie6 <- VAR(train_ts_energie[, c("Sectorul_Energetic", "Rentabilitate_sp")], p = 1, type = "const")
causality(model_var_energie6, cause = "Rentabilitate_sp")$Granger






h <- nrow(data_test_energie)
forecast_var_energie <- predict(model_var_train_energie, n.ahead = h, ci = 0.95)
forecast_energie <- forecast_var_energie$fcst$Sectorul_Energetic

plot(forecast_var_energie,name="Sectorul_Energetic")

# 4. Data frame forecast
df_forecast_energie <- data.frame(
  Date = data_test_energie$Date,
  Forecast = forecast_energie[, "fcst"],
  Lower = forecast_energie[, "lower"],
  Upper = forecast_energie[, "upper"]
)

# 5. Plot forecast
ggplot(df_forecast_energie, aes(x = Date, y = Forecast)) +
  geom_line(color = "darkblue", size = 1) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "darkblue", alpha = 0.2) +
  labs(title = "Prognoza Sectorului Energetic (VAR)",
       y = "Rentabilitate prognozată",
       x = "Dată") +
  theme_minimal()

# 6. Comparație cu valorile reale
forecast_df_energie <- data.frame(
  Data = data_test_energie$Date,
  Real = data_test_energie$Sectorul_Energetic,
  Previziune = forecast_energie[, "fcst"],
  Low_95 = forecast_energie[, "lower"],
  High_95 = forecast_energie[, "upper"]
)

ggplot(forecast_df_energie, aes(x = Data)) +
  geom_line(aes(y = Real), color = "black", size = 1.2) +
  geom_line(aes(y = Previziune), color = "darkblue", linetype = "dashed", size = 1) +
  geom_ribbon(aes(ymin = Low_95, ymax = High_95), fill = "darkblue", alpha = 0.2) +
  labs(title = "Prognoză vs Valori reale – Sectorul Energetic",
       y = "Rentabilitate",
       x = "Dată") +
  theme_minimal()

# 7. Erori forecast
real_values_energie <- forecast_df_energie$Real
predicted_values_energie <- forecast_df_energie$Previziune

rmse_energie <- rmse(real_values_energie, predicted_values_energie)
mae_energie <- mae(real_values_energie, predicted_values_energie)

cat("RMSE:", round(rmse_energie, 4), "\n")
cat("MAE:", round(mae_energie, 4), "\n")



# ####VAR toate sectoarele ------------------------------------------------

summary(model_var_train_tehnologie)
stargazer(model_var_train_tehnologie[['varresult']],type = 'text')
#Roots of the characteristic polynomial:
#  0.4674 0.4674 0.4546 0.1492 0.1492 0.0667 0.0667 <1  Modelul este stabil
#Dobanda_diff.l1                -3.071* 
#Pe termen scurt, dobânda pare a avea un impact negativ asupra sectorului tehnologic, 
#dar efectul nu este puternic semnificativ.
#R2                              0.051
#Adjusted R2                     0.016
#F Statistic (df = 7; 188)       1.450
#Corelatia dintre Sector si Rentabilitate_SP este 0.79




summary(model_var_train_financiar)
stargazer(model_var_train_financiar[['varresult']],type = 'text')
#Roots of the characteristic polynomial:
# 0.4714 0.4491 0.4491 0.1852 0.1852 0.04083 0.00385 <1  Modelul este stabil
#Sectorul_Financiar.l1          -0.217**
#Rentabilitate_Aur.l1           -0.218***
#Rentabilitate_sp.l1             0.266*

#R2                              0.077
#Adjusted R2                     0.043
#F Statistic (df = 7; 188)       2.256**
#Corelatia dintre Sector si Rentabilitate_SP este 0.69



summary(model_var_train_sanatate)
stargazer(model_var_train_sanatate[['varresult']],type = 'text')
#Roots of the characteristic polynomial:
# 0.4561 0.4497 0.4497 0.1554 0.1554 0.1474 0.1474 <1  Modelul este stabil
#Dobanda_diff.l1                -2.283**
#Rentabilitate_Titei.l1          0.052*
#Rentabilitate_Aur.l1           -0.135**

#R2                              0.073
#Adjusted R2                     0.039
#F Statistic (df = 7; 188)      2.121**
#Corelatia dintre Sector si Rentabilitate_SP este 0.600989



summary(model_var_train_industrial)
stargazer(model_var_train_industrial[['varresult']],type = 'text')
#Roots of the characteristic polynomial:
#0.4555 0.4555 0.4543 0.1337 0.1337 0.08417 0.02893 <1  Modelul este stabil
#Dobanda_diff.l1                -3.514*
#Rentabilitate_Aur.l1           -0.189**

#R2                              0.045
#Adjusted R2                     0.010
#F Statistic (df = 7; 188)       1.275
#Corelatia dintre Sector si Rentabilitate_SP este 0.847964


summary(model_var_train_consum)
stargazer(model_var_train_consum[['varresult']],type = 'text')
#Roots of the characteristic polynomial:
#0.4808 0.4808 0.4578 0.1526 0.1526 0.0972 0.02379 <1  Modelul este stabil
#Dobanda_diff.l1                      -1.962*
#IPC.l1                              2.202***
#Rentabilitate_Aur.l1                -0.152***

#R2                                    0.097
#Adjusted R2                           0.064
#F Statistic (df = 7; 188)           2.896***
#Corelatia dintre Sector si Rentabilitate_SP este 0.678565



model_var_train_energie <- VAR(train_ts_energie, p = 1, type = "const")
summary(model_var_train_energie)
stargazer(model_var_train_energie[['varresult']],type = 'text')
#Roots of the characteristic polynomial:
#0.4803 0.4637 0.4637 0.194 0.194 0.05647 0.05647 <1  Modelul este stabil
#Rentabilitate_Aur.l1           -0.202**

#R2                              0.051
#Adjusted R2                     0.015
#F Statistic (df = 7; 188)       1.436
#Corelatia dintre Sector si Rentabilitate_SP este 0.585160