#1. Podpunkt
library(readr)
library(dplyr)
library(readxl)

getwd()
setwd("/Users/wiktorsocha/Desktop")

diamenty=read_excel("datasets.xlsx")

View(diamenty)

#Nierealistyczne wartości carat

unrealistic_carats <- c(0.0001, 0.0111, 21, 85, 86, 91, 100, 321, 1022, 2121, 21212, 34241)
unrealistic_carats

#Czyszczenie danych

diamenty_clean <- diamenty %>%
  
#Naprawa formatowania kolumny 'cut'
  
mutate(cut = trimws(cut)) %>%
  
#Zmiana kolumn na numeryczne
  
mutate(across(c(carat, depth, table, price, x, y, z), as.numeric)) %>%
  
#Usunięcie wierszy z brakami danych
  
filter(!is.na(carat) & !is.na(depth) & !is.na(table) & !is.na(price) & !is.na(x) & !is.na(y) & !is.na(z)) %>%
  
#Usunięcie wierszy z nierealistycznymi wartościami
  #1.Realistyczna masa karatowa (między 0.1 a 15) i bez znanych błędów
  #2.Realistyczna depth (40-80%)
  #3.Realistyczne table (40-95%)
  #4.Realistyczna cena (dodatnia, bez ekstremow)
  #5.Realistyczne wymiary (dodatnie, z górnym limitem, bez bledow jak z=2121)
  
filter(carat > 0.1 & carat < 15 & !(carat %in% unrealistic_carats), depth > 40 & depth < 80, table > 40 & table < 95, price > 0 & price < 200000, x > 0 & y > 0 & z > 0 & x < 30 & y < 30 & z < 30 & z != 2121)
  
#Sprawdzenie czyszczenia danych

cat("Liczba wierszy przed czyszczeniem:", nrow(diamenty), "\n")
cat("Liczba wierszy po czyszczeniu:", nrow(diamenty_clean), "\n")

View(diamenty_clean)

#Wstepna analiza zmiennych

summary(diamenty_clean)

#2. Podpunkt

#Do zbudowania modelu predykcyjnego zmiennej objaśnianej "Price" użyję zmiennych objaśniających "carat", "cut", "color", "clarity"

#Wymiary (x, y, z) są silnie skorelowane z carat, więc je pominiemy aby uniknąć problemu współliniowości

install.packages("caTools")

library(dplyr)
library(caTools)
library(ggplot2)

#Sprawdzenie typów zmiennych kategorycznych i przekształcenie na faktory z odpowiednimi poziomami

#Ustalenie kolejności poziomów zmiennych, od najgorszej do najlepszej jakości diamentów

diamenty_clean = diamenty_clean %>% mutate(cut = factor(cut, levels = c("Fair", "Good", "Very Good", "Premium", "Ideal"), ordered = TRUE), color = factor(color, levels = c("J", "I", "H", "G", "F", "E", "D"), ordered = TRUE), clarity = factor(clarity, levels = c("I1", "SI2", "SI1", "VS2", "VS1", "VVS2", "VVS1", "IF"), ordered = TRUE)

#Transformacja logarytmiczna zmiennych price i carat

diamenty_model_data = diamenty_clean %>% mutate(log_price = log(price), log_carat = log(carat)) %>%

#Wybor zmiennych
  
select(log_price, log_carat, cut, color, clarity, depth, table)

#Podział danych na zbiór treningowy (75%) i testowy (25%)
#Ustawienie ziarna losowosci

set.seed(123)

split = sample.split(diamenty_model_data$log_price, SplitRatio = 0.70)
train_data = subset(diamenty_model_data, split == TRUE)
test_data = subset(diamenty_model_data, split == FALSE) 

cat("Rozmiar zbioru treningowego:", nrow(train_data), "\n")
cat("Rozmiar zbioru testowego:", nrow(test_data), "\n")  

#Budowa modelu regresji liniowej

#Predykcja logarytmu zmiennej 'price' (log_price) na podstawie log_carat i pozostałych zmiennych ('carat', 'cut', 'color', 'clarity', 'depth', 'table')

model = lm(log_price ~ log_carat + cut + color + clarity + depth + table, data = train_data)
model

#Najważniejsze informacje modelu (współczynniki, istotność, R-kwadrat)

summary(model)
  
#Predykcja na zbiorze testowym
predictions_log <- predict(model, newdata = test_data)
predictions_log

#RMSE dla logarytmu ceny (średnia oczekiwana różnica miedzy wartoscia przewidywana a rzeczywista)
rmse_log=sqrt(mean((test_data$log_price - predictions_log)^2))
cat("RMSE (dla log_price):", rmse_log, "\n")

#R-kwadrat dla zbioru testowego  

sse = sum((test_data$log_price - predictions_log)^2)
sst = sum((test_data$log_price - mean(test_data$log_price))^2)  
r_squared_test = 1 - sse/sst
cat("R-kwadrat (na zbiorze testowym):", r_squared_test, "\n")

#Wizualizacja: Rzeczywiste vs Przewidywane wartości (w skali logarytmicznej)

plot_data=data.frame(Actual=test_data$log_price, Predicted=predictions_log)
ggplot(plot_data, aes(x = Actual, y = Predicted)) + geom_point(alpha = 0.5) + geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + labs(title = "Rzeczywiste vs Przewidywane log(cena) na zbiorze testowym", x = "Rzeczywiste log(cena)", y = "Przewidywane log(cena)") + theme_minimal() + coord_fixed()

#3. Podpunkt

install.packages("gridExtra")
install.packages("patchwork")
library("patchwork")
library(ggplot2)
library(gridExtra)

#Wykresy gęstości dla ceny i logarytmu ceny, porównanie zmiennej zlogarytmowanej i niezlogarytmowanej

p1 = ggplot(diamenty_clean, aes(x = price)) + geom_density(fill = "lightblue", alpha = 0.7) + scale_x_continuous(labels = scales::comma) + labs(title = "Rozkład Ceny Diamentów", x = "Cena ($)", y = "Gęstość") + theme_minimal()

p2 = ggplot(diamenty_model_data, aes(x = log_price)) + geom_density(fill = "lightgreen", alpha = 0.7) + labs(title = "Rozkład Logarytmu Ceny", x = "Log(Cena)", y = "Gęstość") + theme_minimal()

#Porównanie dwóch wykresów
grid.arrange(p1, p2, ncol = 2)

#Zależność 'price' od zmiennych objaśniających:

# a) Log(price) vs Log(carat): Silna, pozytywna liniowa zależność. Większa masa (karat) wiąże się z wyższą ceną

# b) Log(price) vs Jakość Szlifu (cut): Lepszy szlif prowadzi do wyższej ceny

# c) Log(price) vs Kolor (color): Diamenty o "lepszych" kolorach (bliższych bezbarwności, D > E > F ...) są droższe

# d) Log(price) vs Czystość (clarity): Wyższa czystość (mniej inkluzji, IF > VVS1 > ...) oznacza wyższą cenę


# a)

p_carat=ggplot(diamenty_model_data, aes(x = log_carat, y = log_price)) + geom_point(alpha = 0.1) + labs(title = "Log(Cena) vs Log(Karat)", x = "Log(Karat)", y = "Log(Cena)") + theme_minimal()

# b)

p_cut=ggplot(diamenty_model_data, aes(x = cut, y = log_price, fill = cut)) + geom_boxplot() + labs(title = "Log(Cena) vs Szlif", x = "Szlif", y = "Log(Cena)") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# c)

p_color=ggplot(diamenty_model_data, aes(x = color, y = log_price, fill = color)) + geom_boxplot() + labs(title = "Log(Cena) vs Kolor", x = "Kolor", y = "Log(Cena)") + theme_minimal()

# d)

p_clarity=ggplot(diamenty_model_data, aes(x = clarity, y = log_price, fill = clarity)) + geom_boxplot() + labs(title = "Log(Cena) vs Czystość", x = "Czystość", y = "Log(Cena)") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Wykresy zależności poszczegolnych zmiennych

library(patchwork)

((p_carat | p_cut)/(p_color | p_clarity))


#Sprawdzenie poprawności modelu, zakonczenie

#Obliczanie przewidywanych wartości do zbioru testowego

test_data$fitted_log_price=predict(model, newdata = test_data)

#Obliczanie reszt dla zbioru testowego

test_data$residuals=test_data$log_price - test_data$fitted_log_price

#Wykres reszty vs dopasowane wartości

p_residuals <- ggplot(test_data, aes(x = fitted_log_price, y = residuals)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Reszty vs Dopasowane Wartości (zbiór testowy)",
       x = "Dopasowane Log(Cena)",
       y = "Reszty") +
  theme_minimal()

p_residuals

#Normalność rozkładu reszt

p_hist_res = ggplot(test_data, aes(x = residuals)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.05, fill = "lightblue", color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(test_data$residuals), sd = sd(test_data$residuals)), color = "red", size = 1) +
  labs(title = "Histogram reszt", x = "Reszty", y = "Gęstość") +
  theme_minimal()

p_hist_res

#Wykres kwantylowy reszt

p_qq_res = ggplot(test_data, aes(sample = residuals)) + stat_qq() + stat_qq_line() + labs(title = "Wykres Q-Q dla reszt", x = "Kwantyle teoretyczne", y = "Kwantyle z próbki") + theme_minimal()

p_qq_res






