#Analityka Biznesowa
#Analiza danych z wykorzystaniem języka R oraz Wykorzystanie modeli uczenia maszynowego w analizie danych
#Projekt zaliczeniowy Data Science - Analiza danych dotyczących ataków serca w Chinach
#Autro: Jakub Gbyl


#1. Instalacja pakietów i wczytanie danych

getwd()
setwd("C:/Users/gbylj/Desktop")
getwd()
#zainstaluj pakiet "readr"
install.packages("readr")
#zainstaluj pakiet "tidyverse"
install.packages("tidyverse")
install.packages("rpart")
install.packages("dummy")
install.packages("ggcorrplot")
install.packages("randomForest")
install.packages("caTools")
install.packages("caret")
install.packages("rpart.plot")
install.packages("ROSE")

#załaduj pakiety
library(readr)
library(tidyverse)
library(ggplot2)
library(rpart)
library(dummy)
library(ggcorrplot)
library(randomForest)
library(caTools)
library(caret)
library(rpart.plot)
library(ROSE)

#sprawdź czy posiadasz plik "heart_attack_china_youth_vs_adult" 
file.exists("heart_attack_china_youth_vs_adult.csv")
#odczytaj plik "heart_attack_china_youth_vs_adult" 
Dane_ataki_serca <- read_csv("heart_attack_china_youth_vs_adult.csv")
#pokaż dane 
Dane_ataki_serca
glimpse(Dane_ataki_serca)

#2.Zdefiniowanie problemu badawczego

#Celem projektu jest analiza danych dotyczących ataków serca w Chinach, aby zdefiniować jakie czynniki wpływają na wystąpienie ataku serca i próba stworzenie modelu, który przewidzi ryzyko wystąpienia ataku serca na podstawie załadowanych danych.


#3. Przygotowanie danych

#sprawdzam jakość danych
str(Dane_ataki_serca)
#przeprowadzam podsumowanie danych
summary(Dane_ataki_serca)
#sprawdzam czy są braki danych
missing_values_summary <- Dane_ataki_serca %>%
  summarise_all(~ sum(is.na(.)))
#wyświetlam wyniki braków danych
print(missing_values_summary)
#Nie ma brakujących wartości w danych
#sprawdzam czy są duplikaty danych
sum(duplicated(Dane_ataki_serca))
#Nie ma duplikatów w danych
#sprawdzam czy są wartości ujemne w kolumnach numerycznych Age, Blood_Pressure, Cholesterol, BMI, Stress_Level, Income_Level, Sleep_Hours, Heart_Rate, Physical_Activity_Hours, Dietary_Fiber_Intake, Sodium_Intake, Genetic_Risk_Score, Screen_Time
kolumny <- c("Age", "Blood_Pressure", "Cholesterol", "BMI", "Stress_Level", 
             "Income_Level", "Sleep_Hours", "Heart_Rate", "Physical_Activity_Hours", 
             "Dietary_Fiber_Intake", "Sodium_Intake", "Genetic_Risk_Score", "Screen_Time")
ujemne_wartosci <- sapply(Dane_ataki_serca[kolumny], function(x) any(x < 0))
ujemne_wartosci
#wartości ujemne Income_Level, Cholesterol
#wartości ujemne mogą występować w Income_Level, jeśli ktoś ma długi, wydaje więcej niż zarabia. Cholesterol nie może być ujemny.
#pokazuję ujemne wartości z kolumny Cholesterol
Dane_ataki_serca[Dane_ataki_serca$Cholesterol < 0, ]
#usuwam ujemne wartości z kolumny Cholesterol
Dane_ataki_serca$Cholesterol[Dane_ataki_serca$Cholesterol < 0] <- NA
#sprawdzam czy są braki danych
sum(is.na(Dane_ataki_serca))
#usuwam braki danych
Dane_ataki_serca <- na.omit(Dane_ataki_serca)
#sprawdzam czy są braki danych
sum(is.na(Dane_ataki_serca))
#sprawdzam czy są litery w kolumnach numerycznych Age, Blood_Pressure, Cholesterol, BMI, Stress_Level, Income_Level, Sleep_Hours, Heart_Rate, Physical_Activity_Hours, Dietary_Fiber_Intake, Sodium_Intake, Genetic_Risk_Score, Screen_Time
litery <- sapply(Dane_ataki_serca[kolumny], function(x) any(grepl("[a-zA-Z]", x)))
litery
#pokazuje kolumnę Genetic_Risk_Score
Dane_ataki_serca$Genetic_Risk_Score
#usuwam litery z kolumny Genetic_Risk_Score
Dane_ataki_serca$Genetic_Risk_Score <- as.numeric(gsub("[a-zA-Z]", "", Dane_ataki_serca$Genetic_Risk_Score))
#sprawdzam czy są braki danych
sum(is.na(Dane_ataki_serca))
#usuwam braki danych
Dane_ataki_serca <- na.omit(Dane_ataki_serca)
#pokazuję kolumnę Physical_Activity_Hours gdzie występują litery
Dane_ataki_serca$Physical_Activity_Hours
#usuwam litery z kolumny Physical_Activity_Hours
Dane_ataki_serca$Physical_Activity_Hours <- as.numeric(gsub("[a-zA-Z]", "", Dane_ataki_serca$Physical_Activity_Hours))
#sprawdzam czy są braki danych
sum(is.na(Dane_ataki_serca))
#usuwam braki danych
Dane_ataki_serca <- na.omit(Dane_ataki_serca)
#sprawdzam czy są braki danych
sum(is.na(Dane_ataki_serca))
#pokazuje dane w formie tabeli
View(Dane_ataki_serca)
# Usuwam wartości poniżej 75 i powyżej 150 z kolumny Blood_Pressure, ponieważ wartości te są nieprawidłowe, człowiek nie mógłby fukncjonować z cisnieniem krwi poniżej 75 i powyżej 150
Dane_ataki_serca <- Dane_ataki_serca %>%
  filter(Blood_Pressure >= 75 & Blood_Pressure <= 150)

#Przeprowadzam uproszczenie ilości zmiennych i usuwam kolumnę ID i Cultural_Background. Tło kulturowe w mojej ocenie nie jest warunkiem wpływjaacym na wystąpienie ataku serca.
Dane_ataki_serca <- Dane_ataki_serca %>%
  select(-ID)
Dane_ataki_serca <- select(Dane_ataki_serca, -Cultural_Background)
View(Dane_ataki_serca)
#Upraszczam dane, aby ułatwić interpretację. Zakrąglam wartości w kolumnach Blood_Pressure, Cholesterol, BMI, Stress_Level, Income_Level, Sleep_Hours, Heart_Rate, Physical_Activity_Hours, Dietary_Fiber_Intake, Sodium_Intake, Genetic_Risk_Score, Screen_Time do 2 miejsc po przecinku
Dane_ataki_serca$Blood_Pressure <- round(Dane_ataki_serca$Blood_Pressure, 2)
Dane_ataki_serca$Cholesterol <- round(Dane_ataki_serca$Cholesterol, 2)
Dane_ataki_serca$BMI <- round(Dane_ataki_serca$BMI, 2)
Dane_ataki_serca$Stress_Level <- round(Dane_ataki_serca$Stress_Level, 2)
Dane_ataki_serca$Income_Level <- round(Dane_ataki_serca$Income_Level, 2)
Dane_ataki_serca$Sleep_Hours <- round(Dane_ataki_serca$Sleep_Hours, 2)
Dane_ataki_serca$Heart_Rate <- round(Dane_ataki_serca$Heart_Rate, 2)
Dane_ataki_serca$Physical_Activity_Hours <- round(Dane_ataki_serca$Physical_Activity_Hours, 2)
Dane_ataki_serca$Dietary_Fiber_Intake <- round(Dane_ataki_serca$Dietary_Fiber_Intake, 2)
Dane_ataki_serca$Sodium_Intake <- round(Dane_ataki_serca$Sodium_Intake, 2)
Dane_ataki_serca$Genetic_Risk_Score <- round(Dane_ataki_serca$Genetic_Risk_Score, 2)
Dane_ataki_serca$Screen_Time <- round(Dane_ataki_serca$Screen_Time, 2)
View(Dane_ataki_serca)

#Sprawdzam kolumnę Age_Group czy wartości Youth i Adult przypisane są wełdug określonego wzoru
#Nie ma wzoru, edług własnej wiedzy i danych określam wartość Youth jako Age>= 12  i Age <=18 oraz określam wartość Adult jako Age >= 19 & Age <= 59

Dane_ataki_serca <- Dane_ataki_serca %>%
  mutate(Age_Group = case_when(
    Age >= 12 & Age <= 18 ~ "Youth",
    Age >= 19 & Age <= 59 ~ "Adult",
    TRUE ~ as.character(Age_Group)  # Zostawiam inne wartości bez zmian, jeśli istnieją
  ))

#4.Przeprowadzam analizę danych 

#Zaczynam od zmiennej Heart_Attack
table(Dane_ataki_serca$Heart_Attack)
#ile osób miało atak serca ?
Dane_ataki_serca %>%
  count(Heart_Attack)

# Zliczam poszczególne wartości zmiennej Heart_Attack
dane_heart_attack <- Dane_ataki_serca %>%
  count(Heart_Attack)

# Przedstawiam na wykresie kołowym jakie wartości przyjmuje zmienna Heart_Attack
ggplot(dane_heart_attack, aes(x = "", y = n, fill = Heart_Attack)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_minimal() +
  labs(title = "Rozkład zmiennej Heart_Attack",
       fill = "Atak serca") +
  scale_fill_brewer(palette = "Set3")

#Tworzę histogramy dla zmiennych numerycznych 

ggplot(Dane_ataki_serca, aes(x = Age)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Histogram wieku osób",
       x = "Wiek",
       y = "Liczba osób")

#Tworze wykres pudełkowy dla wieku w zależności od ataku serca 
ggplot(Dane_ataki_serca, aes(x = Heart_Attack, y = Age, fill = Heart_Attack)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Wykres pudełkowy dla wieku w zależności od ataku serca",
       x = "Atak serca",
       y = "Wiek") +
  scale_fill_manual(values = c("Yes" = "red", "No" = "blue"))

ggplot(Dane_ataki_serca, aes(x = Heart_Attack, y = Age)) +
  geom_boxplot(fill = "red", color = "black") +
  labs(title = "Boxplot zmiennej 'Age' w zależności od 'Heart_Attack'")



#Wykres pudełkowy dla Blood_Pressure

ggplot(Dane_ataki_serca, aes(x = Heart_Attack, y = Blood_Pressure, fill = Heart_Attack)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Wykres pudełkowy dla ciśnienia krwi w zależności od ataku serca",
       x = "Atak serca",
       y = "Ciśnienie krwi") +
  scale_fill_manual(values = c("Yes" = "red", "No" = "blue"))



#Wykres pudłekowy dla Cholesterol 

ggplot(Dane_ataki_serca, aes(x = Heart_Attack, y = Cholesterol, fill = Heart_Attack)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Wykres pudełkowy dla cholesterolu w zależności od ataku serca",
       x = "Atak serca",
       y = "Cholesterol") +
  scale_fill_manual(values = c("Yes" = "red", "No" = "blue"))



#Wykres pudełkowy dla BMI

ggplot(Dane_ataki_serca, aes(x = Heart_Attack, y = BMI, fill = Heart_Attack)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Wykres pudełkowy dla BMI w zależności od ataku serca",
       x = "Atak serca",
       y = "BMI") +
  scale_fill_manual(values = c("Yes" = "red", "No" = "blue"))



#Wykres pudełkowy dla Stress_Level
ggplot(Dane_ataki_serca, aes(x = Heart_Attack, y = Stress_Level, fill = Heart_Attack)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Wykres pudełkowy dla poziomu stresu w zależności od ataku serca",
       x = "Atak serca",
       y = "Poziom stresu") +
  scale_fill_manual(values = c("Yes" = "red", "No" = "blue"))



#Wykres pudełkowy dla Income_Level
ggplot(Dane_ataki_serca, aes(x = Heart_Attack, y = Income_Level, fill = Heart_Attack)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Wykres pudełkowy dla poziomu dochodów w zależności od ataku serca",
       x = "Atak serca",
       y = "Poziom dochodów") +
  scale_fill_manual(values = c("Yes" = "red", "No" = "blue"))



#Wykres pudełkowy dla Sleep_Hours

ggplot(Dane_ataki_serca, aes(x = Heart_Attack, y = Sleep_Hours, fill = Heart_Attack)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Wykres pudełkowy dla godzin snu w zależności od ataku serca",
       x = "Atak serca",
       y = "Godziny snu") +
  scale_fill_manual(values = c("Yes" = "red", "No" = "blue"))



#Wykres pudełkowy dla Heart_Rate

ggplot(Dane_ataki_serca, aes(x = Heart_Attack, y = Heart_Rate, fill = Heart_Attack)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Wykres pudełkowy dla tętna w zależności od ataku serca",
       x = "Atak serca",
       y = "Tętno") +
  scale_fill_manual(values = c("Yes" = "red", "No" = "blue"))



#Wykres pudełkowy dla Physical_Activity_Hours

ggplot(Dane_ataki_serca, aes(x = Heart_Attack, y = Physical_Activity_Hours, fill = Heart_Attack)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Wykres pudełkowy dla godzin aktywności fizycznej w zależności od ataku serca",
       x = "Atak serca",
       y = "Godziny aktywności fizycznej") +
  scale_fill_manual(values = c("Yes" = "red", "No" = "blue"))


#Wykres pudełkowy dla Dietary_Fiber_Intake

ggplot(Dane_ataki_serca, aes(x = Heart_Attack, y = Dietary_Fiber_Intake, fill = Heart_Attack)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Wykres pudełkowy dla spożycia błonnika w zależności od ataku serca",
       x = "Atak serca",
       y = "Spożycie błonnika") +
  scale_fill_manual(values = c("Yes" = "red", "No" = "blue"))


#Wykres pudełkowy dla Sodium_Intake

ggplot(Dane_ataki_serca, aes(x = Heart_Attack, y = Sodium_Intake, fill = Heart_Attack)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Wykres pudełkowy dla spożycia sodu w zależności od ataku serca",
       x = "Atak serca",
       y = "Spożycie sodu") +
  scale_fill_manual(values = c("Yes" = "red", "No" = "blue"))


#Wykres pudełkowy dla Genetic_Risk_Score

ggplot(Dane_ataki_serca, aes(x = Heart_Attack, y = Genetic_Risk_Score, fill = Heart_Attack)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Wykres pudełkowy dla Genetic_Risk_Score w zależności od ataku serca",
       x = "Atak serca",
       y = "Genetic Risk Score") +
  scale_fill_manual(values = c("Yes" = "red", "No" = "blue"))


#Wykres pudełkowy dla Screen_Time

ggplot(Dane_ataki_serca, aes(x = Heart_Attack, y = Screen_Time, fill = Heart_Attack)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Wykres pudełkowy dla czasu spędzonego przed ekranem w zależności od ataku serca",
       x = "Atak serca",
       y = "Czas spędzony przed ekranem (godziny)") +
  scale_fill_manual(values = c("Yes" = "red", "No" = "blue"))


#Wykres słupkowy dla Gender - Liczba osób z podziałem na płeć, które dozanały ataku serca
ggplot(Dane_ataki_serca, aes(x = Heart_Attack, fill = Gender)) +
  geom_bar(position = "dodge") +
  labs(title = "Liczba osób z podziałem na płeć, które dozanały ataku serca")
#Wniosek: Kobiety i mężczyźni doznawali ataków serca równie często, osoby dla których płeć określono jako "other" doznają ataków serca w podobnej proporcji. Płeć nie wpływa na doznanie ataku serca

#Wykres słupkowy dla Diabetes - Liczba osób z cukrzycą, które dozanały ataku serca
ggplot(Dane_ataki_serca, aes(x = Heart_Attack, fill = Diabetes)) +
  geom_bar(position = "dodge") +
  labs(title = "Liczba osób z cukrzycą, które dozanały ataku serca")
#Wniosek: Liczba osób z cukrzycą, które doznały ataku serca jest mniejsza niż liczba osób bez cukrzycy, które doznały ataku serca. Cukrzyca nie wpływa na wystąpienie ataku serca

#Wykres słupkowy dla Smoking - Liczba osób palących, które dozanały ataku serca
ggplot(Dane_ataki_serca, aes(x = Heart_Attack, fill = Smoking)) +
  geom_bar(position = "dodge") +
  labs(title = "Liczba osób palących, które dozanały ataku serca")
#Wniosek: Liczba osób palących, które dozanały ataku serca jest mniejsza niż liczba osób nie palących, które doznały ataku serca

#Wykres słupkowy dla Alcohol_Consumption
ggplot(Dane_ataki_serca, aes(x = Heart_Attack, fill = Alcohol_Consumption)) +
  geom_bar(position = "dodge") +
  labs(title = "Liczba osób spożywajacych alkohol, które dozanały ataku serca")
#Wniosek: Najwięcej ataków serca dostają osoby, które nie spożywają alkoholu. Spożywanie alkoholu nie zwieksza szansy na doznanie ataku serca

#Wykres słupkowy dla Exercise_Level
ggplot(Dane_ataki_serca, aes(x = Heart_Attack, fill = Exercise_Level)) +
  geom_bar(position = "dodge") +
  labs(title = "Liczba osób ćwiczących, które dozanały ataku serca")
#Wniosek: Najwięcej ataków serca doznają osoby ćwiczące w średnim zakresie, zaznaczając, że osób ćwiczących w średnim zakresie jest najwięcej

#Wykres słupkowy dla Family_History
ggplot(Dane_ataki_serca, aes(x = Heart_Attack, fill = Family_History)) +
  geom_bar(position = "dodge") +
  labs(title = "Liczba osób z historią rodzinną ataku serca, które dozanały ataku serca")
#Wniosek: Osób z historią rodzinną ataku serca, które doznały ataku serca, jest mniej niż osób bez historii rodzinej ataku serca, które doznały ataku serca

#Wykres słupkowy dla Diet_Type
ggplot(Dane_ataki_serca, aes(x = Heart_Attack, fill = Diet_Type)) +
  geom_bar(position = "dodge") +
  labs(title = "Liczba osób z podziałem na typ diety, które dozanały ataku serca")
#Wniosek: Najwięcej ataków serca doznają osoby, które utrzymują umiarkowaną dietę

#Wykres słupkowy dla Residence
ggplot(Dane_ataki_serca, aes(x = Heart_Attack, fill = Residence)) +
  geom_bar(position = "dodge") +
  labs(title = "Liczba osób z podziałem na miejsce zamieszkania, które dozanały ataku serca")
#Wniosek: Osoby mieszkające w mieście doznają więcej ataktów serca niż osoby mieszkajace na wsi

#Wykres słupkowy dla Education_Level
ggplot(Dane_ataki_serca, aes(x = Heart_Attack, fill = Education_Level)) +
  geom_bar(position = "dodge") +
  labs(title = "Liczba osób z podziałem na poziom edukacji, które dozanały ataku serca")
#Wniosek: Wiecej jest osób z ponadpostawowym poziomem wykształcenia, kóre doznają ataków serca, niż osób z podstawowym i wyższym wykształceniem, które doznają ataków serca. Najmniej osób doznajacych ataków serca posiada podstawowy poziom wykształcenia

#Wykres słupkowy dla Employment_Status
ggplot(Dane_ataki_serca, aes(x = Heart_Attack, fill = Employment_Status)) +
  geom_bar(position = "dodge") +
  labs(title = "Liczba osób z podziałem na status zatrudnienia, które dozanały ataku serca")
#Wniosek: Najwięcej jest osób pracujących, które doznają ataków serca, nastepnie studentów a najmniej jest osób bezrobotnych, które doświdczają ataków serca


#Wykres słupkowy dla Marital_Status
ggplot(Dane_ataki_serca, aes(x = Heart_Attack, fill = Marital_Status)) +
  geom_bar(position = "dodge") +
  labs(title = "Liczba osób z podziałem na stan cywilny, które dozanały ataku serca")
#Wniosek: Najwięcej jest osób zamężnych, które doznały ataku serca, nastepnie osób wolnych, a najmniej jest osób rozwiedzionyck które doznały ataku serca


#Wykres słupkowy dla Medication
ggplot(Dane_ataki_serca, aes(x = Heart_Attack, fill = Medication)) +
  geom_bar(position = "dodge") +
  labs(title = "Liczba osób przyjmujących leki, które dozanały ataku serca")
#Wniosek:Osób, które nie przyjmują leków i dostały ataków serca jest więcj niż osób które przyjmują leki i doznały ataki serca. Zażywanie leków może być warunkiem wpływającym na doznanie ataków serca. Zażywanie leków może zmniejszać ryzyko doznania ataku serca

#Wyres słupkowy dla Hypertension
ggplot(Dane_ataki_serca, aes(x = Heart_Attack, fill = Hypertension)) +
  geom_bar(position = "dodge") +
  labs(title = "Liczba osób z nadciśnieniem, które dozanały ataku serca")
#Wniosek: Osób bez nadcisnienia, która doznały atatków serca jest więcej niż, tych z nadciśnieniem, które doznały ataku serca. Nadciśnienie nie wpływa na atak serca

#Wyres słupkowy dla Age_Group
ggplot(Dane_ataki_serca, aes(x = Heart_Attack, fill = Age_Group)) +
  geom_bar(position = "dodge") +
  labs(title = "Liczba młodzieży i dorosłych, którzy dozanali ataku serca")
#Wniosek: Liczba dorosłych, którzy doznają ataków serca jest wieksza niż liczba młodzieży, która doznaje ataków serca. Kategoria wiekowa wpływa na to czy osoba dozna ataku serca.



#Konwertuje potrzebne do modelowania kolumny zastepując wartości zmiennych kategorycznych liczbami

library(dplyr)

#Przekształcam kolumnę zmiennej Heart_Attack w factor dla którego wartości przyjmują 1 dla Yes i 0 dla No

Dane_ataki_serca <- Dane_ataki_serca %>%
  mutate(Heart_Attack = ifelse(Heart_Attack == "Yes", 1, 0))

#Przeprowadzam konwersję zmiennej Heart_Attack do typu factor
Dane_ataki_serca <- Dane_ataki_serca %>%
  mutate(Heart_Attack = as.factor(Heart_Attack))


Dane_ataki_serca <- Dane_ataki_serca %>%
  mutate(Gender = case_when(
    Gender == "Male" ~ 1,
    Gender == "Female" ~ 0.5,
    Gender == "Other" ~ 0
  ))

Dane_ataki_serca <- Dane_ataki_serca %>%
  mutate(Diabetes = ifelse(Diabetes == "Yes", 1, 0))

Dane_ataki_serca <- Dane_ataki_serca %>%
  mutate(Smoking = ifelse(Smoking == "Yes", 1, 0))

Dane_ataki_serca <- Dane_ataki_serca %>%
  mutate(Alcohol_Consumption = case_when(
    Alcohol_Consumption == "None" ~ 0,
    Alcohol_Consumption == "Moderate" ~ 0.5,
    Alcohol_Consumption == "Heavy" ~ 1
  ))

Dane_ataki_serca <- Dane_ataki_serca %>%
  mutate(Exercise_Level = case_when(
    Exercise_Level == "Low" ~ 0,
    Exercise_Level == "Moderate" ~ 0.5,
    Exercise_Level == "High" ~ 1
  ))

Dane_ataki_serca <- Dane_ataki_serca %>%
  mutate(Family_History = ifelse(Family_History == "Yes", 1, 0))

Dane_ataki_serca <- Dane_ataki_serca %>%
  mutate(Diet_Type = case_when(
  Diet_Type == "Unhealthy" ~ 0,
  Diet_Type == "Moderate" ~ 0.5,
  Diet_Type == "Healthy" ~ 1))

Dane_ataki_serca <- Dane_ataki_serca %>%
  mutate(Residence = ifelse(Residence == "Urban", 1, 0))

Dane_ataki_serca <- Dane_ataki_serca %>%
  mutate(Education_Level = case_when(
    Education_Level == "Primary" ~ 0,
    Education_Level == "Secondary" ~ 0.5,
    Education_Level == "Tertiary" ~ 1
  ))

Dane_ataki_serca <- Dane_ataki_serca %>%
  mutate(Employment_Status = case_when(
    Employment_Status == "Employed" ~ 1,
    Employment_Status == "Student" ~ 0.5,
    Employment_Status == "Unemployed" ~ 0
  ))

Dane_ataki_serca <- Dane_ataki_serca %>%
  mutate(Marital_Status = case_when(
    Marital_Status == "Married" ~ 1,
    Marital_Status == "Single" ~ 0.5,
    Marital_Status == "Divorced" ~ 0
  ))

Dane_ataki_serca <- Dane_ataki_serca %>%
  mutate(Medication = ifelse(Medication == "Yes", 1, 0))

Dane_ataki_serca <- Dane_ataki_serca %>%
  mutate(Hypertension = ifelse(Hypertension == "Yes", 1, 0))


Dane_ataki_serca <- Dane_ataki_serca %>%
  mutate(Age_Group = ifelse(Age_Group == "Adult", 1, 0))



#Wyświetlam pierwsze kilka wierszy danych, aby sprawdzić modyfikację
head(Dane_ataki_serca)
view(Dane_ataki_serca)


#Przeprowadzam analizę korelacji

Dane_ataki_serca_numeric <- Dane_ataki_serca %>%
  select_if(is.numeric)

cor_matrix <-cor(Dane_ataki_serca_numeric, use = "pairwise.complete.obs")

print(cor_matrix)

#Wizualizuję korelacje

library(ggcorrplot)
ggcorrplot(cor_matrix, lab = TRUE, colors = c("red", "white", "green"),
           title = "Korelacja macierzy Dane_Ataki_Serca")

#Bazując na wiedzy naukowej opisującej czynniki najbardziej wpływajace na ryzyko wystąpienia zawału serca usuwam zbędne tabele, aby uprościć model:

kolumny_do_zachowania <- c("Age", "Gender","Heart_Attack", "Blood_Pressure", "Cholesterol", "Diabetes", "Smoking", 
                           "Alcohol_Consumption", "Exercise_Level", "BMI", "Family_History", "Stress_Level", "Age_Group")

# Usuwanie kolumn, które nie są wymienione w kolumny_do_zachowania
Dane_ataki_serca <- Dane_ataki_serca[, kolumny_do_zachowania]

glimpse(Dane_ataki_serca)

Dane_ataki_serca$Heart_Attack <- as.factor(Dane_ataki_serca$Heart_Attack)


Dane_ataki_serca_numeric <- Dane_ataki_serca %>%
  select_if(is.numeric)

cor_matrix <-cor(Dane_ataki_serca_numeric, use = "pairwise.complete.obs")

print(cor_matrix)

#Wizualizuję korelacje nr.2 po zredukowaniu kolumn 

library(ggcorrplot)
ggcorrplot(cor_matrix, lab = TRUE, colors = c("red", "white", "green"),
           title = "Korelacja macierzy Dane_Ataki_Serca")

#5. Przygotowuje dane do modelowania i tworzę modele

#**Model 1 - Random Forest**

library(caTools)
set.seed(123)
split <- sample.split(Dane_ataki_serca$Heart_Attack, SplitRatio = 0.8)
train_data <- subset(Dane_ataki_serca, split == TRUE)
test_data <- subset(Dane_ataki_serca, split == FALSE)

library(ROSE)
library(caret)


#Przeprowadzam balansowanie danych za pomocą ROSE
dane_zbalansowane <- ROSE(Heart_Attack ~ ., data = train_data)$data

#Trenuje modelu Random Forest
model_rf <- randomForest(Heart_Attack ~ ., data = dane_zbalansowane, ntree = 100)

#Przeprowadzam predykcję na danych testowych
predykcje_test <- predict(model_rf, newdata = test_data)

# Macierz pomyłek i inne metryki
confusionMatrix(predykcje_test, test_data$Heart_Attack)
print(confusionMatrix(predykcje_test, test_data$Heart_Attack))


#Model 1 - ogólna dokładność (Accuracy) wynosi 87%, model ma bardzo niską specyficzność (Specificity) i znikome Kappa. Oznacza to, że model bardzo słabo radzi sobie z poprawnym przewidywaniem klasy "1" (atak serca), mimo że całkiem nieźle przewiduje klasę "0" (brak ataku).  Model w zasadzie prawie zawsze przewiduje "0", co prowadzi do wysokiej dokładności tylko dlatego, że klasa "0" jest znacznie liczniejsza.

#Sprawdzam czy ROSE działa poprawnie 

table(dane_zbalansowane$Heart_Attack)

# Sprawdzenie rozkładu klas
print("Tabela częstości:")
print(table(dane_zbalansowane$Heart_Attack))

print("\nProporcje klas:")
print(prop.table(table(dane_zbalansowane$Heart_Attack)))

# Wykres słupkowy
library(ggplot2)
ggplot(dane_zbalansowane, aes(x = Heart_Attack, fill = Heart_Attack)) +
  geom_bar() +
  labs(title = "Rozkład klas po zastosowaniu ROSE",
       x = "Atak serca",
       y = "Liczba obserwacji") +
  theme_minimal()
#ROSE poprawnie zbalansował zbiór danych 

#2 Przeprowadzam regularyzację w celu polepszenia modelu 

install.packages("glmnet")
library(glmnet)
# Upewnij się, że kolumna Heart_Attack jest typu factor
Dane_ataki_serca$Heart_Attack <- as.factor(Dane_ataki_serca$Heart_Attack)

# Przygotowanie macierzy cech i wektora celów
x <- model.matrix(Heart_Attack ~ ., Dane_ataki_serca)[,-1]
y <- Dane_ataki_serca$Heart_Attack

# Dopasowanie modelu z użyciem regresji logistycznej z regularyzacją
model <- cv.glmnet(x, y, alpha = 1, family = "binomial")

# Wyświetlenie wyników modelu
print(model)

model_glmnet <- cv.glmnet(x, y, alpha = 1, family = "binomial", lambda = seq(0.0001, 1, by = 0.01)) # Zmiana zakresu lambda
print(model_glmnet)

any(is.na(Dane_ataki_serca))

#Przeprowadzam uproszczony test by sprawdzić czy problem wyników modelu leży w jakości danych 

# Weź tylko kilka wierszy i kolumn
dane_male <- Dane_ataki_serca[1:1000, c("Heart_Attack", "Age", "Blood_Pressure", "Cholesterol")]

# Konwertuj Heart_Attack na factor
dane_male$Heart_Attack <- as.factor(dane_male$Heart_Attack)

# Utwórz x i y
x_male <- model.matrix(Heart_Attack ~ ., dane_male)[,-1]
y_male <- dane_male$Heart_Attack

# Skaluj x
x_male_scaled <- scale(x_male)

# Uruchom cv.glmnet
model_glmnet_male <- cv.glmnet(x_male_scaled, y_male, alpha = 1, family = "binomial")
print(model_glmnet_male)


#Zmiana wartości lambda w cv.glmnet sama w sobie nie rozwiąże problemu, jeśli problem leży w danych.


#Biorę tylk kilka wierszy i kolumn w ramach testu
dane_male <- Dane_ataki_serca[1:1000, c("Heart_Attack", "Age", "Age_Group")]

# Konwertuje Heart_Attack na factor
dane_male$Heart_Attack <- as.factor(dane_male$Heart_Attack)

# Utwórz x i y
x_male <- model.matrix(Heart_Attack ~ ., dane_male)[,-1]
y_male <- dane_male$Heart_Attack

# Skaluj x
x_male_scaled <- scale(x_male)

# Uruchom cv.glmnet
model_glmnet_male <- cv.glmnet(x_male_scaled, y_male, alpha = 1, family = "binomial")
print(model_glmnet_male)

coef(model_glmnet_male, s = "lambda.min")

#**Model 2 - XGBoost**

library(xgboost)

# Przekształć dane do formatu macierzy
x_train <- as.matrix(train_data[, -which(names(train_data) == "Heart_Attack")])  # Usuń kolumnę celu
y_train <- as.numeric(as.character(train_data$Heart_Attack))  # Kolumna celu (konwersja na numeryczny)

x_test <- as.matrix(test_data[, -which(names(test_data) == "Heart_Attack")])
y_test <- as.numeric(as.character(test_data$Heart_Attack))




# Parametry modelu
params <- list(
  objective = "binary:logistic",  # Klasyfikacja binarna
  eval_metric = "logloss",        # Metryka ewaluacji (logarytmiczna strata)
  max_depth = 6,                  # Maksymalna głębokość drzewa
  eta = 0.1,                      # Szybkość uczenia
  nthread = 4                     # Liczba wątków
)

# Trenowanie modelu
model_xgb <- xgboost(
  data = x_train,
  label = y_train,
  params = params,
  nrounds = 100,                  # Liczba iteracji
  verbose = 1                     # Wyświetl postęp
)




# Predykcja na danych testowych
preds <- predict(model_xgb, x_test)

# Przekształć przewidywania na wartości binarne (0 lub 1)
preds_binary <- ifelse(preds > 0.5, 1, 0)


# Macierz pomyłek
confusion_matrix <- table(Predicted = preds_binary, Actual = y_test)
print(confusion_matrix)

# Dokładność (Accuracy)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))

# Precyzja (Precision)
precision <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])
print(paste("Precision:", precision))

# Czułość (Recall)
recall <- confusion_matrix[1, 1] / sum(confusion_matrix[, 1])
print(paste("Recall:", recall))

# F1-score
f1_score <- 2 * (precision * recall) / (precision + recall)
print(paste("F1-Score:", f1_score))


# Przykład dostrajania hiperparametrów z użyciem pakietu `caret`
library(caret)

# Definiowanie siatki hiperparametrów
xgb_grid <- expand.grid(
  nrounds = c(100, 200),
  max_depth = c(3, 6, 9),
  eta = c(0.01, 0.1, 0.3),
  gamma = c(0, 0.1, 0.2),
  colsample_bytree = c(0.6, 0.8, 1.0),
  min_child_weight = c(1, 3, 5),
  subsample = c(0.6, 0.8, 1.0)
)

# Kontrola walidacji krzyżowej
train_control <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  allowParallel = TRUE
)

# Trenowanie modelu z dostrajaniem
xgb_tuned <- train(
  x = x_train,
  y = as.factor(y_train),
  method = "xgbTree",
  trControl = train_control,
  tuneGrid = xgb_grid,
  verbose = TRUE
)

# Najlepsze hiperparametry
print(xgb_tuned$bestTune)


# Ważność zmiennych
importance_matrix <- xgb.importance(model = model_xgb)
print(importance_matrix)

# Wizualizacja ważności zmiennych
xgb.plot.importance(importance_matrix)


#Na podstawie ważności zmiennych zachowuje jedynie istotne dla modelowania kolumny 


kolumny_do_zachowania2 <- c("Age","Heart_Attack", "Blood_Pressure", "Cholesterol",   
                           "Alcohol_Consumption", "BMI", "Stress_Level")

# Usuwaam kolumny, które nie są wymienione w kolumny_do_zachowania
Dane_ataki_serca <- Dane_ataki_serca[, kolumny_do_zachowania2]


Dane_ataki_serca$Heart_Attack <- as.factor(Dane_ataki_serca$Heart_Attack)


Dane_ataki_serca_numeric <- Dane_ataki_serca %>%
  select_if(is.numeric)

cor_matrix <-cor(Dane_ataki_serca_numeric, use = "pairwise.complete.obs")

print(cor_matrix)


#Wizualizuję korelacje nr.3 po zredukowaniu kolumn 

library(ggcorrplot)
ggcorrplot(cor_matrix, lab = TRUE, colors = c("red", "white", "green"),
           title = "Korelacja macierzy Dane_Ataki_Serca")


library(caTools)
set.seed(123)
split <- sample.split(Dane_ataki_serca$Heart_Attack, SplitRatio = 0.8)
train_data <- subset(Dane_ataki_serca, split == TRUE)
test_data <- subset(Dane_ataki_serca, split == FALSE)



x_train <- as.matrix(train_data[, -which(names(train_data) == "Heart_Attack")])  # Usuń kolumnę celu
y_train <- as.numeric(as.character(train_data$Heart_Attack))  # Kolumna celu (konwersja na numeryczny)

x_test <- as.matrix(test_data[, -which(names(test_data) == "Heart_Attack")])
y_test <- as.numeric(as.character(test_data$Heart_Attack))

#Ustawiam parametry modelu
params <- list(
  objective = "binary:logistic",  # Klasyfikacja binarna
  eval_metric = "logloss",        # Metryka ewaluacji (logarytmiczna strata)
  eta = 0.01,                     # Szybkość uczenia
  max_depth = 3,                  # Maksymalna głębokość drzewa
  gamma = 0.0,                    # Minimalna strata do podziału
  colsample_bytree = 0.6,         # Frakcja zmiennych do użycia w każdym drzewie
  min_child_weight = 1,           # Minimalna waga dziecka
  subsample = 0.8                 # Frakcja obserwacji do użycia w każdym drzewie
)

#Trenuję model
model_xgb <- xgboost(
  data = x_train,
  label = y_train,
  params = params,
  nrounds = 200,                  # Liczba iteracji
  verbose = 1                     # Wyświetl postęp
)

#Predykcja na danych testowych
preds <- predict(model_xgb, x_test)

# Przekształć przewidywania na wartości binarne (0 lub 1)
preds_binary <- ifelse(preds > 0.5, 1, 0)

#Ocena modelu

#Macierz pomyłek
confusion_matrix <- table(Predicted = preds_binary, Actual = y_test)
print("Confusion Matrix:")
print(confusion_matrix)

#Dokładność (Accuracy)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))

#Precyzja (Precision)
precision <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])
print(paste("Precision:", precision))

#Czułość (Recall)
recall <- confusion_matrix[1, 1] / sum(confusion_matrix[, 1])
print(paste("Recall:", recall))

#F1-score
f1_score <- 2 * (precision * recall) / (precision + recall)
print(paste("F1-Score:", f1_score))

#Ważność zmiennych
importance_matrix <- xgb.importance(model = model_xgb)
print("Variable Importance:")
print(importance_matrix)

#Wizualizacja ważności zmiennych
xgb.plot.importance(importance_matrix)


#Wniosek: W używanych danych nie ma silnych liniowych zależności między zmiennymi numerycznymi. Dlatego wykorzystałem modelowanie w XGBoost, który skupia się na nieliniowych zależnościach.




# Załaduj potrzebne pakiety
library(ggplot2)

#Przygotowanie danych
# Wybierz tylko kolumny numeryczne
dane_numeryczne <- Dane_ataki_serca %>%
  select_if(is.numeric)

#Skalowanie danych
dane_skalowane <- scale(dane_numeryczne)

#Wykonanie PCA
pca_result <- prcomp(dane_skalowane, center = TRUE, scale. = TRUE)

# Podsumowanie PCA
summary(pca_result)

# Wyświetl wyniki PCA
print(pca_result)

#Wizualizacja wyników
#Tworzymę ramkę danych z wynikami PCA
pca_data <- as.data.frame(pca_result$x)

# Dodajemy kolumnę Heart_Attack do wizualizacji
pca_data$Heart_Attack <- Dane_ataki_serca$Heart_Attack

# Wykres punktowy dla dwóch pierwszych składowych głównych (PC1 i PC2)
ggplot(pca_data, aes(x = PC1, y = PC2, color = Heart_Attack)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "PCA: Wizualizacja dwóch pierwszych składowych głównych",
       x = "PC1 (Pierwsza składowa główna)",
       y = "PC2 (Druga składowa główna)",
       color = "Atak serca") +
  scale_color_manual(values = c("0" = "blue", "1" = "red"))

# Wykres wariancji wyjaśnionej przez składowe główne
var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
var_explained_df <- data.frame(PC = paste0("PC", 1:length(var_explained)),
                               Variance = var_explained)

ggplot(var_explained_df, aes(x = PC, y = Variance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Wariancja wyjaśniona przez składowe główne",
       x = "Składowe główne (PC)",
       y = "Procent wariancji wyjaśnionej") +
  scale_y_continuous(labels = scales::percent)

#Wniosek PCA nie przyniosło znaczącej redukcji wymiarowości


dane_zbalansowane <- ROSE(Heart_Attack ~ ., data = train_data, seed = 123)$data
table(dane_zbalansowane$Heart_Attack)


precision <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])
recall <- confusion_matrix[1, 1] / sum(confusion_matrix[, 1])
f1_score <- 2 * (precision * recall) / (precision + recall)
print(paste("F1-Score:", f1_score))


Dane_ataki_serca$Heart_Attack <- factor(Dane_ataki_serca$Heart_Attack, levels = c(0, 1), labels = c("No", "Yes"))

levels(Dane_ataki_serca$Heart_Attack)


# Podziel dane na zbiór treningowy i testowy
set.seed(123)
split <- sample.split(Dane_ataki_serca$Heart_Attack, SplitRatio = 0.8)
train_data <- subset(Dane_ataki_serca, split == TRUE)
test_data <- subset(Dane_ataki_serca, split == FALSE)

# Użyj modelu (np. XGBoost)
library(xgboost)
x_train <- as.matrix(train_data[, -which(names(train_data) == "Heart_Attack")])
y_train <- as.numeric(train_data$Heart_Attack) - 1  # Konwersja na 0 i 1
x_test <- as.matrix(test_data[, -which(names(test_data) == "Heart_Attack")])
y_test <- as.numeric(test_data$Heart_Attack) - 1

# Parametry modelu
params <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  max_depth = 6,
  eta = 0.1,
  nthread = 4
)

# Trenowanie modelu
model_xgb <- xgboost(
  data = x_train,
  label = y_train,
  params = params,
  nrounds = 100,
  verbose = 1
)

# Predykcja na danych testowych
preds <- predict(model_xgb, x_test)
preds_binary <- ifelse(preds > 0.5, 1, 0)

# Macierz pomyłek
confusion_matrix <- table(Predicted = preds_binary, Actual = y_test)
print(confusion_matrix)


#Zapisz modele
saveRDS(model_rf, "model_rf.rds")
saveRDS(model_xgb, "model_xgb.rds")


#Wniosek końcowy: Ze wzgędu na niskie korelacje między zmiennymi numerycznymi, które określają warunki wpływające na wystąpienie ataku serca oraz wyniki modeli Random Forest i XGBoost, nie jesteśmy w stanie przewidzieć precyzyjnie ataku serca na podstawie dostępnych danych. Po przeprowadzeniu selekcji danych, analizie danych, korelacji oraz zbudowaniu modeli, wraz z naniesieniem poprawek doszedłem do konkluzji, że zbiór danych wykorzystany do stworzenia projektu nie zawiera informacji, które pozwoliłby stworzyć model przewidujący atak serca z dużą dokładnością. Głównym powodem są zmienne, których korelacje ze sobą są marginalne.
