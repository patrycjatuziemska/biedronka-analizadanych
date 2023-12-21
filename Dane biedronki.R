dane <- read.csv("C:/Users/pssz/Desktop/Folder wszystkich folderów/PG - analityka/Analiza danych/Biedronki.csv")

install.packages("tidyverse")
install.packages("dlookr")
install.packages("editrules")
install.packages("VIM")
install.packages("deducorrect")
install.packages("ISLR")

library(tidyverse)
library(dlookr)
library(editrules)
library(VIM)
library(deducorrect)
library(ISLR) 

dane

# Sprawdzam, ile przypadków danych jest kompletnych (czyli brak NA)
sum(complete.cases(dane))

# Sprawdzam, czy dane zawierają jakieś inne niż NA wartości specjalne
is.special <- function(x){
  if (is.numeric(x)) !is.finite(x) else is.na(x)}
sapply(dane, is.special)

# Tworzę pętlę, która ma przypisać ew. specjalne wartości do NA
for (n in colnames(dane)){
  is.na(dane[[n]]) <- is.special(dane[[n]])
}

# Wydruk statystyk opisowych dla zmiennych 
summary(dane)

# Instaluję pakiet naniar, celem zwizualizowania obecności NA 
install.packages("naniar")
library(naniar)
vis_miss(dane)
# Ciekawa obserwacja (to są te NA czy ich nie ma?) 
dane %>% 
  select(Tax.5.,Total)
gg_miss_fct(dane2, fct = Total)

# Tworzę wykresy dla każdej zmiennej, aby sprawdzić jak wygląda ich ukształtowanie
boxplot(dane$Unit.price)
boxplot(dane$Tax.5.) #zawiera wartości odstające
boxplot(dane$Total) #zawiera wartości odstające 
boxplot(dane$cogs) #zawiera wartości odstające
boxplot(dane$gross.margin.percentage) 
boxplot(dane$gross.income) #zawiera wartości odstające
boxplot(dane$Rating)

# Wyselekcjonowanie wartości odstających 
odstające_Tax.5 <- boxplot.stats(dane$Tax.5.)$out
odstające_Tax.5_r <- which(dane$Tax.5. %in% odstające_Tax.5)
dane[odstające_Tax.5_r,]

odstające_Total <- boxplot.stats(dane$Total)$out
odstające_Total_r <- which(dane$Total %in% odstające_Total)
dane[odstające_Total_r,]

odstające_cogs <- boxplot.stats(dane$cogs)$out
odstające_cogs_r <- which(dane$cogs %in% odstające_cogs)
dane[odstające_cogs_r,]

odstające_gross.income <- boxplot.stats(dane$gross.income)$out
odstające_gross.income_r <- which(dane$gross.income %in% odstające_gross.income)
dane[odstające_gross.income_r,]






