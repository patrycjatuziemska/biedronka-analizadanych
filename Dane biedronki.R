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

Przechodząc do czyszczenia, cały proces rozpoczeliśmy od sprawdzenia, ile przypadków danych jest kompletnych, czyli czy występując NA. Jednak braki danych to nie wszystko. Postanowiliśmy sprawdzić, czy w zbiorze danych znajdują się jakieś wartości specjalne. Do tego posłużyła specjalna funkcja. Na wypadek, gdyby pojawiły się jakieś wartości specjalne, zdecydowaliśmy się, że przypiszemy im wartości NA. W tym celu wykorzystaliśmy pętlę. Tak przeprowadzone operacje pozwalają wydrukować statystyki opisowe dla zmiennych. 

```{r}
sum(complete.cases(dane))

is.special <- function(x){
  if (is.numeric(x)) !is.finite(x) else is.na(x)}
sapply(dane, is.special)

for (n in colnames(dane)){
  is.na(dane[[n]]) <- is.special(dane[[n]])
}
summary(dane)
```

Powyższy kod pozwala stwierdzić, że w zbiorze danych nie ma NA. Jednak postanowiliśmy zagłębić się jeszcze w słuszność tego twierdzenia. Ostatecznym potwierdzeniem będzie dla nas wizualizacja obecności ewentualnych NA. W tym celu pobraliśmy specjalny pakiet "naniar". 
```
# Wizualizacja brakujących danych dla całego zbioru za pomocą pakietu naniar. 
```
#Warto jednak spojrzeć dzięki wykresom na braki danych poszczególnych zmiennych. Przedstawimy to za pomocą konkretnego typu --> wykresów punktowych w relacji do zmiennej Total.
```{r}
vis_miss(dane)
```
#Niesamowite narzędzie naniar pozwala na stworzenie shadowmap. Takie wykresy pokażemy również dla wybranych zmiennych
```{r}
ggplot(dane, aes(x = Tax.5., y = Total)) +
  geom_density_2d_filled() +
  theme_minimal() +
  ggtitle("Shadowmapa dla Tax.5. vs Total")
```

```{r}
ggplot(dane, aes(x = Unit.price, y = Total)) +
  geom_density_2d_filled() +
  theme_minimal() +
  ggtitle("Shadowmap dla Unit.price vs Total")

```{r}
ggplot(dane, aes(x = cogs, y = Total)) +
  geom_density_2d_filled() +
  theme_minimal() +
  ggtitle("Shadowmap dla cogs vs Total")
```{r}
ggplot(dane, aes(x = gross.income, y = Total)) +
  geom_density_2d_filled() +
  theme_minimal() +
  ggtitle("Shadowmap dla gross.margin.percentage vs Total")
```
Co do braków danych mamy już absolutną pewność. Dzięki temu możemy pójść dalej i spojrzeć na to jak ukształtowują się zmienne jakościowe (liczbowe). W tym celu wykorzystamy popularną metodę wizualizacji - wykresy pudełkowe.Taki zabieg na danych pozwoli na wyselekcjonowanie tych zmiennych ciągłych, które zawierają wartości odstające. 

```{r}
boxplot(dane$Unit.price)
boxplot(dane$Tax.5.) #zawiera wartości odstające
boxplot(dane$Total) #zawiera wartości odstające 
boxplot(dane$cogs) #zawiera wartości odstające
boxplot(dane$gross.margin.percentage) 
boxplot(dane$gross.income) #zawiera wartości odstające
boxplot(dane$Rating)
```
