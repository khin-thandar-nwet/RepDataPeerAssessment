---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
Hola compañeros este es mi trabajo en realidad pienso que dejarlo en español puede ser una buena idea.

## Loading and preprocessing the data


```r
datos <- read.csv(file = "activity.csv", header = TRUE)
datos$date <- as.Date(datos$date)
```

A continuacion veremos cual es el total de pasos tomados por dia y su media
## What is mean total number of steps taken per day?

```r
totalpasos <- tapply(datos$steps, datos$date, sum)
head(totalpasos)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##         NA        126      11352      12116      13294      15420
```

```r
hist(totalpasos)
```

![](PA1_template_files/figure-html/histograma-1.png)<!-- -->

La media y mediana de la cantidad de pasos al dia son


```r
media1 <- mean(totalpasos, na.rm = TRUE)
mediana1 <- median(totalpasos, na.rm = TRUE)
media1
```

```
## [1] 10766.19
```

```r
mediana1
```

```
## [1] 10765
```

## What is the average daily activity pattern?


```r
pasos_tiempo <- tapply(datos$steps, datos$interval, mean, na.rm = TRUE)
plot(x= names(pasos_tiempo),y=pasos_tiempo, type = "l", xlab = "Time", ylab = "Steps")
```

![](PA1_template_files/figure-html/promintervalo-1.png)<!-- -->

El valor maximo se encuentra en 


```r
maximo <- which.max(pasos_tiempo)
tmaximo <- cbind(names(pasos_tiempo[maximo]), pasos_tiempo[maximo])
colnames(tmaximo) <- c("Time", "Steps")
rownames(tmaximo) <- ""
tmaximo
```

```
##  Time  Steps             
##  "835" "206.169811320755"
```

## Imputing missing values
El numero de NA´s en el conjunto de datos es:

```r
sum(is.na(datos$steps))
```

```
## [1] 2304
```

Para imputar los datos entonces tomaremos la media de los datos y se la asignaremos a los faltantes


```r
datos2 <- datos
datos2$steps[which(is.na(datos2$steps))] <- mean(datos2$steps, na.rm = TRUE)
totalpasos2 <- tapply(datos2$steps, datos2$date, sum)
head(totalpasos2)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##   10766.19     126.00   11352.00   12116.00   13294.00   15420.00
```

```r
hist(totalpasos2)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

Las nuevas media y mediana son

```r
media2 <- mean(totalpasos2, na.rm = TRUE)
mediana2 <- median(totalpasos2, na.rm = TRUE)
difmean <- media2 - media1
difmedian <- mediana2 - mediana1
```

Vemos que la diferencia es positiva y que la media aumenta en 0 y la mediana en 1.1886792
The impact is minimal in the mean and median of the steps.

## Are there differences in activity patterns between weekdays and weekends?
Se determino si el registro es un dia entre semana o un dia de fin de semana


```r
datos2$dias <- weekdays(datos2$date)
datos2$dias[which(datos2$dias=="sábado" | datos2$dias=="domingo")] <- "weekend"
datos2$dias[which(datos2$dias!="weekend")] <- "weekday"
```

Ahora veremos si hay diferencia en la tendencia


```r
week <- datos2[datos2$dias=="weekday", ]
weekend <- datos2[datos2$dias=="weekend", ]

pasos_week <- tapply(week$steps, week$interval, mean, na.rm = TRUE)
pasos_weekend <- tapply(weekend$steps, weekend$interval, mean, na.rm = TRUE)

par(mfrow=c(1,2))
plot(x= names(pasos_week),y=pasos_week, type = "l", xlab = "Time", ylab = "Steps", col="blue", ylim = c(0,200), main = "WEEKDAY")

plot(x= names(pasos_weekend),y=pasos_weekend, type = "l", xlab = "Time", ylab = "Steps", col="red", ylim = c(0,200), main = "WEEKEND")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
