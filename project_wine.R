#importing the dataset
wine <- read.csv('WineQT.csv')
head(wine)
hist(wine$quality)
wine$taste <- ifelse(wine$quality < 6, 'bad', 'good')
wine$taste[wine$quality == 6] <- 'normal'
wine$taste <- as.factor(wine$taste)
wine$taste
str(wine)

library(ggplot2)
library(gridExtra)
n1 <- qplot(x = fixed.acidity, data = wine,binwidth = 0.1) +
  scale_x_continuous(breaks = seq(4, 16, 1))
n2 <- qplot(x = volatile.acidity, data = wine, binwidth = 0.01) +
  scale_x_continuous(breaks = seq(0.12, 1.58, 0.1))
n3 <- qplot(x = citric.acid, data = wine, binwidth = 0.01) +
  scale_x_continuous(breaks = seq(0, 1, 0.1))
n4 <- qplot(x = density, data = wine)
n5 <- qplot(x = pH, data = wine)
n6 <- qplot(x = alcohol, data = wine)
grid.arrange(n1, n2, n3, n4, n5, n6, ncol = 2)

p1up = qplot(x = taste, y = alcohol, data = wine, geom = "boxplot")
p2up = qplot(x = taste, y = sulphates, data = wine, geom = "boxplot")
p3up = qplot(x = taste, y = citric.acid, data = wine, geom = "boxplot")
p4up = qplot(x = taste, y = fixed.acidity, data = wine, geom = "boxplot")
grid.arrange(p1up, p2up, p3up, p4up, ncol = 2)

p1d = qplot(x = taste, y = volatile.acidity,  data = wine, geom = "boxplot")
p2d = qplot(x = taste, y = pH, data = wine,geom = "boxplot")
p3d = qplot(x = taste, y = density, data = wine, geom = "boxplot")
p4d = qplot(x = taste, y = density,  data = wine,geom = "boxplot")

grid.arrange(p1d, p2d, p3d, ncol = 2)


wine$quality.factor <- factor(wine$quality)
p1<- ggplot(aes(x = log10(sulphates), y = alcohol, colour = quality.factor), 
       data = wine) + 
  geom_point(aes(size = quality.factor)) +
  scale_color_brewer(type = 'div', palette="Set1") +
  scale_x_continuous(lim=c(quantile(log10(wine$sulphates), 0.01),
                           quantile(log10(wine$sulphates), 0.99)))+
  scale_y_continuous(lim=c(quantile(wine$alcohol, 0.01),
                           quantile(wine$alcohol, 0.99))) 

p2<- ggplot(aes(x = fixed.acidity, y = density, colour = quality.factor), 
       data = wine) + 
  geom_point(size = 4) +
  #geom_point() +
  scale_color_brewer(type = 'div', palette="Set1") +
  scale_x_continuous(lim=c(quantile(wine$fixed.acidity, 0.01),
                           quantile(wine$fixed.acidity, 0.99))) +
  scale_y_continuous(lim=c(quantile(wine$density, 0.01),
                           quantile(wine$density, 0.99)))

p3 <- ggplot(aes(x = pH, y = total.sulfur.dioxide, colour = quality.factor), 
             data = wine) + 
  geom_point(aes(size = quality.factor)) +
  scale_color_brewer(type = 'div', palette="Set1") +
  scale_x_continuous(lim=c(quantile(wine$pH, 0.01),
                           quantile(wine$pH, 0.99))) +
  scale_y_continuous(lim=c(quantile(wine$total.sulfur.dioxide, 0.01),
                           quantile(wine$total.sulfur.dioxide, 0.99)))

p4 <- ggplot(aes(x = log10(total.sulfur.dioxide), 
                 y = log10(free.sulfur.dioxide), colour = quality.factor), 
             data = wine) + 
  geom_point(aes(size = quality.factor)) +
  #geom_point(aes(size = 12)) + 
  scale_color_brewer(type = 'div', palette="Set1") +
  scale_x_continuous(lim=c(quantile(log10(wine$total.sulfur.dioxide),
                                    0.01),
                           quantile(log10(wine$total.sulfur.dioxide),
                                    0.99))) +
  scale_y_continuous(lim=c(quantile(log10(wine$free.sulfur.dioxide),
                                    0.01),
                           quantile(log10(wine$free.sulfur.dioxide),
                                    0.99)))
grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
