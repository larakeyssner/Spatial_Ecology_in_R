#pixels are training for the model of classification --> group in areas or characteristics found 
#k-means distance --> clustering method to assess distances --> if close, more likely related to the closest group/class and will be assigned to this group 
#code to classify data 

library(terra)
library(imageRy)
library(ggplot2)


im.list() #list of the package to copy and paste 
m1992 <- im.import("matogrosso_l5_1992219_lrg.jpg")
plot(m1992)
m2006 <- im.import("matogrosso_ast_2006209_lrg.jpg")
plot(m2006)
sun <- im.import("Solar_Orbiter_s_first_views_of_the_Sun_pillars.jpg")

sunc <- im.classify(sun, num_cluster=3) #unsuperwised classification -> already plot 
#random selection of the points - it can look different for each one 

sunc2 <- im.classify(sun, num_cluster=4)

par(mfrow=c(2,1))
plot(sun)
plot(sunc)

m1992c <- im.classify(m1992, num_cluster=2)
m2006c <- im.classify(m2006, num_cluster=2)
#1 =rainforest and 2= human + water 

#calculation frequencies 
f1992 <- freq(m1992c)

#prportions f/tot
tot1992c <- ncell(m1992c)
prop1992 = f1992$count / tot1992c #calculation of the proportion

perc1992 = prop1992 *100 #for% 17% human, 83% forest 

#in one line 
perc1992 = freq(m1992c) *100 / ncell(m1992c)


perc2006 = freq(m2006c) *100 / ncell(m2006c) #45% forest, 54% human 

#lets implement a dataframe with three columns 
#class perc1992 and perc2006

class <- c("human", "forest")
perc1992 <- c(17, 83)
perc2006 <- c(55, 45)
table <- data.frame(class, perc1992, perc2006)
table 

#FINAL GRAPH (aes aestatics) --> color =z-axis 
ggplot(table, aes(x=class, y=perc1992, color=class)) +
geom_bar(stat="identity", fill="white")
#nur das Gerüst + für mehr also data rein --> geo.bar für histogram BEST CHOICE 


ggplot(table, aes(x=class, y=perc1992, color=class)) +
geom_point(stat="identity", fill="white") #no bar but points 


ggplot(table, aes(x=class, y=perc1992, color=class)) +
geom_line(stat="identity", fill="white") #no bar but lines 

ggplot(table, aes(x=class, y=perc2006, color=class)) +
geom_bar(stat="identity", fill="white") #he prefers white


install.packages("patchwork")
library(patchwork)
#next to each other 
p1 <- ggplot(table, aes(x=class, y=perc1992, color=class)) +
geom_bar(stat="identity", fill="white")

p2 <-ggplot(table, aes(x=class, y=perc2006, color=class)) +
geom_bar(stat="identity", fill="white") #he prefers white

p1+p2 #next to each other 

p1/p2 #on top 

#scales are different 
p1sc <- ggplot(table, aes(x=class, y=perc1992, color=class)) +
  geom_bar(stat="identity", fill="white") + 
  ylim (c(0,100)) 

p2sc <-ggplot(table, aes(x=class, y=perc2006, color=class)) +
  geom_bar(stat="identity", fill="white") +
  ylim (c(0,100))









