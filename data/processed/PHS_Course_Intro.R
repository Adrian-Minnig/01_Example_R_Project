# Important Notes and Exercices from the Introduction Lecture
# R is the engine of a car and RStudio is the dashboard

# Everything done in RStudio can also be done in basic R. RSTudio is just an
# easier to use interface.
# The following steps are how to get started working with RStudio.

# The working directory can also be set using the gear wheel icon 
#in the bottom right window.

# Example of creating data:
id<-c(1,2,3,4,5)
# The same thing can be done with:
id<-c(1:5)

id<-seq(1,5,1)
id<-seq(1,5,1)

# The following things created are objects. These can be seen in the environment
# after having been created.
names<-c("Mark", "Jack","Anna", "Jill", "Tom")
names
gender<-c(0,0,1,1,0)
gender

a<-1:5
b<-6:10

mat<-cbind(a,b)        # This command binds coloumns
mat<-rbind(a,b)           # This command binds rows


dat<-data.frame(ID=id, Name=names, Gender=gender)  
dat
dat [1, ]                      # Comma separates coloumns and rows
dat[2,2 ]


dat

sel<- dat$Gender == 1               # Two equal signs will make a comparison
sum(sel)                            # This sums up the number of TRUE comparisons

dat[sel, ] 

getwd()
data<-read.csv("perulung_ems.csv")
getwd()
getwd()

str(data)       # this can be used to inspect data
head(data, 10)
tail(data, 10)
summary(data)
View(data)

# So far, we worked with base R tools. In RSTudio, lots of packages have
# useful functions. tidyverse makes a lot of statistical work easier
library(tidyverse)

