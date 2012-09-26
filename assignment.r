###############################
######## Assignment 4 #########
###############################

# Longitudinal characteristics

rm(list = ls())
setwd("/Users/emanuelastruffolino/Desktop/SequenceCourse/Lezione 4_25092012")
library (TraMineRextras)
data(biofam)

#From the previous assignments

biofam.lab<-c("Parent", "Left", "Married","Left+Marr","Child","Left+Child","Left+Marr+Child","Divorced")
biofam.shortlab<-c("P", "L", "M", "LM", "C","LC","LMC", "D")
biofam.seq<-seqdef(biofam[,10:25], states=biofam.shortlab,labels=biofam.lab,biofam$weight, xtstep=5)
biofam$cohort <- cut(biofam$birthyr, c(1900,1930,1940,1950,1960),
                     labels=c("1900-1929", "1930-1939", "1940-1949", "1950-1959"), right=FALSE)

#1. Continuing with the biofam data, build a table with the sequence length, 
  #the number of transitions, the number of subsequences, the longitudinal 
  #entropy, the turbulence and the complexity index.

biofam.lenght <- seqlength(biofam.seq)
biofam.transn <- seqtransn(biofam.seq)
biofam.subsn <- seqsubsn(biofam.seq)
biofam.ient <- seqient(biofam.seq)
biofam.ici <- seqici(biofam.seq)
biofam.turb <- seqST(biofam.seq)

tab <- data.frame(seqlength(biofam.seq), seqtransn(biofam.seq),
                 seqsubsn(biofam.seq), seqient(biofam.seq), seqici(biofam.seq), seqST(biofam.seq))
tab [1:4,]

#2. Using summary(), look at the min, max, mean, medians and quartiles 
  #of the distribution of each of the computed longitudinal characteristics.
summary (tab)

#3. Display the histogram of each longitudinal characteristic but the length 
  #in a same graphic.
par(mfrow = c(2, 3))
hist(biofam.transn, ylab = "Number of transition",col = "magenta")
hist(biofam.subsn, ylab = "Number of subsequences",col = "light blue")
hist(biofam.ient, ylab = "Entropy",col = "green")
hist(biofam.ici, ylab = "Complexity Index",col = "yellow")
hist(biofam.turb, ylab = "Turbolence",col = "orange")

#4. Generate the sequences of distinct successive states (DSS) and the table 
  #with the duration in the distinct successive states. Display the last 6 of them.
biofam.dss<-seqdss(biofam.seq) 
biofam.dss [195:200,]
biofam.dur<-seqdur(biofam.seq) 
biofam.dur [195:200,1:4]

#5. Compute the mean and the variance of the time spent in the successive states. 
  #Display their summaries. (Hint: use the apply function and specify na.rm=TRUE)
biofam.sucsta <- seqistatd (biofam.seq)
biofam.sucsta [1:3,]
colMeans(biofam.sucsta, na.rm = TRUE, dims = 1)
# OR
apply(biofam.sucsta, 2, mean,na.rm = TRUE)
apply(biofam.sucsta, 2, var,na.rm = TRUE)

#6. Generate a scatterplot matrix for comparing the Entropy with the Turbulence and 
  #Complexity Index.
plot(biofam.ient, biofam.turb, ylab = "Turbulence", xlab = "Entropy",
     col = "light green")

#7. Compare the distributions of the complexity index by birth cohorts using boxplots.
boxplot(biofam.ici ~ biofam$cohort, col = "LightBlue"
        main = "Complexity index", xlab = "Cohort")

#8. Regress the complexity index on the birth cohort, the sex and the language 
  #of the questionnaire. Comment the results.
lm.ici <- lm(biofam.ici ~ biofam$cohort + plingu02 + sex, data = biofam)
lm.ici
