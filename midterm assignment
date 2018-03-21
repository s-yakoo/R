#Data Management task

midterm <- read.table("midterm.txt")
str(midterm)
treat.label <- c("Control","CBT","FT")
midterm$treats <- factor(treat.label[midterm$Treat])
DIFF <- midterm$Postwt - midterm$Prewt
PCNT.DIFF <- (DIFF/midterm$Prewt)*100
midterm$pcnt.diff <- (PCNT.DIFF)


# Data Analytic/ Programming Tasks
attach(midterm)

#1. create a presentable side by side boxplot of the percent difference for each treatment group
boxplot(midterm$pcnt.diff~midterm$treats, main="Boxplot of percent difference in weight per group", ylab=("Percent Difference"))

# 2. Write a function:

x <-midterm$pcnt.diff
f <-  as.factor(midterm$treats)
median.test <- function(x,f,round=3) {
  data.frame(cbind(n=as.factor(length(midterm$pcnt.diff~midterm$treats)),
             median=round(tapply(midterm$pcnt.diff, midterm$treats, median),round),
             min=round(tapply(midterm$pcnt.diff, midterm$treats, min),round),
             max=round(tapply(midterm$pcnt.diff, midterm$treats, max),round)))

}

median.test(x,f)

if (length(unique(midterm$treats)) <3){
    wilcox.test(midterm$pcnt.diff~midterm$treats)
}else{
    kruskal.test(midterm$pcnt.diff~midterm$treats)
}

kruskal.test(midterm$pcnt.diff~midterm$treats)$p.value

detach(midterm)
