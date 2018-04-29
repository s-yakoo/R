library(gplots)


## these results are for the total number of times tool has an annotation with group. does not include hhypothetical proteins
counts_annotations <- (matrix(c(33586
                                      ,40598
                                      ,38807
                                      ,16446
                                      ,16664
                                      ,18346
                                      ,21759
                                      ,28009
                                      ,27564), nrow = 3, ncol = 3))


rownames(counts_annotations) <- c("Prokka","RAST","Interproscan")
colnames(counts_annotations) <- c("Vibrio","Firmicutes","Bacteroides")

counts_annotations
barplot2(counts_annotations, main = "Annotations", xlab = "species", ylab = "Total count", beside = TRUE, col = c('light blue', 'medium blue','dark blue'), legend=(rownames(counts_annotations)))

#legend(locator(1),rownames(counts_annotations, title("Tools")))

## these are the amount of times tool matches annotation as hypothetical protein

hypothetical <- (matrix(c(19241,
                                    11700,
                                    3035,
                                    10147,
                                    6238,
                                    1404,
                                    21611,
                                    15587,
                                    3695), nrow = 3, ncol = 3))

rownames(hypothetical) <- c("Prokka","RAST","Interproscan")
colnames(hypothetical) <- c("Vibrio","Firmicutes","Bacteroides")

hypothetical
barplot2(hypothetical, main = "Hypothetical Proteins", xlab = "species", ylab = "Total count", beside = TRUE, col = c('light blue', 'medium blue','dark blue'), legend=(rownames(hypothetical)))


##combine two graphs
par(mfrow=c(1,2))


jpeg(file="hypo_nonHypo.jpeg")

barplot2(counts_annotations, main = "Annotations", xlab = "species", ylab = "Total count", beside = TRUE, col = c('light blue', 'medium blue','dark blue'), legend=(rownames(counts_annotations)))

barplot2(hypothetical, main = "Hypothetical Proteins", xlab = "species", ylab = "Total count", beside = TRUE, col = c('light blue', 'medium blue','dark blue'), legend=(rownames(hypothetical)))

dev.off()
#dataframe for tools

tools_df <- data.frame(row.names = c("PP", "RR", "II", "PR", "PI", "RI"),
                       total= c(222718, 253714, 198443, 497793, 424856, 473094),
                       threshold_29 = c(217079, 248600, 187542, 308960, 226727, 193288))

#Ratio of the total/true annotations
tools_df$percent_ratio = (tools_df$threshold_29/tools_df$total)*100

#save table
write.csv(tools_df,'tools.csv')

#This ensures that the numeric values are not converted to string values.
#final_tools_df <- as.data.frame(t(tools_df))

#graph the percents per tool
barplot(tools_df$percent_ratio, main = "Ratio of Similar Annotations per Tool", 
        names.arg = c("PP", "RR", "II", "PR", "PI", "RI"), 
        xlab = "Tools", ylab = "Percent", col = "dark blue")


#dataframe for groups of species
groups_df <- data.frame(row.names = c("VV", "FF", "BB","VF","VB","FB"), 
                        total=c(1157824, 86381, 601981, 77767, 79026, 67639),
                        threshold_29 = c(748688, 61521, 395717, 62535, 61544, 52191))

#ratio fo the total/true annotations
groups_df$percent_ratio = (groups_df$threshold_29/groups_df$total)*100

#save table
write.csv(groups_df,'groups.csv')

#This ensures that the numeric values are not converted to string values.

#final_groups_df <- as.data.frame(t(groups_df, round(2)))

#graph the percents per group of species
barplot(groups_df$percent_ratio, main = "Ratio of Similar Annotations per Group of Species", 
        names.arg = c("VV", "FF", "BB","VF","VB","FB"), xlab = "Group of Species", 
        ylab = "Percent", col = "dark blue")

