setwd("~/Dropbox/R_work/comment_free_with_lb/in vitro/")

df1<-read.csv("GqNBvGqNS2hr_no_comment.diff", header = F, sep="\t")
df2<-read.csv("GqNBvGqNP2hr_no_comment.diff", header = F, sep="\t")
df3<-read.csv("GqNBvGqNS6hr_no_comment.diff", header = F, sep="\t")
df4<-read.csv("GqNBvGqNP6hr_no_comment.diff", header = F, sep="\t")
df5<-read.csv("BgNBvBgNS2hr_no_comment.diff", header = F, sep="\t")
df6<-read.csv("BgNBvBgNP2hr_no_comment.diff", header = F, sep="\t")
df7<-read.csv("BgNBvBgNS6hr_no_comment.diff", header = F, sep="\t")
df8<-read.csv("BgNBvBgNP6hr_no_comment.diff", header = F, sep="\t")
df9<-read.csv("BgGqNBvBgGqNS2hr_no_comment.diff", header = F, sep="\t")
df10<-read.csv("BgGqNBvBgGqNP2hr_no_comment.diff", header = F, sep="\t")
df11<-read.csv("BgGqNBvBgGqNS6hr_no_comment.diff", header = F, sep="\t")
df12<-read.csv("BgGqNBvBgGqNP6hr_no_comment.diff", header = F, sep="\t")

dataFiles <- lapply(Sys.glob("*.diff"), read.table,  sep="\t")

#read in files from dir then split into separate dataframes
temp<-list.files(pattern = "*.diff")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i],sep="\t", header = F))

# change list back to individual data frames
rst<-ls(pattern = "*.diff")
diff_list <- mget(rst)


new_file<-list()
new_file2<-list()

library(ggplot2)
library(plotrix)
library(RColorBrewer)
library(gplots)
library(plyr)
library(reshape2)

#create new column with threshold information 
for(i in 1:12){
  for(j in 1:nrow(dataFiles[[i]])){
   if(dataFiles[[i]]$V3[j]>-1&& dataFiles[[i]]$V3[j]<1){
     dataFiles[[i]]$threshold[j]<-"Non-DEG"
    }else{
      dataFiles[[i]]$threshold[j]<-"DEG"
    }
  }
}

lapply(seq_along(dataFiles), function(i) dataFiles[[i]])



dataFiles[[1]]$facet<-"a"
dataFiles[[2]]$facet<-"b"
dataFiles[[3]]$facet<-"c"
dataFiles[[4]]$facet<-"d"
dataFiles[[5]]$facet<-"e"
dataFiles[[6]]$facet<-"f"
dataFiles[[7]]$facet<-"g"
dataFiles[[8]]$facet<-"h"
dataFiles[[9]]$facet<-"i"
dataFiles[[10]]$facet<-"j"
dataFiles[[11]]$facet<-"k"
dataFiles[[12]]$facet<-"l"

for(i in 1:nrow(df1)){
  if(df1$V3[i]>-1&& df1$V3[i]<1){
    df1$threshold[i]<-"Non-DEG"
  }else{
    df1$threshold[i]<-"DEG"
  }
}

for(i in 1:nrow(df2)){
  if(df2$V3[i]>-1&& df2$V3[i]<1){
    df2$threshold[i]<-"Non-DEG"
  }else{
    df2$threshold[i]<-"DEG"
  }
}

for(i in 1:nrow(df3)){
  if(df3$V3[i]>-1&& df3$V3[i]<1){
    df3$threshold[i]<-"Non-DEG"
  }else{
    df3$threshold[i]<-"DEG"
  }
}

for(i in 1:nrow(df4)){
  if(df4$V3[i]>-1&& df4$V3[i]<1){
    df4$threshold[i]<-"Non-DEG"
  }else{
    df4$threshold[i]<-"DEG"
  }
}

for(i in 1:nrow(df5)){
  if(df5$V3[i]>-1&& df5$V3[i]<1){
    df5$threshold[i]<-"Non-DEG"
  }else{
    df5$threshold[i]<-"DEG"
  }
}

for(i in 1:nrow(df6)){
  if(df6$V3[i]>-1&& df6$V3[i]<1){
    df6$threshold[i]<-"Non-DEG"
  }else{
    df6$threshold[i]<-"DEG"
  }
}

for(i in 1:nrow(df7)){
  if(df7$V3[i]>-1&& df7$V3[i]<1){
    df7$threshold[i]<-"Non-DEG"
  }else{
    df7$threshold[i]<-"DEG"
  }
}

for(i in 1:nrow(df8)){
  if(df8$V3[i]>-1&& df8$V3[i]<1){
    df8$threshold[i]<-"Non-DEG"
  }else{
    df8$threshold[i]<-"DEG"
  }
}

for(i in 1:nrow(df9)){
  if(df9$V3[i]>-1&& df9$V3[i]<1){
    df9$threshold[i]<-"Non-DEG"
  }else{
    df9$threshold[i]<-"DEG"
  }
}

for(i in 1:nrow(df10)){
  if(df10$V3[i]>-1&& df10$V3[i]<1){
    df10$threshold[i]<-"Non-DEG"
  }else{
    df10$threshold[i]<-"DEG"
  }
}

for(i in 1:nrow(df11)){
  if(df11$V3[i]>-1&& df11$V3[i]<1){
    df11$threshold[i]<-"Non-DEG"
  }else{
    df11$threshold[i]<-"DEG"
  }
}

for(i in 1:nrow(df12)){
  if(df12$V3[i]>-1&& df12$V3[i]<1){
    df12$threshold[i]<-"Non-DEG"
  }else{
    df12$threshold[i]<-"DEG"
  }
}


df1$facet<-"a"
df2$facet<-"b"
df3$facet<-"c"
df4$facet<-"d"
df5$facet<-"e"
df6$facet<-"f"
df7$facet<-"g"
df8$facet<-"h"
df9$facet<-"i"
df10$facet<-"j"
df11$facet<-"k"
df12$facet<-"l"

df1_gibb<-df1[grepl("Gibb_*", df1$V1), ]
df2_gibb<-df2[grepl("Gibb_*", df2$V1), ]
df3_gibb<-df3[grepl("Gibb_*", df3$V1), ]
df4_gibb<-df4[grepl("Gibb_*", df4$V1), ]

df5_bg<-df5[grepl("BG_*", df5$V1), ]
df6_bg<-df6[grepl("BG_*", df6$V1), ]
df7_bg<-df7[grepl("BG_*", df7$V1), ]
df8_bg<-df8[grepl("BG_*", df8$V1), ]

df9_bg<-df9[grepl("BG_*|Gibb_*", df9$V1), ]
df10_bg<-df10[grepl("BG_*|Gibb_*", df10$V1), ]
df11_bg<-df11[grepl("BG_*|Gibb_*", df11$V1), ]
df12_bg<-df12[grepl("BG_*|Gibb_*", df12$V1), ]

df<-cbind(plot1.1,plot1.2,plot1.3,plot1.4,plot1.5,plot1.6,plot1.7,plot1.8,plot1.9,plot1.10,plot1.11,plot1.12)
df_plot<-as.data.frame(df)

sum(df$threshold == "DEG", na.rm=TRUE)

res_gibb2<- subset(df2_gibb, threshold =="DEG")
res_gibb3<- subset(df3_gibb, threshold =="DEG")
res_bg5<- subset(df5_bg, threshold =="DEG")
res_bg6<- subset(df6_bg, threshold =="DEG")
res_bg8<- subset(df8_bg, threshold =="DEG")
res_bg9<- subset(df9_bg, threshold =="DEG")
res_bg10<- subset(df10_bg, threshold =="DEG")
res_bg11<- subset(df11_bg, threshold =="DEG")
res_bg12<- subset(df12_bg, threshold =="DEG")

write.csv(res_gibb2, "res_gibb2hrNP.csv")
write.csv(res_gibb3, "res_gibb6hrNS.csv")
write.csv(res_bg5, "res_bg2hrNS.csv")
write.csv(res_bg6, "res_bg2hrNP.csv")
write.csv(res_bg8, "res_bg6hrNP.csv")
write.csv(res_bg9, "res_bggq2hrNS.csv")
write.csv(res_bg10, "res_bggq2hrNP.csv")
write.csv(res_bg11, "res_bggq6hrNS.csv")
write.csv(res_bg12, "res_bggq6hrNP.csv")


df_gibb<-diff_list[[1]][grepl("Gibb_*", diff_list[[1]]$V1), ]

df2<-subset(df, threshold=="DEG")


###order the treatments before plotting###
#Turn your 'treatment' column into a character vector
df_plot$facet <- as.character(df_plot$facet)
#Then turn it back into an ordered factor
df_plot$facet  <- factor(df_plot$facet, levels=unique(df_plot$facet))

(plot1.1 <- ggplot(df1_gibb, aes(x = V5)) +  
  geom_point(aes(y = V3, col = threshold),shape = 21, size = 3, show.legend = FALSE) +
  geom_hline(yintercept=0, linetype = "dashed")+
  scale_colour_manual(name="Gene expression",  
                      values = c("DEG"="red", "Non-DEG"="black"
                                 ))+
  xlab("") +
  ylab("")+
  scale_x_continuous(limits = c(-5, 5))+
  scale_y_continuous(limits = c(-5, 5))+
  theme_bw() +
  theme(panel.grid.minor = element_blank())+
  theme(panel.grid.major = element_blank())+
  theme(axis.text.x = element_text(colour="grey20",size=14,angle=0,hjust=.8,vjust=.2,face="plain"),
        axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=18,angle=90,hjust=.5,vjust=.5,face="plain"),
        legend.text = element_text(colour="black", size=14, face="bold"),
        legend.key.size = unit(4, "mm"),
        legend.background = element_rect(fill="gray90", size=.3, linetype="solid", color = "black"), 
        legend.title = element_text(colour="Black", size=16, face="plain")))

(plot1.2 <- ggplot(df2_gibb, aes(x = V5)) +  
    geom_point(aes(y = V3, col = threshold),shape = 21, size = 3, show.legend = FALSE) +
    geom_hline(yintercept=0, linetype = "dashed")+
    scale_colour_manual(name="Gene expression",  
                        values = c("DEG"="red", "Non-DEG"="black"
                        ))+
    xlab("") +
    ylab("")+
    scale_x_continuous(limits = c(-5, 5))+
    scale_y_continuous(limits = c(-5, 5))+
    theme_bw() +
    theme(panel.grid.minor = element_blank())+
    theme(panel.grid.major = element_blank())+
    theme(axis.text.x = element_text(colour="grey20",size=14,angle=0,hjust=.8,vjust=.2,face="plain"),
          axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
          axis.title.x = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title.y = element_text(colour="grey20",size=18,angle=90,hjust=.5,vjust=.5,face="plain"),
          legend.text = element_text(colour="black", size=14, face="bold"),
          legend.key.size = unit(4, "mm"),
          legend.background = element_rect(fill="gray90", size=.3, linetype="solid", color = "black"), 
          legend.title = element_text(colour="Black", size=16, face="plain")))

(plot1.3 <- ggplot(df3_gibb, aes(x = V5)) +  
    geom_point(aes(y = V3, col = threshold),shape = 21, size = 3, show.legend = FALSE) +
    geom_hline(yintercept=0, linetype = "dashed")+
    scale_colour_manual(name="Gene expression",  
                        values = c("DEG"="red", "Non-DEG"="black"
                        ))+
    xlab("") +
    ylab("")+
    scale_x_continuous(limits = c(-5, 5))+
    scale_y_continuous(limits = c(-5, 5))+
    theme_bw() +
    theme(panel.grid.minor = element_blank())+
    theme(panel.grid.major = element_blank())+
    theme(axis.text.x = element_text(colour="grey20",size=14,angle=0,hjust=.8,vjust=.2,face="plain"),
          axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
          axis.title.x = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title.y = element_text(colour="grey20",size=18,angle=90,hjust=.5,vjust=.5,face="plain"),
          legend.text = element_text(colour="black", size=14, face="bold"),
          legend.key.size = unit(4, "mm"),
          legend.background = element_rect(fill="gray90", size=.3, linetype="solid", color = "black"), 
          legend.title = element_text(colour="Black", size=16, face="plain")))

(plot1.4 <- ggplot(df4_gibb, aes(x = V5)) +  
    geom_point(aes(y = V3, col = threshold),shape = 21, size = 3, show.legend = FALSE) +
    geom_hline(yintercept=0, linetype = "dashed")+
    scale_colour_manual(name="Gene expression",  
                        values = c("DEG"="red", "Non-DEG"="black"
                        ))+
    xlab("") +
    ylab("")+
    scale_x_continuous(limits = c(-5, 5))+
    scale_y_continuous(limits = c(-5, 5))+
    theme_bw() +
    theme(panel.grid.minor = element_blank())+
    theme(panel.grid.major = element_blank())+
    theme(axis.text.x = element_text(colour="grey20",size=14,angle=0,hjust=.8,vjust=.2,face="plain"),
          axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
          axis.title.x = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title.y = element_text(colour="grey20",size=18,angle=90,hjust=.5,vjust=.5,face="plain"),
          legend.text = element_text(colour="black", size=14, face="bold"),
          legend.key.size = unit(4, "mm"),
          legend.background = element_rect(fill="gray90", size=.3, linetype="solid", color = "black"), 
          legend.title = element_text(colour="Black", size=16, face="plain")))

(plot1.5 <- ggplot(df5_bg, aes(x = V5)) +  
    geom_point(aes(y = V3, col = threshold),shape = 21, size = 3, show.legend = FALSE) +
    geom_hline(yintercept=0, linetype = "dashed")+
    scale_colour_manual(name="Gene expression",  
                        values = c("DEG"="red", "Non-DEG"="black"
                        ))+
    xlab("") +
    ylab("")+
    scale_x_continuous(limits = c(-5, 5))+
    scale_y_continuous(limits = c(-5, 5))+
    theme_bw() +
    theme(panel.grid.minor = element_blank())+
    theme(panel.grid.major = element_blank())+
    theme(axis.text.x = element_text(colour="grey20",size=14,angle=0,hjust=.8,vjust=.2,face="plain"),
          axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
          axis.title.x = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title.y = element_text(colour="grey20",size=18,angle=90,hjust=.5,vjust=.5,face="plain"),
          legend.text = element_text(colour="black", size=14, face="bold"),
          legend.key.size = unit(4, "mm"),
          legend.background = element_rect(fill="gray90", size=.3, linetype="solid", color = "black"), 
          legend.title = element_text(colour="Black", size=16, face="plain")))

(plot1.6 <- ggplot(df6_bg, aes(x = V5)) +  
    geom_point(aes(y = V3, col = threshold),shape = 21, size = 3, show.legend = FALSE) +
    geom_hline(yintercept=0, linetype = "dashed")+
    scale_colour_manual(name="Gene expression",  
                        values = c("DEG"="red", "Non-DEG"="black"
                        ))+
    xlab("") +
    ylab("")+
    scale_x_continuous(limits = c(-5, 5))+
    scale_y_continuous(limits = c(-5, 5))+
    theme_bw() +
    theme(panel.grid.minor = element_blank())+
    theme(panel.grid.major = element_blank())+
    theme(axis.text.x = element_text(colour="grey20",size=14,angle=0,hjust=.8,vjust=.2,face="plain"),
          axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
          axis.title.x = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title.y = element_text(colour="grey20",size=18,angle=90,hjust=.5,vjust=.5,face="plain"),
          legend.text = element_text(colour="black", size=14, face="bold"),
          legend.key.size = unit(4, "mm"),
          legend.background = element_rect(fill="gray90", size=.3, linetype="solid", color = "black"), 
          legend.title = element_text(colour="Black", size=16, face="plain")))

(plot1.7 <- ggplot(df7_bg, aes(x = V5)) +  
    geom_point(aes(y = V3, col = threshold),shape = 21, size = 3, show.legend = FALSE) +
    geom_hline(yintercept=0, linetype = "dashed")+
    scale_colour_manual(name="Gene expression",  
                        values = c("DEG"="red", "Non-DEG"="black"
                        ))+
    xlab("") +
    ylab("")+
    scale_x_continuous(limits = c(-5, 5))+
    scale_y_continuous(limits = c(-5, 5))+
    theme_bw() +
    theme(panel.grid.minor = element_blank())+
    theme(panel.grid.major = element_blank())+
    theme(axis.text.x = element_text(colour="grey20",size=14,angle=0,hjust=.8,vjust=.2,face="plain"),
          axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
          axis.title.x = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title.y = element_text(colour="grey20",size=18,angle=90,hjust=.5,vjust=.5,face="plain"),
          legend.text = element_text(colour="black", size=14, face="bold"),
          legend.key.size = unit(4, "mm"),
          legend.background = element_rect(fill="gray90", size=.3, linetype="solid", color = "black"), 
          legend.title = element_text(colour="Black", size=16, face="plain")))

(plot1.8 <- ggplot(df8_bg, aes(x = V5)) +  
    geom_point(aes(y = V3, col = threshold),shape = 21, size = 3, show.legend = FALSE) +
    geom_hline(yintercept=0, linetype = "dashed")+
    scale_colour_manual(name="Gene expression",  
                        values = c("DEG"="red", "Non-DEG"="black"
                        ))+
    xlab("") +
    ylab("")+
    scale_x_continuous(limits = c(-5, 5))+
    scale_y_continuous(limits = c(-5, 5))+
    theme_bw() +
    theme(panel.grid.minor = element_blank())+
    theme(panel.grid.major = element_blank())+
    theme(axis.text.x = element_text(colour="grey20",size=14,angle=0,hjust=.8,vjust=.2,face="plain"),
          axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
          axis.title.x = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title.y = element_text(colour="grey20",size=18,angle=90,hjust=.5,vjust=.5,face="plain"),
          legend.text = element_text(colour="black", size=14, face="bold"),
          legend.key.size = unit(4, "mm"),
          legend.background = element_rect(fill="gray90", size=.3, linetype="solid", color = "black"), 
          legend.title = element_text(colour="Black", size=16, face="plain")))

(plot1.9 <- ggplot(df9_bg, aes(x = V5)) +  
    geom_point(aes(y = V3, col = threshold),shape = 21, size = 3, show.legend = FALSE) +
    geom_hline(yintercept=0, linetype = "dashed")+
    scale_colour_manual(name="Gene expression",  
                        values = c("DEG"="red", "Non-DEG"="black"
                        ))+
    xlab("") +
    ylab("")+
    scale_x_continuous(limits = c(-5, 5))+
    scale_y_continuous(limits = c(-5, 5))+
    theme_bw() +
    theme(panel.grid.minor = element_blank())+
    theme(panel.grid.major = element_blank())+
    theme(axis.text.x = element_text(colour="grey20",size=14,angle=0,hjust=.8,vjust=.2,face="plain"),
          axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
          axis.title.x = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title.y = element_text(colour="grey20",size=18,angle=90,hjust=.5,vjust=.5,face="plain"),
          legend.text = element_text(colour="black", size=14, face="bold"),
          legend.key.size = unit(4, "mm"),
          legend.background = element_rect(fill="gray90", size=.3, linetype="solid", color = "black"), 
          legend.title = element_text(colour="Black", size=16, face="plain")))

(plot1.10 <- ggplot(df10_bg, aes(x = V5)) +  
    geom_point(aes(y = V3, col = threshold),shape = 21, size = 3, show.legend = FALSE) +
    geom_hline(yintercept=0, linetype = "dashed")+
    scale_colour_manual(name="Gene expression",  
                        values = c("DEG"="red", "Non-DEG"="black"
                        ))+
    xlab("") +
    ylab("")+
    scale_x_continuous(limits = c(-5, 5))+
    scale_y_continuous(limits = c(-5, 5))+
    theme_bw() +
    theme(panel.grid.minor = element_blank())+
    theme(panel.grid.major = element_blank())+
    theme(axis.text.x = element_text(colour="grey20",size=14,angle=0,hjust=.8,vjust=.2,face="plain"),
          axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
          axis.title.x = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title.y = element_text(colour="grey20",size=18,angle=90,hjust=.5,vjust=.5,face="plain"),
          legend.text = element_text(colour="black", size=14, face="bold"),
          legend.key.size = unit(4, "mm"),
          legend.background = element_rect(fill="gray90", size=.3, linetype="solid", color = "black"), 
          legend.title = element_text(colour="Black", size=16, face="plain")))

(plot1.11 <- ggplot(df11_bg, aes(x = V5)) +  
    geom_point(aes(y = V3, col = threshold),shape = 21, size = 3, show.legend = FALSE) +
    geom_hline(yintercept=0, linetype = "dashed")+
    scale_colour_manual(name="Gene expression",  
                        values = c("DEG"="red", "Non-DEG"="black"
                        ))+
    xlab("") +
    ylab("")+
    scale_x_continuous(limits = c(-5, 5))+
    scale_y_continuous(limits = c(-5, 5))+
    theme_bw() +
    theme(panel.grid.minor = element_blank())+
    theme(panel.grid.major = element_blank())+
    theme(axis.text.x = element_text(colour="grey20",size=14,angle=0,hjust=.8,vjust=.2,face="plain"),
          axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
          axis.title.x = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title.y = element_text(colour="grey20",size=18,angle=90,hjust=.5,vjust=.5,face="plain"),
          legend.text = element_text(colour="black", size=14, face="bold"),
          legend.key.size = unit(4, "mm"),
          legend.background = element_rect(fill="gray90", size=.3, linetype="solid", color = "black"), 
          legend.title = element_text(colour="Black", size=16, face="plain")))

(plot1.12 <- ggplot(df12_bg, aes(x = V5)) +  
    geom_point(aes(y = V3, col = threshold),shape = 21, size = 3, show.legend = FALSE) +
    geom_hline(yintercept=0, linetype = "dashed")+
    scale_colour_manual(name="Gene expression",  
                        values = c("DEG"="red", "Non-DEG"="black"
                        ))+
    xlab("") +
    ylab("")+
    scale_x_continuous(limits = c(-5, 5))+
    scale_y_continuous(limits = c(-5, 5))+ 
    theme_bw() +
    theme(panel.grid.minor = element_blank())+
    theme(panel.grid.major = element_blank())+
    theme(axis.text.x = element_text(colour="grey20",size=14,angle=0,hjust=.8,vjust=.2,face="plain"),
          axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
          axis.title.x = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title.y = element_text(colour="grey20",size=18,angle=90,hjust=.5,vjust=.5,face="plain"),
          legend.text = element_text(colour="black", size=14, face="bold"),
          legend.key.size = unit(4, "mm"),
          legend.position = "bottom",
          legend.background = element_rect(fill="gray90", size=.3, linetype="solid", color = "black"), 
          legend.title = element_text(colour="Black", size=16, face="plain")))

(plot2 <- ggplot(df12_bg, aes(x = V5)) +  
    geom_point(aes(y = V3, col = threshold),shape = 21, size = 3) +
    geom_hline(yintercept=0, linetype = "dashed")+
    scale_colour_manual(name="Gene expression",  
                        values = c("DEG"="red", "Non-DEG"="black"
                        ))+
    xlab("log2 fold change") +
    ylab("")+
    scale_x_continuous(limits = c(-5, 5))+
    scale_y_continuous(limits = c(-5, 5))+ 
    theme_bw() +
    theme(panel.grid.minor = element_blank())+
    theme(panel.grid.major = element_blank())+
    theme(axis.text.x = element_text(colour="grey20",size=14,angle=0,hjust=.8,vjust=.2,face="plain"),
          axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
          axis.title.x = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title.y = element_text(colour="grey20",size=18,angle=90,hjust=.5,vjust=.5,face="plain"),
          legend.text = element_text(colour="black", size=14, face="bold"),
          legend.key.size = unit(4, "mm"),
          legend.position = "bottom",
          legend.background = element_rect(fill="gray90", size=.3, linetype="solid", color = "black"), 
          legend.title = element_text(colour="Black", size=16, face="plain")))

library(grid)
library(lattice)
library(gridExtra)

p1<-grid.arrange(arrangeGrob(plot1.1, plot1.2, plot1.3, plot1.4,plot1.5, plot1.6, plot1.7, plot1.8, plot1.9, plot1.10, plot1.11, plot1.12, nrow = 3), 
                 left = textGrob("GFOLD", rot = 90, vjust = 1, gp = gpar(fontfamily = "Times", size = 30, fontface = "bold")), 
                                                                         bottom = textGrob("log fold change", gp = gpar(fontfamily = "Times", size = 30, fontface = "bold")))

svg("in vitro tests.svg", width = 12, height = 7)
plot (p1)# Make plot
dev.off()

svg("key.svg", width = 12, height = 7)
plot (plot2)# Make plot
dev.off()
