setwd("E:/Fudan_Luoqiang_ADHD_Project/20221203_ADHD200数据库/202302/数据分析/1Handness等问题")
library(readxl)
ADHD_200 <- read_excel("ADHD200数据分析hand_01_718.xlsx")
#方差分析: 协变量：(age, sex, handedness, total intracranial volume, IQ, and site)  
str(ADHD_200) 
ADHD_200[5:20] <- lapply(ADHD_200[5:20], as.numeric)
ADHD_200 <- as.data.frame(ADHD_200)
#共计592人；其中NC:419人，ADHD:共173人。
library(lme4)
library(lmerTest)
#section ADHD(1) vs NC(0) ----
#surface area
result_ADHD_NC_SA <- c()
for (i in 26:31){
  fit <- lm(scale(ADHD_200[,i]) ~ ADHDorNot +
                Gender + Age + Handedness + as.factor(Site) + TCV + Full4IQ,data = ADHD_200)
  result_ADHD_NC_SA <- rbind(result_ADHD_NC_SA,
                                c(colnames(ADHD_200)[i], coef(summary(fit))[2,c(1,3,4)])) #将目标变量(2代表ADHD症状)的β, t, p存储
}
result_ADHD_NC_SA <- as.data.frame(result_ADHD_NC_SA)
colnames(result_ADHD_NC_SA)[4] = 'pvalue'
result_ADHD_NC_SA$P.Adj <- p.adjust(result_ADHD_NC_SA$pvalue, method = 'fdr', n = length(result_ADHD_NC_SA$pvalue))
