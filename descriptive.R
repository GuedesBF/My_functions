#The 'descriptive' function takes a dataframe, digits and normality_test arguments, and returns basic
#descriptive statistics for every numeric variable in the df:
# is.normal: a logical vector of whether the distribution is normal (p=0.05) or not, whent tested
# by the method kolgorov-smirnov ("ks") or Shapiro ("shapiro")
# mean, median, Q1=quantile 0.25, Q3=quantile 0.75, SD = standard deviation.
# 'output' is median(Q1-Q3) for non-normal data or mean +-SD for normal data.

descriptive<-function(x=data.frame(), digits=2, na.rm=TRUE, normality.test="shapiro"){
        if (!normality.test %in% c("shapiro", "ks")){
                stop("normality test must be either 'shapiro' or 'ks'")
        }
        library(stats)
        is.normal<-character()
        medians<-numeric()
        Q1<-numeric()
        Q3<-numeric()
        means<-numeric()
        SDs<-numeric()
        output<-character()
        for (i in seq_along(x)){
                if (is.numeric(x[,i])){
                        medians[i]<-median(x[,i], na.rm = na.rm)
                        Q1[i]<-quantile(x[,i], 0.25, na.rm = na.rm)
                        Q3[i]<-quantile(x[,i], 0.75, na.rm = na.rm)
                        means[i]<-round(mean(x[,i], na.rm = na.rm), digits = digits)
                        SDs[i]<-round(sd(x[,i], na.rm=TRUE), digits = digits)
                        if (normality.test=="shapiro"){
                                p.value<-shapiro.test(x[,i])$p.value
                        } else if (normality.test=="ks"){
                                p.value<-ks.test(x[,i], "pnorm", means[i], SDs[i])$p.value
                        }
                        if (p.value<=0.05){
                                is.normal[i]<-FALSE
                                output[i]<-paste0(medians[i], " (", Q1[i], "-", Q3[i], ")")
                        }else{
                                is.normal[i]<-TRUE
                                output[i]<-paste0(means[i], " +-", SDs[i])
                        }
                }else  {
                        is.normal[i]<-NA
                        means[i]<-NA
                        medians[i]<-NA
                        Q1[i]<-NA
                        Q3[i]<-NA
                        SDs[i]<-NA
                        output[i]<-NA
                }
        }      
        
        df<-data.frame(rbind( "normal distr"=is.normal, "median"=medians, "Q1"=Q1, "Q3"=Q3, "mean"=means, "SD"=SDs, "output"=output))
        names(df)<-colnames(x)
        df
}
