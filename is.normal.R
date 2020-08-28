#given a vector x, a normality.test (default=="shapiro", alternative=="ks"), is.normal returns a logical value of TRUE if normality.test$p.value<=0.05 (non-normal),
#FALSE if normality.test$pvalue>0.05 (normal) or NA if not numeric. A a density plot, and a qq plot will be ploted if "plots" argument is set to TRUE (true by default)

is.normal<-function(x, normality.test="shapiro", plots=FALSE){
        if (!normality.test %in% c("shapiro", "ks")){
                stop("normality test must be either 'shapiro' or 'ks'")
        }
        library(stats)
        library(ggplot2)
        library(ggpubr)
        if (is.numeric(x)){
                if (plots==TRUE){
                        dens<-plot(ggdensity(x))
                        qq<-plot(ggqqplot(x))
                        }
                if (normality.test=="shapiro"){
                ifelse(shapiro.test(x)$p.value<=0.05, FALSE, TRUE)}
                else if (normality.test=="ks"){
                ifelse(ks.test(x, "pnorm", mean(x), sd(x))$p.value<=0.05, FALSE, TRUE)
                }
        } else {
                NA
        }
}