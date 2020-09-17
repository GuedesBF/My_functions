#given a vector x, a normality.test (default=="shapiro", alternative=="ks"), is.normal returns a logical value of TRUE if normality.test$p.value<=0.05 (non-normal),
#FALSE if normality.test$pvalue>0.05 (normal) or NA if not numeric. A a density plot, and a qq plot will be ploted if "plots" argument is set to TRUE (true by default)

is.normal<-function(x, normality.test="shapiro", hist=FALSE){
        if (!normality.test %in% c("shapiro", "lillie")){
                stop("normality test must be either 'shapiro' or 'lillie'")
        }
        library(nortest)
        if (is.numeric(x)){
                if (hist==TRUE){
                        hist(x)
                        }
                if (normality.test=="shapiro"){
                ifelse(shapiro.test(x)$p.value<=0.05, FALSE, TRUE)}
                else if (normality.test=="lillie"){
                ifelse(lillie.test(x)$p.value<=0.05, FALSE, TRUE)
                }
        } else {
                NA
        }
}