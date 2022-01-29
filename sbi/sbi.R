#!/usr/bin/env Rscript
#' ---
#' title: SBI 2022 - sbi.R - functions and operators
#' author: Detlef Groth, University of Potsdam
#' date: 2022-01-30
#' ---
#' 
#' ## R functions for Statistical Bioinformatics - 2022-01-30
#' 
#' <a name="home"> </a>
#' 
#' This is a collection of useful functions shown in my teachings 
#' in the course Statistical Bioinformatics using R in 2021.
#' Below you find the documentation for the following functions 
#' and operators:
#' 
#' Functions:
#'
#'   * [sbi$assoc](#assoc) - patch for assocplot to display residuals with color codes (plot).
#'   * [sbi$cohensD](#cohensD) - effect size for the difference between two means (stats)
#'   * [sbi$cohensF](#cohensF) - effect size for Anova (stats)
#'   * [sbi$cohensH](#cohensH) - effect size for 2x2 contingency tables (stats)
#'   * [sbi$cohensW](#cohensW) - effect size for 2x2 and larger contingency tables (stats)
#'   * [sbi$corplot](#corplot) - visualize a correlation with abline (plot)
#'   * [sbi$corr](#corr) - calculate all pairwise correlations for a given data frame or matrix (stats)
#'   * [sbi$corrplot](#corrplot) - visualize a matrix of pairwise correlations (plot)
#'   * [sbi$cor.var](#cor.var) - create data vector with a certain correlation to an other vector (stats)
#'   * [sbi$cor.vars](#cor.vars) - create data frame with random values matching the given correlation matrix (stats)
#'   * [sbi$cv](#cv) - coefficient of variation (stats).
#'   * [sbi$dict](#dict) - create a key value list with unique keys (data)
#'   * [sbi$dpairs](#dpairs) - improved pairs plot with xyplot, boxplot or assocplot depending on the variable types (plot)
#'   * [sbi$dpairs.legend](#dpairs.legend) - adding legends to pairs and dpairs plots (plot)
#'   * [sbi$drop_na](#drop_na) - remove all rows where any of the given columns contain a NA (data)
#'   * [sbi$epsilonSquared](#epsilonSquared) - effect size measure for a Kruskal or Wilcox test (stats)
#'   * [sbi$etaSquared](#etaSquared) - effect size measure for Anova or a linear model (stats) 
#'   * [sbi$file.cat](#file.cat) - displays a file to the terminal (file)
#'   * [sbi$file.head](#file.head) - displays the first lines of a file to the terminal (file)
#'   * [sbi$fmt](#fmt) - Python like formatting of strings using curly braces (text)
#'   * [sbi$gmean](#gmean) - geometric mean for a numerical vector, mean of products (stats)
#'   * [sbi$hmean](#hmean) - harmonic mean for a numerical vector, mean of ratios (stat)
#'   * [sbi$import](#import) - module like sourcing of R files relative to the main script (package)
#'   * [sbi$is.dict](#is.dict) - check if a list is a key-value list with unique keys (data)
#'   * [sbi$kurtosis](#kurtosis) - fourth central moment of a distribution (stats)
#'   * [sbi$mhist](#mhist) - lattice like histograms (plot).
#'   * [sbi$mi](#mi) - mutual information for two numerical variables or a binned table (stats)
#'   * [sbi$modus](#modus) - get the modus for a categorical variable (stats).
#'   * [sbi$package.deps](#package.deps) - return the dependencies of the given package (package)
#'   * [sbi$package.request](#package.request) - checks if the requested package is already installed, if not it will be installed silently (package)
#'   * [sbi$pastel](#pastel) - create up to 20 pastel colors (plot)
#'   * [sbi$pca.biplot](#pca.biplot) - improved biplot for pca objects (plot)
#'   * [sbi$pca.pairs](#pca.pairs) - improved pairs plot for pca objects (plot)
#'   * [sbi$pca.plot](#pca.plot) - improved screeplot for pca objects (plot)
#'   * [sbi$pcor](#pcor) - calculate partial correlation for two variables corrected for one or more others (stats)
#'   * [sbi$pcor.test](#pcor.test) - calculate partial correlation and test statistics for two variables corrected for one or more others (stats)
#'   * [sbi$readDataFiles](#readDataFiles) - read a directory of data files with the same function and settings (data).
#'   * [sbi$report.pval](#report.pval) - report a p-value using threshold or star syntax  (stats)
#'   * [sbi$skewness](#skewness) - third central moment of a distribution (stats)
#'   * [sbi$sem](#sem) - calculate standard error of the mean (stats)
#'   * [sbi$smartbind](#smartbind) - combine two data frame even if the have different column names (data).
#'
#' Operators:
#' 
#'   * [%ni%](#ni) - not-in operator (operator)
#'   * [%>%](#pipe) - pipe operator (operator)
#' 
#' Variables:
#' 
#' The variables are considered readonly.
#' 
#'   * FILENAME - the actual filename of the script and the path to it, can be used to source files other files with relative paths
#'   * VERSION - the last change date of the file
#' 
#' ## Usage
#' 
#' To use the functions just source the file sbi.R into your R application:
#'
#' ```{r eval=FALSE}
#' # notrun
#' source("sbi.R")
#' ```
#' 
#' ```{r echo=FALSE,results="hide"}
#' if (file.exists("sbi.R")) {
#'  source("sbi.R")
#' } else if (file.exists("../sbi.R")) {
#'   source("../sbi.R")
#' }
#' ```
#' All function of this file are added to an environment variable called _sbi_. 
#' To have a look which variables and functions exists in this namesspace you can use the function call
#' `ls(sbi)`. Only function start ing with lowercase letters should be used in your scripts:
#' 
#' ```{r}
#' ls(sbi,pattern="^[a-z]")
#' ```
#' 


sbi=new.env()
sbi$VERSION = "2022.01.30"
# where is the file sbi.R
# store it in the filename variable
.calls=sys.calls()
srx=grep("^source",.calls)
idx=srx[length(srx)]
sbi$FILENAME = gsub("source\\(.(.+).\\)","\\1",.calls[idx])
rm(.calls,srx,idx)
library(MASS)
#' ## Functions
#'
#' 
#' <a name="dassoc"> </a>
#' **sbi$assoc(...,shade=TRUE)** 
#' 
#' > Create assocplots with residual coloring.
#' 
#' > This function updates the standard assocplot function from the graphics package 
#'   with the ability to display residual colors. In blue and red are shown groups with 
#'   residuals above +4 or below -4 in light colors are shown residuals between 2 and 4 for positive and -4 and -2 for negative residuals.
#' 
#' > Arguments:
#' 
#' > - _..._ - arguments delegated to the standard assocplot function.
#'   - _shade_ - should the residuals been shown, default: TRUE
#'
#' > Examples:
#' 
#' > ```{r fig=TRUE,fig.cap=""}
#'   x <- margin.table(HairEyeColor, c(1, 2))
#'   sbi$assoc(x)
#' > ```
#'
sbi$assoc <- function (...,shade=TRUE) {
    # https://stackoverflow.com/questions/38732663/how-to-insert-expression-into-the-body-of-a-function-in-r
    funins <- function(f, expr = expression(x<-2*x), after=1) {
        body(f)<-as.call(append(as.list(body(f)), expr, after=after))
        f
    }

    expr=expression({
        # DG: changed for shade
        residuals=chisq.test(x)$residuals
        cols=c('#CF3761','#E18E9E','#E0E0E0','#96A2DF','#4267E0')
        resis=c()
        # R plots from top right to lower left ...
        # we rearrange the colors
        for (c in ncol(x):1) {
            resis=c(resis,residuals[,c])
        }
        acols=cols[cut(resis,
                       breaks=c(-Inf,-4,-2,2,4,Inf),
                       labels=c(1,2,3,4,5))]
        rect(z[, 1] - e/2, z[, 2], z[, 1] + e/2, z[, 2] + d, col = acols)
    })
    if (shade) {
       g <- funins(graphics::assocplot, expr,after=length(body(graphics::assocplot)))
       g(...)
   } else {
       graphics::assocplot(...)
   }
    
}

#' 
#' <a name='cohensD'> </a>
#' **sbi$cohensD(num,cat,paired=FALSE)**
#' 
#' > Effect size for the difference between two means.
#'
#' > The function cohensD calculates the effect size for the difference between two means.
#'   Due to Cohen's rule of thumb values of 0.2 to 0.5 are considered to stand 
#'   for small effects, values from 0.5 to 0.8 represent medium effects and values above 0.8 represent large effects. 

#' >  Alternatively you could use the biserial correlation coefficient _r_ as demonstrated in the example below.
#' 
#' > Arguments:
#' 
#' > - *num* - vector with numerical values
#'   - *cat* - vector with two grouping variables, having the same length as num
#'   - *paired* - are the data paired, default: FALSE
#' 
#' > Returns:  Cohen's d value
#' 
#' > Examples:
#' 
#' > ```{r}
#'   cohensD=sbi$cohensD
#'   set.seed(125)
#'   data(sleep)
#'   with(sleep,cohensD(extra,group))
#'   x1=rnorm(100,mean=20,sd=1)
#'   x2=rnorm(100,mean=22,sd=1)
#'   g1=rep('A',100)
#'   g2=rep('B',100)
#'   # difference should be around 2SD
#'   cohensD(c(x1,x2),as.factor(c(g1,g2)))
#'   # biseriell correlation coefficient as alternative
#'   # value is as well large
#'   cor(c(x1,x2),as.numeric(as.factor(c(g1,g2))))
#' > ```
#'
#' > See also: [sbi$cohensW](#cohensW)


sbi$cohensD <- function (num, cat,paired=FALSE) {
    if (paired) {
        tt=t.test(num ~ cat,paired=paired)
        return(tt$statistic[[1]]/sqrt(length(num/2)))
    }   
    tt.agg=aggregate(num,by=list(cat),
        mean,na.rm=TRUE)
    pooledSD <- function(x, y) {
        x=x[!is.na(x)]
        y=y[!is.na(y)]
        sq.devs <- (c(x - mean(x), y - mean(y)))^2
         n <- length(sq.devs)
        return(sqrt(sum(sq.devs)/(n - 2)))
    }
    d=abs(tt.agg$x[1]-tt.agg$x[2])/pooledSD(
        num[cat==levels(cat)[1]],
        num[cat==levels(cat)[2]])
    return(d)
} 

#' 
#' <a name='cohensF'> </a>
#' **sbi$cohensF(x,g)**
#' 
#' > Effect size for an Anova.
#'
#' > The function cohensF calculates the effect size for two variables in an Anova.
#'   Due to Cohen's rule of thumb values of 0.1 to 0.25 are considered to stand 
#'   for small effects, values from 0.25 to 0.4 represent medium effects and values above 0.4 represent large effects. 
#' 
#' > Arguments:
#' 
#' > - _x_ a numercial vector
#'   - _g_ a factor variable with at least three levels
#' 
#' > Returns:  Cohen's f value
#' 
#' > Examples:
#' 
#' > ```{r}
#'   data(iris)
#'   sbi$cohensF(iris$Sepal.Width, iris$Species)
#'   sbi$cohensF(iris$Sepal.Length, iris$Species)
#' > ```
#'
#' > See also: [sbi$cohensD](#sbi$cohensD), [sbi$etaSquared](#sbi$etaSquared)
#' 
sbi$cohensF <- function (x,g) {
    mod=aov(x~g)
    esq=sbi$etaSquared(mod)
    cf=sqrt(esq/(1-esq))
    return(cf[[1]])
}

#' 
#' <a name='cohensH'> </a>
#' **sbi$cohensH(tab)**
#' 
#' > Effect size for a 2x2 contingency table.
#'
#' > The function cohensH calculates the effect size for 2x2 contingency tables. 
#'   Due to Cohen's rule of thumb values of 0.2 to 0.5 are considered to stand 
#'   for small effects, values from 0.5 to 0.8 represent medium effects and values above 0.8 represent large effects. 
#' 
#' > Arguments:
#' 
#' > - *tab* a 2x2 contingency table
#' 
#' > Returns:  Cohen's h value
#' 
#' > Examples:
#' 
#' > ```{r}
#'   # data from New Eng. J. Med. 329:297-303, 1993
#'   azt=as.table(matrix(c(76,399,129,332), byrow=TRUE,ncol=2))
#'   rownames(azt)=c("AZT","Placebo")
#'   colnames(azt)=c("DiseaseProgress", "NoDiseaseProgress")
#'   sbi$cohensH(azt)
#' > ```
#'
#' > See also: [sbi$cohensW](#cohensW)

sbi$cohensH <- function (tab) {
    pt=prop.test(tab)
    h=2*abs(asin(sqrt(pt$estimate[1]))-
        asin(sqrt(pt$estimate[2])))
    return(h[[1]])
}

#' 
#' <a name="cohensW"> </a>
#' **sbi$cohensW(tab)**
#' 
#' > Effect size for 2x2 and larger contingency tables.
#' 
#' > The function *sbi$cohensW* calculates the effect size for contingency tables. 
#'   Due to Cohen's rule of thumb values of 0.1 to 0.3 are considered to stand 
#'   for small effects, values from 0.3 to 0.5 represent medium effects and values 
#'   above 0.5 represent large effects.
#' 
#' > Arguments:
#' 
#' > - *tab* - continency table with counts, usually created using the table 
#'           command for two variables.
#'
#' > Return: Cohen's w value
#' 
#' > Examples:
#' 
#' > ```{r}
#'   data(Titanic)
#'   Titanic[1,1,,]
#'   sbi$cohensW(Titanic[1,1,,])
#'   # Data from New Eng. J. Med. 329:297-303, 1993
#'   azt=as.table(matrix(c(76,399,129,332), byrow=TRUE,ncol=2))
#'   rownames(azt)=c("AZT","Placebo")
#'   colnames(azt)=c("DiseaseProgress", "NoDiseaseProgress")
#'   sbi$cohensW(azt)
#' > ```
#' 
#' > See also: [sbi$cohensH](#cohensH)

sbi$cohensW <- function (tab) {
    options(warn=-1)
    pe=prop.table(chisq.test(tab)$expected)
    options(warn=0)
    po=prop.table(tab)
    w=sqrt(sum(((po-pe)^2)/pe))
    
    return(w[[1]])
}


#' 
#' <a name="corr"> </a>
#' **sbi$corr(data,method="pearson",use="pairwise.complete.obs")** 
#' 
#' > Calculate pairwise correlations for a given data frame or matrix.
#' 
#' > Arguments:
#' 
#' > - _data_ - matrix or data frame where the variables are in the columns, NA's are allowed.
#'   - _method_ - type of correlation to be determined, either 'pearson', 'spearman' or 'kendall', default: 'pearson'
#'   - _use_ - how to deal with NA's, default: 'pairwise.complete.obs'
#' 
#' > Returns: list with the following components:
#' 
#' > - _estimate_ - matrix with correlation values
#'   - _p.value_ - matrix with p-values
#'   - _method_ - character string with the used correlation method
#' 
#' > Example:
#' 
#' > ```{r}
#'   data(swiss)
#'   lapply(sbi$corr(swiss)[1:2],round,2)
#' > ```
#' 
#' > See also: [sbi$corrplot](#corrplot)

sbi$corr <- function (data,method='pearson',use='pairwise.complete.ob') {
    mt=matrix(0,nrow=ncol(data),ncol=ncol(data))
    colnames(mt)=rownames(mt)=colnames(data)
    mt.pval=mt
    diag(mt)=1
    for (i in 1:(ncol(data)-1)) {
        for (j in i:ncol(data)) {
            rt=cor.test(data[,i],data[,j],
                        method=method,use=use)
            mt[i,j]=mt[j,i]=rt$estimate
            mt.pval[i,j]=mt.pval[j,i]=rt$p.value
        }
    }
    return(list(estimate=mt,p.value=mt.pval,method=method))
}

#' 
#' <a name="corplot"> </a>
#' **sbi$corplot(x,y,col='red',pch=19,...)** 
#' 
#' > Visualize a correlation with abline.
#' 
#' > Arguments:
#' 
#' > - _x_ - numerical vector
#'   - _y_ - numerical vector
#'   - _col_ - plotting character color
#'   - _pch_ - the plotting symbol for the correlations, default: 19
#'   - _method_ - the correlation method, 'pearson', 'spearman' or 'kendall', default: 'pearson'
#'   - _..._ - arguments delegated to the plot function
#' 
#' > Example:
#' 
#' > ```{r fig=TRUE,fig.width=6,fig.height=6,fig.cap="Corplot with abline"}
#'   data(swiss)
#'   sbi$corplot(swiss$Fertility,swiss$Agriculture,
#'     xlab="Fertility",ylab="Agriculture")
#' > ```
#' 
#' > See also: [sbi$corrplot](#corrplot)
#' 
sbi$corplot = function (x,y,col='red',pch=19,cex=2,method="pearson",...) {
    p=cor.test(x,y,method=method,use="complete.obs")$p.value
    star=sbi$report.pval(p,star=TRUE)
    r=paste('r = ',round(cor(x,y,method=method,
                             use="complete.obs"),2),star,sep="")
    plot(x~y,main=r,col=col,pch=pch,cex=cex,...);
    abline(lm(x~y),col=col,lwd=2)
    box()
}
#' 
#' <a name="corrplot"> </a>
#' **sbi$corrplot(mt,...)** 
#' 
#' > Visualize a correlation matrix.
#' 
#' > Arguments:
#' 
#' > - _mt_ - matrix with pairwise correlations
#'   - _text.lower_ - should in the lower diagonal the correlation coefficient be shown, default: TRUE
#'   - _text.upper_ - should in the upper diagonal the correlation coefficient be shown, default: FALSE
#'   - _pch_ - the plotting symbol for the correlations, default: 19
#'   - _p.mat_ - matrix with p-values to strike out insignificant p-values, default: NULL (not used)
#'   - _alpha_ - significance threshold for _p.mat_, default: 0.05
#'   - _cex.sym_ - character expansion for the correlation symbols, default: 5
#'   - _cex.r_ - character expansion for the r-values if _text.lower_ or _text.upper_ are set to TRUE, default: 1
#'   - _cex.lab_ - character expansion for the variable text labels, default: 1.4
#'   - _..._ - arguments delegated to the plot function
#' 
#' > Example:
#' 
#' > ```{r fig=TRUE,fig.width=12,fig.height=10,fig.cap=""}
#'   data(swiss)
#'   sw=swiss
#'   colnames(sw)=abbreviate(colnames(swiss),6)
#'   options(warn=-1) # avoid spearman warnings
#'   cr=sbi$corr(sw,method='spearman')
#'   sbi$corrplot(cr$estimate,cex.sym=8,text.lower=TRUE,
#'      cex.r=1.5,p.mat=cr$p.value)
#'   options(warn=0)
#' > ```
#' 
#' > See also: [sbi$corr](#corr)
#' 

sbi$corrplot <- function (mt,text.lower=TRUE, text.upper=FALSE,
                      pch=19,p.mat=NULL,alpha=0.05,
                      cex.sym=5,cex.r=1,cex.lab=1.4,...) {
    if (class(p.mat) == 'NULL') {
        p.mat=mt
        p.mat[]=0
    }
    yend=nrow(mt)+1
    xend=ncol(mt)+1
    plot(1,type="n",xlab="",ylab="",axes=FALSE,
         xlim=c(0,xend),ylim=c(nrow(mt),0),...)
    text(1:(ncol(mt)),0.25,colnames(mt),cex=cex.lab)
    text(0,1:nrow(mt),rownames(mt),cex=cex.lab,pos=4)
    cols=paste("#DD3333",rev(c(15,30, 45, 60, 75, 90, "AA","BB","CC","DD")),sep="")
    cols=c(cols,paste("#3333DD",c(15,30, 45, 60, 75, 90, "AA","BB","CC","DD"),sep=""))
    breaks=seq(-1,1,by=0.1)                  
    sym=identical(rownames(mt),colnames(mt))
    for (i in 1:nrow(mt)) {
        for (j in 1:nrow(mt)) {
            if (sym & i == j) {
                next
            }   
            coli=cut(mt[i,j],breaks=breaks,labels=1:20)
            if (i == j & !sym & text.lower) {
                text(i,j,round(mt[i,j],2),cex=cex.r)
                if (p.mat[i,j]>alpha) {
                    text(i,j,"x",cex=cex.r*2)
                }

            } else if (i < j & text.lower) {
                text(i,j,round(mt[i,j],2),cex=cex.r)
                if (p.mat[i,j]>alpha) {
                    text(i,j,"x",cex=cex.r*2)
                }

            } else if (i > j & text.upper) {
                text(i,j,round(mt[i,j],2),cex=cex.r)
                if (p.mat[i,j]>alpha) {
                    text(i,j,"x",cex=cex.r*2)
                }
            } else {
                points(i,j,pch=pch,cex=cex.sym,col=cols[coli])
                if (p.mat[i,j]>alpha) {
                    text(i,j,"x",cex=cex.sym*0.3)
                }
            }
        }
    }
}

#' 
#' <a name="cor.var"> </a>
#' **sbi$cor.var(x,r,mean=0,sd=1)** 
#' 
#' > Create a vector with a given correlation to another vector.
#' 
#' > Arguments:
#' 
#' > - _x_ - vector numerical values
#'   - _r_ - correlation value between -1 and 1
#'   - _mean_ - mean for the requested data vector, default: 0
#'   - _sd_ - standard deviation for the requested data vector, default: 1
#' 
#' > Returns: numerical vector having the requested pearson correlation to x
#' 
#' > Example:
#' 
#' 
#' > ```{r}
#'   set.seed(123)
#'   A=rnorm(300,mean=160,sd=6)
#'   B=sbi$cor.var(A,r=0.7,mean=80,sd=4)
#'   C=sbi$cor.var(A,r=0.5,mean=40,sd=2)
#'   D=sbi$cor.var(B,r=0.4,mean=18,sd=1)
#'   E=sbi$cor.var(D,r=0.4,mean=30,sd=1)
#'   df=data.frame(A=A,B=B,C=C,D=D,E=E)
#'   summary(df)
#'   round(cor(df),2)
#' > ```
#' 
#' > See also: [sbi$cor.vars](#sbi.cor.vars)

sbi$cor.var = function (x,r,mean=0,sd=1) {
    mu2=mean
    y = rnorm(length(x),mean=mu2,sd=sd)
    d <- data.frame(x = x, y = y)   
    corr <- r  # target correlation
    corr_mat <- matrix(corr, ncol = 2, nrow = 2)
    diag(corr_mat) <- 1
    mvdat <- mvrnorm(n = nrow(d), mu = c(0, 0), 
     Sigma = corr_mat, empirical = TRUE)  
    rx <- rank(mvdat[ , 1], ties.method = "first")
    ry <- rank(mvdat[ , 2], ties.method = "first")
    dx_sorted <- sort(d$x)
    dy_sorted <- sort(d$y)
    dvx=dx_sorted[rx]
    dvy=dy_sorted[ry]
    dvx2=x
    dvy2=dvy[order(dvx)]
    dvy2=dvy2[rank(dvx2,ties.method='random')]
    return(dvy2)
}   

#' 
#' <a name="cor.vars"> </a>
#' **sbi$cor.vars(n,r.matrix,mu.vec=rep(0,ncol(r.matrix))** 
#' 
#' > Create a data frame with n values for each varaible of the given correlation matrix.
#' 
#' > Arguments:
#' 
#' > - _n_ - number of values for each variable
#'   - _r.matrix_ - matrix of correlation values, must be symmetric
#'   - _mu.vec_ - means for the values of each variable, default: 0
#' 
#' > Returns: data frame of size _n x ncol(r.matrix)_
#' 
#' > Example:
#' 
#' 
#' > ```{r}
#' r.mt=matrix(c(1,0.7,0.4, 0.7,1,0.3, 0.4,0.3,1),
#'      byrow=TRUE,ncol=3)
#' mt.vars=sbi$cor.vars(100,r.mt,c(1,10,5))
#' summary(mt.vars)
#' cor(mt.vars)
#' > ```
#' 
#' > See also: [sbi$cor.var](#sbi.cor.var)


sbi$cor.vars = function (n,r.matrix,mu.vec) {
    samples = n
    data = mvrnorm(n, mu=mu.vec, Sigma=r.matrix,
                   empirical=TRUE)
    return(data)
}

#' 
#' <a name="cv"> </a>
#' **sbi$cv(x,na.rm=FALSE)** 
#' 
#' > Calculate the coefficient of variation.
#' 
#' > Arguments:
#' 
#' > - _x_ - vector with positive numerical values.
#'   - _na.rm_ - should NA's be removed, default: FALSE
#' 
#' > Returns: numerical value for the coefficient of variation.
#' 
#' > Example:
#' 
#' 
#' > ```{r}
#'   cv=sbi$cv
#'   cv(rnorm(20,mean=100,sd=4))
#'   cv(c(1,2,3,4))
#'   cv(c(1,2,3,4,NA))
#'   cv(c(1,2,3,4,NA),na.rm=TRUE)
#' > ```

sbi$cv <- function (x,na.rm=FALSE) {
    cv=100*sd(x,na.rm=na.rm)/mean(x,na.rm=na.rm)
    return(cv)
}

#' 
#' <a name="dict"> </a>
#' **sbi$dict(...)** 
#' 
#' > Create list with keys and values, ensuring unique keys (dictionary).
#' 
#' > Arguments:
#' 
#' > - _..._ - arguments with *key=value* pairs
#' 
#' > Returns: a key-value list with unique keys
#' 
#' > Example:
#' 
#' 
#' > ```{r}
#'   options(error=print)
#'   # list's can have duplicate entries
#'   l=list(a=1,a=2)
#'   l$a
#'   l[1]
#'   l[2]
#'   # dict's can't ...
#'   try({d=sbi$dict(a=1,a=1)})
#'   d=sbi$dict(a=1,b=2)
#'   d
#'   sbi$is.dict(d)
#'   options(error=NULL)
#' > ```
#' 

sbi$dict <- function (...) {
    l=list(...)
    if (!sbi$is.dict(l)) {
        stop('Error: dict must have key value pairs
         and keys can not be duplicated!')
    }
    return(l)
}

#' 
#' <a name="dpairs"> </a>
#' **sbi$dpairs(data,...)** 
#' 
#' > Improved pairs plot considering the data types.
#' 
#' > The function _dpairs_ provides an improved pairs plot which accounts
#'   for the data type of the actual variables. It will plot in the 
#'   lower diagonal xy-plots, box-plots or assoc-plots depending on the 
#'   two data types. In the upper diagonal effect sizes and stars for the p-values
#'   for the tests (anova, t.test, chisq.test or cor.test will be shown. In the diagonal 
#'   the data distribution will be outlined. This plot is usually an useful visualization for 3-8 variables.
#' 
#' > Arguments:
#' 
#' > - _data_ - data frame with columns of class factor, numeric or integer.
#'   - _col.box_ - colors for the boxplots, either a single value or a vector of colors for each level of a factor variable, default; 'grey80'
#'   - _col.xy_  - colors for the xy-plots, either a single value of a vector which is as long as the number of data points, default: 'grey60'
#'   - _cex.diag_ - character expansion for the diagonal texts
#'   - _order_   - should the variables be ordered by data type and name, this is recommended as it orders the plots, starting with assocplots, then boxplots and finally xyplots, default: TRUE
#'   - _pch_     - plotting character for xy-plots, default 19 (round circle).
#' 
#' > Returns: numerical value for the excess kurtosis.
#' 
#' > Example:
#'
#' > ```{r fig=TRUE,dpi=72,out.width=700}
#'   data(iris)
#'   dpairs=sbi$dpairs
#'   par(omi = c(0.8, 0.4,0.4,0.4))
#'   dpairs(iris,col.box=2:4,col.xy=rep(c(2:4),each=50),
#'      cex.diag=1.6)
#'   sbi$dpairs.legend(levels(iris$Species),col=2:4,cex=1)
#' > ```
#'
#'  
#' > ```{r eval=FALSE}
#'   par(omi=c(0.5,0.5,0.8,0.2))
#'   library(MASS)
#'   btwt=birthwt; 
#'   for (col in c('low','race','smoke','ptl','ht','ui','ftv')) { 
#'      btwt[,col]=as.factor(btwt[,col]) 
#'   }
#'   dpairs(btwt[,2:8],cex.diag=1.6)
#'    mtext('Birth-Weight data',side=3,outer=TRUE,cex=1.5,line=1)
#'   if (require("palmerpenguins")) {
#'       data(penguins)
#'       peng=penguins
#'       colnames(peng)[3]='bill.len'
#'       colnames(peng)[4]='bill.dep'
#'       colnames(peng)[5]='flip.len'
#'       colnames(peng)[6]='mass'
#'       sbi$dpairs(peng,col.xy=sbi$pastel(3)[as.numeric(peng$species)])
#'       sbi$dpairs.legend(levels(peng$species),
#'           col=sbi$pastel(3)[as.numeric(as.factor(levels(peng$species)))])
#'   }
#' > ```

sbi$dpairs <- function (data,col.box='grey80',col.xy="grey60",cex.diag=2.5,
                    order=TRUE,pch=19) {
    oop=options()
    opar=par()
    options(warn=-1)
    attach(sbi)
    if (any(class(data) %in% "tbl_df")) {
        data=as.data.frame(data)
    }
    if (order) {
        data=data[,sort(colnames(data))]
        res=c(); for (i in 1:ncol(data)) { res=c(res,class(data[,i])) }
        idx=order(res)
        data=data[,idx]
    }
    mai=rep(0.0,4)
    par(mfrow=c(ncol(data),ncol(data)),mai=mai)
    cnames=colnames(data)
    for (i in 1:ncol(data)) {
        for (j in 1:ncol(data)) {
            if (i == j) {
                plot(1,type='n',xlab='',ylab='',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
                text(0.5,0.5,cnames[i],cex=cex.diag)
                par(mai=rep(0,4))
                box(lty=3,col='grey70')
                if (class(data[,i]) == "factor") {
                    rect(0.1,0.1,0.9,0.3,col="grey90")
                    for (ci in cumsum(prop.table(table(data[,i])))) {
                        x=0.1+0.8*ci
                        lines(x=c(x,x),y=c(0.1,0.3))
                    }
                }
                if (class(data[,i]) %in% c("numeric","integer")) {
                    ds=density(data[,i],na.rm=TRUE)
                    ds$x=ds$x-min(ds$x)
                    ds$x=ds$x/max(ds$x)
                    ds$y=(ds$y/max(ds$y)*0.3)
                    polygon(ds,col='grey80')
                }
                par(mai=mai)
            } else if (i > j) {
                if (class(data[,i]) %in% c("numeric","integer") & class(data[,j]) %in% c("numeric","integer")) {
                    plot(data[,i] ~ data[,j],xlab='',ylab='',axes=FALSE,pch=pch,
                         col=col.xy)
                    box(col='grey70')
                    if (j+1 == i) {
                        #axis(3)
                        #axis(4)
                    }
                    if (j == 1) {
                        axis(2)
                    }
                    if (i == ncol(data)) {
                        ticks=axTicks(1) 
                        axis(1,at=ticks[1:(length(ticks)-1)],labels=ticks[1:(length(ticks)-1)],col='grey70')
                    }
                } else if (class(data[,i]) == "factor" & class(data[,j]) == "factor") {
                    par(mai=rep(0.3,4))
                    dassoc(t(table(data[,i],data[,j])))
                    par(mai=rep(0,4))
                    box(lty=3,col='grey70')
                    par(mai=mai)
                } else if (class(data[,i]) %in% c("numeric","integer")) {
                    boxplot(data[,i] ~ data[,j],col=col.box,axes=FALSE)
                    if (j+1 == i) {
                        #axis(3,at=1:length(levels(data[,j])),labels=levels(data[,j]))
                        #axis(4)
                    } 
                    if (j == 1) {
                        ticks=axTicks(2) 
                        axis(2,at=ticks[1:(length(ticks)-1)],labels=ticks[1:(length(ticks)-1)],col='grey70')
                    }
                    if (i == ncol(data)) {
                        axis(1,at=1:length(levels(data[,j])),labels=levels(data[,j]),col="grey70")
                    }

                    box(col="grey70")
                } else if (class(data[,j]) %in% c("numeric","integer")) {
                    boxplot(data[,j] ~ data[,i],col=col.box,axes=FALSE)
                    if (j == 1) {
                        axis(2)
                    }
                    if (i == ncol(data)) {
                        axis(1,at=1:length(levels(data[,j])),labels=levels(data[,j]))
                    }
                    box()

                } 
            } else {
                if (class(data[,i]) %in% c("numeric","integer") & class(data[,j]) %in% c("numeric","integer")) {
                    r=cor.test(data[,i],data[,j])
                    rs=cor.test(data[,i],data[,j],method='spearman')
                    plot(1,type='n',xlab='',ylab='',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
                    text(0.5,0.59,bquote("" ~ r[P] ~ .(sprintf(" = %.2f%s",r$estimate,report.pval(r$p.value,star=TRUE)))),cex=1.5)
                    text(0.5,0.41,bquote("" ~ r[S] ~ .(sprintf(" = %.2f%s",rs$estimate,report.pval(rs$p.value,star=TRUE)))),cex=1.5)
                } else if (class(data[,i]) == "factor" & class(data[,j]) == "factor") {
                    cw=cohensW(table(data[,i],data[,j]))
                    chsq=chisq.test(table(data[,i],data[,j]))
                    plot(1,type='n',xlab='',ylab='',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
                    text(0.5,0.5,sprintf("Cohen's w =\n%.2f %s",cw,report.pval(chsq$p.value,star=TRUE)),cex=1.5)
                    
                } else if (class(data[,i]) %in% c("numeric","integer")) {
                    if (length(levels(data[,j]))==2) {
                        tt=t.test(data[,i] ~ data[,j]) 
                        cd=cohensD(data[,i],data[,j])
                        plot(1,type='n',xlab='',ylab='',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
                        text(0.5,0.5,sprintf("Cohen's d =\n%.2f %s",cd,report.pval(tt$p.value,star=TRUE)),cex=1.5)
                    } else {
                        raov=aov(data[,i] ~ data[,j]) 
                        #recover()
                        rs=etaSquared(raov)
                        pval=report.pval(summary(raov)[[1]][1,5],star=TRUE)
                        plot(1,type='n',xlab='',ylab='',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
                        text(0.5,0.5,bquote(eta~2~sprintf(" = %.2f %s",rs,pval)),cex=1.5)
                    }
                } else if (class(data[,j]) %in% c("numeric","integer")) {
                    if (length(levels(data[,i]))==2) {
                        tt=t.test(data[,j] ~ data[,i]) 
                        cd=cohensD(data[,j],data[,i])
                        plot(1,type='n',xlab='',ylab='',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
                        text(0.5,0.5,sprintf("Cohen's d =\n%.2f %s",cd,report.pval(tt$p.value,star=TRUE)),cex=1.5)
                    } else {
                        raov=aov(data[,j] ~ data[,i]) 
                        rs=etaSquared(raov)
                        pval=report.pval(summary(raov)[[1]][1,5],star=TRUE)
                        plot(1,type='n',xlab='',ylab='',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
                        val=sprintf("%.2f %s",rs,pval)
                        text(0.5,0.5,bquote("" ~ eta^2 ~ " = " ~ .(val)),cex=1.5)
                    }
                } 
                par(mai=rep(0,4))
                box(lty=3,col='grey70')
                par(mai=mai)
            }
        }
    }
    detach(sbi)
    par(opar)    
    options(oop)

}

#' 
#' <a name="dpairs.legend"> </a>
#' **sbi$dpairs.legend(labels,col='grey80',pch=15, side='bottom')** 
#' 
#' > Adding legend top or bottom to a _sbi$dpairs_ or teh standard _pairs_ plot.
#' 
#' > The function _sbi$dpairs.legend_ allows the user to place a legend outside of a 
#'   pairs or dpairs plot.
#' 
#' > Arguments:
#' 
#' > - _labels_ - txt labels to be plotted
#'   - _col_ - colors for the plotting characters
#'   - _pch_  - plotting symbol, default 15
#'   - _side_ - where to place the legend, 'top' or 'bottom', default: 'bottom'
#'   - _cex_ - the character expansion for the legend, default: 2
#' 
#' > Example:
#' 
#' > ```{r eval=FALSE}
#'   data(iris)
#'   par(omi = c(0.8, 0.4,0.8,0.4)) # reserve some space top and bottom
#'   
#'   sbi$dpairs(iris,col.box=2:4,col.xy=rep(c(2:4),each=50))
#'   sbi$dpairs.legend(levels(iris$Species),col=2:4)
#'   mtext('Iris Data',side=3,outer=TRUE,cex=2,line=1)
#' > ```
#' 

sbi$dpairs.legend <- function (labels,col='grey80',pch=15,side="bottom",cex=2) {
    opar=par()
    options(warn=-1)
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
    plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
    legend(side, labels, xpd = TRUE, horiz = TRUE, inset = c(0,0), 
           bty = "n", pch = pch, col = col, cex = cex)
    par(opar)
}

#' 
#' <a name="drop_na"> </a>
#' **sbi$drop_na(x,cols)** 
#' 
#' > Removes all rows where any of the columns contain a NA.
#' 
#' > In contrast to *na.omit*, the method *sbi$drop_na* just checks the given columns 
#'  to delete rows which have any NA in these two columns in the given rows. 
#'  This mimics the tidyr::drop_na function.
#' 
#' > Arguments:
#' 
#' > - _x_ - data frame or matrix
#'   - _cols_ - column names to check for NA's.
#' 
#' > Example:
#' 
#' > ```{r eval=FALSE}
#'   data(iris)
#'   ir=iris
#'   ir[c(1,3),1]=NA
#'   ir[2,2]=NA
#'   ir[4,4]=NA
#'   head(ir)
#'   head(omit.na(ir)) # removes all rows with an NA somewhere
#'   head(sbi$drop_na(ir,1:2)) # just checks the first two columns
#' > ```
#' 

sbi$drop_na = function (x,cols) { 
    idx=which(apply(!is.na(x[,cols]),1,all)); return(x[idx,]) 
} 

#' 
#' <a name="epsilonSquared"> </a>
#' **sbi$epsilonSquared(x,y=NULL)** 
#' 
#' > Calculate the effect size epsilon-squared for variables of a kruskal.test
#'   Cohen's rule of thumb for interpretation is: 0.01-0.09 small, 0.09-0.25 medium and above 0.25 large effect. You can convert eta-squared to a Spearman _r_ by using the square root of epsilon-square.
#' 
#' > Arguments:
#' 
#' > - _x_ - vector with numerical values or a linear model or an aov object.
#'   - _y_ - vector with factor values or numerical vector of a second class
#' 
#' > Returns: numerical value for epsilon-square for given variables.
#' 
#' > Example:
#' 
#' 
#' > ```{r}
#'   data(iris)
#'   sbi$epsilonSquared(iris$Sepal.Length,iris$Species)
#'   # two factor example as well for wilcox.test possible
#'   data(ToothGrowth)
#'   sbi$epsilonSquared(ToothGrowth$len,as.factor(ToothGrowth$dose))
#'   # close to r-square of spearman!
#'   cor(ToothGrowth$len,ToothGrowth$dose,method="spearman")^2
#' > ```
#' 
#' > See also:  [sbi$etaSquared](#etaSquared)
#' 
#' > Todo: Check for correctness ... too many formula's around, I used that one from Tomczak and Tomczak (2014).
#' 

sbi$epsilonSquared <- function (x,y) {
    if (class(y) %in% c("numeric","integer")) {
        H=unname(kruskal.test(list(x,y))$statistic)
        n=length(x[which(!is.na(x) & !is.na(y))])
    }  else {
        H=unname(kruskal.test(x ~ y)$statistic)
        n=sum(table(x,y)) # get rid of NA's

    }    
    es=H/((n^2-1)/(n+1))
    return(unlist(es))
}
#' 
#' <a name="etaSquared"> </a>
#' **sbi$etaSquared(x,y=NULL)** 
#' 
#' > Calculate the effect size eta-squared for an Anova or a linear model. 
#'   Cohen's rule of thumb for interpretation is: 0.01-0.09 small, 0.09-0.25 medium and above 0.25 large effect. You can convert eta-squared to a Pearson r by using the sqrt of eta-square.
#' 
#' > Arguments:
#' 
#' > - _x_ - vector with numerical values or a linear model or an aov object.
#'   - _y_ - either f factor or NULL if x is given as model.
#' 
#' > Returns: numerical value for eta-squares for all given variables in the model x or the value for the variable given in y if x is a numerical variable.
#' 
#' > Example:
#' 
#' 
#' > ```{r}
#'   data(iris)
#'   etaSquared=sbi$etaSquared
#'   etaSquared(iris$Sepal.Length,iris$Species)
#'   etaSquared(lm(iris$Sepal.Length ~ iris$Species))
#'   etaSquared(aov(iris$Sepal.Length ~ iris$Species))
#'   etaSquared(aov(Sepal.Length ~ Species+Sepal.Width+Petal.Length,data=iris))
#' > ```
#' 
#' > See also:  [sbi$cohensF](#cohensF), [sbi$cohensD](#cohensD), [sbi$epsilonSquared](#epsilon-square).
#' 

sbi$etaSquared <- function (x,y=NULL) {
    if (class(x)[1] == "lm") {
        mod=x
        if (length(attr(mod$terms,"dataClasses"))==2) {
            # single factor given
            return(summary(mod)$r.squared)
        } else {
            class(x)="aov"
            return(sbi$etaSquared(x))
        }
    } else if (class(x)[1] == "aov") {
        mod=x
        ss=sum(summary(mod)[[1]][,2])
        sq=summary(mod)[[1]][,2]/ss
        names(sq)=rownames(summary(mod)[[1]])
        sq=sq[1:(length(sq)-1)]
        return(sq)
    } else if (class(x)[1] == "numeric" & class(y)[1] == "factor") {
        mod=aov(x~y)
        return(as.vector((sbi$etaSquared(mod))))

    } else {
        stop("Error: wrong call of 'etaSquared'! Call either 'sbi$etaSquared(num,factor)' or with 'sbi$etaSquared(lm(num~factor))'!")
    }
}

#'
#' <a name="file.cat"> </a>
#' **sbi$file.cat(filename)** 
#' 
#' > Displays a file to the terminal, not to stdout.
#' 
#' > Arguments:
#' 
#' > - _filename_ - filename of a text file
#' 
#' > Returns: Nothing.
#' 
#' > Example:
#' 
#' 
#' > ```{r}
#'   sbi.code=sbi$file.cat(sbi$FILENAME)
#'   nchar(sbi.code)
#' > ```
#' 
#' > See also: [sbi$file.head](#file.head)


sbi$file.cat <- function (filename) {
    return(paste(paste(readLines(filename),
                           collapse="\n")))
}

#'
#' <a name="file.head"> </a>
#' **sbi$file.head(filename,n=6)** 
#' 
#' > Displays the first n lines of a file to the terminal
#' 
#' > Arguments:
#' 
#' > - _filename_ - filename of a text file
#'   - _n_ - number of first lines to display, default: 6
#' 
#' > Returns: Nothing.
#' 
#' > Example:
#' 
#' 
#' > ```{r}
#'   sbi$file.head(sbi$FILENAME,n=10)
#' > ```
#' 
#' > See also: [sbi$file.cat](#file.cat)
#' 

sbi$file.head = function (filename,n=6) {
    if (!file.exists(filename)) {
        stop(paste('Error! File',filename,'does not exist!'))
    }
    fin=file(filename,'r')
    res=readLines(fin,n=n)
    return(res)
}

#'
#' <a name="fmt"> </a>
#' **sbi$fmt(text,...)** 
#' 
#' > Python like formatting of strings using curly braces.
#' 
#' > Arguments:
#' 
#' > - _text_ - text with curly braces as placeholders
#'   - _..._ - replacement texts or numerical values
#' 
#' > Returns: replaced text
#' 
#' > Example:
#' 
#' 
#' > ```{r}
#'   sbi$fmt('I can say {} {}!',"Hello", "World")
#'   sbi$fmt('I can say {2} {1}!',"World", "Hello")
#' > ```

sbi$fmt = function (str,...) {
    args=list(...);
    if (class(args[[1]]) == 'list') {
        args=args[[1]]
    }
    for (i in 1:length(args)) {
        str=sub("\\{\\}",args[[i]],str)
        str=sub(paste("\\{",i,"\\}",sep=''),
                args[[i]],str)
    }
    return(str)
}

#' 
#' <a name="gmean"> </a>
#' **sbi$gmean(x,na.rm=FALSE)** 
#' 
#' > Calculate the geometric mean of a numerical vector. All values in _x_ should be above zero.
#' 
#' > Arguments:
#' 
#' > - _x_ - vector with positive numerical values.
#'   - _na.rm_ - should NA's be removed, default: FALSE
#' 
#' > Returns: numerical value for the geometric mean.
#' 
#' > Example:
#' 
#' > ```{r}
#'   gmean=sbi$gmean
#'   x=10; y=20; z = 30
#'   gmean(c(x,y,z))
#'   gmean(c(x+0.5*x,y,z))
#'   gmean(c(x,y+0.5*y,z))
#'   gmean(c(x,y,z+z*0.5))
#' > ```
#' 

sbi$gmean=function(x,na.rm=FALSE) {
    idx=which(!is.na(x))
    if (na.rm) {
        x=x[idx]
    } else if (length(idx)!= length(x)) {
        return(NA)
    }
    s=x[1]
    for (i in 2:length(x)) {
        s=s*x[i]
    }
    return(s^(1/length(x)))
}

#' 
#' <a name="hmean"> </a>
#' **sbi$hmean(x,na.rm=FALSE)** 
#' 
#' > Calculate the harmonic mean for a numerical vector, the mean of ratios.
#' 
#' > Arguments:
#' 
#' > - _x_ - vector with positive numerical values.
#'   - _na.rm_ - should NA's be removed, default: FALSE
#' 
#' > Returns: numerical value for the harmonic mean.
#' 
#' > Example:
#' 
#' 
#' > ```{r}
#'   hmean=sbi$hmean
#'   hmean(c(60,30)) # average speed
#'   hmean(c(60,30,0)) # speed is zero
#' > ```
#' 

sbi$hmean=function (x,na.rm=FALSE) {
    idx=which(!is.na(x))
    if (na.rm) {
        x=x[idx]
    } else if (length(idx)!= length(x)) {
        return(NA)
    }
    s=1/x[1]
    for (i in 2:length(x)) {
        s=s+(1/x[i])
    }
    return(1/(s/length(x)))
}


#'
#' <a name="import"> </a>
#' **sbi$import(basename)**
#' 
#' > Module like sourcing of R files relative to the main script. 
#' As the source command in R can't use relative filenames, the import function
#' can be used if the Rscript interpreter is used to source other R source code files using relative filepaths in relation to
#' the main script file. The basename must be given without the file extension, usually _.r_ or _.R_. 
#' This function works only in non-interactive mode if the Rscript interpreter was 
#' used to execute the main script.
#' 
#' > Arguments:
#' 
#' > - _basename_ - relative path to the R script in relationship to the main script without the _.R_ or _.r_ extension
#' 
#' > Returns: nothing
#' 
#' > Example:
#' 
#' > The following example sources _other.R_ or _other.r_ in the same directory 
#' as the main script.
#' 
#' > ```{r}
#'   fout = file("test.R",'w')
#'   cat("test = function (msg) { return(paste('testing',msg)) }\n",file=fout)
#'   close(fout)
#'   sbi$import('test')
#'   test("Hello from test!")
#' > ```
#' 

sbi$import <- function (basename) {
    if (interactive()) {
        stop("import works only in R-scripts run with R-script")
    }
    options <- commandArgs(trailingOnly = FALSE)
    file.arg <- "--file="
    idx=grep(file.arg,options)
    script.name <- sub(file.arg, "", options[idx])
    dir=dirname(script.name)
    success=FALSE
    for (ext in c(".R",".r")) {
        fname=paste(basename,ext,sep="")
        if (file.exists(file.path(dir,fname))) {
            source(file.path(dir,fname))
            print(paste("loading",fname,"!"))
            success=TRUE
            break
        }
    }
    if (!success) {
        stop(paste("Error: Module",basename,"not available!"))
    }
}


#'
#' <a name="input"> </a>
#' **sbi$input(prompt)** 
#' 
#' > Replacement for the readline function in non-interactive scripts. 
#'   As the readline function does only works in interactive mode we need an alternative.
#' 
#' > Arguments:
#' 
#' > - _prompt_ - text displayed to ask for input of the user.
#' 
#' > Returns: input entered by the user as string.
#' 
#' > Example:
#' 
#' > ```{.r}
#'   # notrun
#'   x=sbi$input('Enter a numerical value: ')
#'   x=as.numeric(x)
#'   print(paste("x*x is",x*x))
#' > ```
#' 

sbi$input = function (prompt="Enter: ") {
    if (interactive() ){ 
        return(readline(prompt))
    } else {
        cat(prompt);
        return(readLines("stdin",n=1))
    }
}

#'
#' <a name="is.dict"> </a>
#' **sbi$is.dict(listvar)** 
#' 
#' > Check if the given list is a key-value list with unique keys.
#' 
#' > Arguments:
#' 
#' > - _listvar_ - an object of class list
#' 
#' > Returns: TRUE if the list has unique keys for every value, otherwise FALSE
#' 
#' > Example:
#' 
#' > ```{r}
#'   is.dict=sbi$is.dict
#'   l=list(1,2,"b")
#'   is.dict(l)
#'   l=list(a=1,b=2,a=3)
#'   is.dict(l)
#'   l=list(a=1,b=2,c=3)
#'   is.dict(l)
#' > ```
#' 

sbi$is.dict <- function (l) {
    return(length(unique(names(l)))==length(l) )
}

#' 
#' <a name="kurtosis"> </a>
#' **sbi$kurtosis(x,na.rm=FALSE)** 
#' 
#' > Calculate the fourth central moment of a distribution. Values higher than 0
#'   indicate heavy-tailed distributions, values of lower than zero means 
#'   light-tailed (sharp peak) distributions. Values around zero mean normal value
#'   like distribution. As the normal kurtosis formula has for normal distributions
#'   a value of three, usually the excess kurtosis as in this implementation is 
#'   used which involves substraction of 3.
#' 
#' > Arguments:
#' 
#' > - _x_ - vector with positive numerical values.
#'   - _na.rm_ - should NA's be removed, default: FALSE
#' 
#' > Returns: numerical value for the excess kurtosis.
#' 
#' > Example:
#' 
#' 
#' > ```{r}
#'   sbi$kurtosis(1:10)      # very uniform, should be negative
#'   sbi$kurtosis(runif(10,min=1,max=5)+rnorm(10,mean=3,sd=0.2))
#'   sbi$kurtosis(rnorm(100,mean=10,sd=0.5)) # close to zero
#'   sbi$kurtosis(rt(50,df=1)) # higher than normal
#' > ```
#' 
#' > See also: [sbi$skewness](#skewness)
#' 

sbi$kurtosis <- function (x,na.rm=TRUE) {
    if (na.rm) {
        x=x[!is.na(x)]
    } else {
        if (any(is.na(x))) {
            return(NA)
        }
    }
    g=(sum((x-mean(x,na.rm=na.rm))^4)
       /length(x))/(sd(x)^4)-3
    return(g)
}

#' <a name='mhist'> </a>
#' **sbi$mhist(y,groups,cols=NULL,...)**
#' 
#' > Lattice like histograms.
#'
#' > This function creates for 2-4 groups histograms with a stripe on top
#'   using the same y-scales for all groups.
#'
#' > Arguments: 
#' 
#' > - *y* - numerical variable
#'   - *groups* - categorical variable, best 2-4 groups only.
#'   - *cols* - the colors to be used, default: filename extension pattern, default: ".tab"
#' 
#' 
#' > Examples:
#' 
#' > ```{r fig.width=10,fig.height=5,out.width=800}
#'   data(iris)
#'   sbi$mhist(iris$Sepal.Length,iris$Species,cols="skyblue")
#' > ```
#'
sbi$mhist <- function (y,groups,breaks=10,cols='grey80',...) {
    nr=length(levels(groups))
    par(mfrow=c(1,nr),mai=c(0.4,0.6,0.4,0.0))
    breaks=hist(y,plot=FALSE)$breaks
    #recover()
    if (length(cols)==1) {
        cols=rep(cols,length(levels(groups)))
        names(cols)=levels(groups)
    }
    mx=0
    for (gr in levels(groups)) {
        m=max(table(cut(y[groups==gr],breaks=breaks)))
        if (m>mx) {
            mx=m
        }
    }
    mx=mx*1.15
    x=1
    brid=seq(1,length(breaks),by=2)
    #recover()
    for (gr in levels(groups)) {
        hist(y[groups==gr],
          ylab="n",col=cols[[gr]],main='',
          breaks=breaks,xlim=c(min(breaks),max(breaks)),
          ylim=c(0,mx),axes=FALSE,xaxs='i','yaxs'='i');
          
          box(); 
          if (x==1 | x == 3) {
            axis(1,labels=breaks[brid],at=breaks[brid])
          } else {
            axis(3,labels=breaks[brid],at=breaks[brid])
            axis(1,labels=FALSE,at=breaks[brid])
          }
          if (x == 1) { 
            # omit top tick as this is in label region
            ticks=axTicks(2)
            if (ticks[length(ticks)]>0.95*mx) {
                axis(2,labels=ticks[1:(length(ticks)-1)],
                    at=ticks[1:(length(ticks)-1)])
            } else {
                axis(2)
            }
          }
          rect(min(breaks),mx-0.1*mx,max(breaks),mx,col='#FFE4CC')
          text((min(breaks)+max(breaks))/2,((mx-0.1*mx)+mx)/2,gr,adj=0.5,cex=1.5)
          if (length(levels(groups)) %in% c(2,4) & x %in% c(1,3)) {
              par(mai=c(0.4,0.3,0.4,0.3))
              axis(3,labels=FALSE)

          }
          if (length(levels(groups)) %in% c(2,4) & x %in% c(2,4)) {
              par(mai=c(0.4,0.3,0.4,0.3))
              axis(4,labels=FALSE)

          }
          if (length(levels(groups)) %in% c(3,6) & x %in% c(1,4)) {
              par(mai=c(0.4,0.3,0.4,0.3))
          }
          if (length(levels(groups)) %in% c(3,6) & x %in% c(2,5)) {
              par(mai=c(0.4,0.0,0.4,0.6))
          }
          if (length(levels(groups)) %in% c(3,6) & x %in% c(3,6)) {
              axis(4,labels=FALSE)
              par(mai=c(0.4,0.0,0.4,0.6))
          }
          x=x+1
    }
}

#'
#' <a name="mi"> </a>
#' **sbi$mi(x,y=NULL, breaks=4)** 
#' 
#' > Return the mutual information for two vectors or a binned table.
#' 
#' > Arguments:
#' 
#' > - _x_ - either a binned table, a matrix or data.frame or a numerical vector
#'   - _y_ - a numerical vector if x is not a binned table or matrix or data.frame
#'   - _breaks_ - number of breaks to create a binned table if x and y are numerical vectors, default: 4
#'   _ _norm_ - if input is given should the matrix be normalized by dividing the off-diagonal values by the mutual information in the diagonals, so the self mutual information, default: FALSE
#' > Returns: mutual information value as scalar if inout is table or two vectors or as matrix if input is matrix or data.frame
#' 
#' > Example:
#' 
#' > ```{r}
#'   rn1=rnorm(100,mean=10,sd=1);
#'   rn2=rn1+0.5*rnorm(100)
#'   cor(rn1,rn2) # high
#'   cor(rn1,sample(rn2)) #random 
#'   sbi$mi(rn1,rn2) # high 
#'   sbi$mi(rn1,sample(rn2)) #random
#'   sbi$mi(rn1,rn2,breaks=4)
#'   sbi$mi(rn1,rn2,breaks=7)
#'   data(swiss)
#'   round(sbi$mi(swiss),2)
#'   round(sbi$mi(swiss,norm=TRUE),2)
#' > ```

sbi$mi = function (x,y=NULL,breaks=4,norm=FALSE) {
  if (is.matrix(x) | is.data.frame(x)) {
     M=matrix(0,nrow=ncol(x),ncol=ncol(x))
     rownames(M)=colnames(M)=colnames(x)
     for (i in 1:(ncol(x)-1)) {
         for (j in i:ncol(x)) {
             M[i,j]=M[j,i]=sbi$mi(x[,i],x[,j],breaks=breaks)
         }   
     }
     # last cell
     M[ncol(x),ncol(x)]=sbi$mi(x[,ncol(x)],x[,ncol(x)])
     if (norm) {
        M=M/diag(M)
     }
     return(M)
  }
  if (!is.table(x)) {
     if (class(y)[1] != "NULL") {
         x=table(cut(x,breaks=breaks),cut(y,breaks=breaks))        
     } else {
        stop("if x is vector, y must be given as well")
     }
  }
  f1=x/sum(x)
  fx=rowSums(f1)
  fy=colSums(f1)
  fn=fx %o% fy
  f2=fn/sum(fn)
  LR = ifelse(f1>0,log(f1/f2),0)
  MI = sum(f1*LR)
  return(MI)
}

#' 
#'  <a name="package.deps"> </a>
#' **package.deps(pkgName,mode='all')**
#' 
#' > Return the packages which are required by the given package name.
#' 
#' > Arguments:
#' 
#' > - _pkgName_ - an package name given as text string.
#'   - _mode_ - which package names to return, the following modes are available
#'        - 'all' - all required packages
#'        - 'install' - not yet installed packages
#'        - 'nonbase' - packages not in the standard R installation
#'   - _cran_ - the default CRAN site, default: "https://www.freestatistics.org/cran/"
#' 
#' > Returns: list of required packages.
#' 
#' > Example:
#' 
#' > ```{r}
#'   sbi$package.deps('igraph',mode='nonbase')
#'   sbi$package.deps('igraph',mode='all')
#' > ```

sbi$package.deps <- function(pkgName,mode='all',cran="https://lib.ugent.be/CRAN/")  {
    x=pkgName
    if (!interactive()) {
         r <- getOption("repos");
         r["CRAN"] <- cran
         #"https://lib.ugent.be/CRAN/" 
         options(repos=r) 
    }
    require(tools)
    deps=package_dependencies(x,recursive=TRUE)[[1]]
    if (mode == 'install') {
        idx = which(
        !(deps %in% rownames(installed.packages())))
        return(deps[idx])
    } else if (mode == 'nonbase') {
        ipacks=installed.packages()
        bpacks=ipacks[ipacks[,'Priority'] %in% 
            c('base','recommended'),]
        rnms=setdiff(rownames(ipacks),rownames(bpacks))
        return(intersect(deps,rnms))
    } else if (mode == 'all') {
        return(deps)
    } else {
        stop('mode must be either `all`, `install` or `nonbase`!')
    }

}

#' 
#'  <a name="package.request"> </a>
#' **sbi$package.request(pkgName)**
#' 
#' > Checks if a given package exists and if this is not the case, it will install 
#' this and all required packages without asking the user.
#' 
#' > Arguments:
#' 
#' > - _pkgName_ - an package name given as text string
#'   - _cran_ - the default CRAN site, default: "https://www.freestatistics.org/cran/"
#' 
#' > Returns: invisible the success, either TRUE if package could be found/installed and loaded or FALSE if not
#' 
#' > Example:
#' 
#' > ```{r}
#'   # notrun
#'   res=sbi$package.request('argparser')
#'   print(res)
#' > ```
#'

sbi$package.request <- function (pkgName,cran="https://www.freestatistics.org/cran/") {
    if( !is.element(pkgName, .packages(all.available = TRUE)) ) {
        if (!interactive()) {
            # in scripts you need to set the repo
            r <- getOption("repos");
            r["CRAN"] <- cran
            options(repos=r)
        }
        install.packages(pkgName)
    }
    return(require(pkgName,character.only = TRUE))
}

#' 
#' <a name="modus"> </a>
#' **sbi$modus(catvar)** 
#' 
#' > Return the most often level in a categorical variable.
#' 
#' > Arguments:
#' 
#' > - _cat_ - a vector with elements of class factor
#' 
#' > Returns: Most often apparent level in the categorical variable
#' 
#' > Example:
#' 
#' > ```{r}
#'   sbi$modus(c('A','A','B','C'))
#'   sbi$modus(c('A','A','B','B','C'))
#' > ```

sbi$modus = function (cat) {
    tab=table(cat)
    idx=which(max(tab)==tab)
    return(names(tab)[idx])
}

#' 
#' <a name='pastel'> </a>
#' **sbi$pastel(n)**
#' 
#' > Create up to 20 pastel colors
#' 
#' > This is an alternative color creation function for R versions before 3.6 where 
#'   the function `hcl.colors` is not available.
#' 
#' > Arguments:
#' 
#' > - _n_ - number of colors requested, must be within 2 and 20
#' 
#' > Returns: Vector of colors in RGB codes of requested length 'n'
#' 
#' > ```{r fig=TRUE,fig.height=3,fig.width=6}
#' sbi$pastel(4)
#' par(mai=c(0.2,0.2,0.2,0.1))
#' plot(1:20,col=sbi$pastel(20),cex=3,pch=15)
#' > ```
#' 
sbi$pastel = function (n) {
    if(n > 20 |  n < 1) {
        stop("only between 1 and 20 colors can be given" ) 
    }
    pcols= c("#FFC5D0","#FDC8C3","#F6CBB7","#EDD0AE","#E2D4A8","#D4D8A7","#C5DCAB","#B6DFB4","#A8E1BF",
             "#9EE2CB", "#99E2D8","#9BE0E5","#A4DDEF","#B3D9F7","#C4D5FB","#D5D0FC","#E4CBF9","#F0C7F2",
             "#F9C5E9", "#FEC4DD")
             idx=seq(1,20,by=floor(20/n))
             return(pcols[idx])
}

#' 
#' <a name="pca.biplot"> </a>
#' **pca.biplot(pca,...)** 
#' 
#' > Improved biplot for pca objects.
#' 
#' > The function _pca.biplot_ provides an improved biblot for
#'   visualizing the pairwise scores of individual principal components of 
#'   an object created using the function _prcomp_. In contrast to the default 
#'   biplot function  this plot visualizes the data as points and not row numbers,
#'   it allows to display groups using color codes and distribution ellipses.
#' 
#' > Arguments:
#' 
#' > - _pca_ - pca object of class _prcomp_, created using the function _prcomp_.
#'   - _pcs_   - the components to plot, default: c('PC1','PC2')
#'   - _pch_ - plotting character, default: 19
#'   - _col_  - plotting color, default: black
#'   - _arrows_ - should loading arrows be displayed, default: TRUE
#'   - _arrow.fac_ - scaling factor for arrow length, default: 1
#'   - _ellipse_ - should 85 and 95 confidence intervals for the chisq distribution be shown. If this is shown colors for each group using the col argument must be given, default: FALSE
#'   - _ell.fill_ - should a filled 85 percent confidence interval be shown, colors will be used from the plotting color with opacity, default: FALSE
#'   - _xlab_ - custom xlab, if not given the PC name with variance in percent is shown, default: NULL
#'   - _ylab_ - custom ylab, if not given the PC name with variance in percent is shown, default: NULL
#'   - _..._     - additional arguments delegated to the standard plot function
#' 
#' > Returns: None
#' 
#' > Example:
#' 
#' > ```{r fig=TRUE,fig.height=6,fig.width=12,fig.cap=""}
#'   par(mai=c(0.8,0.8,0.2,0.6),mfrow=c(1,2))
#'   data(iris)
#'   pci=prcomp(iris[,1:4],scale=TRUE)
#'   pca.biplot(pci,col=rep(2:4,each=50),ellipse=TRUE,ell.fill=TRUE,
#'       arrow.fac=2.3,arrows=TRUE,main="biplot")
#'   legend('topright',pch=19,col=2:4,levels(iris$Species))
#'   # standard score plot
#'   pca.biplot(pci,col=rep(2:4,each=50),ellipse=FALSE,
#'      arrow.fac=2.3,arrows=FALSE,main="scoreplot")
#' > ```
#' 

pca.biplot = function (pca,pcs=c("PC1","PC2"),
                       pch=19,col='black',
                       arrows=TRUE,arrow.fac=1,
                       ellipse=FALSE,ell.fill=FALSE,xlab=NULL,ylab=NULL,...) {
    if (missing("xlab")) {
        xlab=paste(pcs[1]," (", round(summary(pca)$importance[2,pcs[1]]*100,1),"%)",sep="")
    } 
    if (missing("ylab")) {
        ylab=paste(pcs[2]," (", round(summary(pca)$importance[2,pcs[2]]*100,1),"%)",sep="")
    } 
    plot(pca$x[,pcs[1]],pca$x[,pcs[2]],pch=pch,col=col,type="n",xlab=xlab,ylab=ylab,...)
    abline(h=0,lty=2)
    abline(v=0,lty=2)    
    if (ellipse) {
        if (length(col)!= nrow(pca$x)) {
            stop("colors must have sam elength as data points")
        }
        ell.col=col
        i=1
        for (cl in names(table(ell.col))) {
            C=cov(pca$x[ell.col==cl,c(pcs[1],pcs[2])])    # Covarianz-Matrix C bestimmen
            d85=qchisq(0.85, df = 2)     # 85% - Faktor , um die Ellipse zu skalieren
            M=colMeans(pca$x[ell.col==cl,c(pcs[1],pcs[2])]) #   Mittelwerte (Zentrum) des Clusters
            el=cluster::ellipsoidPoints(C, d85, loc=M)  # Ellipsen-Punkte aus C und M berechnen
            if (ell.fill) {
                colfill=paste(rgb(t(col2rgb(cl))/255),"33",sep="")
                polygon(el,col=colfill,border=NA)
                i=i+1
                next
            }
            lines(el,col=cl,lwd=1.5,lty=2)    #  Ellipse als geschlossene Linies zeichnen
            d95=qchisq(0.95, df = 2)     # 85% - Faktor , um die Ellipse zu skalieren
            el=cluster::ellipsoidPoints(C, d95, loc=M)  # Ellipsen-Punkte aus C und M berechnen
            lines(el,col=cl,lwd=1.5,lty=1)    #  Ellipse als geschlossene Linies zeichnen                        

        }
    }
    points(pca$x[,pcs[1]],pca$x[,pcs[2]],pch=pch,col=col,...)
    if (arrows) {
        loadings=pca$rotation
        arrows(0,0,loadings[,pcs[1]]*arrow.fac,loadings[,pcs[2]]*arrow.fac,
               length=0.1,angle=20,col='black')
        text(loadings[,pcs[1]]*arrow.fac*1.2,loadings[,pcs[2]]*arrow.fac*1.2,
             rownames(loadings),col='black',font=2)
    }

}
#' 
#' <a name="pca.pairs"> </a>
#' **sbi$pca.pairs(pca,...)** 
#' 
#' > Improved pairs plot for pca objects.
#' 
#' > The function _sbi$pca.pairs_ provides an improved pairs plot for
#'   visualizing the pairwise scores of the individual components of an analyses 
#'   using the function _prcomp_. In contrast to the default pairs function 
#'   this plot visualizes in the diagonal as well the variances and 
#'   a density line for the component scores.
#' 
#' > Arguments:
#' 
#' > - _pca_ - pca object which was created using the function _prcomp_.
#'   - _n_   - maximal number of components to visualize, default: 10
#'   - _groups_ - vector with classes having the same length than the inout matrix for prcomp has rows, default: NULL
#'   - _col_ - colors for the plotting, character, default: 'black'
#'   - _pch_  - plotting, symbol, default: 19
#'   - _legend_ - should the legend be displayed on top, default: FALSE
#'   - _..._     - additional arguments delegated to the standard _pairs_ function
#' 
#' > Returns: None
#' 
#' > Example:
#' 
#' > ```{r fig=TRUE,fig.cap=""}
#'   data(iris)
#'   pci=prcomp(iris[,1:4],scale=TRUE)
#'   sbi$pca.pairs(pci,pch=15,groups=iris[,5],
#'      legend=TRUE,oma=c(5,4,4,4),col=as.numeric(iris[,5])+1)
#' > ```

sbi$pca.pairs = function (pca,n=10,groups=NULL,
                      col='black',pch=19,legend=FALSE,...) {
    .n <<- 1
    if (n>ncol(pca$x)) {
        n=ncol(pca$x)
    }
    pst=FALSE
    if (class(groups) != "NULL" & length(col) != length(groups)) {
        coln=length(levels(as.factor(groups)))
        cols=pastel(coln)
        col=cols[as.numeric(as.factor(as.character(groups)))]
        pst=TRUE
    }
    panel.text = function (x,...) {
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        text(0.5,0.5,
             paste(sprintf("%.1f",summary(pca)$importance[2,.n]*100),"%",
                   sep=""),cex=1.5)
        ds=density(pca$x[,.n],na.rm=TRUE)
        ds$x=ds$x-min(ds$x)
        ds$x=ds$x/max(ds$x)
        ds$y=(ds$y/max(ds$y)*0.3)
        polygon(ds,col='grey80')
        .n <<- .n + 1
    }
    pairs(pca$x[,1:n],diag.panel=panel.text,col=col,pch=pch,...)
    if (legend && class(groups) != "NULL") {
        opar=par()
        options(warn=-1)
        par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
        plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
        if (pst) {
            leg.cols=pastel(coln)[as.numeric(as.factor((levels(as.factor(groups)))))]
        } else {
            cols=col
            names(cols)=as.character(groups)
            lcol=cols[unique(names(cols))]
            leg.cols=as.numeric(lcol)
        }
        legend('bottom', levels(as.factor(groups)), xpd = TRUE, 
               horiz = TRUE, inset = c(0,0), 
               bty = "n", pch = pch, col = leg.cols, cex = 1.2)
        
        par(opar)
    }

}
#' 
#' <a name="pca.plot"> </a>
#' **sbi$pca.plot(pca,...)** 
#' 
#' > Improved bar or screeplot for pca objects.
#' 
#' > The function _sbi$pca.plot_ provides an improved bar- or screeplot for
#'   visualizing the variances of the individual components of an analyses 
#'   using the function _prcomp_. In contrast to the default plot function 
#'   this plot visualize cumulative and individual variances in percent.
#' 
#' > Arguments:
#' 
#' > - _pca_ - pca object which was created using the function _prcomp_.
#'   - _n_   - maximal number of components to visualize, default: 10
#'   - _type_ - plotting type either "bar" or "scree", default: "bar"
#'   - _cex_  - character expansion for the legend and the screeplot plotting characters, default: 1.5
#'   - _legend_ - should the legend be displayed on top, default: TRUE
#'   - _pc.col_   - colors for the PC variances, first individual, second color for the cumulative variance, default: c("light blue","grey")
#'   - _..._     - additional arguments delegated to the standard plot function
#' 
#' > Returns: None
#' 
#' > Example:
#' 
#' > ```{r fig=TRUE,fig.width=11,fig.height=6,fig.cap=""}
#'   data(iris)
#'   par(mfrow=c(1,2))
#'   pcai=prcomp(iris[,1:4],scale=TRUE)
#'   sbi$pca.plot(pcai)
#'   sbi$pca.plot(pcai,type="scree",legend=FALSE)
#' > ```
#' 

sbi$pca.plot = function (pca,n=10,type="bar", cex=1.5, 
                     legend=TRUE,xlab="Components",ylab="Variance (%)",
                     pc.col=c("light blue","grey"),...) {
    if (n>ncol(pca$x)) {
        n=ncol(pca$x)
    }
    if (legend) {
        ylim=c(0,120)
    } else {
        ylim=c(0,105)
    }
    if (type=="bar") {
        barplot(summary(pca)$importance[3,1:n]*100,
                ylim=ylim,col='white',
                xlab=xlab,ylab=ylab,axes=FALSE,...)
    } else {
        plot(summary(pca)$importance[3,1:n]*100,type="b",
                ylim=ylim,cex.axis=1.2,lwd=2,cex=cex,
                xlab=xlab,ylab=ylab,axes=FALSE,
                pch=15,col=pc.col[2],...)
        points(summary(pca)$importance[2,1:n]*100,type="b",cex=cex,
                lwd=2,xlab="", pch=15,col=pc.col[1],...)

        axis(1,at=1:n,label=paste("PC",1:n,sep=""))
    }
    axis(2,at=c(20,40,60,80,100),labels=c(20,40,60,80,100))
    if (type == "bar") {
        barplot(summary(pca)$importance[3,1:n]*100,add=TRUE,col=pc.col[2],axes=FALSE)
        barplot(summary(pca)$importance[2,1:n]*100,add=TRUE,col=pc.col[1],axes=FALSE)        
    }
    abline(h=5,lty=2,lwd=0.5)
    abline(h=10,lty=2,lwd=0.5)
    abline(h=90,lty=2,lwd=0.5)
    abline(h=95,lty=2,lwd=0.5)    
    abline(h=100,lty=1,lwd=0.5)    
    if (legend) {
        legend("topleft",c("Component","Cumulative"),col=pc.col,pch=15,cex=1.5,box.lwd=0,ncol=2)
    }
    box()
}

#' <a name='pcor'> </a>
#' **sbi$pcor(x,y,z,method='pearson')**
#' 
#' > Partial correlation between two variables.
#' 
#' > Calculate partial correlation coefficient of either parametric ("Pearson") 
#'   or non-parametric ("Spearman") statistics corrected for one or more other variables.
#' 
#' > Arguments: 
#' 
#' > - _x_ - numeric vector, missing values are allowed
#'   - _y_ - numeric vector, missing values are allowed
#'   - _z_ - numeric vector, matrix or data frame,  missing values are allowed
#'   - _method_ - character string indicating which partial correlation coefficient is to be computed, either "pearson" (default), or or "spearman"
#' 
#' > Returns: Estimate gives the partial correlation coefficient between x and y given z
#' 
#' > Examples:
#' 
#' > ```{r}
#'   y.data <- data.frame(
#'     hl=c(7,15,19,15,21,22,57,15,20,18),
#'     disp=c(0.000,0.964,0.000,0.000,0.921,0.000,0.000,1.006,0.000,1.011),
#'     deg=c(9,2,3,4,1,3,1,3,6,1),
#'      BC=c(1.78e-02,1.05e-06,1.37e-05,7.18e-03,0.00e+00,0.00e+00,0.00e+00,
#'           4.48e-03,2.10e-06,0.00e+00)
#'   )
#'   # partial correlation between "hl" and "disp" given "deg" and "BC"
#'   sbi$pcor(y.data$hl,y.data$disp,y.data[,c("deg","BC")])
#' > ```
#' 
#' > See also: [sbi$pcor.test](#pcor.test)

sbi$pcor = function (x,y,z,method='pearson') {
    r=sbi$pcor.test(x,y,z,method=method)$estimate
    return(r)
}

#' 
#' <a name='pcor'> </a>
#' **sbi$pcor.test(x,y,z,method='pearson')**
#' 
#' > Partial correlation test for two variables.
#' 
#' > Calculate partial correlation coefficient and  parametric 
#'   ("Pearson") or non-parametric ("Spearman") 
#'   test statistics for two variables corrected 
#'   for one or more other variables.
#' 
#' > Arguments: 
#' 
#' > - _x_ - numeric vector, missing values are allowed
#'   - _y_ - numeric vector, missing values are allowed
#'   - _z_ - numeric vector, matrix or data frame,  missing values are allowed
#'   - _method_ - character string indicating which partial correlation coefficient is to be computed, either "pearson" (default), or or "spearman"
#' 
#' > Return: List with the following components: 
#' 
#' > - _estimate_ - gives the partial correlation coefficient between x and y given z
#'   - _p.value_ - gives the p-value of the test
#'   - _statistics_ - gives the value of the test statistics
#'   - _n_ - gives the number of samples after deleting all the missing samples
#'   - _gn_ - gives the number of given variables
#'   - _method_ - gives the correlation method used
#' 
#' > ```{r}
#'   y.data <- data.frame(
#'    hl=c(7,15,19,15,21,22,57,15,20,18),
#'    disp=c(0.000,0.964,0.000,0.000,0.921,0.000,0.000,1.006,0.000,1.011),
#'    deg=c(9,2,3,4,1,3,1,3,6,1),
#'     BC=c(1.78e-02,1.05e-06,1.37e-05,7.18e-03,0.00e+00,0.00e+00,0.00e+00,
#'          4.48e-03,2.10e-06,0.00e+00)
#'   )
#'   # partial correlation between "hl" and "disp" given "deg" and "BC"
#'   sbi$pcor.test(y.data$hl,y.data$disp,y.data[,c("deg","BC")])
#' > ```
#'
#' > See also:[sbi$pcor](#pcor)
#' 
# this implementation is based on ideas in:
# http://r.789695.n4.nabble.com/Partial-correlations-and-p-values-td908641.html
# same results as with ppcor package!!

sbi$pcor.test = function (x,y,z,method='pearson') {
   if (class(z)=='data.frame') {
       z=as.matrix(z)
   }
   if (class(z) == 'matrix') {
       df=data.frame(x=x,y=y) 
       for (col in 1:ncol(z)) {
           df=cbind(df,z=z[,col])
           colnames(df)[ncol(df)]=colnames(z)[col]
       }
   } else {
       df=data.frame(x=x,y=y,z=z)
   }
   frmx=formula(paste("x~",paste(colnames(df)[3:ncol(df)],collapse="+"),sep=""))
   frmy=formula(paste("y~",paste(colnames(df)[3:ncol(df)],collapse="+"),sep=""))
   df=na.omit(df)
   if (method=='spearman') {
       df=as.data.frame(apply(df,2,rank))
   }
   #xres=residuals(lm(df[,1]~df[,3]))
   #yres=residuals(lm(df[,2]~df[,3]))
   xres=residuals(lm(frmx,data=df))
   yres=residuals(lm(frmy,data=df))

   pr=cor(xres,yres)
   gn=ncol(df)-2 # number of z
   n=nrow(df)
   statistic <- pr*sqrt((n-2-gn)/(1-pr^2))
   p.value <- 2*pnorm(-abs(statistic))
   return(data.frame(estimate=pr,p.value=p.value,statistic=statistic,n=n,gn=gn,method=method))
}


#' <a name='readDataFiles'> </a>
#' **sbi$readDataFiles(dir,fun=read.table,pattern=".tab",...)**
#' 
#' > Read a directory of data files in one data frame.
#'
#' > This function reads all files in the given data directory and combines there data
#'   in one data frame. A column with the filename is added at the end to indicate
#'   from which file the data are.
#'
#' > Arguments: 
#' 
#' > - *dir* - a directory with data files
#'   - *fun* - function to be used for data reading, default: read.table
#'   - *pattern* - the filename extension pattern, default: ".tab"
#'   - *...* - remaining arguments are delegated to the data read function.
#' 
#' > Returns:  data frame with all combined data and a column file at the end which ind
#'             indicates the data file.
#' 
#' > Examples:
#' 
#' > ```{r eval=FALSE}
#'   # not run
#'   res=sbi$readDataFiles("datadir",sep="\t")
#' > ```
#'

sbi$readDataFiles <- function (dir,fun=read.table, pattern=".tab",...) {
  x=1
  for (file in list.files(dir,pattern=pattern)) {
      data=fun(file.path(dir,file),...)
      files=rep(file,nrow(data))
      data=cbind(data,file=files)
      if (x==1) {
         data.all=data
      } else {
         data.all=rbind(data.all,data)
      }
      x=x+1   
  }
  return(data.all)
}

#' 
#' <a name="report.pval"> </a>
#' **sbi$report.pval(x,star=FALSE)** 
#' 
#' > Return a p-value of reporting, either giving the three alpha thresholds, 
#'   <0.05, <0.01, or <0.001 or using the star syntax. 
#' 
#' > Arguments:
#' 
#' > -  _x_ - a numerical p-value.
#'   -  _star_ - boolean, should the one-three star syntax be used, default: FALSE.
#' 
#' > Returns: pvalue in shown in alpha-threshold or star syntax. 
#'   If the p-value is not significant, either the value or any empty string is 
#'   returned if the star syntax is used.
#' 
#' > Example:
#' 
#'  
#' > ```{r}
#'   report.pval=sbi$report.pval
#'   report.pval(1/10000)
#'   report.pval(1/10000,star=TRUE)
#'   report.pval(0.02,star=TRUE)
#'   report.pval(0.12,star=TRUE)
#'   report.pval(c(0.001,0.01,0.3,0.02))
#' > ```
#' 

sbi$report.pval <- function (p.val,star=FALSE) {
    if (length(p.val) > 1) {
        return(as.character(lapply(p.val,report.pval)))
    }
    if (p.val <0.001 & star) {
        return('***')
    } else if (p.val <0.001) {
        return('<0.001')
    } else if (p.val <0.01 & star) {
        return('**')
    } else if (p.val <0.01) {
        return('<0.01')
    } else if (p.val <0.05 & star) {
        return('*')
    } else if (p.val <0.05) {
        return('<0.05')
    } else if (star) {
        return("")
    } else {
        return(sprintf("%.2f",p.val))
    }   
}   

#' 
#' <a name="sem"> </a>
#' **sbi$sem(x,na.rm=FALSE)** 
#' 
#' > Calculate the standard error of the mean for a given numerical vector.
#' 
#' > Arguments:
#' 
#' > -  _x_ - a numerical vector.
#'   -  _na.rm_ - logical vecor indicating if missing values should be removed, default: FALSE
#' 
#' > Returns: computed standard error of the mean.
#' 
#' > Example:
#' 
#'  
#' > ```{r}
#'   sem=sbi$sem
#'   sem(rnorm(50,mean=10,sd=3))
#'   sem(rnorm(1000,mean=10,sd=3))
#' > ```
#' 

sbi$sem <- function(x,na.rm=FALSE) {
    sd(x,na.rm=na.rm)/sqrt(length(x[!is.na(x)])) 
}

#' 
#' <a name="skewness"> </a>
#' **sbi$skewness(x,na.rm=FALSE)** 
#' 
#' > Calculate the third central moment of a distribution. Values higher than zero
#'   indicate right-tailed distributions, values of lower than zero mean 
#'   left-tailed distributions. Values around zero mean normal value
#'   like distribution. 
#' 
#' > Arguments:
#' 
#' > - _x_ - vector with positive numerical values.
#'   - _na.rm_ - should NA's be removed, default: FALSE
#' 
#' > Returns: numerical value for the skewness of the vector.
#' 
#' > Example:
#' 
#' 
#' > ```{r}
#'   sbi$skewness(1:100)          # very uniform, 0
#'   sbi$skewness(rnorm(100))     # normal, close to 0
#'   # now with right tail
#'   sbi$skewness(c(rnorm(100,mean=20),rnorm(30,mean=23,sd=2)))
#' > ```
#' 
#' > See also: [sbi$kurtosis](#kurtosis)
#'
sbi$skewness <- function (x,na.rm=TRUE) {
    if (na.rm) {
        x=x[!is.na(x)]
    } else {
        if (any(is.na(x))) {
            return(NA)
        }
    }
    g=(sum((x-mean(x,na.rm=na.rm))^3)/
       length(x))/(sd(x)^3)
    return(g)
}


#' <a name='smartbind'> </a>
#' **sbi$smartbind(x,y)**
#' 
#' > Combine two data frames via rbind even with different column names.
#'
#' > This function combines two data frames or matrices even if they have different
#'   column names. In each data frame or matrix missing columns are first filled up
#'   with NA's. Thereafter data is fused using rbind. Column order is determined based 
#'   on the first given data.
#'
#' > Arguments: 
#' 
#' > - *x* - data frame or matrix
#'   - *y* - data frame or matrix
#' 
#' > Returns:  data frame or matrix, depending on the given input where data from *x* and *y* are combined
#'   using rbind. Missing columns in either of the data frame are filled up with NA's.
#' 
#' > Examples:
#' 
#' > ```{r}
#'   ir1=cbind(rn1=rnorm(nrow(iris)),iris)
#'   ir2=cbind(iris,rn2=rnorm(nrow(iris),mean=10))
#'   head(sbi$smartbind(ir1,ir2))
#'   tail(sbi$smartbind(ir1,ir2))
#' > ```
#' 

sbi$smartbind <- function (x,y) {
    nxcols=setdiff(colnames(y),colnames(x))
    nycols=setdiff(colnames(x),colnames(y))
    for (c in nxcols) {
        x=cbind(x,ncol=rep(NA,nrow(x)))
        colnames(x)[ncol(x)]=c
    }
    for (c in nycols) {
        y=cbind(y,ncol=rep(NA,nrow(y)))
        colnames(y)[ncol(y)]=c
    }
    y=y[,colnames(x)]
    x=rbind(x,y)
    return(x)
}

#' ## Operators
#' 
#'  <a name="ni"> </a>
#' **lhs %ni% rhs  - not-in operator**
#' 
#' > The not-in operator _%ni%_ is the inverse of the in-operator _%in%_.
#' 
#' > Arguments:
#' 
#' > - _lhs_ - a R vector
#'   - _rhs_ - a R vector
#' 
#' > Returns: element in *lhs* which are not in *rhs*
#' 
#' > Example:
#' 
#' > ```{r}
#'   v1=1:5
#'   v2=3:7
#'   v1 %ni% v2
#'   v2 %ni% v1
#' > ```
#' 

`%ni%` = Negate(`%in%`)

#' 
#' <a name='pipe'> </a>
#' **lhs %>% rhs - pipe operator**
#'
#' > Pipe an object forward into a function or call expression.
#' Unlike the `magrittr` pipe, you must supply an actual function 
#' instead of just a function name. For example
#' `mtcars %>% head` will not work, but `mtcars %>% head()` will.
#'
#' > Arguments:
#' 
#' > - _lhs_ - the result you are piping.
#' > - _rhs_ - Where you are piping the result to.
#'
#' > Examples:
#'
#' > ```{r}
#'   data(mtcars)
#'   mtcars %>% head()
#'   mtcars %>% subset(select=c(mpg,cyl,disp,drat)) %>% head()
#' > ```
#' 
#' > Note: Code is from Nathan Eastwood, from his poorman package, LICENSE as well MIT.
#'

'%>%' = function(lhs, rhs) {
  lhs <- substitute(lhs)
  rhs <- substitute(rhs)
  eval(as.call(c(rhs[[1L]], lhs, as.list(rhs[-1L]))), envir = parent.frame())
}

#' 
#' ## Citation
#' 
#' How to cite this package:
#' 
#' ```{.r}
#' # notrun
#' @Misc{Groth2022sbi,
#'   author =   {Detlef Groth},
#'   title =    {sbi.R: {R} functions for Statistical Bioinformatics},
#'   howpublished = {\url{https://github.com/mittelmark/Rcode}},
#'   year = {2022}
#' }
#' ```
#' 
#' Groth, D. (2022). sbi.R - R functions for Statistical Bioinformatics. [https://github.com/mittelmark/Rcode](https://github.com/mittelmark/Rcode)
#'
#' ## Documentation
#' 
#' This documentation was created directly from the file *sbi.R* using the following commands in the terminal using the Rmarkdown package:
#' 
#' ```
#' # notrun
#' Rscript --eval
#' ```
#' 
#' Alternatively you can just extract the plain documentation without output of the code chunks like this:
#' 
#' ```
#' # not run
#' Rscript --docu
#' ```
#' 
#' ## Disclaimer
#' 
#' This file contains a few important R functions to be used for illustration. The functions have much less error checking than function you can find
#' in R packages available on CRAN. If you do serious statistical analysis I recommend to use those packages. Here is a list of packages I recommend
#' very much:
#' 
#' - vcd
#' - corrplot
#' - e1071
#' - effectsize
#' - argparser
#' 
#' ## Copyright
#' 
#' Copyright @ 2022 - Detlef Groth, University of Potsdam
#' 
#' License: MIT - License
#' 

if (sys.nframe() == 0L && !interactive()) {
    usage <-  function () {
        cat("sbi.R - R functions for Statisical Bioinformatics\n\n")
        cat("Detlef Groth at the University of Potsdam\n")
        cat("@ 2022 Detlef Groth, License: MIT\n") 
        cat("\nThis file should be just sourced into your own R code!")
        cat("\nAlternatively you can create the documentation out of this file using:\n\n")
        cat("   Rscript sbi.R --docu\n")
        cat("\nAlternatively you can evaluate all examples using:\n\n")
        cat("   Rscript sbi.R --eval\n")
        cat("\n\n")
    }
    stripCode <- function (infile) {
        cat(" running", sbi$FILENAME,"\n")
        
        fin  = file(sbi$FILENAME, "r")
        
        outfile=gsub("\\.R",".Rmd",sbi$FILENAME)
        fout = file(outfile,'w')
        
        while(length((line = readLines(fin,n=1)))>0) {
            if (grepl("^\\s*#'",line)) {
                line=gsub("^\\s*#' ?","",line)       
                cat(line,"\n",file=fout)
            }
        }
        close(fin)
        close(fout)
        return(outfile)
    }
    argv=commandArgs(trailingOnly=FALSE)
    idx=grep("--file",argv)
    sbi$FILENAME=gsub("--file=","",argv[idx])
    argv=commandArgs(trailingOnly=TRUE)
    if (length(argv) > 0) {
        if (grepl("--docu",argv)) {
            stripCode()
            system("pandoc -s -f markdown -c mini.css -o sbi.html sbi.Rmd ")
        } else if (grepl("--eval",argv)) {
            t1=Sys.time()
            library(rmarkdown)
            outfile=stripCode()
            #knitr::knit(outfile)
            render(outfile,html_document(css="mini.css",theme=NULL))
            cat("Processing done in",round(as.numeric(Sys.time()-t1,units="secs"),2),"seconds!\n")
       }
    } else {
        usage()
    }
}
