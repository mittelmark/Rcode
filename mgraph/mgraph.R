#!/usr/bin/env Rscript
library(rpart)
library(tcltk)
#' ---
#' title: mgraph.R - mini graph module
#' author: Detlef Groth, University of Potsdam
#' date: 2022-05-09
#' ---
#' 
## include "../header.md"
#' 
#' <center>Module Vignette: [mgraph vignette](vignette.html)</center>
#'
#' ## R functions for working with graphs - 2022-05-09
#' 
#' <a name="home"> </a>
#' 
#' This is a collection of useful functions shown in my teachings
#' in the course Statistical Bioinformatics, Practical Bioinformatics,
#' Machine Learning in Bioinformatics and for the course
#' Databases and Practical Programming which deal with graph like
#' data. Below you find the documentation for the following functions 
#' and variables:
#' 
#' Functions:
#'
#'   * [mgraph$new](#new) - create a new graph based on an adjacency matrix or list or some standard graph types
#'   * [mgraph$autonames](#autonames) - create names for nodes and other data structures
#'   * [mgraph$components](#components) - extract graph components
#'   * [mgraph$connect](#connect) - connect graph components
#'   * [mgraph$d2u](#d2u) - create an undirected graph out of a directed graph
#'   * [mgraph$data2graph](#data2graph) - create graph out of a data using different algorithms
#'   * [mgraph$degree](#degree) - number of undirected or incoming and outgoing edges
#'   * [mgraph$graph2data](#graph2data) - correlation data for the given graph
#'   * [mgraph$kroki](#kroki) - visualize diagram code for ditaa, graphviz etc
#'   * [mgraph$layout](#layout) - calculate a two dimensional graph layout
#'   * [mgraph$nodeColors](#nodeColors) - node colors for directed graphs
#'   * [mgraph$plot](#plot) - plot a mgraph object (S3 - plot.mgraph)
#'   * [mgraph$shortest.paths](#shortest.paths) - calculate the shortest path for a given graph
#'   * [mgraph$summary](#summary) - provide basic information about a mgraph object (S3 - summary.mgraph)
#'   * [mgraph$u2d](#u2d) - create an directed graph out of an undirected graph
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
#' To use the functions just source the file mgraph.R into your R application:
#'
#' ```{r eval=FALSE}
#' # notrun
#' source("mgraph.R")
#' ```
#' 
#' ```{r label=source,echo=FALSE,results="hide"}
#' if (file.exists("mgraph.R")) {
#'  source("mgraph.R")
#' } else if (file.exists("../mgraph.R")) {
#'   source("../mgraph.R")
#' }
#' ```
#' 
#' All function of this file are added to an environment variable called _mgraph_. 
#' To have a look which variables and functions exists in this namesspace you can use the function call
#' `ls(mgraph)`. Only function starting with lowercase letters should be used in your scripts:
#' 
#' ```{r label=ls}
#' ls(mgraph,pattern="^[a-z]")
#' ```
#' 

library(MASS)
mgraph=new.env()
mgraph$VERSION = "2022-05-09"
# where is the file mgraph.R
# store it in the filename variable
.calls=sys.calls()
.srx=grep("^source",.calls)
.idx=.srx[length(.srx)]
if (length(.idx)==0) {
    # using Rscript sbi.R
    argv=commandArgs(trailingOnly=FALSE)
    .idx=grep("--file",argv)
    mgraph$FILENAME=gsub("--file=","",argv[.idx])
} else if (grepl("['\"]",.calls[.idx]))  {
    # string given
    mgraph$FILENAME = gsub("source\\(.(.+).\\)","\\1",.calls[.idx])
} else {
    # variable given
    mgraph$FILENAME = eval(parse(text=gsub("source\\((.+)\\)","\\1",.calls[.idx])))
}
rm(.calls,.srx,.idx)
#' ## Functions
#'
#' 
#' <a name="new"> </a>
#' **mgraph$new(x)** 
#' 
#' > Create an new graph object based on either an adjacency matrix or an adjacency list.
#' 
#' > This function ...
#' 
#' > Arguments:
#' 
#' > - _x_ - either a adjacency matrix or an adjacency list, if not given a type must be given
#'   - _type_ - a graph type, one of 'angie', 'band', 'barabasi', 'circle', 'cluster', 'hubs', 'random', 'regular' or 'werner', default: 'random'
#'   - _nodes_ - number of nodes for a given type, default: 10
#'   - _number_ of edges for a given type, not used for type "barabasi", default: 12
#'   - _m_ - number of edges added in each iteration of type is 'barabasi', default: 1
#'   - _k_ - the degree for a regular graph, or the number of clusters for a cluster graph or the number of hubs for a hubs graph, default: 2
#'
#' > Examples:
#' 
#' > ```{r label=new,fig=TRUE,fig.cap="",fig.width=10,fig.height=15}
#'   M <- matrix(0,nrow=7,ncol=7)
#'   rownames(M)=colnames(M)=LETTERS[1:7]
#'   M[c('A','B'),'C']=1
#'   M['C','D']=1
#'   M['D',c('E','F')]=1
#'   M['E','F']=1
#'   G=mgraph$new(M)
#'   set.seed(125)
#'   R = mgraph$new(type="random",nodes=8,edges=9)
#'   summary(R)
#'   A = mgraph$new(type="angie",nodes=8,edges=9)
#'   C = mgraph$new(type="circle",nodes=8)
#'   B = mgraph$new(type="band",nodes=8)
#'   par(mfrow=c(4,2),mai=rep(0.1,4))
#'   plot(G,layout="circle",vertex.color=mgraph$nodeColors(G))
#'   plot(R,layout="circle")
#'   plot(A,layout="circle")
#'   plot(C,layout="circle")
#'   plot(B,layout="circle")
#'   plot(mgraph$new(type="bara",nodes=8,m=1),layout="circle")
#'   CLa=mgraph$new(type="cluster",nodes=16,edges=22,k=2)
#'   CLb=mgraph$connect(CLa)
#'   lay=mgraph$layout(CLb)
#'   plot(CLa,layout=lay);plot(CLb,layout=lay)
#' > ```
#'
mgraph$new <- function (x=NULL,type="random",nodes=10,edges=12,m=1,k=3) {
    types=c("angie","band","barabasi","circle","cluster","hubs","random","regular","werner")
    i=pmatch(type,types)
    if (is.na(i)) {
        stop(paste("Unkown type: Known types are: '",paste(types,collapse="', '"),"'!",sep=""))
    }
    type=types[i]
    if (!is.null(x[1])) {
        if (is.matrix(x) & nrow(x)==ncol(x) & all(rownames(x)== colnames(x))) {
            class(x)="mgraph"
        }
    } else if (is.null(x[1])) {
        if (is.null(type)) {
            stop("Error: Either a matrix or a type must be given for mgraph$new!")
        }
        x=matrix(0,nrow=nodes,ncol=nodes)
        nms=mgraph$autonames(nodes)
        rownames(x)=colnames(x)=nms
        if (type == "random") {
            v=1:length(x[upper.tri(x)])
            idx=sample(v,edges)
            x[upper.tri(x)][idx]=1
        } else if (type %in% c("band","circle")) {
            for (i in 1:((nrow(x)-1))) {
                x[i,i+1]=1
            }
            if (type == "circle") {
                x[nrow(x),1]=1
            }
        } else if (type == "barabasi") {
            x[2, 1]=1
            for (n in 3:ncol(x)) {
                if (m==n) {
                    sel=n-1
                } else {
                    sel=m
                }
                idx=sample(1:(n-1),sel)
                x[idx,n]=1
            }
        } else if (type == "angie") {
            nds=rownames(x)
            done=nds[1]
            nds=nds[-1]
            while (length(nds)>0) {
                tar=sample(done,1)
                x[tar,nds[1]]=1
                done=c(done,nds[1])
                nds=nds[-1]
            }
            while (sum(x) < edges) {
                idx=sample(which(x[upper.tri(x)]==0),1)
                x[upper.tri(x)][idx]=1
            }
        } else if (type == "werner") {
            x=x[1:6,1:6]
            x[c(1,2),3]=1
            x[3,4]=1
            x[4,5:6]=1
            x[5,6]=1
        } else if (type %in% c("cluster","hubs")) {
            size = nodes %/% k
            rem  = nodes %%  k
            esize= edges %/% k
            ree  = edges %%  k
            pos=0
            for (i in 1:k) {
                s=size
                e=esize
                if (rem>0) {
                    s=size+1; rem=rem-1
                }
                if (ree>0) {
                    e=e+1; ree=ree-1
                }
                if (type == "cluster") {
                    g=mgraph$new(type="angie",nodes=s,edges=e)
                } else {
                    g=matrix(c(0,rep(1,s-1),rep(0,s*s-s)),ncol=s,byrow=TRUE)
                }
                x[(pos+1):(pos+nrow(g)),(pos+1):(pos+nrow(g))]=g 
                pos=pos+s

            }
        } else if (type == "regular") {
            if (k %in% c(1,3)) {
                if (nodes %% 2 != 0) {
                    stop("regular graphs with k = 1 or k = 3 must have an even number of nodes")
                }
            }
            if (k == 1) {
                for (i in seq(1,nrow(x)-1,by=2)) {
                    x[i,i+1]=1
                }
            } else if (k <= 3) {
                x=mgraph$new(type="circle",nodes=nodes)
                if (k == 3) {
                    for (i in 1:(nrow(x)/2)) {
                        x[i,i+(nrow(x)/2)]=1
                     }
                 }
            } else {
                stop("Error: Only values of k <= 3 are implemented for regular graphs")
            }
        }
    } else {
        stop("either a matrix or a type must be given")
    }
    class(x)="mgraph"
    return(x)
    
}

#' <a name="autonames"> </a>
#' **mgraph$autonames(prefix,n)** 
#' 
#' > Create names for nodes and other data structures.
#' 
#' > This function aids in creating standard node labels for graphs.
#' 
#' > Arguments:
#' 
#' > - _prefix_ - one or more prefixes to be used.
#'   - _n_ - how many labels
#'
#' > Examples:
#' 
#' > ```{r label=autonames}
#'   mgraph$autonames(12)
#'   mgraph$autonames(12,LETTERS[1:4])
#'   mgraph$autonames(12,"R")
#' > ```
#'
#' > Author: Masiar Novine
#' 

mgraph$autonames <- function (n,prefix=NULL) {
    if (is.null(prefix[1])) {
        if (n<=26) {
            nms=LETTERS[1:n]
        } else if (n <= 26*9) {
            nms=mgraph$autonames(n,prefix=LETTERS)
        } else {
            nms=mgraph$autonames(n,prefix="N")
        }
        return(nms)
    } else {
        nms=prefix
        ln <- ceiling(n / length(nms))
        nms_tmp <- rep(nms, ln)[1:n]
        ptf <- rep(1:ln, each=length(nms))[1:n]
        frmt <- formatC(ptf, flag="0", width=log10(ln) + 1)
        return(paste0(nms_tmp, frmt))
    }
}

#' <a name="components"> </a>
#' **mgraph$components(g)** 
#' 
#' > Return graph components, nodes which are connected are within the same component.
#' 
#' > This function ...
#' 
#' > Arguments:
#' 
#' > - _g_ - a mgraph object or an adjacency matrix or an adjacency list
#'
#' > Examples:
#' 
#' > ```{r label=components,fig=TRUE,fig.cap=""}
#'   mgraph$components(G)
#' > ```
#'

mgraph$components <- function (g) {
    A=g
    A=as.matrix(A)
    A=A+t(A)
    A[A>0]=1
    comp=c()
    P=mgraph$shortest.paths(A)
    nodes=rownames(A)
    x=1
    while (length(nodes) > 0) {
        n=nodes[1]
        idx=which(P[n,] < Inf)
        ncomp=rep(x,length(idx))
        names(ncomp)=rownames(P)[idx]
        comp=c(comp,ncomp)
        nodes=setdiff(nodes,rownames(P)[idx])
        x=x+1
    }
    return(comp[rownames(A)])
}

#' <a name="connect"> </a>
#' **mgraph$connect(g)** 
#' 
#' > Return a graph which has only a single component.
#' 
#' > This function creates a single component graph by adding random edges between different components.
#' 
#' > Arguments:
#' 
#' > - _g_ - a mgraph object or an adjacency matrix or an adjacency list
#'
#' > Examples:
#' 
#' > ```{r label=connect,fig=TRUE,fig.cap=""}
#'   H=mgraph$connect(G)
#'   H
#'   mgraph$components(H)
#' > ```
#'

mgraph$connect = function (g) {
    A=g
    A=as.matrix(A)
    A=A+t(A)
    A[A>0]=1
    P=mgraph$shortest.paths(A)
    if (!any(P==Inf)) {
        return(A)
    }
    comp=mgraph$components(A)
    nodes=c()
    tab=table(comp)
    for (n in names(tab)) {
        c=names(which(comp==n))
        if (tab[[n]] > 2) {
            Am=A[c,c]
            # todo min
            deg=apply(Am,1,sum)
            idx=which(deg>0)
            minval=min(deg[idx])
            idx=which(deg == minval)[1]
            node=c[idx]
        } else {
            node = c[1]
        }
        nodes=c(nodes,node)
    }
    A[nodes,nodes]=1
    diag(A)=0
    return(A)
}

#' <a name="d2u"> </a>
#' **mgraph$d2u(g)** 
#' 
#' > Create an undirected graph out of a directed graph with the same number of edges.
#' 
#' > This function gets an directed graph and convertes all edges from directed ones to undirected ones.
#'   The number of edges should stay the same, the edge sign (+ or -) stays the same.
#' 
#' > Arguments:
#' 
#' > - _g_ - a mgraph object or an adjacency matrix
#'
#' > Examples:
#' 
#' > ```{r label=d2u,fig=TRUE,fig.cap="",fig.width=12,fig.height=6}
#'   par(mfrow=c(1,2),mai=rep(0.2,4))
#'   A=mgraph$new(type="angie",nodes=4,edges=4)
#'   A
#'   U=mgraph$d2u(A)
#'   U
#'   plot(A); plot(U)
#' > ```
#'

mgraph$d2u <- function (g) {
    g[lower.tri(g)]=g[lower.tri(g)]+t(g)[lower.tri(g)]
    g[upper.tri(g)]=g[upper.tri(g)]+t(g)[upper.tri(g)]    
    g[g>0]=1
    g[g<0]=-1
    return(g)
}

#' <a name="data2graph"> </a>
#' **mgraph$data2graph(x,type="rpart")** 
#' 
#' > Create  graph out of a data using different algorithms.
#' 
#' > This function gets some input data and creates an un- or a directed graph
#'   based on the structure of the input data.
#' 
#' > Arguments:
#' 
#' > - _x_ - data frame or matrix with variables in the columns.
#'   - _type_ - which algorithm to use, currently only 'rpart' is possible, default: rpart
#'   - _rs_ - threshold for r-square values, default: 0.01
#'   - _k_ - how many initial candidate variables to consider, default: 10
#'
#' > Examples:
#' 
#' > ```{r label=data2graph,fig=TRUE,fig.cap="",fig.width=9,fig.height=12}
#'   par(mfrow=c(3,2),mai=c(0.2,0.2,0.8,0.1))
#'   A=mgraph$new(type="angie",nodes=8,edges=8)
#'   data=mgraph$graph2data(A,prop=0.02,iter=50)
#'   round(cor(t(data),method="spearman"),2)
#'   B=mgraph$data2graph(t(data),type="rpart",rs=0.05)
#'   C=mgraph$data2graph(t(data),type="pearson",rs=0.05)
#'   D=mgraph$data2graph(t(data),type="kendall",rs=0.1)
#'   E=mgraph$data2graph(t(data),type="lm",rs=0.05)
#'   F=mgraph$data2graph(t(data),type="lm",rs=0.01)
#'   round(B$rs,2)
#'   B$graph
#'   lay=mgraph$layout(A)
#'   plot(A,layout=lay,main="original",vertex.color=mgraph$nodeColors(A)); 
#'   plot(B$graph,layout=lay,main="rpart - rs 0.05")
#'   plot(C$graph,layout=lay,main="pearson - rs 0.05")
#'   plot(D$graph,layout=lay,main="kendall - rs 0.10")
#'   plot(E$graph,layout=lay,main="lm - rs 0.05")
#'   plot(F$graph,layout=lay,main="lm - rs 0.01")
#' > ```
#'

mgraph$data2graph <- function (x,type="rpart",rs=0.01,k=10) {
    types=c("kendall","pearson","linear","lm","rpart","spearman")
    type=types[pmatch(type,types)]
    if (type == "linear") {
        type="lm"
    }
    A=matrix(0,nrow=ncol(x),ncol=ncol(x))
    rownames(A)=colnames(A)=colnames(x)
    RS=A
    if (k > ncol(x)) {
        k=ncol(x)
    }
    if (!is.data.frame(x)) {
        if (is.matrix(x)) {
           x <- as.data.frame(x)
        } else {
            stop("Error: Input d must be a data frame or matrix!")
        }
    }
    if (type == "lm") {
        chosen=list()
        cor_d <- cor(x, method="pearson", use="pairwise.complete.obs")
        diag(cor_d)=0
        for (node in colnames(x)) {
            chosen[node] <- c()
            # MN Use absolute correlation values
            cand <- setdiff(names(sort(rank(abs(cor_d[node, ])), decreasing=TRUE))[1:k],node)
            # MN Can be omitted, because of zero diagonal
            # cand <- cand[-which(cand == node)]
            model <- lm(formula(paste(c(node, "~", "0"), collapse="")), data=x[,c(node,cand)])
            aic <- AIC(model)
            # MN Changed, concentrate in one line
            rsq <- summary.lm(model)$adj.r.squared
            for (target in cand) {
                model_tmp <- lm(formula(paste(c(node, "~", paste(c(chosen[[node]], target), collapse="+")), collapse="")), data=x[,c(node,cand)])
                aic_tmp <- AIC(model_tmp)
                # MN Changed, concentrate in one line
                rsq_tmp <- summary.lm(model_tmp)$adj.r.squared
                if ((aic_tmp < aic && rsq_tmp - rsq >= rs)) {
                    chosen[[node]] <- append(chosen[[node]], target)
                    aic <- aic_tmp
                    RS[node, target] <- rsq_tmp - rsq
                    A[node, target] <- 1
                    rsq <- rsq_tmp
                }
            }
        }
        A=as.matrix(mgraph$d2u(A))
    } else if (type == "rpart") {
        rs.threshold=rs
        # TODO: Mutual information as alternative
        # filtering
        C=cor(x,method="spearman",use="pairwise.complete.obs")
        for (i in 1:ncol(x)) {
            idx=order(abs(C[i,]),decreasing=TRUE)[2:k]
            #idx=which(C[i,]^2>0.01)
            rp=rpart::rpart(formula(paste(colnames(x)[i], "~",
                                          paste( colnames(x)[idx],collapse="+"))),
                                          data=x[,c(i,idx)])
            # check all important variables
            vimps=names(rp$variable.importance)
            rp1=rpart::rpart(formula(paste(colnames(x)[i], "~",vimps[1])),data=x)
            rs=cor(predict(rp1,newdata=x),x[,i],use="pairwise.complete.obs",method="spearman")^2
            if (rs < rs.threshold) {
                break
            }
            A[i,vimps[1]]=rs
            
            rss=c(rs)
            for (vi in 2:length(vimps)) {
                rpi=rpart::rpart(formula(paste(colnames(x)[i], "~",
                                               paste( vimps[1:vi],collapse="+"))),
                                               data=x)
                rs=cor(predict(rpi,newdata=x),x[,i],use="pairwise.complete.obs",method="spearman")^2
                if (rs < sum(rss)+rs.threshold) {
                    break
                }
                A[i,vimps[vi]]=rs-sum(rss)
                rss=c(rss,rs-sum(rss))
            }
        }
        RS=A
        print(round(RS,2))
        A[A>=rs.threshold]=1
        A[A<rs.threshold]=0
        A=as.matrix(mgraph$d2u(A))
        A[abs(C)<0.1]=0
        #A[A==1 & C < 0]=-1
    } else if (type %in% c("pearson","spearman","kendall")) {
        A=cor(x,method=type,use="pairwise.complete.obs")
        diag(A)=0
        if (type %in% c("pearson","spearman")) {
            A=A^2
        } else {
            A=abs(A)
        }
        RS=A
        A[A<rs]=0
        A[A>=rs]=1
    } else {
        stop("Currently only rpart, pearson, spearman and kendall as types are supported!")
    }
    g=mgraph$new(A)
    return(list(graph=g,rs=RS))
}

#' <a name="degree"> </a>
#' **mgraph$degree(g,mode="undirected")** 
#' 
#' > Return the number of undirected or incoming and outgoing edges.
#' 
#' > This function returns the ...
#' 
#' > Arguments:
#' 
#' > - _g_ - a mgraph object or an adjacency matrix
#'
#' > Examples:
#' 
#' > ```{r label=degree,fig=TRUE,fig.cap="",fig.width=12,fig.height=6}
#'   par(mfrow=c(1,2),mai=rep(0.2,4))
#'   A=mgraph$new(type="regular",nodes=8,k=3)
#'   plot(A,layout="circle"); plot(mgraph$d2u(A),layout="circle"); 
#'   mgraph$degree(A)
#'   mgraph$degree(A,mode="out")
#'   mgraph$degree(A,mode="in")
#' > ```
#'
mgraph$degree <- function (g,mode="undirected") {
    if (mode == "undirected") {
        g=mgraph$d2u(g)
    } else if (mode == "in") {
        g=t(g)
    } 
    g[abs(g)!=0]=1
    d=apply(g,1,sum)
    names(d)=rownames(g)
    return(d)
}

#' <a name="graph2data"> </a>
#' **mgraph$graph2data(g,n=100)** 
#' 
#' > Create correlation data for the given graph.
#' 
#' > This function is a short implementation of the algorithm in Novine et. al. 2022.
#' 
#' > Arguments:
#' 
#' > - _g_ - a mgraph object or an adjacency matrix
#'   - _n_ - the number of measurements per node, default: 100
#'   - _iter_ - the number of iterations, default: 15
#'   - _sd_ - initial standard deviation, default: 2
#'   - _val_ - initial node value, default: 100
#'   - _prop_ - proportion of the target node value take from the source node, default: 0.05
#'   - _noise_ - the sd for the noise value added after each iteration using rnorm function with mean 0, default: 1
#'
#' > Examples:
#' 
#' > ```{r label=graph2data,fig=TRUE,fig.cap="",fig.width=12,fig.height=6}
#'   par(mfrow=c(1,2),mai=rep(0.2,4))
#'   A=mgraph$new(type="angie",nodes=8,edges=10)
#'   plot(A,layout="circle"); 
#'   C=mgraph$graph2data(A,prop=0.05)
#'   round(cor(t(C)),2)
#'   sbi$corrplot(cor(t(C)))
#' > ```
#'

mgraph$graph2data <- function (A,n=100,iter=15,val=100,sd=2,prop=0.05,noise=1) {
    res=matrix(0,ncol=n,nrow=nrow(A))
    rownames(res)=rownames(A)
    for (i in 1:n) {
        units=rnorm(nrow(A),mean=val,sd=sd)
        names(units)=rownames(A)
        for (j in 1:iter) {
            for (node in sample(rownames(A))) {
                targets=colnames(A)[which(A[node,]!=0)]
                for (target in sample(targets)) {
                    P=abs(A[node,target])
                    nval=units[[node]]*(prop*P)
                    nval=nval+units[[target]]*(1-(prop*P))
                    if (A[node,target]<0) {
                        diff=nval-units[[target]]
                        nval=units[[target]]-diff
                    }
                    units[[target]]=nval;
                }
            }
            units=units+rnorm(length(units),sd=noise)
        }
        res[,i]=units
    }
    return(res)
}

#' <a name="kroki"> </a>
#' **mgraph$kroki(text,type="ditaa",ext="png")** 
#' 
#' > Attention, often does not work! Create diagram URL's based on kroki supported tools.
#' 
#' > This function is creates a URL which can be easily embedded into Markdown code for displaying
#'   diagrams supported by the online tool [kroki.io](https://kroki.io).
#'   There is as well an online diagram editor, see here [niolesk](https://niolesk.top/).
#' 
#' > Arguments:
#' 
#' > - _text_ - some diagram code, default: "A --> B"
#'   - _filename_ - some input file, either _text_ or _file_ must be given, default: NULL
#'   - _mode_ - diagram type, supported is ditaa, graphviz, and many others, see the kroki website, default: ditaa
#'   - _ext_ - file extension, usally 'png', 'svg' or 'pdf', default: 'png'
#'
#' > Examples:
#' 
#' > ```{r label=kroki}
#'   url1=mgraph$kroki("
#'   digraph g { 
#'      rankdir=\"LR\";
#'      node[style=filled,fillcolor=salmon];
#'      A -> B -> C ; 
#'   }",
#'   type="graphviz")
#'   substr(url1,1,40)
#'   url2=mgraph$kroki("
#'   +---------+    +--------+
#'   |    AcEEF+--->+   BcFEE+
#'   +---------+    +--------+
#'   ")
#' > ```
#' 
#' > To embed the image you can use Markdown code like here:
#' 
#' > ```
#'   # remove space before r letter
#'   ![ ](` r url1 `)
#' > ```
#'
#' > Here the output:
#' 
#' > ![ ](`r url1`)
#' 
#' > And here the image for the second diagram, a Ditaa diagram:
#' 
#' > ![ ](`r url2`)
#' 
#' > The diagram code can be read as well from a file here a Ditaa file:
#' 
#' > ```{r}
#'   cat(sbi$file.head("hw.ditaa"),sep="\n")
#'   url3=mgraph$kroki(filename="hw.ditaa")
#'   nchar(url3)
#' > ```
#' 
#' > ![](`r url3`)
#' 

mgraph$kroki <- function (text="A --> B",filename=NULL,type="ditaa",ext="png") {
    .Tcl("
    proc dia2kroki {text} {
        return [string map {+ - / _ = \"\"}  [binary encode base64 [zlib compress $text]]]
    }
    ")
    if (!is.null(filename)) {
        if (!file.exists(filename)) {
            stop(paste("Error: File",filename,"does not exists!"))
        } else {
            fin=file(filename,'r')
            text=readLines(fin,n=-1L)
            close(fin)
            text=paste(text,collapse="\n")
        }
    }
    url = tclvalue(tcl("dia2kroki",text))
    url= paste("https://kroki.io",type,ext,url,sep="/")
    # memCompress and openssl::base64_encode in R did not work always
    return(url)
}
#' <a name="layout"> </a>
#' **mgraph$layout(g,mode="sam")** 
#' 
#' > Return a layout for a given graph, ready for plotting.
#' 
#' > This function ...
#' 
#' > Arguments:
#' 
#' > - _g_ - a mgraph object or an adjacency matrix or an adjacency list
#'   - _mode_ - either 'mds','sam', 'circle', 'star' ...
#'
#' > Examples:
#' 
#' > ```{r label=layout,fig=TRUE,fig.cap=""}
#'   lay=mgraph$layout(G)
#'   plot(lay, pch=19,col="salmon",cex=5,xlab="",ylab="",axes=FALSE)
#'   text(lay,rownames(G))
#' > ```
#'

mgraph$layout = function (g,mode='sam', noise=FALSE, star.center=NULL) {
    A=g
    if (mode %in% c('mds','sam')) {
        A=mgraph$connect(A)
        sp=mgraph$shortest.paths(A)
        xy=cmdscale(sp)
        rownames(xy)=rownames(A)
        if (mode=='mds') {
            dxy=base::as.matrix(dist(xy))
            diag(dxy)=1
            idx=which(dxy<0.05,arr.ind=TRUE)
            if (nrow(idx)>1) {
                for (i in 1:nrow(idx)) {
                    n=idx[i,1]
                    xy[n,1]=xy[n,1]+rnorm(1,mean=0,sd=0.1)
                    xy[n,2]=xy[n,2]+rnorm(1,mean=0,sd=0.1)
                }
            }
        } else {
            xy=xy+jitter(xy)
            xy=MASS::sammon(sp,y=xy,trace=FALSE)$points
        }
    } else if (mode %in% c('circle','star')) {
        x=0
        y=0
        a=0.5
        b=0.5
        rad2deg <- function(rad) {(rad * 180) / (pi)}
        deg2rad <- function(deg) {(deg * pi) / (180)}
        xy=matrix(0,ncol=2,nrow=length(rownames(A)))
        if (mode == "circle") {
            nodes=rownames(A)
            rownames(xy)=nodes
            for (i in 1:length(nodes)) {
                t=deg2rad((360/length(nodes))*(i-1))
                xp = a*cos(t)*0.75 + x;
                yp = b*sin(t)*0.75 + y;
                xy[nodes[i],]=c(xp,yp)
            }
        } else if (mode == 'star') {
            oorder=rownames(A)
            if (class(star.center)[1] != "NULL") {
                norder=c(which(rownames(A)==star.center),which(rownames(A)!=star.center))
                A=A[norder,norder]
            }
            nodes=rownames(A)
            rownames(xy)=nodes
            xy[1,]=c(0.0,0.0)
            for (i in 2:length(nodes)) {
                t=deg2rad((360/(length(nodes)-1))*(i-2))
                xp = a*cos(t)*0.75 + x;
                yp = b*sin(t)*0.75 + y;
                xy[nodes[i],]=c(xp,yp)
            }
            xy=xy[oorder,]
        }
    } else if (mode == 'grid') {
        n=nrow(A)
        xy=matrix(0,ncol=2,nrow=nrow(A))
        rownames(xy)=rownames(A)
        mody=ceiling(sqrt(n))
        x=0
        y=0
        for (r in rownames(A)) {
            if (x %% mody == 0) {
                y=y+1
                x=0
            }
            x=x+1
            xy[r,]=c(x,y)
        }
    } else {
        stop("unknown layout. Use mds, sam, circle, or grid as layout")
    }
    xy=scale(xy)
    if (noise) {
        xy=xy+rnorm(length(xy),mean=0,sd=0.1)
    }
    colnames(xy)=c("x","y")
    return(xy)
}

#' <a name="nodeColors"> </a>
#' **mgraph$nodeColors(g,colors=c("skyblue","grey80","salmon")** 
#' 
#' > Returns node colors for directed graphs.
#' 
#' > This function simplifies automatic color coding of nodes for directed graphs.
#'   Nodes will be colored based on their degree properties, based
#'   on their incoming and outcoming edges.
#' 
#' > Arguments:
#' 
#' > - _g_ - a mgraph object or an adjacency matrix or an adjacency list
#'   - _col_ - default colors for nodes with only incoming, in- and outgoing and only outgoing edges, default: c("skyblue","grey80","salmon")
#'
#' > Examples:
#' 
#' > ```{r label=nodeColors,fig=TRUE,fig.cap="",fig.width=9,fig.height=4.5}
#'   par(mfrow=c(1,2),mai=rep(0.1,4))
#'   A=mgraph$new(type="random",nodes=6,edges=8)
#'   cols=mgraph$nodeColors(A)
#'   mgraph$degree(A,mode="in")
#'   mgraph$degree(A,mode="out")
#'   cols
#'   plot(A, layout="star")
#'   plot(A, layout="star",vertex.color=cols) 
#' > ```
#'

mgraph$nodeColors <- function (g,col=c("skyblue","grey80","salmon")) {
    colors = rep(col[2],nrow(g))
    out    = mgraph$degree(g,mode="out")
    inc     = mgraph$degree(g,mode="in")    
    colors[out >  0 & inc == 0] = col[3]
    colors[out == 0 & inc >  0] = col[1]    
    return(colors)
}
#' <a name="plot"> </a>
#' **mgraph$plot(g,layout="sam")** 
#' 
#' > Plot a given graph or adjacency matrix.
#' 
#' > This function ... is as well an S3 function for mgraph objects
#' 
#' > Arguments:
#' 
#' > - _g_ - a mgraph object or an adjacency matrix or an adjacency list
#'   - _layout_ - either 'mds','sam', 'circle', 'star' or an two column matrix or data frame with x and y coordinates,default: 'sam'
#'   - _vertex.size_ - the size of the vertex circles, default: 1
#'   - _vertex.color_ - the color of the vertices, default: 'salmon'
#'   - _pch_ - the plotting character for the vertices, default: 19
#'
#' > Examples:
#' 
#' > ```{r label=plot,fig=TRUE,fig.cap="",fig.width=12,fig.height=12}
#'   par(mfrow=c(2,2),mai=rep(0.2,4))
#'   mgraph$plot(G, layout="sam")
#'   plot(G, layout="circle") # plot as S3 function
#'   plot(R, layout="sam",vertex.color="grey90") 
#'   M=matrix(0,ncol=5,nrow=5)
#'   diag(M)=0
#'   M[upper.tri(M)]= as.integer(rlnorm(10,0.35))
#'   M[lower.tri(M)]=t(M)[lower.tri(M)]
#'   rownames(M)=colnames(M)=LETTERS[1:5]
#'   M=mgraph$new(M)
#'   M
#'   identical(M,t(M))
#'   plot(M, layout="circle",vertex.color="grey90",weighted=TRUE,lwd=1)
#' > ```
#'

mgraph$plot = function (g,layout='sam',vertex.size=1,lwd=3,weighted=FALSE,
                        vertex.color="grey80",vertex.cex=1,pch=19,...) {
    A=g
    if (is.matrix(layout) | is.data.frame(layout)) {
        if (ncol(layout) != 2) {
            stop("If a layout matrix or data frame is given two columns are required!")
        }
    } else if (layout %in% c("sam","mds","circle","star")) {
        layout=mgraph$layout(A,mode=layout)
    } else {
        stop("Wrong layout. Either a two column matrix or one of 'sam','mds','circle' or 'star' must be given!")
    }
    if (length(vertex.color) == 1) {
        vertex.color=rep(vertex.color,nrow(g))
    }
    arrow <- function (x,y,cut=0.6,lwd=2,arrow.col="#666666",...) {
        hx <- (1 - cut) * x[1] + cut * x[2]
        hy <- (1 - cut) * y[1] + cut * y[2]
        arrows(hx,hy,x[2],y[2],lwd=lwd,code=0,col=arrow.col,...)
        for (a in c(20,15,10,5)) {
            arrows(x[1],y[1],hx,hy,length=0.06*lwd,angle=a,lwd=lwd,col=arrow.col,...)
        }
    }
    xr=diff(range(layout[,1])/10)
    
    yr=diff(range(layout[,2])/10)
    xlim=c(min(layout[,1])-xr,max(layout[,1])+xr)
    ylim=c(min(layout[,2])-yr,max(layout[,2])+yr)
    g="directed"
    if (identical(A,t(A))) {
        g="undirected"
    }
    plot(layout,xlim=xlim,ylim=ylim,axes=FALSE,xlab="",ylab="",...)
    for (i in 1:nrow(A)) {
        idx=which(A[i,]!= 0)
        for (j in idx) {
            wlwd=lwd
            if (weighted) {
                wlwd=lwd+abs(A[i,j])
            }
            if (g=="undirected") {
                lines(layout[c(i,j),1],layout[c(i,j),2],lwd=wlwd,col="#666666")
            } else {
                arrow(layout[c(i,j),1],layout[c(i,j),2],lwd=wlwd,arrow.col="#666666")
            }
        }
    }
    points(layout,pch=pch,cex=6*vertex.size+0.4,col="black")
    points(layout,pch=pch,cex=6*vertex.size,col=vertex.color)
    text(layout,rownames(A),cex=vertex.cex)
}

# S3 function
plot.mgraph = mgraph$plot

#' <a name="shortest.path"> </a>
#' **mgraph$shortest.path(g)** 
#' 
#' > Calculate the shortest paths between all graph nodes.
#' 
#' > This function ...
#' 
#' > Arguments:
#' 
#' > - _g_ - a mgraph object created with _mgraph$new_.
#'  
#' > Returns: a matrix with the shortest paths
#' 
#' > Examples:
#' 
#' > ```{r label=spath}
#'   G
#'   mgraph$shortest.paths(G)
#'   mgraph$shortest.paths(G,mode="undirected")
#' > ```
#'
mgraph$shortest.paths <- function (g,mode="directed") {
    A=g
    if (mode == "undirected") {
        A=A+t(A)
        A[A!=0]=1
    }
    S=A
    S[]=Inf
    diag(S)=0
    x=1
    S[A > 0 & A < Inf]=1
    while (TRUE) { 
        flag = FALSE 
        for (m in 1:nrow(S)) {
            ns=which(S[m,] == x)
            for (n in ns) {
                for (o in which(A[n,]==1)) {
                    if (o != m) {
                        flag = TRUE
                        if (S[m,o] > x + 1) {
                            S[m,o]=x+1
                            if (mode == "undirected") {
                                S[o,m]=x+1
                            }
                        }
                    }
                }
            }
        }
        if (!flag) {
            break
        }
        x=x+1
    }
    return(S)
}

#' <a name="summary"> </a>
#' **mgraph$summary(g)** 
#' 
#' > Returns basic graph information.
#' 
#' > This function returns basic graph informations about the number of nodes and edges,
#'   if it is a directed graph or not and if there are existing several
#'   unconnected components. This function is as well S3 function `summary.mgraph`.
#' 
#' > Arguments:
#' 
#' > - _g_ - a mgraph object created with _mgraph$new_.
#'  
#' > Returns: a list object with components:
#' 
#' > - _nodes_ - number of nodes
#'   - _edges_ - number of edges
#'   - _directed_ - boolean if graph is directed
#'   - _components_ - the number of graph components, unconnected subgraphs
#' 
#' > Examples:
#' 
#' > ```{r label=summary}
#'   mgraph$summary(G)
#'   summary(H) # as S3 method
#'   names(summary(H))
#' > ```
#'
mgraph$summary <- function (g) {
    A=g
    directed = !identical(A,t(A))
    X=A
    X[X!=0]=1
    if (directed) {
        edges=sum(A)
    } else {
        edges=sum(A)/2
    }
    comp=mgraph$components(A)
    return(unlist(list(nodes=nrow(A),
                edges=edges,
                directed=directed,
                components=max(comp))))
}
# S3 definition
summary.mgraph=mgraph$summary

#' <a name="u2d"> </a>
#' **mgraph$u2d(g)** 
#' 
#' > Create an directed graph out of an undirected graph.
#' 
#' > This function creates a direcred graph based on an undirected graph.
#' 
#' > Arguments:
#' 
#' > - _g_ - a mgraph object created with _mgraph$new_.
#'   - _shuffle_ - should the edge directions been shuffled, if FALSE every time the same graph will be created, default: FALSE
#'  
#' > Returns: a mgraph object
#' 
#' > Examples:
#' 
#' > ```{r label=u2d,fig=TRUE,fig.width=10,fig.height=10,fig.cap=""}
#'   par(mfrow=c(2,2),mai=rep(0.1,4))
#'   G=mgraph$new(type="angie",nodes=7,edges=9)
#'   U=mgraph$d2u(G)
#'   H=mgraph$u2d(U)
#'   identical(G,H)
#'   I=mgraph$u2d(U,shuffle=TRUE)
#'   identical(G,I)
#'   lay=mgraph$layout(G)
#'   plot(G,layout=lay,vertex.color=mgraph$nodeColors(G))
#'   plot(U,layout=lay)
#'   plot(H,layout=lay,vertex.color=mgraph$nodeColors(H))
#'   plot(I,layout=lay,vertex.color=mgraph$nodeColors(I))
#' > ```
#'
mgraph$u2d <- function (g,shuffle=FALSE) {
    if (shuffle) {
        h=g
        idx=sample(rownames(h))
        h=h[idx,idx]
        h[lower.tri(h)]=0
        h=h[rownames(g),rownames(g)]
        class(h)="mgraph"
        return(h)          
    } else {
        g[lower.tri(g)]=0
        class(g)="mgraph"
        return(g)
    }
}
#' 
#' ## Citation
#' 
#' How to cite this package:
#' 
#' ```
#' # notrun
#' @Misc{Groth2022mgraph,
#'   author =   {Detlef Groth},
#'   title =    {mgraph.R - the mini graph module.},
#'   howpublished = {\url{https://github.com/mittelmark/Rcode}},
#'   year = {2022}
#' }
#' ```
#' 
#' Groth, D. (2022). mgraph.R - the mini graph module. [https://github.com/mittelmark/Rcode](https://github.com/mittelmark/Rcode)
#'
#' ## Documentation
#' 
#' This documentation was created directly from the file *mgraph.R* using the following commands in the terminal using the Rmarkdown package:
#' 
#' ```
#' # notrun
#' Rscript mgraph.R --eval
#' ```
#' 
#' Alternatively you can just extract the plain documentation without output of the code chunks like this:
#' 
#' ```
#' # not run
#' Rscript mgraph.R --docu
#' ```
#' 
#' ## Disclaimer
#' 
#' This file contains a few important R functions to be used for illustration and to more easily learn from the code. 
#' The functions have much less error checking than function you can find
#' in R packages available on CRAN. If you do serious statistical analysis with graphs I recommend to use those packages. Here is a list of packages I recommend
#' very much:
#' 
#' - [igraph](https://cran.r-project.org/web/packages/igraph/)
#' - [network](https://cran.r-project.org/web/packages/network/)
#' - [diagram](https://cran.r-project.org/web/packages/diagram)
#' - [huge](https://cran.r-project.org/web/packages/huge/)
#' 
#' ## ChangeLog
#' 
#' - 2022-05-04 - Version 0.1
#'
#' ## Copyright
#' 
#' Copyright @ 2022 - Detlef Groth, University of Potsdam
#' 
#' License: MIT - License
#' 
## Some other variables
if (sys.nframe() == 0L && !interactive()) {
    usage <-  function () {
        cat("mgraph.R - minimal graph module - functions to work with graph data.\n\n")
        cat("Detlef Groth at the University of Potsdam\n")
        cat("@ 2022 Detlef Groth, License: MIT\n") 
        cat("\nThis file should be just sourced into your own R code!")
        cat("\nAlternatively you can create the documentation out of this file using:\n\n")
        cat("   Rscript mgraph.R --docu\n")
        cat("\nAlternatively you can evaluate all examples using:\n\n")
        cat("   Rscript mgraph.R --eval\n")
        cat("Furthermore you can process the R-vignette file for the mgraph module:\n\n")
        cat("   Rscript mgraph.R vignette.Rmd")
        cat("\n\n")
    }
    argv=commandArgs(trailingOnly=TRUE)
    if (length(argv) > 0) {
        argv=commandArgs(trailingOnly=FALSE)
        idx=grep("--file",argv)
        FILENAME=gsub("--file=","",argv[idx])
        sbifile=file.path(dirname(FILENAME),"..", "sbi","sbi.R")
        if (!file.exists(sbifile)) {
            cat("Error: To process the documentation you must place the\n")
            cat(sprintf("       file sbi.R in the directory '%s' as the file mgraph.R.\n",sbifile))
            cat("       You can get the file sbi.R here:\n")
            cat("       https://raw.githubusercontent.com/mittelmark/Rcode/main/sbi/sbi.R\n")
            q()
        } else {
            source(sbifile) 
        }
        argv=commandArgs(trailingOnly=TRUE)
        if (grepl("--docu",argv)) {
            sbi$mkdoc(FILENAME,"../sbi/mini.css",eval=FALSE)
        } else if (grepl("--eval",argv)) {
            sbi$mkdoc(FILENAME,"../sbi/mini.css",eval=TRUE)
        } else if (grepl(".Rmd",argv)) {
            idx=grep(".Rmd",argv)
            library(rmarkdown)
            render(argv[idx],html_document(css="../sbi/mini.css"))
        }
    } else {
        usage()
    }
}
