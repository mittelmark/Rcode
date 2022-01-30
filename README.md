# Rcode

This is a repository with single file R-scripts and some R tutorials.

**Table of Contents**

* Single file R-scripts
    * [bkcd.R](#bkcd)  - xkcd like plots using base graphicsfunctions and operators
    * [sbi.R](#sbi)  - R functions for the course Statistical Bioinformatics

* Tutorials
    * [venn.html](#venn) - programming a simple Venn diagra,
    
## Single file R-scripts
  
Installation of these scripts should be usually done by downloading the R-file
and placing it on a easy to remember place on your filesyste. I prefere to
place such scripts at *~/R/dlib*. That way I can write anywhere in myscripts
something like `source("~/R/dlib/filename.R")` to load the script.


### bkcd.R
<a name="bkcd"> </a>

`bkcd.R`  - xkcd like plots using base graphics functions and operators (requires tikzDevice package and xelatex and ghostscript)

* Download: [bkcd.R](https://raw.githubusercontent.com/mittelmark/Rcode/main/bkcd/bkcd.R)
* Documentation: [bkcd.html](https://htmlpreview.github.io/?https://github.com/mittelmark/Rcode/blob/master/bkcd/bkcd.html)

Here an example, the internal workflow to create such plots written with script code from the R file itself:

![](bkcd/tikz-flowchart.png)

This code is based on example code given on the tex-stackexchange see [here](https://tex.stackexchange.com/questions/74878/create-xkcd-style-diagram-in-tex).

### sbi.R
<a name="sbi"> </a>

`sbi.R`  - R functions for the course Statistical Bioinformatics

* Download: [sbi.R](https://raw.githubusercontent.com/mittelmark/Rcode/main/sbi/sbi.R)
* Documentation: [sbi.html](https://htmlpreview.github.io/?https://github.com/mittelmark/Rcode/blob/master/sbi/sbi.html)

Here an example, a PCA pairs plot create with the function `sbi$pca.pairs`.

![](sbi/pca.png)

## R programming tutorials

The following links provide some programming tutorials to help students in
learning to program R and to create their own plots and graphics.

### Venn Diagrams
<a name="venn"> </a>

Venn diagrams are diagrams showing relations between sets. The following link provides a tutorial where 
a Venn diagram is build using just basic R plot commands on an empty plot surface. 

Here the link: [tutorials/venn-01.html](tutorials/venn-01.html)

![](tutorials/venn-01.png)
