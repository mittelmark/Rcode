#!/usr/bin/env Rscript
#' ---
#' title: Plot chord diagrams for string instruments using R
#' author: Detlef Groth, Schwielowsee, Germany
#' date: 2022-02-12
#' ---
#' 
#' ## NAME
#' <a name="home"> </a>
#' 
#' _chords.R_ - R environment and standalone application to create chord diagrams for string instruments.
#' 
#' ## TABLE OF CONTENTS
#' 
#' * [SYNOPSIS](#synopsis)
#' * [DESCRIPTION](#description)
#' * [USAGE](#usage)
#' * [FUNCTIONS](#functions)
#'     * [chords$chord](#chord) - generate chord diagrams for six-string instruments like the guitar
#'     * [chords$halfsteps](#halfsteps) - calculate the distance between two notes
#'     * [chords$notes](#notes) - display fretboard note positions for string instruments like Guitar and Ukulele
#'     * [chords$picking](#picking) - generate fingerpicking diagrams  for string instruments like  Guitar and Ukulele
#' * [EXAMPLES](#examples)
#' * [CITATION](#citation)
#' * [DOCUMENTATION](#documentation)
#' * [LINKS](#links)
#' * [TODOS](#todos)
#' * [CHANGELOG](#changelog)
#' * [LICENSE / AUTHOR](#license)
#' 
#' ## SYNOPSIS
#' <a name="synopsis"> </a> 
#' 
#' Sourcing:
#' 
#' ``` 
#' source("~/R/dlib/chords.R")
#' chord(chord="C",positions="X32010",fingerings="0RM0I0")
#' chord(chord="Dm",positions="XX0231",fingerings="000MRI")
#' ```
#' 
#' Standalone application:
#' 
#' ```
#' Rscript ~/R/dlib/chords.R --chords C:X32010:0RM0I0 Dm:XX0231:000MRI out.pdf
#' Rscript chords.R --chords --layout 2,3 \
#'          C:0003:000R Dm:2210:MRI0 Em:0432:0RMI \
#'          F:2010:M0I0 G7:0212:0RIM Am:2000:M000  GCEA-C-Dur.svg
#' Rscript ~/R/dlib/chords.R --help
#' Rscript ~/R/dlib/chords.R --docu # create documentation
#' ```
#' 
#' ## DESCRIPTION
#' 
#' The file _chords.R_ contains the code for an R environment _chords_ to create Chord diagrams for string instruments like
#' Guitar and Ukulele.
#' 
#' Requirements:
#' 
#' * R standard installation
#' * Optional: Cairo package for SVG output
#' * Optional: tikzDevice package for LaTeX output 
#' 
#' Public Functions:
#'
#'   * [chords$chord](#chord) - create chord diagrams
#' 
#' ## USAGE
#' <a name="usage"> </a>
#' 
#' ### Source chords.R
#' 
#' To use the functions in your own R programs and analysis just source the 
#' file `chords.R` into your R application. It is in principle recommended to place the 
#' file into the R folder in your HOME directory.
#'
#' ```{r label=source}
#' source("chords.R")
#' ```
#' 
#' All function of this file are added to an environment variable called _chords_. 
#' To have a look which variables and functions exists in this namesspace you can use the function call
#' `ls(chords)`. Here is the output:
#' 
#' ```{r label=ls}
#' ls(chords)
#' ```
#' 
#' Public methods names start with a lowercase letter. You should only use them in your R scripts.
#' 
#' ```{r label=ls2}
#' ls(chords,pattern="^[a-z]")
#' ```
#' 
#' ### Application chords.R
#' 
#' Alternatively, if you make the script executable and you 
#' move into a folder belonging to your PATH variable,
#' you can use the script `chords.R` as a standalone executable you can directly from the terminal create chord charts.
#' Here an invocation to create 6 chords for the C-Dur scale of an Ukulele in GCES tuning.
#' 
#' ```
#' $ chords.R --chords C:0003:000R Dm:2210:MRI0 Em:0432:0RMI \
#'     F:2010:M0I0 G7:0212:0RIM Am:2000:M000  GCEA-C-Dur.png
#' ```
#' 
#' Possible output file formats are :
#' 
#' * `.pdf` - using standard R graphics
#' * `.png` - using standard R graphics
#' * `.svg` - using Cairo-SVG device if available
#' * `.tikz` - using tikzDevice if available
#' 

chords=new.env()
chords$VERSION = "2022.03.21"
.calls=sys.calls()
srx=grep("^source",.calls)
idx=srx[length(srx)]
if (length(idx)==0) {
    # using Rscript sbi.R
    argv=commandArgs(trailingOnly=FALSE)
    idx=grep("--file",argv)
    chords$FILENAME=gsub("--file=","",argv[idx])
} else if (grepl("['\"]",.calls[idx]))  {
    # string given
    chords$FILENAME = gsub("source\\(.(.+).\\)","\\1",.calls[idx])
} else {
    # variable given
    chords$FILENAME = eval(parse(text=gsub("source\\((.+)\\)","\\1",.calls[idx])))
}
rm(.calls,srx,idx)

#' ## FUNCTIONS
#' <a name="functions"> </a>
#'
#' <a name="chord"> </a>
#' 
#' **chords$chord(chord,positions,fingerings,...)** 
#' 
#' > Creates a chord chart for string instruments such as Guitar and Ukulele..
#' 
#' > This function creates ...
#' 
#' > Arguments:
#' 
#' > - _tuning_ - character string which will be displayed at the bottom, if empty not shown, default: "EADGBE"
#'   - _chord_  - the chord name which will be shown on top, if emtpy nothing will be shown, default: "D"
#'   - _nfrets_ - number of frets to be displayed, default: 5
#'   - _cex_    - scaling factor for text, default: 1
#'   - _cex.pch_ - scaling character for the plotting symbols, default: 1
#'   - _positions_ - indicator at which fret to press the string, X means skipping, 0 (zero) not fretting the string, default: "XX0232"
#'   - _fingerings_ - which finger to choose (IMRP syntax - Index, Middle, Ring, Pinky), default: "000IRM"
#'    - _family_     the font family to be used for all the text
#'
#' > Examples:
#' 
#' > Below an example for  Guitar chords in standard tuning for the key C-major.
#' 
#' > ```{r label=chords,fig=FALSE,results="hide",cache=TRUE,fig.width=12,fig.height=8,fig.cap=""}
#' chord=chords$chord
#' par(mfrow=c(2,3),mai=c(0.1,0.1,0.4,0.0))
#' chord(chord="C",positions="X32010",fingerings="0RM0I0",tuning="")
#' chord(chord="Dm",positions="XX0231",fingerings="000MRI",tuning="")
#' chord(chord="Em",positions="022000",fingerings="0IM000",tuning="")
#' chord(chord="Fadd9",positions="XX3213",fingerings="00RMIP",tuning="")
#' chord(chord="G",positions="320003",fingerings="MI000P",tuning="")
#' chord(chord="Am",positions="X02210",fingerings="0RM0I0",tuning="")
#' > ```
#' 


chords$chord = function (tuning="EADGBE",chord="D",nfrets=5,
                         cex=1,cex.pch=1,positions="XX0232",
                         fingerings="000IRM",family="Helvetica") {
    circle <- function(length = 99,scale=1, height=1, width=1) {
        k <- seq(0, 2 * pi, length = length)
        xa <- cos(k + acos(0)/2)/(3/scale+0.1)
        ya <- cos(k - acos(0)/2)/(3/scale+0.1)
        ya=ya*height    
        xa=xa*width
        return(list(x=xa*-1,y=ya))
    }
    cross <- function (height=1,width=1) {
        M=matrix(c(0, 0, 0.5, 0.5, 0, 1, 0.5,0.5, 1,1, 0.5, 0.5, 1, 0, 0.5, 0.5, 0,0),ncol=2,byrow=TRUE)
        xa=M[,1]*height-0.5
        ya=M[,2]*width-0.5
        return(list(x=xa*-1,y=ya))
    }
    plus <- function (height=1,width=1) {
        M=matrix(c(0, 0.5, 0.5, 0.5, 0.5, 0, 0.5,0.5, 1,0.5, 
                   0.5, 0.5, 0.5,1, 0.5, 0.5, 0,0.5),ncol=2,byrow=TRUE)
        xa=M[,1]*height-0.5
        ya=M[,2]*width-0.5
        return(list(x=xa*-1,y=ya))
    }
    par(pty="s")
    ylim=c(120,0)
    if (chord == "0") {
        ylim=c(ylim[1],10)
    } 
    plot(1,type="n",xlim=c(20,90),ylim=ylim,xlab="",ylab="",axes=FALSE, asp=1)
    if (chord != "0") {
        mtext(side=3,chord,line=0,cex=cex*2.5,family=family)
    } 
    start=10
    incr=(90-10)/(nchar(positions)-1)
    # bridge
    lines(c(10,90),y=c(20,20),lwd=6)
    # strings
    circ=circle()
    cross=cross()
    plus=plus()
    p.usr=par("usr")
    xdim=p.usr[2]-p.usr[1]
    ydim=p.usr[4]-p.usr[3]
    circ$x=circ$x/10*xdim*cex.pch
    circ$y=circ$y/10*ydim*cex.pch
    cross$x=cross$x/10*xdim*cex.pch*0.5
    cross$y=cross$y/10*ydim*cex.pch*0.5
    plus$x=plus$x/10*xdim*cex.pch*0.5
    plus$y=plus$y/10*ydim*cex.pch*0.5
    for (i in 0:(nchar(positions)-1)) {
        pos=substr(positions,i+1,i+1)
        if (pos == "X") {
            polygon(x=cross$x+start+i*incr,y=10+cross$y,lwd=3*cex.pch)
            #text(x=start+i*incr,y=10,"X",cex=cex*3,family=family)
        } else if (pos == "0") {
            #text(x=start+i*incr,y=10,"O",cex=cex*3,family=family)
            polygon(x=circ$x+start+i*incr,y=circ$y+10)
        }

        lines(x=rep(start+i*incr,2),y=c(100,20),lwd=2)
    }
    # frets
    yincr=(90-10)/nfrets
    for (i in 1:nfrets) {
        lines(x=c(10,90),y=rep(20+i*yincr,2))
        if ( i %in% c(3,5) ) {
            #text(x=50,y=20+(i-0.5)*yincr,"+",cex=cex*3,family=family)
            polygon(x=plus$x+50,y=plus$y+20+(i-0.5)*yincr,lwd=2*cex.pch)
        }
    }
    # positions
    for (i in 1:nchar(positions)) {
        # frets
        for (j in 1:(nfrets)) {
            # strings
            pos=substr(positions,i,i)
            if (pos == "X" | pos == "0") {
                next
            }
            pos=as.integer(pos)
            if (pos==j) {
                polygon(x=circ$x*1.2+start+(i-1)*incr,
                        y=circ$y*1.2+20+(j-0.5)*yincr,
                        col="#333333")
            }
        }
    }
    # Fingering
    for (i in 0:(nchar(fingerings)-1)) {
        pos=substr(fingerings,i+1,i+1)
        if (pos != "0") {
            text(x=start+i*incr,y=110,pos,cex=cex*1.8,family=family)
        } 
    }
    # Tuning
    for (i in 0:(nchar(tuning)-1)) {
        tune=substr(tuning,i+1,i+1)
        text(x=start+i*incr,y=120,tune,cex=cex*1.8,family=family)
    }
}

#' <a name="halfsteps"> </a>
#' 
#' **chords$halfsteps(n1,n1)** 
#' 
#' > Return the number of halfsteps from note n1 to note n2.
#' 
#' > Arguments:
#' 
#' > - _n1_ - first note
#'   - _n2_  - second note
#'
#' > Returns: number of halfsteps to go from note 1 to note up the scale.
#' 
#' > Examples:
#' 
#' > ```{r}
#' chords$halfsteps('C','C')
#' chords$halfsteps('C','D')
#' chords$halfsteps('D','C')
#' chords$halfsteps('B','A#')
#' chords$halfsteps('B','Bb')
#' > ```
#' 

chords$halfsteps = function (n1,n2) {
    map=c("A#","Bb","C#","Db","D#","Eb","F#","Gb","G#","Ab")
   for (i in 1:(length(map)/2)) {
        n1=gsub(map[i*2-1],map[i*2],n1)
        n2=gsub(map[i*2-1],map[i*2],n2)
    }
    notes=c("C","Db","D","Eb","E","F","Gb","G","Ab","A","Bb","B")
    idx1=which(notes==n1)
    idx2=which(notes==n2)
    n=idx2-idx1
    if (n>=0) {
        return(n)
    } else {
        return(12+n)
    }
}
#' <a name="notes"> </a>
#' 
#' **chords$notes(nfrets,x,y,labels, ...)** 
#' 
#' > Creates fretboard plots for note display for string instruments such as
#'   Guitar or Ukulele.
#' 
#' > Arguments:
#' 
#' > - _nfrets_ - the number of frets to display, default: 5
#'   - _nstrings_  - the number of strings of the instrument, default: 4
#'   - _x_ - x positions of the notes, default: c(0,0,0,2,3,3)
#'   - _y_ - y positions of the notes, default: y=c(2,3,4,2,3,4),
#'   - _labels_ - character vector for labels placed on the notes, default: empty strings
#'   - _col_ - color(s) for the plot symbol, default: 'grey90'
#'   - _..._  - additional  arguments forwarded to the plot function
#'
#' > Examples:
#' 
#' > Below an example for the pentatonic notes of the C-major scale for a standard Ukulele.
#' 
#' > ```{r label=notes,results="hide",cache=FALSE,fig.width=10,fig.height=5,fig.cap=""}
#' par(mfrow=c(1,2),mai=c(0.1,0.2,0.5,0.1),omi=c(0.2,0.3,0.6,0.2))
#' chords$notes(nfrets=4,
#'      x=c(0,0,0,0,2,3,3,2), y=c(1,2,3,4,2,3,4,1),
#'      col=c("grey90","salmon",rep('grey90',4),'salmon'),
#'      labels=c("G","C","E","A","D","G","C","A"))
#' text(2.4,2.5,"X",cex=2)
#' chords$notes(nfrets=4,nstrings=6,
#'      x=c(rep(0,6),3, 2, 2, 1, 3, 3, 3), y=c(1:6,2, 3,4, 5, 5, 6, 1),
#'      col=c(rep("grey90",4),'white','grey90','salmon',rep('grey90',2),'salmon',rep('grey90',2)),
#'      labels=c("E","A","D","G","B","E", "C", "E", "A","C","D","G", "G"))
#' text(2.4,3.5,"X",cex=1.5)
#' mtext("C-Dur Pentatonic Scales",side=3,outer=TRUE,cex=2,line=-1)
#' > ```
#' 
chords$notes = function (nfrets=5,nstrings=4,lwd=4,
                         x=c(0,0,0,2,3,3),
                         y=c(2,3,4,2,3,4),
                         labels=rep('',length(x)),
                         col=rep('grey90',length(x)), ...) {
    plot(1,type="n",xlab="",ylab="",
        xlim=c(-0.5,nfrets),
        ylim=c(0.5,nstrings+0.5),
        axes=FALSE,...)
    for (i in 1:nstrings) {
        lines(x=c(0,nfrets),y=c(i,i),lwd=lwd)
    }
    for (i in 0:nfrets) {
        lines(x=c(i,i),y=c(1,nstrings),lwd=lwd)
    }
    if (length(x)>0) {
        points(x-0.3,y,col=col,cex=6,pch=19)
    } 
    if (length(labels)> 0) {
        text(x-0.3,y,labels=labels)
    }
}

#' <a name="picking"> </a>
#' 
#' **chords$picking(x,y,length,labels, ...)** 
#' 
#' > Creates fingerpicking tabulature for string instruments such as
#'   Guitar or Ukulele.
#' 
#' > Arguments:
#' 
#' > - _strings_ - the strings of the instrument as character vector, default: character string which will be displayed at the bottom, if empty not shown, default: c("D","G","B","E")
#'   - _length_  - length of the picking pattern, default: 8
#'   - _x_ - x positions of the picking pattern, default: 1:8
#'   - _y_ - y positions, position 1 is the lower string, default: y=c(2,3,1,4,2,3,1,4)
#'   - _labels_ - character vector for labels placed on top of the picking symbols, default: rep(c("T","I","T","R"),2)
#'   - _col_ - color for the plot symbol, default: 'grey90'
#'   - _lwd_    - lwd for strings and vertical lines, default: 4
#'   - _cex.main_ - scaling size for the title, default: 3
#'   - _..._  - additional  arguments forwarded to the plot function
#'
#' > Examples:
#' 
#' > Below examples for a Bariton Ukulele, a standard Ukulele and a Guitar.
#' 
#' > ```{r label=picking,fig=FALSE,results="hide",cache=FALSE,fig.width=6,fig.height=12,fig.cap=""}
#' par(mfrow=c(3,1),mai=c(0.1,0.5,0.5,0.1))
#' 
#' picking=chords$picking
#' picking(x=c(1,2,2, 3,4,4, 5,6,6, 7,8,8),
#'             y=rep(c(2,3,4),4),
#'             labels=rep(c("T","I","M"),4),
#'             main="Bariton Ukulele",cex.main=3)
#' picking(strings=c("E","A","D","G","B","E"),
#'         x=1:8,y=c(2,4,5,4,6,4,5,4),
#'             labels=c(0,2,1,2,0,2,1,2),
#'             main="Guitar (Am)",cex.main=3)
#' picking(strings=c("E","A","D","G","B","E"),
#'         x=1:8,y=c(1,4,5,4,6,4,5,4),
#'             labels=rep(0,8),cex=1.2,
#'             main="Guitar (Em) - cex=1.2",cex.main=3)
#' > ```
#' 

chords$picking = function (strings=c("D","G","B","E"),
                           length=8,x=1:8,y=c(2,3,1,4,2,3,1,4),
                           labels=rep(c("T","I","T","R"),2),
                           col=rep("grey90",length(x)),
                           lwd=4,cex=1,axis2=TRUE,...) {
    nstr=length(strings)
    plot(1,type="n",xlab="",ylab="",
        xlim=c(0.5,length+0.5),
        ylim=c(0.5,nstr+0.5),
        axes=FALSE,...)
    
    abline(h=1:nstr,lwd=lwd)
    if (axis2) {
        axis(2,labels=strings,at=1:nstr,cex.axis=cex*2,lwd=4)
    } else {
        axis(2,labels=rep('',nstr),at=1:nstr,lwd=0,lwd.ticks=4)
    }
    axis(4,at=1:nstr,labels=rep('',nstr),lwd=4)
    points(x,y,pch=19,cex=cex*8,col=col)
    text(x,y,labels,cex=cex*1.5)
}

#'
#' ## EXAMPLES
#' <a name="examples"> </a>
#' 
#' Below a few more examples. Let's start with the six main chords of the C major key for the Ukulele:
#' 
#' > ```{r label=chords2,fig=FALSE,results="hide",cache=TRUE,fig.width=12,fig.height=8,fig.cap=""}
#' chord=chords$chord
#' par(mfrow=c(2,3),omi=c(0.2,0.4,0.6,0.1),mai=c(0.1,0.1,0.4,0.0))
#' chord(chord="C",positions="0003",fingerings="000R",tuning="")
#' chord(chord="Dm",positions="2210",fingerings="MRI0",tuning="")
#' chord(chord="Em",positions="0432",fingerings="0RMI",tuning="")
#' chord(chord="F", positions="2010",fingerings="M0I0",tuning="")
#' chord(chord="G7",positions="0212",fingerings="0MIR",tuning="")
#' chord(chord="Am",positions="2000",fingerings="M000",tuning="")
#' mtext("Ukulele (GCEA)",side=3,outer=TRUE,cex=2)
#' > ```
#' 
#' Now an other interesting chord progression for the Ukulele:
#' 
#' > ```{r label=chords3,fig=FALSE,results="hide",cache=TRUE,fig.width=12,fig.height=4,fig.cap=""}
#' chord=chords$chord
#' par(mfrow=c(1,4),omi=c(0.2,0.4,0.6,0.1),mai=c(0.1,0.1,0.4,0.0))
#' chord(chord="Bbadd9",positions="3213",fingerings="RMIP",tuning="")
#' chord(chord="F", positions="2010",fingerings="M0I0",tuning="")
#' chord(chord="Csus4", positions="0013",fingerings="00IP",tuning="")
#' chord(chord="Dmadd11", positions="0210",fingerings="0MI0",tuning="")
#' mtext("Ukulele (GCEA)",side=3,outer=TRUE,cex=2)
#' > ```
#' 
#' Let's now create a Guitar picking pattern over three chords 
#' with indication of the fretted strings, not with indications of the used finger:
#' 
#' > ```{r label=pick3,fig.width=12,fig.height=5,fig.cap=""}
#' par(mfrow=c(1,3),mai=c(0.0,0.1,0.6,0.0),omi=c(0.5,0.5,0.5,0.5))
#' picking=chords$picking
#' col="cornsilk"
#' picking(strings=c("E","A","D","G","B","E"),
#'         x=1:8,y=c(2,4,5,4,6,4,5,4),
#'             labels=c(0,2,1,2,0,2,1,2),
#'             main="Am",cex.main=3,cex=1.2,col=col)
#' picking(strings=c("E","A","D","G","B","E"),
#'         x=1:8,y=c(1,4,5,4,6,4,5,4),
#'             labels=rep(0,8),axis2=FALSE,
#'             main="Em",cex.main=3,cex=1.2,col=col)
#' picking(strings=c("E","A","D","G","B","E"),
#'         x=1:8,y=c(3,4,5,4,6,4,5,4),
#'             labels=c(0,2,3,2,2,2,3,2),axis2=FALSE,
#'             main="D",cex.main=3,cex=1.2,col=col)
#' > ```
#'  
#' Setting the notes by hand,  as could be seen in the notes-plot above,  
#' on the right positions is a little bit tedious.  
#' Think about setting this on longer fretboard diagrams. 
#' We can use the halfstep functionality to calculate automatically 
#' the the right positions:
#' 
#' ```{r label=autoscale,fig=TRUE,fig.width=8,fig.height=4,fig.cap=""}
#' par(mai=c(0.4,0.4,0.4,0.1))
#' chords$notes(nfrets=8,nstrings=4,x=c(),y=c(),labels=c())
#' i=0
#' text(x=c(2.5,4.5,6.5,7.5),y=rep(2.5,4),'X',cex=1.5)
#' for (str in c('G','C','E','A')) {
#'     i=i+1
#'     for (pn in c('C','D','E','G','A')) {
#'        hs=chords$halfsteps(str,pn)
#'        if (hs<9) {
#'          col='grey90'
#'          if (pn=='C') { col ="salmon" }
#'          points(hs-0.5,i,col=col,pch=19,cex=6)
#'          text(hs-0.5,i,pn,cex=1.3)
#'        }
#'     }
#' }
#' ```
#'
#' ## CITATION
#' <a name="citation"> </a>
#' 
#' How to cite this script:
#' 
#' ```
#' # notrun
#' @Misc{Groth2022chords,
#'   author =   {Detlef Groth},
#'   title =    {chords.{R} - {R} functions to create chord and fretboard diagrams for string instruments.},
#'   howpublished = {\url{https://github.com/mittelmark/Rcode}},
#'   year = {2022}
#' }
#' ```
#' 
#' Groth, D. (2022). chords.{R} - {R} functions to create chord and fretboard diagrams for string instruments. [https://github.com/mittelmark/Rcode](https://github.com/mittelmark/Rcode)
#' 
#' ## DOCUMENTATION
#' <a name="documentation"> </a>
#' 
#' This documentation was created directly from the file *chords.R* using the following commands in the terminal:
#' 
#' ```
#' # notrun
#' Rscript chords.R --docu
#' ```
#' 
#' The script `chords.R` can be executed directly from the command line as described above, using the docu flag all lines starting with 
#' the `#'` character sequence are extracted and then processed using the library [rmarkdown](https://cran.r-project.org/web/packages/rmarkdown) which evaluates the embedded code chunks.
#' 
#' ## LINKS
#' <a name="links"> </a>
#' 
#' * [Javascript and Python tools](https://forum.codeselfstudy.com/t/displaying-guitar-chord-diagrams-with-javascript-and-python/2522)
#' 
#' ## TODO's
#' <a name="todos"> </a>
#' 
#' * Ukulele (done)
#' * Kapo display
#' * flexibility in placing the Chord indicator, top or bottom
#' * fret indicator on the right 3rd, 4th, etc.
#' * plotting scales "0000|0R111|0011|0101|001R" from top to bottom (R is root indicator)
#' * rotate the plot
#' * changing fonts as well in terminal application mode
#' 
#' ## CHANGELOG
#' <a name="changelog"> </a>
#' 
#' * 2022-01-30 (Version 0.1) - initial release
#' * 2022-02-07 (Version 0.2) - adapting circles and crosses all to polygons
#' * 2022-02-12 (Version 0.3) - adding fingerpicking and notes diagrams
#' 
#' ## DISCLAIMER
#' 
#' There is no warranty of any kind! Use at your own risk!!
#' 
#' ## LICENSE (MIT)
#' <a name="license"> </a>
#' 
#' Copyright 2022 Detlef Groth, Scwielowsee, Germany
#' 

#' Permission is hereby granted, free of charge, to any person obtaining a
#' copy of this software and associated documentation files (the "Software"),
#' to deal in the Software without restriction, including without limitation
#' the rights to use, copy, modify, merge, publish, distribute, sublicense,
#' and/or sell copies of the Software, and to permit persons to whom the
#' Software is furnished to do so, subject to the following conditions: 
#'
#' The above copyright notice and this permission notice shall be included in
#' all copies or substantial portions of the Software. 
#'
#' THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#' IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#' FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#' AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#' LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
#' FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
#' DEALINGS IN THE SOFTWARE. 
#' 

if (sys.nframe() == 0L && !interactive()) {
    Usage = function () {
        cat("chords.R - create Chord diagrams for string instruments\n")
        cat("\nUsage:\n\n   Rscript chords.R chord:positions:fingerings [chord:postions:fingerings] outfile.(pdf|png|svg)")
        cat("\n\nExample:\n\n   Rscript chords.R --chords Fadd9:XX3213:00RMIP Fadd9.png\n\n")
        cat("   With the command line argument layout you can change the layout.\n   Here a 2 rows 3 column layout example:\n\n")
        cat(" Rscript chords.R --chords --layout 2,3 C:0003:000R Dm:2210:MRI0 Em:0432:0RMI \\ \n                  F:2010:M0I0 G7:0212:0RIM Am:2000:M000  GCEA-C-Dur.svg\n\n")
    }
    # if running via Rscript
    argv=commandArgs()
    idx=grep("--file",argv)
    chords$FILENAME=gsub("--file=","",argv[idx])
    filename=chords$FILENAME
    if (any(grepl("--docu",argv))) {
        t1=Sys.time()
        cat(" running", filename,"\n")
        
        fin  = file(filename, "r")
        
        outfile=gsub("\\.R",".Rmd",filename)
        fout = file(outfile,'w')
        while(length((line = readLines(fin,n=1)))>0) {
            if (grepl("^\\s*#'",line)) {
                line=gsub("^\\s*#' ?","",line)       
                cat(line,"\n",file=fout)
            }
        }
        close(fin)
        close(fout)
        library(rmarkdown)
        render(outfile,html_document(css="mini.css",theme=NULL))
        cat("Processing done in",round(as.numeric(Sys.time()-t1,units="secs"),2),"seconds!\n")
    } else if (any(grepl("--help",argv))) {
        Usage()
    } else if (any(grepl("--test",argv))) {
        print("Not yet implemented")
    } else if (any(grepl("--chords",argv))) {
        idx=grep("--chords",argv)
        lay=NULL
        if (any(grepl("--layout",argv))) {
            idl=grep("--layout",argv)
            lay=argv[idl+1]
            lay=as.numeric(strsplit(lay,",")[[1]])
            argv=argv[-(c(idl,idl+1))]
        }
        outfile=argv[length(argv)]
        if (!grepl("\\.(svg|pdf|png)$",outfile)) {
            print("Error: last argument must be an outfile with extension pdf, png, svg or tikz!\n")
        } else if (idx+1==length(argv)) {
            print("Error: The chord is missing try chords.R --chords Fadd9:XX3213:00RMIP Fadd9.png")
        } else {
            chrds=argv[(idx+1):(length(argv)-1)]
            w=6
            h=8
            if (is.null(lay[1])) {
                w=w*length(chrds)
            } else {
                w=w*lay[2]
                h=h*lay[1]
            }
            ext=gsub(".+\\.","",outfile)
            if (ext=="pdf") {
                pdf(outfile,width=w,height=h)
            } else if (ext == "png") {
                png(outfile,width=w*70,height=h*70)
            } else if (ext == "svg") {
                svg(outfile,width=w,height=h)
            } else{
                stop("svg and tikz not yet implemented")
            }
            if (is.null(lay)) {
                par(mfrow=c(1,length(chrds)),mai=c(0.4,0.0,0.4,0.0),omi=c(0.1,0.1,0.4,0.1))
            } else {
                par(mfrow=c(lay[1],lay[2]),mai=c(0.4,0.0,0.4,0.0),omi=c(0.1,0.1,0.4,0.1))
            }
            for (chrd in chrds) {
                opt=strsplit(chrd,":")[[1]]
                if (length(opt) != 3) {
                    cat("Invalid chord defintion: '",chrd,"'\n",sep="")
                } else {
                    chords$chord(chord=opt[1], positions=opt[2],fingerings=opt[3],tuning="")
                }
            }
            dev.off()
        }
    } else {
        Usage()
    }
}


