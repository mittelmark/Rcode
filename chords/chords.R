#!/usr/bin/env Rscript
#' ---
#' title: Plot chord diagrams for string instruments using R
#' author: Detlef Groth, University of Potsdam
#' date: 2022-01-30
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
#'     * [chords$chord](#chord) - generate chord diagrams for six-string inststruments like the guitar
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
#' To use the functions in your own R programs and analysis just source the file `bkcd.R` into your R application. It is recommended to place the file into the R folder in your HOME directory.
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
chords$VERSION = "2022.01.30"
.calls=sys.calls()
srx=grep("^source",.calls)
idx=srx[length(srx)]
chords$FILENAME = gsub("source\\(.(.+).\\)","\\1",.calls[idx])
rm(.calls,srx,idx)

#' ## FUNCTIONS
#' <a name="functions"> </a>
#'
#' <a name="chord"> </a>
#' 
#' **chords$chord(which=c(1,2))** 
#' 
#' > Creates xkcd compatible axes.
#' 
#' > This function creates ...
#' 
#' > Arguments:
#' 
#' > - _tuning_ - character string which will be displayed at the bottom, if empty not shown, default: "EADGBE"
#'   - _chord_  - the chord name which will be shown on top, if emtpy nothing will be shown, default: "D"
#'   - _nfrets_ - number of frets to be displayed, default: 5
#'   - _cex_    - scaling factor for fontsizes, default: 1
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
                         cex=1,positions="XX0232",
                         fingerings="000IRM",family="Helvetica") {
    circle <- function(length = 99,scale=1, height=1, width=1) {
        k <- seq(0, 2 * pi, length = length)
        xa <- cos(k + acos(0)/2)/(3/scale+0.1)
        ya <- cos(k - acos(0)/2)/(3/scale+0.1)
        ya=ya*height    
        xa=xa*width
        return(list(x=xa*-1,y=ya))
    }
    par(pty="s")
    ylim=c(120,0)
    if (chord == "0") {
        ylim=c(ylim[1],10)
    } 
    plot(1,type="n",xlim=c(20,90),ylim=ylim,xlab="",ylab="",axes=FALSE, asp=1)
    if (chord != "0") {
        mtext(side=3,chord,line=-1,cex=cex*3.5,family=family)
    } 
    start=10
    incr=(90-10)/(nchar(positions)-1)
    # bridge
    lines(c(10,90),y=c(20,20),lwd=6)
    # strings
    circ=circle()
    p.usr=par("usr")
    xdim=p.usr[2]-p.usr[1]
    ydim=p.usr[4]-p.usr[3]
    circ$x=circ$x/10*xdim*cex
    circ$y=circ$y/10*ydim*cex
    for (i in 0:(nchar(positions)-1)) {
        pos=substr(positions,i+1,i+1)
        if (pos == "X") {
            text(x=start+i*incr,y=10,"X",cex=cex*3,family=family)
        } else if (pos == "0") {
            text(x=start+i*incr,y=10,"O",cex=cex*3,family=family)
            #polygon(x=circ$x+start+i*incr,y=circ$y+10)
        }

        lines(x=rep(start+i*incr,2),y=c(100,20),lwd=2)
    }
    # frets
    yincr=(90-10)/nfrets
    for (i in 1:nfrets) {
        lines(x=c(10,90),y=rep(20+i*yincr,2))
        if ( i %in% c(3,5) ) {
            text(x=50,y=20+(i-0.5)*yincr,"+",cex=cex*3,family=family)
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
            text(x=start+i*incr,y=110,pos,cex=cex*2.2,family=family)
        } 
    }
    # Tuning
    for (i in 0:(nchar(tuning)-1)) {
        tune=substr(tuning,i+1,i+1)
        text(x=start+i*incr,y=120,tune,cex=cex*2,family=family)
    }
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
#' ## CITATION
#' <a name="citation"> </a>
#' 
#' How to cite this script:
#' 
#' ```
#' # notrun
#' @Misc{Groth2022chords,
#'   author =   {Detlef Groth},
#'   title =    {chords.{R} - {R} functions to create chord diagrams for string instruments.},
#'   howpublished = {\url{https://github.com/mittelmark/Rcode}},
#'   year = {2022}
#' }
#' ```
#' 
#' Groth, D. (2022). chords.{R} - {R} functions to create chord diagrams for string instruments. [https://github.com/mittelmark/Rcode](https://github.com/mittelmark/Rcode)
#' 
#' ## DOCUMENTATION
#' <a name="documentation"> </a>
#' 
#' This documentation was created directly from the file *bkcd.R* using the following commands in the terminal:
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
        cat("\nUsage: Rscript chords.R chord:positions:fingerings [chord:postions:fingerings] outfile.(pdf|png|svg)")
        cat("\nExample: Rscript chords.R --chords Fadd9:XX3213:00RMIP Fadd9.png")
        cat("   with command line argument layout you can change the layout: here a 2 rows 3 column layout:\n")
        cat(" Rscript chords.R --chords --layout 2,3 C:0003:000R Dm:2210:MRI0 Em:0432:0RMI F:2010:M0I0 G7:0212:0RIM Am:2000:M000  GCEA-C-Dur.svg\n")
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
                png(outfile,width=w*70*length(chrds),height=h*70)
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


