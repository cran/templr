#title: BFGS
#help: General-purpose Optimization
#type: Optimization
#authors: R core team
#references: Nash (1990), Zhu, Byrd, Lu-Chen and Nocedal from Netlib
#require: future, github:MASCOTNUM/templr
#options: maxit='10'
#options.help: maxit=maximum number of iterations
#input: x=list(min=0,max=1)
#output: y=0.01

BFGS <- function(options) {
    algorithm = new.env()
    algorithm$options <- options
    
    algorithm$id <- floor(1000*runif(1))
    
    algorithm$maxit <- as.integer(options$maxit)
    
    return(algorithm)
}

getInitialDesign <- function(algorithm, input, output) {
    algorithm$input <- input
    algorithm$output <- output  
    algorithm$d <- length(input)
    library(future)
    library(templr)
    wd = getwd()
    algorithm$job = future(evaluator=plan("multisession"),lazy = FALSE,{
        sink(file.path(wd,paste0('BFGS_',algorithm$id,'.out')),type='output')
        print("Starting optim()")
        o = optim(par=(min_input(input)+max_input(input))/2,
                  fn=function(x) {
                      ask_Y(id=algorithm$id,x=matrix(x,ncol=algorithm$d))}, 
                  lower=min_input(input), upper=max_input(input), 
                  method="L-BFGS-B",control=list(maxit=algorithm$maxit))
        print("optim() ended")
        print(o)
        sink(type='output')
        ask_Y(id=algorithm$id,matrix(NaN,ncol=algorithm$d))
    })
    algorithm$i = 0
    
    Sys.sleep(.1)
    
    Xn = ask_X(id=algorithm$id)
    algorithm$s = nrow(Xn)
    print(Xn)
    names(Xn) <- names(algorithm$input)
    return(Xn)
}

getNextDesign <- function(algorithm, X, Y) {
    if ((algorithm$maxit*(algorithm$d+4)) < algorithm$i) return(NULL) # almost equiv. to maxit coutning gradients
    algorithm$i = algorithm$i + 1
    
    y = Y[(nrow(Y)-algorithm$s+1):nrow(Y),]

    tell_Y(id=algorithm$id,y)

    if (is.na(y)) return(NULL)
    
    Sys.sleep(.1)

    Xn = ask_X(id=algorithm$id)
    algorithm$s = nrow(Xn)
    names(Xn) <- names(algorithm$input)
    return(Xn)
}

displayResults <- function(algorithm, X, Y) {
    displayResultsTmp(algorithm, X[-nrow(X),], Y[-nrow(Y),]) # remove last NaN
}

displayResultsTmp <- function(algorithm, X, Y) {
    algorithm$files <- "plot.png"
    png(file = algorithm$files, height = 600, width = 600)
    red=(Y-min(Y))/(max(Y)-min(Y))
    if (ncol(X)>1) {
        #pairs(X,col=rgb(r=red,g=0,b=1-red),Y=Y,d=nrow(X),panel=panel.vec)
        pairs(X,col=rgb(r=red,g=0,b=1-red),panel=panel.novec)
        #pairs(cbind(X,Y))
    } else 
        plot(X,Y,col=rgb(r=red,g=0,b=1-red))
    dev.off()
    
    ## return HTML string containing plot image
    return(paste0("<HTML name='points'>",
                 "<img src='",algorithm$files,"' width='600' height='600'/>",
                 "<br/></HTML>",
                 "<min>",min(Y),"</min>",
                "<argmin>",
                 paste0(collapse = ",",X[which.min(Y),]),
                "</argmin>"))
}

panel.novec <- function(x, y, col, ...) {
    #points(x,y,col=col)
    for (i in 1:(length(x))) {
        n0 = i
        x0 = x[n0]
        y0 = y[n0]
        points(x=x0,y=y0,col=col[n0],pch=20)
        if (exists("x0p")) {
            lines(x=c(x0p,x0),y=c(y0p,y0),col=col[n0],lty=3)
        }
        x0p=x0
        y0p=y0
    }
}
