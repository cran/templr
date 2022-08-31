#' ask&tell component function to 'ask' objective function evaluation.
#' @param x input values of objective function to compute
#' @param X.tmp temporary "X" values file (default: "X.todo")
#' @param Y.tmp temporary "Y" values file (default: "Y.done")
#' @param force_cleanup should we cleanup temporary files before writing (possible conflicting asktell calls) ? (default: FALSE)
#' @template ask-doc
#'
#' @return output value of objective function, as given by tell_Y() call in parallel session
#' @export
#'
#' @examples 
#' \dontrun{ ### Assumes you can use two independant R sessions
#' ## In main R session
#'   ask_Y(x=123)
#' ## In another R session
#'   ask_X() # returns 123
#'   tell_Y(y=456)
#' ## Then ask_Y in main R session returns with value '456'
#' }
ask_Y <- function(x,
                  id=0, X.tmp="X.todo", Y.tmp="Y.done", tmp_path=file.path(tempdir(),"..","asktell.tmp"), 
                  sleep_step=0.1, sleep_init=0, timeout=360000,
                  trace=function(...) cat(paste0(...,"\n")), clean = TRUE, force_cleanup=FALSE) {

    if (!is.null(tmp_path) && !dir.exists(tmp_path))
        if (!dir.create(tmp_path,showWarnings = FALSE,recursive = TRUE))
            stop(paste0("Could not use tmp_path directory: ",tmp_path))
    
    if (file.exists(file = Y_file(id,Y.tmp,tmp_path))) {
        if (!force_cleanup)
            stop(paste0("This id:'",id,"' is already in use. Please choose another one, or use 'force_cleanup=T'"))
        else {
            last=read.io(Y_file(id,Y.tmp,tmp_path),trace = trace)
            warning(paste0("This id:'",id,"' is already in use. Cleanup this data:",print(last)))
        }
    }
    
    if (is.function(trace)) trace("?Y(",paste0(collapse = ",",x),") ")
    
    if (file.exists(file = X_file(id,X.tmp,tmp_path))) {
        if (!force_cleanup)
            stop(paste0("This id:'",id,"' is already in use. Please choose another one, or use 'force_cleanup=T'"))
        else {
            last=read.io(X_file(id,X.tmp,tmp_path),trace = trace)
            warning(paste0("This id:'",id,"' is already in use. Cleanup this data:",print(last)))
        }
    }
    
    write.io(x,file = X_file(id,X.tmp,tmp_path),trace = trace)
    
    Sys.sleep(sleep_init)
    t=0
    lock=file.path(tempdir(),paste0("ask_Y_",id,rint()))
    file.create(lock)
    while (!file.exists(file = Y_file(id,Y.tmp,tmp_path)) & (timeout>0 & t<timeout)) {
        Sys.sleep(sleep_step)
        t=t+sleep_step
        if (!file.exists(lock)) stop("ask_Y break !")
        if (is.function(trace)) trace("\b.")
    }
    file.remove(lock)
    if (timeout>0 & t>=timeout) stop("ask_Y timeout !")
    Sys.sleep(sleep_step)
    if (is.function(trace)) trace("\b,")
    
    y = read.io(file = Y_file(id,Y.tmp,tmp_path),clean = clean,trace = trace)
    
    if (is.function(trace)) trace("\b(",paste0(collapse = ",",y),")")

    return(y)
}

#' ask&tell component function to 'ask' objective function gradient evaluation using finite difference.
#' @param x input values of objective function gradient to compute
#' @param dX finite difference applied to input values to compute gradient
#' @param dX.tmp temporary "X" values file (default: "dX.todo")
#' @param dY.tmp temporary "Y" values file (default: "dY.done")
#' @param force_cleanup should we cleanup temporary files before writing (possible conflicting asktell calls) ? (default: FALSE)
#' @template ask-doc
#'
#' @return output value of objective function gradient, as given by tell_dY() call in parallel session
#' @export
#'
#' @examples 
#' \dontrun{ ### Assumes you can use two independant R sessions
#' ## In main R session
#'   ask_dY(x=123)
#' ## In another R session
#'   ask_dX() # returns 123
#'   tell_dY(y=456)
#' ## Then ask_dY in main R session returns with value '456'
#' }
ask_dY <- function(x, dX=0.001, 
                   id=0, dX.tmp="dX.todo", dY.tmp="dY.done", tmp_path=file.path(tempdir(),"..","asktell.tmp"), 
                   sleep_step=0.1, sleep_init=0,  timeout=360000,
                   trace=function(...) cat(paste0(...,"\n")),clean = TRUE, force_cleanup=FALSE) {
    
    if (!is.null(tmp_path) && !dir.exists(tmp_path))
        if (!dir.create(tmp_path,showWarnings = FALSE,recursive = TRUE))
            stop(paste0("Could not use tmp_path directory: ",tmp_path))
    
    if (file.exists(file = dY_file(id,dY.tmp,tmp_path))) {
        if (!force_cleanup)
            stop(paste0("This id:'",id,"' is already in use. Please choose another one, or use 'force_cleanup=T'"))
        else {
            last=read.io(dY_file(id,dY.tmp,tmp_path),trace = trace)
            warning(paste0("This id:'",id,"' is already in use. Cleanup this data:",print(last)))
        }
    }
    
    if (is.function(trace)) trace("?dY(",paste0(collapse = ",",x),") ")

    if (!is.matrix(x)) x=matrix(x,ncol=length(x))
    d = ncol(x)
    # build the finite difference X matrix
    xdx = t(matrix(x,d,d+1))
    for (i in 1:d) {
        dx = dX
        if (xdx[1+i,i] + dx > 1) {dx = -dx}
        xdx[1+i,i] = xdx[1+i,i] + dx
    }
    
    if (file.exists(file = dX_file(id,dX.tmp,tmp_path))) {
        if (!force_cleanup)
            stop(paste0("This id:'",id,"' is already in use. Please choose another one, or use 'force_cleanup=T'"))
        else {
            last=read.io(dX_file(id,dX.tmp,tmp_path),trace = trace)
            warning(paste0("This id:'",id,"' is already in use. Cleanup this data:",print(last)))
        }
    }
    
    write.io(xdx,file = dX_file(id,dX.tmp,tmp_path),trace = trace)
    
    Sys.sleep(sleep_init)
    t=0
    lock=file.path(tempdir(),paste0("ask_dY_",id,rint()))
    file.create(lock)
    while (!file.exists(file = dY_file(id,dY.tmp,tmp_path)) & (timeout>0 & t<timeout)) {
        Sys.sleep(sleep_step)
        t=t+sleep_step
        if (!file.exists(lock)) stop("ask_dY break !")
        if (is.function(trace)) trace("\b.")
    }
    file.remove(lock)
    if (timeout>0 & t>=timeout) stop("ask_dY timeout !")
    Sys.sleep(sleep_step)
    if (is.function(trace)) trace("\b,")
    
    ydy = read.io(file = dY_file(id,dY.tmp,tmp_path),clean = clean,trace = trace)
    
    # extract the finite differences for Y vector
    dy = array(-1,d)
    for (i in 1:d) {
        dy[i] = (ydy[i+1] - ydy[1]) / (xdx[i+1,i] - xdx[1,i])
    }
    
    if (is.function(trace)) trace("\b(",paste0(collapse = ",",dy),")")
    
    return(dy)
}


#' ask&tell component function to 'ask' where objective function evaluation is required.
#' @param X.tmp temporary "X" values file (default: "X.todo")
#' @template ask-doc
#'
#' @return input value of objective function to compute externally
#' @export
#'
#' @examples 
#' \dontrun{ ### Assumes you can use two independant R sessions
#' ## In main R session
#'   ask_Y(x=123)
#' ## In another R session
#'   ask_X() # returns 123
#'   tell_Y(y=456)
#' ## Then ask_dY in main R session returns with value '456'
#' }
ask_X <- function(id=0, 
                  X.tmp="X.todo", tmp_path=file.path(tempdir(),"..","asktell.tmp"), 
                  sleep_step=0.1, sleep_init=0.1,timeout=360000,
                  trace=function(...) cat(paste0(...,"\n")), clean = TRUE) {
    
    if (!is.null(tmp_path) && !dir.exists(tmp_path))
        if (!dir.create(tmp_path,showWarnings = FALSE,recursive = TRUE))
            stop(paste0("Could not use tmp_path directory: ",tmp_path))
    
    if (is.function(trace)) trace("?X ")
    
    Sys.sleep(sleep_init)
    t=0
    lock=file.path(tempdir(),paste0("ask_X_",id,rint()))
    file.create(lock)
    while (!file.exists(file = X_file(id,X.tmp,tmp_path)) & (timeout>0 & t<timeout)) {
        Sys.sleep(sleep_step)
        t=t+sleep_step
        if (!file.exists(lock)) stop("ask_X break !")
        if (is.function(trace)) trace("\b:")
    }
    file.remove(lock)
    if (timeout>0 & t>=timeout) stop("ask_X timeout !")
    Sys.sleep(sleep_step)
    if (is.function(trace)) trace("\b;")
    
    x = read.io(file = X_file(id,X.tmp,tmp_path),clean = clean,trace = trace)
    
    if (is.function(trace)) trace("\b(",paste0(collapse = ",",x),")")
    
    return(x)
}

#' ask&tell component function to 'ask' where objective function gradient evaluation is required.
#' @param dX.tmp temporary "X" values file (default: "dX.todo")
#' @template ask-doc
#'
#' @return input values of objective function to compute externally
#' @export
#'
#' @examples 
#' \dontrun{ ### Assumes you can use two independant R sessions
#' ## In main R session
#'   ask_dY(x=123)
#' ## In another R session
#'   ask_dX() # returns 123
#'   tell_dY(y=456)
#' ## Then ask_dY in main R session returns with value '456'
#' }
ask_dX <- function(id=0, dX.tmp="dX.todo", tmp_path=file.path(tempdir(),"..","asktell.tmp"), 
                   sleep_step=0.1, sleep_init=0, timeout=360000,
                   trace=function(...) cat(paste0(...,"\n")), clean=TRUE) {
    
    if (!is.null(tmp_path) && !dir.exists(tmp_path))
        if (!dir.create(tmp_path,showWarnings = FALSE,recursive = TRUE))
            stop(paste0("Could not use tmp_path directory: ",tmp_path))
    
    if (is.function(trace)) trace("?dX ")
    
    Sys.sleep(sleep_init)
    t=0
    lock=file.path(tempdir(),paste0("ask_dX_",id,rint()))
    file.create(lock)
    while (!file.exists(file = dX_file(id,dX.tmp,tmp_path)) & (timeout>0 & t<timeout)) {
        Sys.sleep(sleep_step)
        t=t+sleep_step
        if (!file.exists(lock)) stop("ask_dX break !")
        if (is.function(trace)) trace("\b:")
    }
    file.remove(lock)
    if (timeout>0 & t>=timeout) stop("ask_dX timeout !")
    Sys.sleep(sleep_step)
    if (is.function(trace)) trace("\b;")
    
    dx = read.io(file = dX_file(id,dX.tmp,tmp_path),clean = clean,trace = trace)
    
    if (is.function(trace)) trace("\b(",paste0(collapse = ",",dx),")")

    return(dx) #as.matrix(dx[,2:ncol(dx)]))
}

#' ask&tell component function to 'tell' objective function value to waiting 'ask_Y' call in another R session.
#' @param y output value of objective function to return
#' @param Y.tmp temporary "Y" values file (default: "Y.done")
#' @param force_cleanup should we cleanup temporary files before writing (possible conflicting asktell calls) ? (default: FALSE)
#' @template tell-doc
#'
#' @return input value of objective function to compute externally
#' @export
#'
#' @examples 
#' \dontrun{ ### Assumes you can use two independant R sessions
#' ## In main R session
#'   ask_Y(x=123)
#' ## In another R session
#'   ask_X() # returns 123
#'   tell_Y(y=456)
#' ## Then ask_dY in main R session returns with value '456'
#' }
tell_Y <- function(y, 
                   id=0, Y.tmp="Y.done", tmp_path=file.path(tempdir(),"..","asktell.tmp"), 
                   trace=function(...) cat(paste0(...,"\n")), force_cleanup=FALSE) {
    
    if (!is.null(tmp_path) && !dir.exists(tmp_path))
        if (!dir.create(tmp_path,showWarnings = FALSE,recursive = TRUE))
            stop(paste0("Could not use tmp_path directory: ",tmp_path))
    
    if (file.exists(file = Y_file(id,Y.tmp,tmp_path))) {
        if (!force_cleanup)
            stop(paste0("This id:'",id,"' is already in use. Please choose another one, or use 'force_cleanup=T'"))
        else {
            last=read.io(Y_file(id,Y.tmp,tmp_path),trace = trace)
            warning(paste0("This id:'",id,"' is already in use. Cleanup this data:",print(last)))
        }
    }
    
    if (is.function(trace)) trace("!Y=",y)
    
    write.io(y,file = Y_file(id,Y.tmp,tmp_path),trace = trace)
}

#' ask&tell component function to 'tell' objective function value to waiting 'ask_Y' call in another R session.
#' @param dy output value of objective function gradient to return
#' @param dY.tmp temporary "Y" values file (default: "dY.done")
#' @param force_cleanup should we cleanup temporary files before writing (possible conflicting asktell calls) ? (default: FALSE)
#' @template tell-doc
#'
#' @return input value of objective function to compute externally
#' @export
#'
#' @examples 
#' \dontrun{ ### Assumes you can use two independant R sessions
#' ## In main R session
#'   ask_dY(x=123)
#' ## In another R session
#'   ask_dX() # returns c(123, 123.123)
#'   tell_dY(dy=c(456,456.123))
#' ## Then ask_dY in main R session returns with value '1'
#' }
tell_dY <- function(dy, 
                    id=0, dY.tmp="dY.done", tmp_path=file.path(tempdir(),"..","asktell.tmp"), 
                    trace=function(...) cat(paste0(...,"\n")), force_cleanup=FALSE) {
    
    if (!is.null(tmp_path) && !dir.exists(tmp_path))
        if (!dir.create(tmp_path,showWarnings = FALSE,recursive = TRUE))
            stop(paste0("Could not use tmp_path directory: ",tmp_path))
    
    if (file.exists(file = dY_file(id,dY.tmp,tmp_path))) {
        if (!force_cleanup)
            stop(paste0("This id:'",id,"' is already in use. Please choose another one, or use 'force_cleanup=T'"))
        else {
            last=read.io(dY_file(id,dY.tmp,tmp_path),trace = trace)
            warning(paste0("This id:'",id,"' is already in use. Cleanup this data:",print(last)))
        }
    }
    
    if (is.function(trace)) trace("!dY=",dy)
    
    write.io(dy,file = dY_file(id,dY.tmp,tmp_path),trace = trace)
}

# test: x=123;write.io(x,"x.dat");read.io("x.dat")
# test: x=matrix(c(123,456),ncol=2);write.io(x,"x.dat");read.io("x.dat")
# test: x=matrix(c(123,456,789,101),ncol=2);write.io(x,"x.dat");read.io("x.dat")
write.io <- function(data,file, 
                     trace=function(...) cat(paste0(...,"\n"))) {
    i=0
    path=dirname(file); if (!dir.exists(path)) dir.create(path)
    while(file.exists(file) & i<100) {
        Sys.sleep(0.05); i=i+1; if (is.function(trace)) trace("\b ")
    }
    
    if (i>=100) stop("file ",file, " already exists !")
    
    if (is.function(trace)) trace("\b>")
    
    saveRDS(data,file=file)
    # utils::write.table(data,file=file,row.names=FALSE,header=TRUE)
}

read.io <- function(file,clean=TRUE, 
                    trace=function(...) cat(paste0(...,"\n"))) {
    t = NULL
    i=0
    try(t <- readRDS(file)
        #as.matrix(utils::read.table(file=file,header=TRUE))
        ,silent=TRUE)
    while(is.null(t) & i<10) {
        Sys.sleep(0.05)
        i=i+1
        if (is.function(trace)) trace("\b ")
        try(t <-  readRDS(file)
            #as.matrix(utils::read.table(file=file,header=TRUE))
            ,silent=TRUE)
    }
    if (is.null(t) & i<10 & is.function(trace)) trace(":)")
    #t <- as.matrix(utils::read.table(file=file,header=TRUE))
    if (clean) while(file.exists(file)) {
        file.remove(file)
        if (is.function(trace)) trace("\b-")
        Sys.sleep(0.1)
    }
    if (is.function(trace)) trace("\b<")
    return(t)
}

X_file <- function(id,X.tmp,tmp_path) {
    if (!is.null(tmp_path)) {
        X.tmp = file.path(tmp_path,X.tmp)
    }
    X_file = paste(sep="_",X.tmp,id)
    #if(isTRUE(.trace)) print(X_file)
    
    return(X_file)
}

dX_file <- function(id,dX.tmp,tmp_path) {
    if (!is.null(tmp_path)) {
        dX.tmp = file.path(tmp_path,dX.tmp)
    }
    dX_file = paste(sep="_",dX.tmp,id)
    
    return(dX_file)
}

Y_file <- function(id,Y.tmp,tmp_path) {
    if (!is.null(tmp_path)) {
        Y.tmp = file.path(tmp_path,Y.tmp)
    }
    Y_file = paste(sep="_",Y.tmp,id)
    #if(isTRUE(.trace)) print(Y_file)
    
    return(Y_file)
}

dY_file <- function(id,dY.tmp,tmp_path) {
    if (!is.null(tmp_path)) {
        dY.tmp = file.path(tmp_path,dY.tmp)
    }
    dY_file = paste(sep="_",dY.tmp,id)
    
    return(dY_file)
}

rint <- function() {
    floor(10000*stats::runif(1))
}