#' Parse algorithm file and returns its (header) indos and methods
#'
#' @param file Template algorithm file to parse
#'
#' @return list of header infos and environment containing methods <constructor>,getInitialDesign,getNextDesign,displayResults
#' @export
#'
#' @examples
#' parse.algorithm(system.file("Brent.R", package="templr"))
parse.algorithm = function(file) {
    if (!file.exists(file)) stop("Cannot find Algorithm file ",file)
    lines=readLines(file)
    
    name=unlist(strsplit(file,"/"))
    name=sub("\\.R","",name[length(name)])
    if (nchar(name)==0) name="null"
    
    title="?"
    help="?"
    authors="?"
    version="?"
    references="?"
    type="?"
    output="?"
    requires=NULL
    options = list()
    options.default = list()
    options.help = list()
    
    for (i in 1:length(lines)) {
        if (isTRUE(strtrim(lines[i],7)=="#title:")) {
            title=sub("#title:\\s*","",lines[i])
        } else if (isTRUE(strtrim(lines[i],6)=="#help:")) {
            help=sub("#help:\\s*","",lines[i])
        } else if (isTRUE(strtrim(lines[i],9)=="#version:")) {
            version=sub("#version:\\s*","",lines[i])
        } else if (isTRUE(strtrim(lines[i],9)=="#authors:")) {
            authors=sub("#authors:\\s*","",lines[i])
        } else if (isTRUE(strtrim(lines[i],12)=="#references:")) {
            references=sub("#references:\\s*","",lines[i])
        } else if (isTRUE(strtrim(lines[i],6)=="#type:")) {
            type=sub("#type:\\s*","",lines[i])
        } else if (isTRUE(strtrim(lines[i],8)=="#output:")) {
            output=sub("#output:\\s*","",lines[i])
        } else if (isTRUE(strtrim(lines[i],9)=="#require:")) {
            requires=strsplit(sub("#require:\\s*","",lines[i]),"[;, ]")[[1]]
        } else if (isTRUE(strtrim(lines[i],9)=="#options:")) {
            str_repl = gsub(perl = T,"(,)(?=(?:[^']|'[^']*')*$)",';',sub("#options:\\s*","",lines[i]))
            options_str=strsplit(str_repl,"[;,]")
            for (os in options_str[[1]]){
                ko <- gsub(" ","",fixed=T,unlist(strsplit(unlist(os),"=")))
                options[[ko[1]]]=gsub("'","",ko[2])
                #print(paste0(ko[1],": ", ko[2]," -> ",gsub("(\\|)(.*)","",ko[2])))
                options.default[[ko[1]]]=gsub("'","",gsub("(\\|)(.*)","",ko[2]))
                options.help[[ko[1]]]="?"
            }
        } else if (isTRUE(strtrim(lines[i],14)=="#options.help:")) {
            str_repl = gsub(perl = T,"(,)(?=(?:[^']|'[^']*')*$)",';',sub("#options.help:\\s*","",lines[i]))
            options_str=strsplit(str_repl,"[;,]")
            for (os in options_str[[1]]){
                ko <- unlist(strsplit(unlist(os),"="))
                options.help[[ko[1]]]=gsub("'","",ko[2])
            }
        }
    }
    
    e = new.env()
    s=NULL
    try(s<-source(file,local=e),silent=F)
    if(is.null(s)) {
        stop(paste0("Cannot source file ",file,":\n",geterrmessage())) #,":\n",paste0(collapse="\n",traceback())))
    }
    
    e$new = e[[name]]
    
    if (exists("requires")) import(gsub(" ","",fixed=T,requires))
            
    return(list(name=name,authors=authors,references=references,help=help,type=tolower(type),output=output,options=options,options.default=options.default,options.help=options.help,requires= gsub(" ","",requires),envir=e))
}

#' Read algorithm file and returns one header info
#'
#' @param file Template algorithm file to read
#' @param info header info to return
#'
#' @return list of header infos
#' @export
#'
#' @examples
#' read.algorithm(system.file("Brent.R", package="templr"),"help")
read.algorithm = function(file,info="help"){
    lines=readLines(file)
    
    name=unlist(strsplit(file,"/"))
    name=sub(".R","",name[length(name)])
    
    title=NA
    help=NA
    version=NA
    authors=NA
    references=NA
    type=NA
    output=NA
    requires=NA
    options = list()
    options.default = list()
    options.help = list()
    
    for (i in 1:length(lines)) {
        # print(paste0("> ",lines[i]))
        if (isTRUE(strtrim(lines[i],7)=="#title:")) {
            title=sub("#title:\\s*","",lines[i])
        } else if (isTRUE(strtrim(lines[i],6)=="#help:")){
            help=sub("#help:\\s*","",lines[i])
        } else if (isTRUE(strtrim(lines[i],9)=="#version:")) {
            version=sub("#version:\\s*","",lines[i])
        } else if (isTRUE(strtrim(lines[i],6)=="#type:")) {
            type=sub("#type:\\s*","",lines[i])
        } else if (isTRUE(strtrim(lines[i],12)=="#references:")) {
            references=sub("#references:\\s*","",lines[i])
        } else if (isTRUE(strtrim(lines[i],9)=="#authors:")) {
            authors=sub("#authors:\\s*","",lines[i])
        } else if (isTRUE(strtrim(lines[i],8)=="#output:")) {
            output=sub("#output:\\s*","",lines[i])
        } else if (isTRUE(strtrim(lines[i],9)=="#require:")) {
            requires=strsplit(sub("#require:\\s*","",lines[i]),"[;, ]")[[1]]
        } else if (isTRUE(strtrim(lines[i],9)=="#options:")) {
            str_repl = gsub(perl = T,"(,)(?=(?:[^']|'[^']*')*$)",';',sub("#options:\\s*","",lines[i]))
            options_str=strsplit(str_repl,"[;,]")
            for (os in options_str[[1]]){
                ko <- unlist(strsplit(unlist(os),"="))
                options[[ko[1]]]=gsub("'","",ko[2])
                options.default[[ko[1]]]=gsub("'","",gsub("(\\|)(.*)","",ko[2]))
            }
        } else if (isTRUE(strtrim(lines[i],14)=="#options.help:")) {
            str_repl = gsub(perl = T,"(,)(?=(?:[^']|'[^']*')*$)",';',sub("#options.help:\\s*","",lines[i]))
            options_str=strsplit(str_repl,"[;,]")
            for (os in options_str[[1]]){
                ko <- unlist(strsplit(unlist(os),"="))
                options.help[[ko[1]]]=gsub("'","",ko[2])
            }
        }
    }
    
    return(list(name=name,authors=authors,help=help,type=tolower(type),output=output,requires= gsub(" ","",requires),options=options,options.default=options.default,options.help=options.help)[[info]])
}



#' Apply a template algorithm file to an objective function
#'
#' @param algorithm_file tempalted algorithm file
#' @param objective_function function to apply algorithm on
#' @param input list of input arguments of function (eg. list(x1=list(min=0,max=1),x2=list(min=10,max=20)))
#' @param options algorithm options to overload default ones
#' @param work_dir working directory to run algorithm. will store output files, images, ..
#' @param trace display running info
#' @param silent quietness
#'
#' @return algorithm result (and algorithm object & files as attributes)
#' @export
#'
#' @examples
#' run.algorithm(
#'   system.file("Brent.R", package="templr"),
#'   function(x) sin(x)-0.75,
#'   list(x=list(min=0,max=pi/2)),
#'   work_dir=tempdir()
#'   )
run.algorithm <- function(algorithm_file,
                          objective_function,
                          input,
                          options=NULL,
                          work_dir=".",
                          trace=function(...) cat(paste0(...,"\n")),silent=FALSE) { #},work_dir=paste0(tempdir(),floor(1000*runif(1)))) {
    
    if (!is.function(trace)) trace = function(...){}
    
    output = utils::capture.output(print(match.call()))
    output = strsplit(output,"objective_function = ",fixed = T)[[1]][2]
    output = strsplit(output,",",fixed = T)[[1]][1]

    # algorithm_file = normalizePath(algorithm_file)
    
    trace(paste0("Parsing code... (in ",algorithm_file, " from ",getwd(),")"))
    algorithm = NULL
    try(algorithm <- parse.algorithm(algorithm_file),silent = silent)
    if(is.null(algorithm)) {
        stop("Error while parsing code")
    }
    
    prev.path = getwd()
    on.exit(setwd(prev.path))
    
    dir.create(work_dir, showWarnings = !silent)
    setwd(work_dir)
    
    # print.env(algorithm$envir)
    
    trace("Instanciating algorithm...")
    instance = NULL
    def_options=algorithm$options
    for (o in names(def_options)) def_options[[o]]=gsub("\\|.*","",def_options[[o]])
    for (o in names(options)) def_options[[o]]=options[[o]]
    options=def_options
    
    try(instance <- algorithm$envir$new(options),silent = silent)
    if(is.null(instance)) {
        setwd(prev.path)       
        stop("Error while instanciating")
    }
    #return(list(new=geterrmessage(),init="",next="",display=""))
    
    trace("Initializing algorithm...")
    X0 = NULL
    try(X0 <- algorithm$envir$getInitialDesign(instance, input, output),silent = silent)
    if(is.null(X0)) {
        setwd(prev.path)
        stop("Error while computing getInitialDesign")
    }
    if(!is.matrix(X0)) X0=as.matrix(X0,ncol=length(input),byrow = T)
    colnames(X0) <- names(input)
    F = function(X) {
        m=matrix(apply(X,1,objective_function),nrow=nrow(X));
        colnames(m)<-output;
        m
    }
    
    #X0 = from01(X0,input) #X.min=Xmin.model(objective_function),X.max=Xmax.model(objective_function))
    Y0 = F(X0)
    Xi = X0
    Yi = Y0
    
    finished = FALSE
    i = 0
    while (!finished) {
        i = i + 1
        trace(paste0("Iterating algorithm... ",i))
        err = NULL
        Xj = NULL
        tryCatch(Xj <- algorithm$envir$getNextDesign(instance,Xi,Yi),error=function(e) err <<- e)
        if(!is.null(err)) {
            setwd(prev.path)
            stop("Error while computing getNextDesign\n",paste.XY(Xi,Yi))
        }
        #colnames(Xj)<-names(input)
        
        if (is.null(Xj) | any(is.na(Xj)) | any(is.nan(Xj)) | length(Xj) == 0) {
            finished = TRUE
        } else {
            #Xj = from01(Xj,X.min=Xmin.model(objective_function),X.max=Xmax.model(objective_function))
            Yj = F(Xj)
            Xi = rbind(Xi,Xj)
            Yi = rbind(Yi,Yj)
        }
    }
    
    trace("Display results...")
    res = NULL
    try(res <- algorithm$envir$displayResults(instance,Xi,Yi),silent = silent)
    if(is.null(res)) {
        setwd(prev.path)
        stop("Error while computing displayResults\n",paste.XY(Xi,Yi))
    }
    
    # if (!is.null(instance$files)) {
    #     for (f in instance$files){
    #         res = gsub(f,file.path(out.path,f),res)
    #         file.rename(f,file.path(out.path,f))
    #     }}
    setwd(prev.path)
    
    attr(res,"files")<-instance$files
    attr(res,"algorithm")<-instance
    return(res)
}

paste.XY = function(X,Y) {
    return(paste0("X=\n",
                  paste0(utils::capture.output(print(X)),collapse = "\n"),
                  "\nY=\n",
                  paste0(utils::capture.output(print(Y)),collapse = "\n")))
}
