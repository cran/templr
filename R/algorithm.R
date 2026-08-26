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
    if (!is.character(file) || length(file)!=1 || is.na(file) || nchar(file)==0) stop("file must be a non-empty character string")
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
            options_str=strsplit(str_repl,"[;]")
            for (os in options_str[[1]]){
                ko <- gsub(" ","",fixed=T,unlist(strsplit(unlist(os),"=")))
                options[[ko[1]]]=gsub("'","",ko[2])
                options.default[[ko[1]]]=gsub("'","",gsub("(\\|)(.*)","",ko[2]))
                options.help[[ko[1]]]="?"
            }
        } else if (isTRUE(strtrim(lines[i],14)=="#options.help:")) {
            str_repl = gsub(perl = T,"(,)(?=(?:[^']|'[^']*')*$)",';',sub("#options.help:\\s*","",lines[i]))
            options_str=strsplit(str_repl,"[;]")
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
    if (!is.character(file) || length(file)!=1 || is.na(file) || nchar(file)==0) stop("file must be a non-empty character string")
    if (!file.exists(file)) stop("Cannot find Algorithm file ",file)
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
            options_str=strsplit(str_repl,"[;]")
            for (os in options_str[[1]]){
                ko <- gsub(" ","",fixed=T,unlist(strsplit(unlist(os),"=")))
                options[[ko[1]]]=gsub("'","",ko[2])
                options.default[[ko[1]]]=gsub("'","",gsub("(\\|)(.*)","",ko[2]))
            }
        } else if (isTRUE(strtrim(lines[i],14)=="#options.help:")) {
            str_repl = gsub(perl = T,"(,)(?=(?:[^']|'[^']*')*$)",';',sub("#options.help:\\s*","",lines[i]))
            options_str=strsplit(str_repl,"[;]")
            for (os in options_str[[1]]){
                ko <- unlist(strsplit(unlist(os),"="))
                options.help[[ko[1]]]=gsub("'","",ko[2])
            }
        }
    }

    return(list(name=name,authors=authors,help=help,type=tolower(type),output=output,requires= gsub(" ","",requires),options=options,options.default=options.default,options.help=options.help)[[info]])
}


#' Parse algorithm string result in R list
#' @param result templated algorithm result string
#'
#' @return list of string parsed: extract XML or JSON content
#' @export
#' @examples
#'  list.results(paste0(
#'   "<HTML name='minimum'>minimum is 0.523431237543406 found at ...</HTML>",
#'   "<min> 0.523431237543406 </min>",
#'   "<argmin>[ 0.543459029033452,0.173028395040855 ]</argmin>"))
list.results = function(result) {
    all_results = xml2::xml_children(xml2::read_xml(paste0("<result>",result,"</result>")))
    result_list = list()
    for (a in all_results) {
        if (xml2::xml_name(a)=="HTML")
            result_list[[xml2::xml_name(a)]] = gsub("\"","\\\"",xml2::xml_text(a))
        else
            try({result_list[[xml2::xml_name(a)]] <- jsonlite::fromJSON(gsub("'","\\'",xml2::xml_text(a)))})
    }
    result_list
}

paste.XY = function(X,Y) {
    return(paste0("X=\n",
                  paste0(utils::capture.output(print(X)),collapse = "\n"),
                  "\nY=\n",
                  paste0(utils::capture.output(print(Y)),collapse = "\n")))
}

#' Apply a template algorithm file to an objective function
#'
#' @param algorithm_file templated algorithm file
#' @param objective_function function to apply algorithm on
#' @param input list of input arguments of function (eg. list(x1=list(min=0,max=1),x2=list(min=10,max=20)))
#' @param output list of output names
#' @param options algorithm options to overload default ones
#' @param work_dir working directory to run algorithm. will store output files, images, ..
#' @param overwrite if FALSE, stop with an error when work_dir already contains saved data (.Rds) from a previous run instead of silently overwriting it (default: TRUE)
#' @param trace display running info
#' @param silent quietness
#' @param save_data enable (by default) saving of data (in .Rds) along algorithm iterations.
#'
#' @return algorithm result (and algorithm object & files as attributes)
#' @export
#' @importFrom  utils capture.output
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
                          output=NULL,
                          options=NULL,
                          work_dir=".",
                          overwrite=TRUE,
                          trace=function(...) cat(paste0(...,"\n")),silent=FALSE,save_data=TRUE) {

    if (!is.function(trace)) trace = function(...){}
    if (!is.function(objective_function)) stop("objective_function must be a function")
    if (is.null(input) || length(input)==0) stop("input must be a non-empty list")

    if (is.null(output)) { # Use objective function name if no output arg provided
        output = utils::capture.output(print(match.call()))
        output = strsplit(output,"objective_function = ",fixed = T)[[1]][2]
        output = strsplit(output,",",fixed = T)[[1]][1]
    }

    trace(paste0("# Parsing code... (in ",algorithm_file, " from ",getwd(),")"))
    algorithm = NULL
    try(algorithm <- parse.algorithm(algorithm_file),silent = silent)
    if(is.null(algorithm)) {
        trace(traceback())
        stop("Error while parsing code")
    }
    
    prev.path = getwd()
    on.exit(setwd(prev.path))
    
    dir.create(work_dir, showWarnings = !silent)
    if (save_data && !overwrite) {
        existing_rds = list.files(work_dir, pattern="\\.Rds$")
        if (length(existing_rds)>0)
            stop(paste0("work_dir '",work_dir,"' already contains saved data (",paste0(collapse=", ",existing_rds),"). Use overwrite=TRUE to replace it, or choose another work_dir."))
    }
    setwd(work_dir)

    instance = NULL
    def_options=algorithm$options
    for (o in names(def_options)) def_options[[o]]=gsub("\\|.*","",def_options[[o]])
    for (o in names(options)) def_options[[o]]=options[[o]]
    options=def_options
    
    if (save_data) saveRDS(options,file.path(".",paste0("options.Rds")))
    
    trace("# Instanciating algorithm...")
    t0 = Sys.time() # time stamp to evaluate time between iterations
    try(instance <- algorithm$envir$new(options),silent = silent)
    t1 = Sys.time()-t0
    trace(paste0("                      ... in ",format(t1,digits=3)," s"))
    if(is.null(instance)) {
        trace(traceback())
        stop("Error while instanciating")
    }

    trace("# Initializing algorithm...")
    X0 = NULL
    t0 = Sys.time() # time stamp to evaluate time between iterations
    try(X0 <- algorithm$envir$getInitialDesign(instance, input, output),silent = silent)
    t1 = Sys.time()-t0
    trace(paste0("                      ... in ",format(t1,digits=3)," s"))
    if(is.null(X0)) {
        trace(traceback())
        stop("Error while computing getInitialDesign")
    }
    if (save_data) saveRDS(instance,file.path(".",paste0("algorithm_0.Rds")))
    
    if(!is.matrix(X0)) X0=as.matrix(X0,ncol=length(input),byrow = T)
    colnames(X0) <- names(input)
    if (save_data) saveRDS(X0,file.path(".",paste0("X_0.Rds")))
    if (!silent) trace(capture.output(print(X0)))

    F = function(X) {
        m = matrix(apply(X,1,objective_function),nrow=nrow(X),byrow=T);
        colnames(m)<-output;
        m
    }
    
    trace("Compute objective function")
    t0 = Sys.time() # time stamp to evaluate time between iterations
    Y0 = F(X0)
    t1 = Sys.time()-t0
    trace(paste0("                      ... in ",format(t1,digits=3)," s"))
    if (save_data) saveRDS(Y0,file.path(".",paste0("Y_0.Rds")))
    if (!silent) trace(capture.output(print(Y0)))

    Xi = X0
    Yi = Y0
    
    finished = FALSE
    i = 0
    while (!finished) {
        
        # Try temp analysis
        trace("# Display tmp results...")
        restmp = NULL
        t0 = Sys.time() # time stamp to evaluate time between iterations
        try(restmp <- algorithm$envir$displayResultsTmp(instance,Xi,Yi),silent = silent)
        t1 = Sys.time()-t0
        trace(paste0("                      ... in ",format(t1,digits=3)," s"))
        trace(restmp)
        if (save_data) if(!is.null(restmp)) saveRDS(restmp,file.path(".",paste0("resultstmp_",i,".Rds")))
        
        i = i + 1
        trace(paste0("# Iterating algorithm... ",i))
        err = NULL
        Xj = NULL
        t0 = Sys.time() # time stamp to evaluate time between iterations
        tryCatch(Xj <- algorithm$envir$getNextDesign(instance,Xi,Yi), error=function(e) {err <<- e; e})
        t1 = Sys.time()-t0
        trace(paste0("                      ... in ",format(t1,digits=3)," s"))
        if(!is.null(err)) {
            stop("Error while computing getNextDesign:\n",err,"\n with data:\n",paste.XY(Xi,Yi))
        }

        if (save_data) saveRDS(Xi,file.path(".",paste0("X_",i,".Rds")))
        if (save_data) saveRDS(Yi,file.path(".",paste0("Y_",i,".Rds")))
        if (save_data) saveRDS(instance,file.path(".",paste0("algorithm_",i,".Rds")))

        if (is.null(Xj) | any(is.na(Xj)) | any(is.nan(Xj)) | length(Xj) == 0) {
            finished = TRUE
        } else {
            trace("Compute objective function")
            t0 = Sys.time() # time stamp to evaluate time between iterations
            Yj = F(Xj)
            t1 = Sys.time()-t0
            trace(paste0("                      ... in ",format(t1,digits=3)," s"))
            Xi = rbind(Xi,Xj)
            Yi = rbind(Yi,Yj)

            if (!silent) trace(capture.output(print(Xi)))
            if (!silent) trace(capture.output(print(Yi)))
        }
    }
    
    trace("# Display results...")
    res = NULL
    t0 = Sys.time() # time stamp to evaluate time between iterations
    try(res <- algorithm$envir$displayResults(instance,Xi,Yi),silent = silent)
    t1 = Sys.time()-t0
    trace(paste0("                      ... in ",format(t1,digits=3)," s"))
    if(is.null(res)) {
        stop("Error while computing displayResults\n",paste.XY(Xi,Yi))
    }
    trace(res)
    if (save_data) saveRDS(res,file.path(".",paste0("results.Rds")))

    if (save_data) saveRDS(instance,file.path(".",paste0("algorithm.Rds")))

    setwd(prev.path)

    attr(res,"files")<-instance$files
    attr(res,"algorithm")<-instance
    return(res)
}