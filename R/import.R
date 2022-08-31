#' Dependencies loader, supports many protocols like github:, gitlab:, ... using remotes::instal_...
#' Will create a local '.lib' directory to store packages installed
#' 
#' @param ... dependencies/libraries/packages to load
#' @param lib.loc use to setup a dedicated libPath directory to install packages
#' @param trace display info
#'
#' @importFrom remotes install_github
#' @return (list of) load status of packages (TRUE/FALSE)
#' @export
#'
#' @examples
#' if(interactive()){
#'   import('VGAM')
#' }
import = function(..., 
                  lib.loc=NULL,
                  trace=function(...) cat(paste0(...,"\n"))) {
    if (!is.function(trace)) trace = function(...){}
    
    libs <- list(...)
    loaded = list() # for return
    if (length(libs)>0) {
        if (!all(is.na(libs))) {
            for (l in stats::na.exclude(unlist(libs))) {
                if (nchar(l)>0) { # else ignore & continue
                    trace(paste0("Import '",l,"'"))
                    
                    if (isTRUE(grep(":",l)==1)) { # GitHub or ...
                        src=gsub(":.*","",l)
                        n=gsub(".*/","",l)
                        path=gsub(".*:","",gsub("/[a-zA-Z0-9]*","",l))
                    } else { # CRAN
                        src=NULL
                        n=l
                        path=NULL
                    }
                    
                    in_base = F
                    try(in_base <- base::library(n,logical.return = T,character.only = T, quietly = T),silent = T)
                    if (!in_base) {
                        in_loc = F
                        try(in_loc <- base::library(n,logical.return = T,character.only = T, quietly = T,lib.loc = lib.loc) ,silent = T)
                        if (!in_loc) {
                            if (!is.null(src)) {
                                trace(paste0("  Using 'remotes' to install ",l))
                                if (!base::library("remotes",logical.return = T,character.only = T, quietly = T))
                                    import("remotes", lib.loc = lib.loc, trace=trace)
                                eval(parse(text=paste0("try(remotes::install_",src,"(file.path(path,n),force=T))")))
                            } else {
                                trace(paste0("  Using install.packages to install ",l))
                                try(utils::install.packages(l,lib = lib.loc,keep_outputs=T,dependencies=T),silent=F)
                            }
                            if (is.character(lib.loc)) 
                                trace(paste0("    Available packages in ",lib.loc,": ", paste0(collapse=", ",list.files(lib.loc))))
                        } else {
                            loaded[[l]] <- TRUE
                            if (is.character(lib.loc)) 
                                trace(paste0("  Loaded package ",l," from ",lib.loc,": ",paste0(collapse=", ",list.files(lib.loc))))
                        }
                        
                        try_load=F
                        try(try_load <- base::library(n,logical.return = T,character.only = T, quietly = T,lib.loc = lib.loc),silent = T)
                        if (!try_load) {
                            try(try_load <- base::library(n,logical.return = T,character.only = T, quietly = F,lib.loc = lib.loc),silent = F)
                            loaded[[l]] <- FALSE
                            stop(paste0("Cannot load package ",l,": ",paste0(collapse=", ",list.files(lib.loc))))
                        } else
                            loaded[[l]] <- TRUE
                    } else {
                        trace(paste0("Loaded package ",l," from ",paste0(collapse=", ",.libPaths())))
                        loaded[[l]] <- TRUE
                    }
                }
            }
            return(invisible(loaded))
        } else stop(paste0("No package to load"))
    } else stop(paste0("Empty list of package to load"))
}


