#' Dependencies loader, supports many protocols like github:, gitlab:, ... using remotes::instal_...
#' Will create a local '.lib' directory to store packages installed
#' 
#' @param ... dependencies/libraries/packages to load
#' @param lib.loc use to setup a dedicated libPath directory to install packages
#' @param force if TRUE, (re)install the package even if a version is already loadable, overwriting any previous install
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
                  force=FALSE,
                  trace=function(...) cat(paste0(...,"\n"))) {
    if (!is.function(trace)) trace = function(...){}

    # Checks whether the version of an already loadable package matches a requested tag/version
    version_matches_tag = function(name, tag, lib.loc) {
        if (is.null(tag)) return(TRUE) # no specific version requested: anything loadable is fine
        v = tryCatch(as.character(utils::packageVersion(name, lib.loc = lib.loc)), error = function(e) NA)
        if (is.na(v)) return(FALSE)
        identical(v, gsub("^v","",tag))
    }

    if (is.character(lib.loc) && !dir.exists(lib.loc)) {
        trace(paste0("Creating library directory ",lib.loc))
        dir.create(lib.loc, recursive = TRUE)
    }

    libs <- list(...)
    loaded = list() # for return
    failed = character(0) # names of packages that failed to load, checked after the loop
    if (length(libs)>0) {
        if (!all(is.na(libs))) {
            for (l in stats::na.exclude(unlist(libs))) {
                if (nchar(l)>0) { # else ignore & continue
                    trace(paste0("Import '",l,"'"))
                    
                    # CRAN
                    src=NULL
                    name=l
                    path=NULL
                    tag=NULL
                    if (isTRUE(grep(":",l)==1)) { # GitHub or ...
                        src=gsub(":.*","",l)
                        name=gsub(".*/","",l)
                        path=gsub(".*:","",gsub("/.*","",l))
                        if (grepl("@",name)) {
                            tag=gsub(".*@","",name)
                            name=gsub("@.*","",name)
                        }
                    }
                    
                    trace(paste0("  Package name: ",name))
                    trace(paste0("  Source: ",ifelse(is.null(src),"CRAN",src)))
                    trace(paste0("  Path: ",ifelse(is.null(path),"N/A",path)))
                    trace(paste0("  Tag: ",ifelse(is.null(tag),"N/A",tag)))

                    
                    in_base = FALSE
                    if (!force) try(in_base <- base::library(name,logical.return = TRUE,character.only = TRUE, quietly = TRUE),silent = TRUE)
                    if (isTRUE(in_base) && !version_matches_tag(name, tag, NULL)) {
                        trace(paste0("  Installed version of ",name," does not match requested tag '",tag,"', reinstalling"))
                        in_base = FALSE
                    }
                    if (isFALSE(in_base)) {
                        in_loc = FALSE
                        if (!force) try(in_loc <- base::library(name,logical.return = TRUE,character.only = TRUE, quietly = TRUE,lib.loc = lib.loc) ,silent = TRUE)
                        if (isTRUE(in_loc) && !version_matches_tag(name, tag, lib.loc)) {
                            trace(paste0("  Installed version of ",name," does not match requested tag '",tag,"', reinstalling"))
                            in_loc = FALSE
                        }
                        if (isFALSE(in_loc)) {
                            if (!is.null(src)) {
                                trace(paste0("  Using 'remotes' to install ",name))
                                if (!base::library("remotes",logical.return = TRUE,character.only = TRUE, quietly = TRUE))
                                    import("remotes", lib.loc = lib.loc, trace=trace)
                                eval(parse(text=paste0("try(remotes::install_",src,"(file.path(path,paste0(name, ifelse(is.null(tag), '', paste0('@', tag)))),force= TRUE))")))
                            } else {
                                trace(paste0("  Using install.packages to install ",name))
                                try(utils::install.packages(l,lib = lib.loc,keep_outputs= TRUE,dependencies= TRUE),silent=FALSE)
                            }
                            if (is.character(lib.loc)) 
                                trace(paste0("    Available packages in ",lib.loc,": ", paste0(collapse=", ",list.files(lib.loc))))
                        } else {
                            loaded[[name]] <- TRUE
                            if (is.character(lib.loc)) 
                                trace(paste0("  Loaded already available package ",name," from ",lib.loc,": ",paste0(collapse=", ",list.files(lib.loc))))
                        }
                        
                        try_load=FALSE
                        try(try_load <- base::library(name,logical.return = TRUE,character.only = TRUE, quietly = TRUE,lib.loc = lib.loc),silent = TRUE)
                        if (isFALSE(try_load)) {
                            try(try_load <- base::library(name,logical.return = TRUE,character.only = TRUE, quietly = FALSE,lib.loc = lib.loc),silent = F)
                            loaded[[name]] <- FALSE
                            failed <- c(failed, name)
                            trace(paste0("  Cannot load package ",name,": ",paste0(collapse=", ",list.files(lib.loc))))
                        } else
                            loaded[[name]] <- TRUE
                    } else {
                        trace(paste0("Loaded package ",name," from ",paste0(collapse=", ",.libPaths())))
                        loaded[[name]] <- TRUE
                    }
                }
            }
            if (length(failed)>0)
                stop(paste0("Cannot load package(s): ",paste0(collapse=", ",failed)))
            return(invisible(loaded))
        } else stop(paste0("No package to load"))
    } else stop(paste0("Empty list of package to load"))
}


