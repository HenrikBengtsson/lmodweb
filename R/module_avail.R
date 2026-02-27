#' Get All Environment Modules in a Module Path
#'
#' @param info ...
#'
#' @param distros (optional) Character vector of Linux-distribution specific software to list.
#'
#' @param onMissingPath ...
#'
#' @param force If TRUE, cached results are ignored.
#'
#' @return ...
#'
#' @importFrom utils file_test
#' @importFrom R.utils mstr
#' @importFrom jsonlite fromJSON
#' @importFrom utils file_test
#' @importFrom gtools mixedorder
#' @export
module_avail <- local({
  ## Memoization
  .cache <- list()
  
  function(info, distros = character(0L), onMissingPath = getOption("onMissingPath", c("okay", "error", "warning", "ignore")), force = FALSE) {
    ## Always sort under the 'C' locale
    old_collate <- Sys.getlocale("LC_COLLATE")
    on.exit(Sys.setlocale("LC_COLLATE", old_collate))
    Sys.setlocale("LC_COLLATE", "C")
    
    stopifnot(is.list(info), "module_path" %in% names(info))
    onMissingPath <- match.arg(onMissingPath)
    stopifnot(length(force) == 1L, is.logical(force), !is.na(force))

    module_path <- info$module_path
    if (all(!nzchar(module_path))) {
      stop("Specified empty folder(s): ", paste(sQuote(module_path), collapse = ", "))
    }

    message("module_avail() ...", appendLF = FALSE)
    
    key <- paste(module_path, collapse = ", ")
    if (!force && !is.null(res <- .cache[[key]])) {
      message("already cached")
      return(res)
    }

    message(""); mstr(list(info = info))
    
    if (onMissingPath != "okay") {
      unknown <- module_path[!file_test("-x", module_path)]
      if (length(unknown) > 0) {
        msg <- sprintf("No such folder(s): %s", paste(sQuote(unknown), collapse = ", "))
        if (onMissingPath == "error") stop(msg)
        if (onMissingPath == "warning") warning(msg)
        return(NULL)
      }
    }
    
    json <- spider(module_path, force = force)
    x <- fromJSON(json)

    ## SPECIAL: Handle Linux-distribution-specific modules
    if (length(distros) > 0) {
      pattern <- "_[[:alpha:]]+[[:digit:]_]*/"
      idxs <- grep(pattern, x$package)
      keep_pattern <- sprintf("_(%s)/", paste(distros, collapse = "|"))
      keep <- grepl(keep_pattern, x$package[idxs])
      ## Hide the ones for non-wanted distros by setting their versions
      ## to empty list (see below)
      for (idx in idxs[!keep]) x$versions[[idx]] <- list()
    }
    
    ## (b) Drop Linux distribution prefixes, e.g. _centos7 and _rocky8
    x$package <- sub("_[[:alpha:]]+[[:digit:]_]*/", "", x$package)

    ## (c0) Fix invalid versions$parent data frames
    x$versions <- lapply(x$versions, function(version) {
      parent <- version$parent
      if (is.data.frame(parent)) {
        if (ncol(parent) == 0L) version$parent <- vector("list", nrow(parent))
      }
      version
    })

    ## (c) Merge
    t <- table(x$package)
    t <- t[t > 1]
    for (pkg in names(t)) {
      idxs <- which(x$package == pkg)
      versions <- do.call(rbind, x$versions[idxs])
      
      ## Sort (because now versions might be out of order)
      ## WORKAROUND: replace . with _ to get 1.9 get before 1.10
      o <- mixedorder(gsub(".", "_", versions$versionName, fixed = TRUE))
      versions <- versions[o, ]
      x$versions[[idxs[1]]] <- versions

      ## Find (maximum) default version
      ## WORKAROUND: replace . with _ to get 1.9 get before 1.10
      default <- x$defaultVersionName[idxs]
      ## Ignore hidden modules
      default <- grep("^[^.]", default, value = TRUE)
      if (length(default) > 1L) {
        o <- mixedorder(gsub(".", "_", default, fixed = TRUE))
        default <- default[o]
        default <- default[length(default)]
      }	
      x$defaultVersionName[[idxs[1]]] <- default

      ## Hide the already merged ones by setting their versions to an
      ## empty list (see below)
      for (idx in idxs[-1]) x$versions[[idx]] <- list()
    }
    
    ## Mixed sort by module name, e.g. miniconda3-py39 < miniconda3-py310
    ## WORKAROUND: Make 'bowtie' come before 'bowtie2'
    tweaked_names <- paste0(x$package, "0")
    o <- mixedorder(tweaked_names)
    
    x <- x[o,]
    keep <- !grepl("^[.]", x$package)
    x <- x[keep,]
    message("done")


    message("Prune ...")
    versions <- x$versions
    versions <- lapply(versions, FUN = function(version) {
      path <- version$path
      ## WORKAROUND
      keep <- lapply(module_path, FUN = function(mpath) grepl(mpath, path, fixed = TRUE))
      keep <- Reduce(`|`, keep)
      if (!all(keep)) {
        version <- version[keep, ]
        path <- path[keep]
	version$path <- path
      }
      version
    })

    ## Drop hidden modules
    ns <- vapply(versions, FUN.VALUE = NA_integer_, FUN = function(version) {
      ## Hidden modules have version == list()
      if (is.null(dim(version))) 0L else nrow(version)
    })
    stopifnot(is.numeric(ns), !anyNA(ns))
    x <- x[ns > 0, ]

    attr(x, "info") <- info
    message("done")

    .cache[[key]] <<- x

    x
  }
})
