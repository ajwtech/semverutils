library(R6)
#'semVer is a class to create an object for working with Semantic versioning.
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @keywords data
#' 
semVer <- R6Class("semVer",
                  public = list(
                    higherThanAll =  function(versions) {
                    semVer200Parser <- "^(?P<major>0|[1-9]\\d*)\\.(?P<minor>0|[1-9]\\d*)\\.(?P<patch>0|[1-9]\\d*)(?:-(?P<prerelease>(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\.(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\\+(?P<build>[0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?$"
                    parsedVersion <- regexpr(semVer200Parser, versions, perl = TRUE)
                    if(-1 %in% parsedVersion){warning("Not all of the strings passed are compliant with Semantic Versioning")}
                    #Run the comp, return  True or false
                    if(private$Version %in% versions){
                      return(FALSE)
                    }else{
                      if(private$Version == sort(unlist(c(private$Version,versions)),TRUE)[1]){
                        return(TRUE)
                      }else{
                        return(FALSE)
                      }
                      
                    }
                  },
                  SemVerVersion = "2.0.0",
                  initialize = function( Version = "", SemVerVersion = ""){
                          if(Version == ""){
                            private$Version <- "0.0.0"
                          }else{
                            self$version <- Version
                            
                          }
                          if(SemVerVersion == ""){
                            self$SemVerVersion <- "2.0.0"
                          }else{
                            self$SemVerVersion <- SemVerVersion
                          }
                          #Add call to Parse my version string
                        }),
                  active = list(
                    
                    version = function(version = private$Version){
                          if (version == private$Version){return(private$Version)}
                      
                          if (self$SemVerVersion %in% c("2.0.0","")){
                            semVer200Parser <- "^(?P<major>0|[1-9]\\d*)\\.(?P<minor>0|[1-9]\\d*)\\.(?P<patch>0|[1-9]\\d*)(?:-(?P<prerelease>(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\.(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\\+(?P<build>[0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?$"
                            parsedVersion <- regexpr(semVer200Parser, version, perl = TRUE)
                            
                            if (parsedVersion == -1){
                              warning(paste(version, "is not compliant with Semantic Versioning. No changes made"))
                              return()
                            }else{
                              private$Version <- version
                              private$Major <- substr(version, 
                                               attributes(parsedVersion)$capture.start[,"major"], 
                                               attributes(parsedVersion)$capture.start[,"major"] + attributes(parsedVersion)$capture.length[,"major"]-1)
                              private$Minor <- substr(version, 
                                               attributes(parsedVersion)$capture.start[,"minor"], 
                                               attributes(parsedVersion)$capture.start[,"minor"] + attributes(parsedVersion)$capture.length[,"minor"]-1)
                              private$Patch <- substr(version, 
                                               attributes(parsedVersion)$capture.start[,"patch"], 
                                               attributes(parsedVersion)$capture.start[,"patch"] + attributes(parsedVersion)$capture.length[,"patch"]-1)
                              private$PreRelease <- substr(version, 
                                                    attributes(parsedVersion)$capture.start[,"prerelease"], 
                                                    attributes(parsedVersion)$capture.start[,"prerelease"] + attributes(parsedVersion)$capture.length[,"prerelease"]-1)
                              private$Build <- substr(version, 
                                               attributes(parsedVersion)$capture.start[,"build"], 
                                               attributes(parsedVersion)$capture.start[,"build"] + attributes(parsedVersion)$capture.length[,"build"]-1)
                            }
                            return(private$Version)
                          }
                        },
                    getMajor = function(){return(private$Major)},
                    getMinor = function(){return(private$Minor)},
                    getPatch = function(){return(private$Patch)},
                    getPreRelease = function(){return(private$PreRelease)},
                    getBuild = function(){return(private$Build)}
                        ),
                  private = list(Version = "character",
                                 Major = "numeric",
                                 Minor = "numeric",
                                 Patch = "numeric",
                                 PreRelease = "character",
                                 Build = "numeric")
                      )