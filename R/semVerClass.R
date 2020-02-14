#' semVer is an R6 class to create an object for working with Semantic
#' versioning
#' @export
semVer <- R6::R6Class("semVer",
  public = list(

    #' @description
    #' \code{higherThanAll()} test if the vector of versions passed in are
    #' all lower than this object
    #' @param Versions character vector of semantically formatted
    #' version numbers
    #' @importFrom foreach foreach %do%
    #' @examples
    #' sem <- semVer$new("3.0.0")
    #' sem$higherThanAll(c("2.0.2", "0.1.0"))
    #' @return atomic boolean
    higherThanAll = function(Versions) {
      parsedVersion <- regexpr(private$SemVer200Parser, Versions, perl = TRUE)
      if (-1 %in% parsedVersion) {
        warning("Not all of the strings passed are compliant with Semantic Versioning")
      }
      # Run the comp, return  True or false
      if (private$Version %in% Versions) {
        return(FALSE)
      } else {

        higher <- foreach::foreach(v = Versions, .packages="foreach") %do% {
          ver <- semVer$new(v)
          if(private$Major > ver$getMajor){
           return(TRUE)
          }else{
            if(private$Major == ver$getMajor){
              if(private$Minor > ver$getMinor){
                return(TRUE)
              }else{
                if(private$Minor == ver$getMinor){
                  if(private$Patch > ver$getPatch ){
                    return(TRUE)
                  }else{
                    if(private$Patch == ver$getPatch){
                      #at this point the versions are exactly the same
                      #need to look at dot separated prerelease info
                      comps <- suppressWarnings(strsplit(
                        private$PreRelease,split = ".",
                        fixed = TRUE)[[1]] > strsplit(
                          ver$getPreRelease,split = ".",
                          fixed = TRUE)[[1]])
                      if (any(comps)){
                        return(TRUE)
                      }else{
                       #check to see if the length of one is longer
                        if(length(private$PreRelease > length(ver$getPreRelease))){
                          return(TRUE)
                        }
                      }
                    }
                  }
                }
              }
            }
          }
          return(FALSE)
        }
        if(FALSE %in% higher) return(FALSE)

        return(TRUE)
      }
    },
    #' @field SemVerVersion version of semantic versioning rules to be followed
    #' currently only version 2.0.0 is supported.
    SemVerVersion = "2.0.0",
    #' @description
    #' initializes a new semVer object with the version supplied
    #' @param Version character of semantically formatted version numbers
    #' @param SemVerVersion defaults to "2.0.0" no other options currently
    #' available.
    #' @examples
    #' sem <- semVer$new("3.0.0")
    #' @return semVer object holding version data
    initialize = function(Version = "", SemVerVersion = "2.0.0") {
      if (Version == "") {
        private$Version <- "0.0.0"
      } else {
        self$version <- Version
      }
      if (SemVerVersion == "") {
        self$SemVerVersion <- "2.0.0"
      } else {
        self$SemVerVersion <- SemVerVersion
      }
      # Add call to Parse my version string
    }
  ),
  active = list(
    #' @description
    #' Sets or gets the version of the object.
    #' @param Version character of semantically formatted version numbers
    #' @examples
    #' sem <- semVer$ini("3.0.0")
    #' sem$version
    #' sem$version <- "3.0.0-Alpha.Release+build.297"
    #'
    #' @return current version
    version = function(version = private$Version) {
      if (version == private$Version) {
        return(private$Version)
      }

      if (self$SemVerVersion %in% c("2.0.0", "")) {
        parsedVersion <- regexpr(private$SemVer200Parser, version, perl = TRUE)

        if (parsedVersion == -1) {
          warning(paste(version, "is not compliant with Semantic Versioning. No changes made"))
          return()
        } else {
          private$Version <- version
          private$Major <- substr(
            version,
            attributes(parsedVersion)$capture.start[, "major"],
            attributes(parsedVersion)$capture.start[, "major"] + attributes(parsedVersion)$capture.length[, "major"] - 1
          )
          private$Minor <- substr(
            version,
            attributes(parsedVersion)$capture.start[, "minor"],
            attributes(parsedVersion)$capture.start[, "minor"] + attributes(parsedVersion)$capture.length[, "minor"] - 1
          )
          private$Patch <- substr(
            version,
            attributes(parsedVersion)$capture.start[, "patch"],
            attributes(parsedVersion)$capture.start[, "patch"] + attributes(parsedVersion)$capture.length[, "patch"] - 1
          )
          private$PreRelease <- substr(
            version,
            attributes(parsedVersion)$capture.start[, "prerelease"],
            attributes(parsedVersion)$capture.start[, "prerelease"] + attributes(parsedVersion)$capture.length[, "prerelease"] - 1
          )
          private$Build <- substr(
            version,
            attributes(parsedVersion)$capture.start[, "build"],
            attributes(parsedVersion)$capture.start[, "build"] + attributes(parsedVersion)$capture.length[, "build"] - 1
          )
        }
        return(private$Version)
      }
    },
    #' @description
    #' Gets the perl style regular expression for parsing semantic version numbers
    #' @return regular expression
    getV2Parser = function() {
      return(private$SemVer200Parser)
    },
    #' @description
    #' Gets the major portion of the version number
    #' @return Major Version Number
    getMajor = function() {
      return(private$Major)
    },
    #' @description
    #' Gets the minor portion of the version number
    #' @return Minor Version Number
    getMinor = function() {
      return(private$Minor)
    },
    #' @description
    #' Gets the Patch portion of the version number
    #' @return Patch Version Number
    getPatch = function() {
      return(private$Patch)
    },
    #' @description
    #' Gets the prerelease portion of the version number
    #' @return prelease Version string
    getPreRelease = function() {
      return(private$PreRelease)
    },
    #' @description
    #' Gets the Build portion of the version number
    #' @return Build Version string
    getBuild = function() {
      return(private$Build)
    }
  ),
  private = list(
    SemVer200Parser = "^(?P<major>0|[1-9]\\d*)\\.(?P<minor>0|[1-9]\\d*)\\.(?P<patch>0|[1-9]\\d*)(?:-(?P<prerelease>(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\.(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\\+(?P<build>[0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?$",
    Version = "character",
    Major = "numeric",
    Minor = "numeric",
    Patch = "numeric",
    PreRelease = "character",
    Build = "numeric"
  )
)
