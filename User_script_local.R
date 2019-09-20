# Set user object based on computer signature
#   User path used for specifying which files paths to use

user <- switch(
  Sys.info()["nodename"], 
  "SWC-KFORNEY-L" = "KAF", 
  "DESKTOP-MU7J614" = "EAB",
  "SWC-SWOODMAN-L" = "SMW"
)