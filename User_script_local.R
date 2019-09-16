

# 
user <- switch(
  Sys.info()["nodename"], 
  "SWC-KFORNEY-L" = "KAF", 
  "DESKTOP-MU7J614" = "EAB",
  "SWC-SWOODMAN-L" = "SMW"
)