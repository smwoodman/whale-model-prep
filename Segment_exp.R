x <- read.csv("../whale-model-prep_data/Segments/LgWhale_CCE_91_14_3km_Segs_BF0_6.csv")

range(x$mlon)
range(x$mlat)


table(x$year)
# 1991 1993 1996 2001 2005 2008 2009 2014 
# 3569 2254 5314 3549 4465 4266 1547 3645 

table(x$month)
# 6    7    8    9   10   11   12 
# 285 1751 7102 8463 5997 4146  865