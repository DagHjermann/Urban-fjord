
#
# Problematic space characters in Oracle
#
# Probably caused by reading data from Excel in Linux (Jupyterhub)  
#

library(dplyr)
library(niRvana)

sql <- "select * from NIVADATABASE.METHOD_DEFINITIONS where rownum < 5;"
sql <- "select count(*) from NIVADATABASE.METHOD_DEFINITIONS;"
get_nivabase_data(sql)

# Define spaces
space_nobreak <- charToRaw("\xc2\xa0") %>% rawToChar()
space_normal <- charToRaw("\x20") %>% rawToChar()

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Get data added by Benno 6.11  
#
# No-breaking space (UTF-8 hex code: c2 a0) in 3 NAME and 13 UNIT  
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

sql <- "select * from NIVADATABASE.METHOD_DEFINITIONS where TRUNC(ENTERED_DATE) = TO_DATE('2023-11-06', 'yyyy-mm-dd')"
df1 <- get_nivabase_data(sql)
df1

# Check one
df1$NAME[5]
charToRaw(df1$NAME[5])
substr(df1$NAME[5], 4, 4) %>% charToRaw()
# c2 a0

# Search for non-breaking space
sel <- grepl(space_nobreak, df1$NAME)
sel   # 3 cases
df1$NAME[sel]

# Search for non-breaking space
sel <- grepl(space_nobreak, df1$UNIT)
sum(sel)  # 13
df1$UNIT[sel]

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# In all NAME in entire METHOD DEFINITIONS
#
# Turns out that there are no more use of non-breaking space than the ones found above 
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Find all NAME with "normal" space - works 
sql <- "select * from NIVADATABASE.METHOD_DEFINITIONS where NAME like '% %';"
df2 <- get_nivabase_data(sql)
nrow(df2)  # 4286

# Find all NAME with "normal" space, method 2 - works 
sql <- paste0("select METHOD_ID, NAME from NIVADATABASE.METHOD_DEFINITIONS where NAME like '%", space_normal, "%';")
df2 <- get_nivabase_data(sql)
nrow(df2)  # 4286
  
# Find all NAME with "no-breaking" space - doesn't work   
sql <- paste0("select METHOD_ID, NAME from NIVADATABASE.METHOD_DEFINITIONS where NAME like '%", space_nobreak, "%';")
df3 <- get_nivabase_data(sql)
nrow(df3)  # 0

# Get all NAME and UNIT     
sql <- "select METHOD_ID, NAME, UNIT from NIVADATABASE.METHOD_DEFINITIONS;"
df4 <- get_nivabase_data(sql)
nrow(df4)  # 18451

# Search for non-breaking space among all of these
sel <- grepl(space_nobreak, df4$NAME)
sum(sel)  # 3
df4$NAME[sel]

# Search for non-breaking space among all of these
sel <- grepl(space_nobreak, df4$UNIT)
sum(sel)  # 13



