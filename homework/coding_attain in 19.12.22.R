### PIPELINE FROM UKBIOBANK
load("G:/phenotype estimation/F__phenotype estimation_trybd.Rdata")
x1807 <- irnt(trybd[,"f.1807.0.0"])
head(x1807)
irnt <- function(cts_variable) {
  set.seed(1234) # This is thQe same as was used by PHESANT - for checking.
  n_cts <- length(which(!is.na(cts_variable)))
  quantile_cts <- (rank(cts_variable, na.last = "keep", ties.method = "random") - 0.5) / n_cts
  # use the above to check, but also use frank for the real thing
  cts_IRNT <- qnorm(quantile_cts)	
  return(cts_IRNT)
}

variablelistfile <- fread("variable-info/outcome_info_final_pharma_nov2019.tsv")
datacodingfile <- read.table("variable-info/data-coding-ordinal-info-nov2019-update.txt")
### regular pattern
# > a <- "x2190_0_0"
# > gsub("^x", "", a)
# [1] "2190_0_0"
# > varx <- gsub("^x", "", a)
# > varx <- gsub("_[0-9]+$", "", varx)
# > varx
# [1] "2190_0"
# > varxShort <- gsub("^x", "", a)
# > varxShort
# [1] "2190_0_0"
# > varxShort <- gsub("_[0-9]+_[0-9]+$", "", varxShort)
# > varxShort
# [1] "2190"

##读取同一目录下的所有文件
path <- "F:/Rfile/OD-B/Data" ##文件目录
fileNames <- dir(path)  ##获取该路径下的文件名
filePath <- sapply(fileNames, function(x){ 
  paste(path,x,sep='/')})   ##生成读取文件路径
data <- lapply(filePath, function(x){
  read.csv(x, header=T)})  ##读取数据，结果为list
##从数据库中读取数据类似上面，获取要数据库里的文件名，写个正则筛选文件名后for循环读取。
outPath <- "F:/Rfile/OD-B/Consequence" ##输出路径
out_fileName <- sapply(names(data),function(x){
  paste(x, ".csv", sep='')}) ##csv格式
out_filePath  <- sapply(out_fileName, function(x){
  paste(outPath ,x,sep='/')}) ##输出路径名
##输出文件
for(i in 1:length(data)){
  write.csv(data[[i]], file=out_filePath[i], row.name=F) 
}


# getwd()
# [1] "C:/Users/Uwe/Documents/Rdevel/coverage"
# setwd("../stackoverflow/")   # .. means go up one directory level
# getwd()
# [1] "C:/Users/Uwe/Documents/Rdevel/stackoverflow"
# setwd("../..")   # go up two levels
# getwd()
# [1] "C:/Users/Uwe/Documents"
# getwd()
# [1] "C:/Users/Uwe/Documents"
# setwd("./Rdevel/")   # . denotes actual directory
# getwd()
# [1] "C:/Users/Uwe/Documents/Rdevel"
# getwd()
# [1] "C:/Users/Uwe/Documents"
# setwd("Rdevel")
# getwd()
# [1] "C:/Users/Uwe/Documents/Rdevel"


cor.test(x,y)$p.value
names(cor.test(x,y))
print.cor.test
