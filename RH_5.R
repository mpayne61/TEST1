
rankall <- function(outcome,rank1)
{
con <<- gsub(" ", "", outcome, fixed = TRUE) 
num <<- rank1  
  
out <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
out1 <- subset(out, select = c(2,7,11,17,23)) 

out2 <- transform(out1,"heartattack" = as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),
"heartfailure" = as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),"pneumonia" = as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)) 
out3 <- subset(out2,select = c(1,2,6,7,8))


if (con == "heartattack")
{
out4 <- out3[,c(1,2,3)]
} else if (con == "heartfailure")
{
out4 <- out3[,c(1,2,4)]
} else if (con == "pneumonia")
{
out4 <- out3[,c(1,2,5)]
} else 
{
out4 <- data.frame()
}

if(nrow(out4) == 0) { 
  stop("invalid outcome") 
}

#out5 <- out4[complete.cases(out4),]
out5 <- out4
out6 <- out5[ order(out5[,2], out5[,3], out5[,1]),]
out7 <- transform(out6, rank= ave(out6[,3],out6[,2],FUN=function(rank) order(rank,decreasing=F))) 
maxr <- as.numeric(apply(out7[4],2,max))

#to here

if (rank1 == "worst")
{
out71 <- aggregate(out7$rank~out7$State, data = out7, FUN = max)
colnames(out71) <- c("State","rank")
out72 <- merge(out71, out7)
out8 <- data.frame(out72[,c(1,2,3,4)])
}else if (rank1 == "best")
{
out71 <- aggregate(out7$rank~out7$State, data = out7, FUN = min)
colnames(out71) <- c("State","rank")
out72 <- merge(out71, out7)
out8 <- data.frame(out72[,c(1,2,3,4)])
}else if (rank1 <= maxr)
{
out8 <- out7[out7$rank == rank1,]
}else  
{
out8 <- data.frame(t(c(NA)))  
}

if (nrow(out8) == 0) { 
  stop("invalid outcome") 
}


if (con == "heartattack")
{
  out8[(out8$heartattack %in% c(NA)),]$Hospital.Name = NA 
} else if (con == "heartfailure")
{
  out8[(out8$heartfailure %in% c(NA)),]$Hospital.Name = NA 
} else if (con == "pneumonia")
{
  out8[(out8$pneumonia %in% c(NA)),]$Hospital.Name = NA 
} else 
{
  out8 <- data.frame()
}








out9 <- out8[,c(2,1,2)]
row.names(out9)<-NULL
print(out9)
}







