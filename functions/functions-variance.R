
library(tidyverse);
library(pryr);
library(parallel);
library(stringr)
library(rvest)
sampleVariance = function(entry, method)
{
	if(method=="naive")
	{
	  n=sum=sumSq=0
	 
	  for(item in entry)
	  {
	    n <- n+1
	    sum <- sum+item
	    sumSq <- sumSq + item*item
	  }
	  result <- (sumSq-(sum*sum)/n)/(n-1)
	}
  else #two-pass algorithm
  {
    n=sum1=sum2=0
    for(item in entry)
    {
      n <- n+1
      sum1 <- sum1+item
    }
    mean = sum1/n
    for(item in entry)
    {
      sum2 <- sum2 + (x-mean)*(x-mean)
    }
    result = sum2/(n-1)
  }
  result;
}
doMode = function(entry)
{
	 Mode <- unique(entry)
   Mode[which.max(tabulate(match(entry, Mode)))]
}
doSummary = function(entry)
{
	length(entry)
  sum(is.na(entry))
  mean(entry,na.rm = TRUE)
  median(entry)
  doMode(entry)
  sv <- sampleVariance(entry, "naive")
  sd <- sqrt(sv)
}