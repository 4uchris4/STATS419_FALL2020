
library(tidyverse);
library(pryr);
library(parallel);
library(stringr)
library(rvest)

transposeMatrix = function(mat)
{
	t(mat)
}
rotateMatrix90 = function(mat)
{
  t(apply(mat, 2, rev))
}
rotateMatrix180 = function(mat)
{
  rotateMatrix90(mat)
  rotateMatrix90(mat)
}
rotateMatrix270 = function(mat)
{
  rotateMatrix180(mat)
  rotateMatrix90(mat)
}
