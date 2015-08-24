source("helpers.R")

sidos = readRDS("data/sido.rds")
guguns = readRDS("data/gugun.rds")
dongs = readRDS("data/dong.rds")

f_crawler(2006, 2015, "r", f_getRent)
f_crawler(2006, 2015, "t", f_getTrade)
