# 04_tweet report
# upload_report to pastebin
library(RCurl)
library(rtweet)
library(gridExtra)


string <- readr::read_file(filename)
api_dev_key <- "NiorPulLlq9Hxp_DmnjAjdhXrJLgTrt_"
pbPost <- function(text = "hello world!", title = "Untitled", 
                   user_key = "", expire = "10M"){
  expiry <- c("N", "10M", "1H", "1D", "1W", "2W", "1M")
  if(!expire %in% expiry) stop("Expiration time not valid - check pastebin.com/api")
  if(user_key %in% "") password <- ""
  # code creates pastebin post(s) based on input text
  # this code requires a developer key! A user key is optional
  # posts are public by default
  # posts as guest by default - otherwise, 
  # api_user_password needs to be defined before using code
  pasteList <- NA
  for(i in 1:length(text)){
    post <- postForm(uri = 'https://pastebin.com/api/api_post.php',
                     api_option = "paste",
                     api_dev_key = api_dev_key,
                     api_paste_private = '1',
                     api_paste_expire_date = '2W',
                     api_paste_format = '',
                     api_user_key = '',
                     api_paste_code = text[i]
    )
    pasteList <- c(pasteList, post[1])
  }
  pasteList[!is.na(pasteList)]
}
p <- pbPost(string)

salutation <- "Buonasera! Di seguito i risultati del report odierno."
link <- paste("I risultati sono scaricabili da", p)
ttweet <- paste(salutation, centro, link)
ttweet

if(exists("latest")){
  png("/tmp/latest.png", width=480,height=480,bg = "white")
  grid.table(latest)
  dev.off()
  
  post_tweet(ttweet, media ="/tmp/latest.png")
} else {
  post_tweet(ttweet)
}
