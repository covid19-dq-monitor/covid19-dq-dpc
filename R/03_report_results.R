# 03_report_results
require(googlesheets)
options(width = 5000)
mat <- !values(results_delta)[[1]]


filename <- paste0("output/report/", Sys.Date(),"_dpc_dq_report.txt" )

sink(file = filename,  type = "output")

cat("\n##############################################################\n")
cat("##################  SUMMARY  #################################")
cat("\n##############################################################\n")
cat("\n\n")
result_delta_print <- results_delta$._value
print(summary(results_delta))

x <- apply(mat, 2, FUN = function(x){
  if(sum(x) > 0) {
    (`[`(as.data.frame(delta_today), x, ))
    }
  }
  )
cat("\n\n")
cat("\n##############################################################\n")
cat("##################  DETAILS  #################################\n")
cat("##############################################################\n")
cat("\n\n")


for(i in 1:length(x)){ 
  if(is.data.frame(x[[i]])) {
    cat(paste("Details: ------ Check", names(x)[i], "--------\n\n"))
    print(x[[i]])
    cat("------------------------------------------------\n")
  }}

sink()
