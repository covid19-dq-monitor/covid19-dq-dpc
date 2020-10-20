# 02_apply_checks
# read rules from yaml

rules_delta <- validator(.file="rules.yaml" )
delta_today <- filter(delta, data == max(data))
results_delta <- (confront(delta_today, rules_delta))
nfails <- sum(summary(results_delta)$fails)
if(nfails == 0) {
  centro <- "Nessun check ha dato errore."
} else if (nfails == 1) {
  centro <- paste(nfails, "check ha dato errore!")
} else {
  centro <- paste(nfails, "check hanno dato errore!")
}
