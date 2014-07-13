x = 5; y = "indeed"
message("you entered ", dQuote(x))
message("you entered ", dQuote(paste(x, y)))
z="Error in best("
st="BB"
aq=", "
bt="Stroke"
z1=") : invalid state"
options(useFancyQuotes = FALSE)
message(z,dQuote(st),aq,dQuote(bt),z1)
