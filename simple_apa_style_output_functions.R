####simple apa style output functions 

###comparing two means 
##t-tests 
#one-sample t-test 
intext_ttest_1 <- function(sample_data, mu){
  setClass(Class = "apa_output", slots = c(test = "ANY", text = "character"))
  rounded_digits = 3
  test <- t.test(x = sample_data, mu = mu)
  text <- sprintf("M = %s, SD = %s, t(%s) = %s, p = %s, d = %s",
                  round(mean(sample_data), digits = rounded_digits), 
                  round(sd(sample_data), digits = rounded_digits),
                  length(sample_data)-1,
                  round(test$statistic, digits = rounded_digits),
                  round(test$p.value, digits = rounded_digits), 
                  round(abs((mean(sample_data)-mu)/sd(sample_data)), digits = rounded_digits))
  return(new("apa_output", test = test, text = text))
}








