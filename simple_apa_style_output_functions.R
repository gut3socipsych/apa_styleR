####simple apa style output functions 

###comparing two means 
##t-tests 
#one-sample t-test 
intext_ttest_1 <- function(sample_data, mu){
  require(xtable)
  setClass(Class = "apa_output", slots = c(test = "ANY", text = "character", table = "ANY"))
  options(xtable.comment = F)
  #run t-test using t.test function from stats package 
  test <- t.test(x = sample_data, mu = mu)
  #create results dataframe based on htest object, mean, and sd result and calculate cohen's d
  result_df <- data.frame(M = mean(sample_data, na.rm = T), 
                          SD = sd(sample_data, na.rm = T),
                          df = length(sample_data)-1,
                          t = test$statistic, 
                          p = test$p.value, 
                          d = abs((mean(sample_data, na.rm = T)-mu)/sd(sample_data)))
  #create in-text output 
  text <- sprintf("M = %.3f, SD = %.3f, t(%s) = %.3f, p = %.3f, d = %.3f", 
                  result_df$M, result_df$SD, result_df$df, result_df$t, result_df$p, result_df$d)
  table <- xtable(result_df, row.names = "samples", comment = F)
  return(new("apa_output", test = test, text = text, table = table))
}









