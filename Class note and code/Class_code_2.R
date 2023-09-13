
# Tyranny of fees

library(tidyverse)
jalsh <- fmxdat::Jalshtr

netofee <- function(jalsh, fee = 10*1e-4){
  
  jalsh %>% mutate(YM = format(date, "%Y%B")) %>% 
    group_by(YM) %>% filter(date == last(date)) %>% 
    ungroup() %>% arrange(date) %>% mutate(Returns = TRI / lag(TRI)-1) %>% 
    slice(-1) %>% mutate(Netofees = Returns - fee/12) %>% 
    rename( !!glue("Fee_{fee}") := Netofees) %>% select(date, glue("Fee_{fee}")) 
    
  # rename(!!paste(..) := colname)
}

netofee_withPerformancefee <- function(jalsh, fee = 10*1e-4, PF = 0.2){
  
  df1 <- jalsh %>% mutate(YM = format(date, "%Y%B")) %>% 
    group_by(YM) %>% filter(date == last(date)) %>% 
    ungroup() %>% arrange(date) %>% mutate(Returns = TRI / lag(TRI)-1)
  
    left_join(df1, 
              df1 %>% mutate(set.seed(123), rnum = rnorm(n(), 0.001,0.01)) %>% mutate(benchmark = Returns - rnum) %>% select(date, benchmark),
              by = "date"
              ) %>% 
      mutate(Netofees = Returns - ifelse(Returns > benchmark*0.2, fee/12 + PF * (Returns - benchmark), fee/12)) %>% 
      rename( !!glue("Fee_{fee}") := Netofees) %>% select(date, glue("Fee_{fee}")) 
    
  # rename(!!paste(..) := colname)
}

c(10*1e-4, 25*1e-4, 50*1e-4, 100*1e-4, 200*1e-4, 250*1e-4, 300*1e-4) %>% 
  as.list() %>% 
  map(~netofee(jalsh, fee = .)) %>% 
  reduce(left_join, by = "date") %>% gather(portfolio, return, -date) %>% 
  group_by(portfolio) %>% mutate(cp = cumprod(1+return)) %>% mutate(cp = cp / first(cp)) %>% 
  ungroup() %>% 
  ggplot() + geom_line(aes(date, cp, color = portfolio)) + 
  fmxdat::theme_fmx()




# ================================================================================
# ================================================================================

mutate(cp = cumprod(1+Returns)) %>% filter(date == last(date))
summarise(final_return = prod(1+Returns))
# Returns:
N = 36
rollret <- 
jalsh %>% mutate(YM = format(date, "%Y%B")) %>% 
  group_by(YM) %>% filter(date == last(date)) %>% 
  ungroup() %>% arrange(date) %>% mutate(Returns = TRI / lag(TRI)-1) %>% slice(-1) %>% 
mutate( Roll_Return = RcppRoll::roll_prod(1+Returns, n=N, fill = NA, align = "right") ^ (12/N)-1)

# Sanity:
# rollret %>% slice(1:36) %>% summarise(pp = prod(1+Returns)^(12/36)-1)
# rollret %>% filter(!is.na(Roll_Return))






# for capping:
x <- 10
while( x  >= 5) {
  print(x)
  x <- x - 1
  if(x == 5) message("Success")
}
fmxdat::DAX








