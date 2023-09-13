library(tidyverse)
rets <- fmxdat::SP500 %>% mutate(Return = SP500 / lag(SP500)-1) %>% select(-SP500)

qe <- 
  # rmsfuns::dateconverter(StartDate = first(rets$date), EndDate = last(rets$date), Transform = "weekdayEOQ")
  rets %>% mutate(YM = format(date, "%B%Y")) %>% group_by(YM) %>% filter(date == last(date)) %>% 
  mutate( month = format(date, "%b")) %>% filter(month %in% c("Dec", "Mar", "Jun", "Sep") ) %>% pull(date)

# rets %>% mutate(MY = format(date, "%b - '%y"))

rets %>% 
  mutate(Return = coalesce(Return, 0)) %>% 
  arrange(date) %>% 
  mutate(Index = cumprod(1+Return)) %>% 
  filter(date %in% qe) %>% 
  mutate(Return_Q = Index / lag(Index)-1)



library(tidyverse);library(fmxdat)

p <- 
  
  fmxdat::Jalshtr %>% mutate(Index = "ALSI") %>% 
  
  ggplot() + 
  
  geom_line(aes(date, TRI, color = Index), size = 2, alpha = 0.7) + 
  # Nice clean theme, with many additions that are now simplified (explore this yourself):
  # E.g. using fmxdat::ggpts, we can change the sizes more easily in millimeters. 
  # theme_fmx also offers simplified size settings, as e.g. below:
  fmxdat::theme_fmx(title.size = ggpts(30), 
                    subtitle.size = ggpts(28),
                    caption.size = ggpts(25),
                    # Makes nicer caption. If no caption given, this will break function, so careful:
                    CustomCaption = T) + 
  
  # crisp colours:
  fmxdat::fmx_cols() + 
  
  labs(x = "", y = "Cumulative Returns", caption = "Note:\nCalculation own",
       title = "Illustrating fmxdat Auxilliary functions for ggplot",
       subtitle = "If not subtitle, make blank and Subtitle size small to make a gap\nbetween plot and Title. Test this yourself") + 
  
  guides(color = F)

# Finplot now adds finishing touches easily:

fmxdat::finplot(p, x.vert = T, x.date.type = "%Y", 
                x.date.dist = "2 years")




options(dplyr.summarise.inform = F)
library(lubridate)

# make returns monthly for illustration:
idx <- fmxdat::SA_Indexes %>% 
  filter(Tickers %in% c("FINI15TR Index", "INDI25TR Index", "JALSHTR Index", 
                        "MIDCAPTR Index", "RESI20TR Index", "TOP40TR Index")) %>% 
  
  mutate(YM = format(date, "%Y%B")) %>% 
  arrange(date) %>% 
  group_by(Tickers, YM) %>% filter(date == last(date)) %>% 
  group_by(Tickers) %>% 
  mutate(ret = Price/lag(Price) - 1) %>% select(date, Tickers, 
                                                ret) %>% ungroup()

idx %>% 
  filter(!is.na(ret)) %>% 
  group_by(Tickers) %>% 
  # mutate(cp = cumprod(1+ret)) %>% filter(date == last(date))
  # shortcut:
  summarise( tot_ret = prod( 1 + ret), N = n()) %>% 
  mutate(tot_ret = (1+tot_ret)^(12/N) -1 )

idx %>% group_by(Tickers) %>% filter(date == first(date))









