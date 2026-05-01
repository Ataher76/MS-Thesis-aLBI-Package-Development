library(aLBI)
library(readxl)
library(dplyr)
library(ggplot2)

bmodel <- read_excel("Bin3.xlsx", sheet = 2) # import your dataet by name you set

FishPar(data = bmodel,
        progress = T,
        resample = 1000,
        Linf = 82.12,
        #Linf_sd = 0.5, ; this set 0.5 by default
        Lmat = 41.5
        #Lmat_sd = 0.5; this set 0.5 by default
        )

# Preserve values each time you run for a single scenario. you will run above code for 4/6/8 times for differen value of Linf and Lmat
# I'll provide a excel file where you will preserve the values and then follow the code below
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggsci)



# sensitivity for length parameters
senalbi <- read_excel("SenA.xlsx", sheet = 2)

senlen <- senalbi %>% select(1,2, 4:6)

senlen$Code <- as.factor(senlen$Code)


senlen %>% 
  #select(2:5) %>%  
  pivot_longer(cols = 3:5, names_to = "range", values_to = "value") %>% 
  ggplot(aes(Code, value, fill = Par))+
  geom_boxplot(show.legend = F)+
  facet_wrap(~Par, scales = "free_y")+
  scale_fill_npg()+
  theme_bw()





# sensitivity for froese LBIs
senalbi <- read_excel("SenA.xlsx", sheet = 3)

senlen <- senalbi %>% select(1,2, 4:6)

senlen$Code <- as.factor(senlen$Code)


senlen %>% 
  #select(2:5) %>%  
  pivot_longer(cols = 3:5, names_to = "range", values_to = "value") %>% 
  ggplot(aes(Code, value, fill = Par))+
  geom_boxplot(show.legend = F)+
  facet_wrap(~Par, scales = "free_y")+
  scale_color_npg()+
  theme_bw()
