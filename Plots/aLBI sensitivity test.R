library(openxlsx)
library(readxl)
library(dplyr)
library(aLBI)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(readxl)

FishPar(data = lenfreq02, resample = 1000, progress = F)

?ExData


#openxlsx::write.xlsx(spal, "ExampleData.xlsx", overwrite = T)

FrequencyTable(data = ExData, bin_width = NULL)

bw6 <- FrequencyTable(data = ExData, bin_width = 6)
lfq6 <- bw6$lfreq

lfqres <- FishPar(data = lfq6, resample = 1000, progress = F)



lpar <- lfqres$estimated_length_par
fpar <- lfqres$estimated_froese_par
fqpar <- lfqres$estimated_freq_par

sheet_ls <- list(
  sheet1 = data.frame(lpar),
  sheet2 = data.frame(fpar),
  sheet3 = data.frame(fqpar)
)
write.xlsx(sheet_ls, "BaseM.xlsx")

# with bin length 7
bw7 <- FrequencyTable(data = ExData, bin_width = 7)
lfq7 <- bw7$lfreq

lfqres <- FishPar(data = lfq7, resample = 1000, progress = F)


lpar <- lfqres$estimated_length_par
fpar <- lfqres$estimated_froese_par
fqpar <- lfqres$estimated_freq_par

sheet_ls <- list(
  sheet1 = data.frame(lpar),
  sheet2 = data.frame(fpar),
  sheet3 = data.frame(fqpar)
)
write.xlsx(sheet_ls, "BaseM.xlsx")


# with bin length 8
bw8 <- FrequencyTable(data = ExData, bin_width = 8)
lfq8 <- bw8$lfreq


lfqres <- FishPar(data = lfq8, resample = 1000, progress = F)

lpar <- lfqres$estimated_length_par
fpar <- lfqres$estimated_froese_par
fqpar <- lfqres$estimated_freq_par

sheet_ls <- list(
  sheet1 = data.frame(lpar),
  sheet2 = data.frame(fpar),
  sheet3 = data.frame(fqpar)
)
write.xlsx(sheet_ls, "BaseM.xlsx", overwrite = T )

# with bin length 9

bw9 <- FrequencyTable(data = ExData, bin_width = 9)
lfq9 <- bw9$lfreq

lfqres <- FishPar(data = lfq9, resample = 1000, progress = F)


lpar <- lfqres$estimated_length_par
fpar <- lfqres$estimated_froese_par
fqpar <- lfqres$estimated_freq_par

sheet_ls <- list(
  sheet1 = data.frame(lpar),
  sheet2 = data.frame(fpar),
  sheet3 = data.frame(fqpar)
)
write.xlsx(sheet_ls, "BaseM.xlsx", overwrite = T )

# with bin length 10

bw10 <- FrequencyTable(data = ExData, bin_width = 10)
lfq10 <- bw10$lfreq

lfqres <- FishPar(data = lfq10, resample = 1000, progress = F)


lpar <- lfqres$estimated_length_par
fpar <- lfqres$estimated_froese_par
fqpar <- lfqres$estimated_freq_par

sheet_ls <- list(
  sheet1 = data.frame(lpar),
  sheet2 = data.frame(fpar),
  sheet3 = data.frame(fqpar)
)
write.xlsx(sheet_ls, "BaseM.xlsx", overwrite = T )

# with bin length 9

bw11 <- FrequencyTable(data = ExData, bin_width = 11)
lfq <- bw11$lfreq

bw12 <- FrequencyTable(data = ExData, bin_width = 12)
lfq <- bw12$lfreq

bw13 <- FrequencyTable(data = ExData, bin_width = 13)
lfq <- bw15$lfreq


# with bin length 5
bw5 <- FrequencyTable(data = ExData, bin_width = 5)
lfq5 <- bin5 <- bw5$lfreq

lfqres <- FishPar(data = lfq5, resample = 1000, progress = F)


lpar <- lfqres$estimated_length_par
fpar <- lfqres$estimated_froese_par
fqpar <- lfqres$estimated_freq_par

sheet_ls <- list(
  sheet1 = data.frame(lpar),
  sheet2 = data.frame(fpar),
  sheet3 = data.frame(fqpar)
)
write.xlsx(sheet_ls, "BaseM.xlsx", overwrite = T )

# with bin length 4
bw4 <- FrequencyTable(data = ExData, bin_width = 4)
lfq4 <- bw4$lfreq

lfqres <- FishPar(data = lfq4, resample = 1000, progress = F)


lpar <- lfqres$estimated_length_par
fpar <- lfqres$estimated_froese_par
fqpar <- lfqres$estimated_freq_par

sheet_ls <- list(
  sheet1 = data.frame(lpar),
  sheet2 = data.frame(fpar),
  sheet3 = data.frame(fqpar)
)
write.xlsx(sheet_ls, "BaseM.xlsx", overwrite = T )

# with bin length 3

bw3 <- FrequencyTable(data = ExData, bin_width = 3)
lfq3 <- bw3$lfreq

lfqres <- FishPar(data = lfq3, resample = 1000, progress = F)


lpar <- lfqres$estimated_length_par
fpar <- lfqres$estimated_froese_par
fqpar <- lfqres$estimated_freq_par

sheet_ls <- list(
  sheet1 = data.frame(lpar),
  sheet2 = data.frame(fpar),
  sheet3 = data.frame(fqpar)
)
write.xlsx(sheet_ls, "BaseM.xlsx", overwrite = T )


# with bin length 2
bw2 <- FrequencyTable(data = ExData, bin_width = 2)
lfq2 <- bw2$lfreq

lfqres <- FishPar(data = lfq2, resample = 1000, progress = F)

lpar <- lfqres$estimated_length_par
fpar <- lfqres$estimated_froese_par
fqpar <- lfqres$estimated_freq_par

sheet_ls <- list(
  sheet1 = data.frame(lpar),
  sheet2 = data.frame(fpar),
  sheet3 = data.frame(fqpar)
)
write.xlsx(sheet_ls, "BaseM.xlsx", overwrite = T )

# with bin length 2
bw1 <- FrequencyTable(data = ExData, bin_width = 1)
lfq <- bw1$lfreq

lfqres <- FishPar(data = lfq2, resample = 1000, progress = F)

lpar <- lfqres$estimated_length_par
fpar <- lfqres$estimated_froese_par
fqpar <- lfqres$estimated_freq_par

sheet_ls <- list(
  sheet1 = data.frame(lpar),
  sheet2 = data.frame(fpar),
  sheet3 = data.frame(fqpar)
)

write.xlsx(sheet_ls, "BaseM.xlsx", overwrite = T )
# visualization of sensitivity data




# estimated length parameters
sdata <- read_excel("SensitivityData.xlsx", sheet = 1)

sdata %>%
  filter(BinLen <= 5) %>%
  pivot_longer(cols = 3:5, names_to = "estimates", values_to = "values") %>%
  dplyr::select(BinLen, Parameter, estimates, values) %>%
  ggplot(aes(as.factor(BinLen), values, fill = Parameter))+
  geom_boxplot(show.legend = F)+
  #geom_violin(show.legend = F, alpha = 0.2)+
  #geom_jitter(size = 2, alpha = 0.5, show.legend = F)+
  facet_wrap(~Parameter, scales = "free_y")+
  theme_bw()

# Summarizing the data
summaryL <-  sdata %>%
  filter(BinLen == 3) %>%
  pivot_longer(cols = 3:5, names_to = "estimates", values_to = "values") %>%
  group_by(Parameter, BinLen) %>%
  summarise(mean = round(mean(values), digits = 2), sd = round(sd(values), digits = 2), .groups = "drop") %>%
  mutate(mean_pm = paste0(mean, "±" , sd))

write.xlsx(summaryL, "summaryL.xlsx", overwrite = T)

# Calculating LM_ration for all bin length
lmration <- sdata %>%
  dplyr::select(1:3) %>%
  filter(Parameter == c("Lmat", "Lopt") & BinLen <= 5) %>%
  pivot_wider(names_from = Parameter, values_from = Mean_estimate) %>%
  mutate(LM_ratio = Lmat/Lopt)

write.xlsx(lmration, "LMration.xlsx", overwrite = T)

# Calculating percentage of deviation for the lenght parameters

lendeviation <- sdata %>%
  select(Parameter, BinLen, Mean_estimate) %>%
  filter(BinLen <= 5) %>%
  pivot_wider(names_from = BinLen, values_from = "Mean_estimate") %>%
  select(1, 4, 6,5,3,2) %>%
  mutate(Bin1 = (`3` - `1`)/ `3`*100,
         Bin2 = (`3` - `2`)/ `3`*100,
         Bin4 = (`3` - `4`)/ `3`*100,
         Bin5 = (`3` - `5`)/ `3`*100
         )

openxlsx::write.xlsx(lendeviation, "lendev.xlsx", overwrite = T)


# Lmax
myanovadata <- sdata %>%
  filter(Parameter == "Lmax") %>%
  pivot_longer(cols = 3:5, names_to = "estimates", values_to = "value") %>%
  select(BinLen, value)

OneWayAnova(data = myanovadata)

# Linf
myanovadata <- sdata %>%
  filter(Parameter == "Linf") %>%
  pivot_longer(cols = 3:5, names_to = "estimates", values_to = "value") %>%
  dplyr::select(2, 4)

OneWayAnova(data = myanovadata)

ExData %>% View()


# Lmat
myanovadata <- sdata %>%
  filter(Parameter == "Lmat") %>%
  pivot_longer(cols = 3:5, names_to = "estimates", values_to = "value") %>%
  dplyr::select(2, 4)

OneWayAnova(data = myanovadata)

# Lopt
myanovadata <- sdata %>%
  filter(Parameter == "Lopt") %>%
  pivot_longer(cols = 3:5, names_to = "estimates", values_to = "value") %>%
  dplyr::select(2, 4)

OneWayAnova(data = myanovadata)


# Lopt_m10
myanovadata <- sdata %>%
  filter(Parameter == "Lopt_m10") %>%
  pivot_longer(cols = 3:5, names_to = "estimates", values_to = "value") %>%
  dplyr::select(2, 4)

OneWayAnova(data = myanovadata)

# Lopt_p10
myanovadata <- sdata %>%
  filter(Parameter == "Lopt_p10") %>%
  pivot_longer(cols = 3:5, names_to = "estimates", values_to = "value") %>%
  dplyr::select(2, 4)

OneWayAnova(data = myanovadata)



# estimated percentage of froese parameters

fdata <- read_excel("SensitivityData.xlsx", sheet = 2)

froediff <- fdata %>%
  select(1:3) %>%
  filter(BinLen <= 5) %>%
  pivot_wider(names_from = "BinLen", values_from = "Mean_estimate") %>%
  select(1,4,6,5,3,2) %>%
  mutate( Bin1 =  (`3`-`1`),
          Bin2 =  (`3`-`2`),
          Bin4 =  (`3`-`4`),
          Bin5 =  (`3`-`5`)

          )

write.xlsx(froediff, "froesebindiff.xlsx", overwrite = T)

#`3`   `1`   `2`   `4`   `5`

fdata %>%
  filter(BinLen <= 5) %>%
  pivot_longer(cols = 3:5, names_to = "estimates", values_to = "values") %>%
  mutate(Parameter = factor(Parameter, levels = c("Pmat", "Popt", "Pmega"))) %>%
  dplyr::select(BinLen, Parameter, estimates, values) %>%
  ggplot(aes(as.factor(BinLen), values, fill = Parameter))+
  geom_boxplot(show.legend = F)+
  #geom_violin(show.legend = F, alpha = 0.2)+
  #geom_jitter(size = 2, alpha = 0.5, show.legend = F)+
  facet_wrap(~Parameter, scales = "free_y")+
  theme_bw()

#Summary forese
summaryF <-  fdata %>%
  filter(BinLen == 3) %>%
  pivot_longer(cols = 3:5, names_to = "estimates", values_to = "values") %>%
  group_by(Parameter, BinLen) %>%
  summarise(mean = round(mean(values), digits = 2), sd = round(sd(values), digits = 2), .groups = "drop") %>%
  mutate(mean_pm = paste0(mean, "±" , sd))

write.xlsx(summaryF, "summaryF.xlsx", overwrite = T)


# calculating Pobj for all

pobj <- fdata %>%
  filter(BinLen <= 5) %>%
  pivot_longer(cols = 3:5, names_to = "estimates", values_to = "values") %>%
  filter(estimates == "Mean_estimate") %>%
  dplyr::select(1, 2, 4) %>%
  group_by(BinLen) %>%
  mutate( Pobj = sum(values))

write.xlsx(pobj, "Pobj.xlsx", overwrite = T)

# Pmat
myanovadata <- fdata %>%
  filter(Parameter == "Pmat") %>%
  pivot_longer(cols = 3:5, names_to = "estimates", values_to = "value") %>%
  dplyr::select(2, 4)

OneWayAnova(data = myanovadata)


# Popt
myanovadata <- fdata %>%
  filter(Parameter == "Popt") %>%
  pivot_longer(cols = 3:5, names_to = "estimates", values_to = "value") %>%
  dplyr::select(2, 4)

OneWayAnova(data = myanovadata)


# Popt
myanovadata <- fdata %>%
  filter(Parameter == "Pmega") %>%
  pivot_longer(cols = 3:5, names_to = "estimates", values_to = "value") %>%
  dplyr::select(2, 4)

OneWayAnova(data = myanovadata)

data <- chickwts %>%
  select(feed, weight)

OneWayAnova(data)


dd <- lfq3

write.xlsx(dd, "modelFreq.xlsx", overwrite = T)


# Creating the bar plot with the LSB and TSB values

ltdata <- read_excel("SensitivityData.xlsx", sheet = 5)

ltdata %>%
  pivot_longer(cols = 2:3, names_to = "Biomass", values_to = "values") %>%
  ggplot(aes(as.factor(BinLen), values, fill = Biomass))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label = values),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 4) +
  theme_classic()+
  labs(x = "Bin Size", y = "Probability (%)", fill = "Legend")


library(aLBI)

#bin12345
aLBI::FrequencyTable(data = ExData, bin_width = 5, output_file = "Bin5.xlsx")



# for binsize 1
bin1 <- read_excel("Bin1.xlsx", sheet = 2)

aLBI::FishPar(data = bin1, resample = 1000, progress = F)

# for binsize 2
bin2 <- read_excel("Bin2.xlsx", sheet = 2)

aLBI::FishPar(data = bin2, resample = 1000, progress = F)

# for binsize 3
bin3 <- read_excel("Bin3.xlsx", sheet = 2)

aLBI::FishPar(data = bin3, resample = 1000, progress = F)


# for binsize 4
bin4 <- read_excel("Bin4.xlsx", sheet = 2)

aLBI::FishPar(data = bin4, resample = 1000, progress = F)


# for binsize 5
bin5 <- read_excel("Bin5.xlsx", sheet = 2)

aLBI::FishPar(data = bin5, resample = 1000, progress = F)


# Stock status for bin 1

aLBI::FishSS(data = cpdata,
             Pobj = 5.36,
             Pmat = 2.30,
             Popt = 2.61,
             LM_ratio = 0.93)

# Stock status for bin 3
aLBI::FishSS(data = cpdata,
             Pobj = 5.51,
             Pmat = 2.37,
             Popt = 2.72,
             LM_ratio = 0.93)

# Stock status for bin 3
aLBI::FishSS(data = cpdata,
             Pobj = 4.77,
             Pmat = 2.04,
             Popt = 2.38,
             LM_ratio = 0.93)

# Stock status for bin 4
aLBI::FishSS(data = cpdata,
             Pobj = 6.66,
             Pmat = 2.91,
             Popt = 3.26,
             LM_ratio = 0.93)

# Stock status for bin 5
aLBI::FishSS(data = cpdata,
             Pobj = 5.08,
             Pmat = 2.22,
             Popt = 2.43,
             LM_ratio = 0.93)



# estimated length parameters
sdata <- read_excel("SensitivityData.xlsx", sheet = 1)



lmax <- sdata %>% pivot_longer(cols = 3:5, names_to = "CI", values_to = "value") %>% 
  filter(Parameter == "Lmax") %>% select(BinLen, value)
  
OneWayAnova(data = lmax)

linf <- sdata %>% pivot_longer(cols = 3:5, names_to = "CI", values_to = "value") %>% 
  filter(Parameter == "Linf") %>% dplyr::select(BinLen, value)

OneWayAnova(data = linf)


lmat <- sdata %>% pivot_longer(cols = 3:5, names_to = "CI", values_to = "value") %>% 
  filter(Parameter == "Lmat") %>% dplyr::select(BinLen, value)

OneWayAnova(data = lmat)



lopt <- sdata %>% pivot_longer(cols = 3:5, names_to = "CI", values_to = "value") %>% 
  filter(Parameter == "Lopt") %>% dplyr::select(BinLen, value)

OneWayAnova(data = lopt)

