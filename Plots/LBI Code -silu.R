# Load necessary libraries
library(readxl)
library(aLBI)

# Read your Excel file directly from the given path
lenfreq_data <- read_excel("C:/Users/DELL/onedrive/Desktop/LC.xlsx")

# View the data (optional)
print(lenfreq_data)
# Running the FishPar function
results <- FishPar(data = lenfreq_data, resample = 1000, progress = FALSE)
# Viewing the results
results$estimated_length_par
results$estimated_froese_par
results$estimated_freq_par
results$forese_ind_vs_target
results$LM_ratio
results$Pobj
# You are all done with the first function and get the values of estimated parameters fron the console
# now for the second function
cpdata <- CPdata
#Now call the function as followed:
FishSS(data = cpdata,
        Pmat =71.5,
       Popt = 31.98198,
       Pobj = 110.2375,
       LM_ratio = 0.9741467)
# Find the values from the console 

        