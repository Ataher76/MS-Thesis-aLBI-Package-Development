#' Calculate Length-Based Indicators for Fish Stock Assessment
#'#'@importFrom graphics abline axis barplot box boxplot hist legend lines par rect segments text
#' @importFrom stats complete.cases density loess predict quantile
#' @importFrom graphics abline axis barplot box boxplot hist legend lines par rect segments text
#' @description This function calculates various length-based indicators for fish stock assessment
#' using length frequency data and bootstrap resampling.
#' @param data A data frame containing two columns: Length and Frequency.
#' @param resample An integer indicating the number of bootstrap resamples.
#' @param progress A logical value indicating whether to display progress.
#' @return A list containing estimated length parameters, Froese indicators, and other relevant metrics.
#' @examples
#' data <- data.frame(Length = c(10, 20, 30, 40, 50), Frequency = c(5, 10, 15, 20, 25))
#' FishPar(lenfreq01, 100, progress = FALSE)
#' utils::data("lenfreq01", package = "aLBI")
#' utils::data("lenfreq02", package = "aLBI")
#' @export

froeseFun <- function(data, resample, progress) {

  oldpar <- par(no.readonly = TRUE)  # Save current par settings
  on.exit(par(oldpar))  # Reset par settings when function exits
  # Code that modifies par settings
  par(mfrow = c(2, 2))

  # Load necessary datasets within the function
  utils::data("lenfreq01", package = "aLBI")
  utils::data("lenfreq02", package = "aLBI")

  # Check if it is a valid dataframe
  if (!is.data.frame(data)) {
    stop("Input is not a data frame.")
  }

  # Check if data frame is a list and convert into a data frame
  if (is.list(data)) {
    data <- as.data.frame(data)
  }

  colnames(data)[1:2] <- c("Length", "Frequency")

  # Check if the dataframe has exactly two columns
  if (ncol(data) != 2) {
    stop("The dataframe must have exactly two columns as Length and Frequency")
  }

  # cleaning the data or removing the NA values
  data <- data[complete.cases(data), ]


  # Define a function to calculate parameters for confidence intervals
  CalParCI <- function(data, Lmax) {
    # Calculate Linf
    Linf <- Lmax / 0.95

    # Calculate Lmat
    Lmat <- 10^(0.8979 * log10(Linf) - 0.0782)

    # Calculate Lopt
    Lopt <- 10^(1.053 * log10(Lmat) - 0.0565)

    # Calculate Lopt_p10
    Lopt_p10 <- Lopt + Lopt / 10

    # Calculate Lopt_m10
    Lopt_m10 <- Lopt - Lopt / 10

    return(c(Lmax, Linf, Lmat, Lopt, Lopt_p10, Lopt_m10))
  }

  # Store parameter estimates and Froese indicators from bootstrap samples
  parameter_estimates <- matrix(NA, nrow = resample, ncol = 6)
  #froese_indicators <- matrix(NA, nrow = resample, ncol = 3)  # For Pmat, Popt, Pmega

  # Perform bootstrap resampling
  if (progress) pb <- utils::txtProgressBar(min = 0, max = resample, style = 3)

  for (i in 1:resample) {
    # Generate bootstrap sample
    bootstrap_sample <- data[sample(nrow(data), replace = TRUE), ]

    # Calculate Lmax for the bootstrap sample
    Lmax_bootstrap <- max(bootstrap_sample[[1]])

    # Calculate length parameters
    parameter_estimates[i, ] <- CalParCI(bootstrap_sample, Lmax_bootstrap)

    if (progress) utils::setTxtProgressBar(pb, i)
  }

  if (progress) close(pb)

  # Calculate mean and confidence intervals of bootstrap estimates
  mean_estimates <- apply(parameter_estimates, 2, mean)
  lower_bound <- apply(parameter_estimates, 2, function(x) quantile(x, probs = 0.025))
  upper_bound <- apply(parameter_estimates, 2, function(x) quantile(x, probs = 0.975))

  #Some note and extracted length parameters
  Lmax <- mean_estimates[1]
  Linf <- mean_estimates[2]
  Lmat <- mean_estimates[3]
  Lopt <- mean_estimates[4]
  Lopt_p10 <- mean_estimates[5]
  Lopt_m10 <- mean_estimates[6]

  # counting total number of sample
  sumT <- sum(data[[2]])
  # Calculating the length at maturity ratio from the mean_estimated
  LM_ratio <-  (Lmat / Lopt)


  froese_names <- c("Pmat", "Popt", "Pmega")
  # Calculate froese indicator for mean estimates
  sum_mat <- sum(ifelse(data[[1]] >= Lmat & data[[1]] <= Lmax, data[[2]], 0))
  Pmat <- (sum_mat / sumT) * 100

  # Calculate sum_opt and Popt
  sum_opt <- sum(ifelse(data[[1]] >= Lopt_m10 & data[[1]] <= Lopt_p10, data[[2]], 0))
  Popt <- (sum_opt / sumT) * 100

  # Calculate sum_mega and Pmega
  sum_mega <- sum(ifelse(data[[1]] >= Lopt_p10 & data[[1]] <= Lmax, data[[2]], 0))
  Pmega <- (sum_mega / sumT) * 100

  # collecting froese mean indicators
  mean_froese <- c(Pmat, Popt, Pmega)
  #Calculating Pobj
  Pobj_mean <- sum(Pmat + Popt + Pmega)

  # extracting the upper CI for all the estimates
  # upper bounds
  LmaxU <- upper_bound[1]
  LinfU <- upper_bound[2]
  LmatU <- upper_bound[3]
  LoptU <- upper_bound[4]
  Lopt_p10U <- upper_bound[5]
  Lopt_m10U <- upper_bound[6]

  ### Calculate froese indicator for upper CI  estimates
  sum_matU <- sum(ifelse(data[[1]] >= LmatU & data[[1]] <= LmaxU, data[[2]], 0))
  PmatU <- (sum_matU / sumT) * 100

  # Calculate sum_opt and Popt
  sum_optU <- sum(ifelse(data[[1]] >= Lopt_m10U & data[[1]] <= Lopt_p10U, data[[2]], 0))
  PoptU <- (sum_optU / sumT) * 100

  # Calculate sum_mega and Pmega
  sum_megaU <- sum(ifelse(data[[1]] >= Lopt_p10U & data[[1]] <= LmaxU, data[[2]], 0))
  PmegaU <- (sum_megaU / sumT) * 100

  # Collecting froese upper indicators
  upper_froese <- c(PmatU, PoptU, PmegaU)
  # Collecting froese upper indicators
  Pobj_Upper <- sum(PmatU + PoptU + PmegaU)

  # extracting the lower CI for all the estimates
  # lower bounds
  LmaxL <- lower_bound[1]
  LinfL <- lower_bound[2]
  LmatL <- lower_bound[3]
  LoptL <- lower_bound[4]
  Lopt_p10L <- lower_bound[5]
  Lopt_m10L <- lower_bound[6]

  ### Calculate froese indicator for lower CI  estimates
  sum_matL <- sum(ifelse(data[[1]] >= LmatL & data[[1]] <= LmaxL, data[[2]], 0))
  PmatL <- (sum_matL / sumT) * 100

  # Calculate sum_opt and Popt
  sum_optL <- sum(ifelse(data[[1]] >= Lopt_m10L & data[[1]] <= Lopt_p10L, data[[2]], 0))
  PoptL <- (sum_optL / sumT) * 100

  # Calculate sum_mega and Pmega
  sum_megaL <- sum(ifelse(data[[1]] >= Lopt_p10L & data[[1]] <= LmaxL, data[[2]], 0))
  PmegaL <- (sum_megaL / sumT) * 100

  # Collecting froese upper indicators
  lower_froese <- c(PmatL, PoptL, PmegaL)
  Pobj_Lower <- sum(PmatL + PoptL + PmegaL)

  # all pobj
  # Creating a vector with the Pobj
  Pobj <- c(Mean = Pobj_mean, lower_value = Pobj_Upper , upper_value = Pobj_Lower)


  par(mfrow=c(2, 3)) # Setting up a 2x3 grid for the plots

  parameter_names <- c("Lmax", "Linf", "Lmat", "Lopt", "Lopt_p10", "Lopt_m10")

  for (i in 1:6) {
    hist(parameter_estimates[,i], main=parameter_names[i], xlab=parameter_names[i], ylab="Frequency", col="lightblue" )
    abline(v = mean_estimates[i], col = "red", lwd = 2)
    #segments(mean_estimates[i], 0, mean_estimates[i], max(hist(parameter_estimates[, i], plot = FALSE)$counts), col = "red2", lwd = 2)
    segments(lower_bound[i], 0, lower_bound[i], max(hist(parameter_estimates[, i], plot = FALSE)$counts), col = "black", lwd = 2, lty = "dotted" )
    segments(upper_bound[i], 0, upper_bound[i], max(hist(parameter_estimates[, i], plot = FALSE)$counts), col = "black", lwd = 2, lty = "dotted" )

  }

  #par(mfrow=c(2, 3)) # Setting up another 2x3 grid for the density plots

  for (i in 1:6) {
    dens <- density(parameter_estimates[,i])
    plot(dens, main=parameter_names[i], col="blue", lwd=1.5, xlim=c(min(parameter_estimates[,i]), max(parameter_estimates[,i])))
    abline(v = mean_estimates[i], col = "red", lwd = 2)
    #segments(lower_bound[i], 0, mean_estimates[i], max(dens$y), col = "yellow3", lwd = 1.5 )
    segments(lower_bound[i], 0, lower_bound[i], max(dens$y), col = "black", lwd = 2, lty = "dotted" )
    segments(upper_bound[i], 0, upper_bound[i], max(dens$y), col = "black", lwd = 2, lty = "dotted" )

  }


  # Create data frames for parameter estimates and confidence intervals
  estimated_length_par <- data.frame(
    Parameters = parameter_names,
    Mean_estimate = mean_estimates,
    Lower_CI = lower_bound,
    Upper_CI = upper_bound
  )

  #Creating boxplot with this data frame

  # Reshape the data into long format
  # Create an empty dataframe to store the long format
  long_df <- data.frame(Parameters = character(),
                        Interval = character(),
                        Value = numeric(),
                        stringsAsFactors = FALSE)

  # Create a list to store each subset of the dataframe
  df_list <- list(estimated_length_par$Parameters,
                  estimated_length_par$Parameters,
                  estimated_length_par$Parameters)
  names(df_list) <- c("Mean_estimate", "Lower_CI", "Upper_CI")

  # Populate the long dataframe
  for (i in names(df_list)) {
    long_df <- rbind(long_df, data.frame(Parameters = df_list[[i]],
                                         Interval = rep(i, nrow(estimated_length_par)),
                                         Value = estimated_length_par[[i]],
                                         stringsAsFactors = FALSE))
  }

  #Converting the order of my parameters as i want
  long_df$Parameters <- factor(long_df$Parameters, levels = parameter_names)

  par(mfrow = c(1,1))
  # Create boxplot
  boxplot(Value ~ Parameters,
          data = long_df,
          main = "Estimated Length Parameters",
          xlab = "Length Parameters",
          ylab = "Length (cm)",
          col = "lightblue",
          border = "black",
          notch = FALSE,
          #names = parameter_names,
          ylim = range(c(long_df$Value))

  )


  # for froese indicators in the same way as the estimated lenght parameters
  froese_estimates <- data.frame(
    Parameters = froese_names,
    Mean_froese = mean_froese,
    Lower_CI = upper_froese,
    Upper_CI = lower_froese
  )


  #Creating boxplot with this dataframe
  # Reshape the data into long format
  # Create an empty dataframe to store the long format
  long_df_froese <- data.frame(Parameters = character(),
                               Interval = character(),
                               Value = numeric(),
                               stringsAsFactors = FALSE)

  # Create a list to store each subset of the dataframe
  df_list_froese <- list(froese_estimates$Parameters,
                         froese_estimates$Parameters,
                         froese_estimates$Parameters)
  names(df_list_froese) <- c("Mean_froese", "Lower_CI", "Upper_CI")

  # Populate the long dataframe
  for (i in names(df_list_froese)) {
    long_df_froese <- rbind(long_df_froese, data.frame(Parameters = df_list_froese[[i]],
                                                       Interval = rep(i, nrow(froese_estimates)),
                                                       Value = froese_estimates[[i]],
                                                       stringsAsFactors = FALSE))
  }

  #Converting the order of my parameters as i want
  long_df_froese$Parameters <- factor(long_df_froese$Parameters, levels = froese_names)

  par(mfrow = c(1,1))
  # Create boxplot
  boxplot(Value ~ Parameters,
          data = long_df_froese,
          main = "Estimated Froese Sustainability Indicators",
          xlab = "Froese Indicators",
          ylab = "Percentage (%)",
          col = "lightblue",
          border = "black",
          notch = FALSE,
          #names = parameter_names,
          ylim = range(c(long_df_froese$Value))

  )


  #1. First graph of frequency distribution for length frequency data
  par( mfrow = c(1,1))

  barplot(data[[2]] ~ data[[1]],
          main = "Lenght Frequency Plot",
          xlab = "Length Class",
          ylab = "Frequency",
          ylim = c(0 , max(data[[2]]*1.2)),
          col= "#69b3a2")

  #geom_histogram(aes(x = mult, y = ..density..), fill = "lightblue")
  values <- loess(data[[2]] ~ data[[1]])
  lines(predict(values), col = "red", lwd = 2)
  legend( "topright", legend = c("Observed frequency", "Modelled frequency"), col = c("#69b3a2" , "red"),
          pch = c(15, NA), lty = c(NA, 1), cex = 1 , lwd = 2, seg.len = 1.5 )


  #Creating the data frame froese indicator vs target

  forese_ind_vs_target <- data.frame(
    Parameters = froese_names,
    Froese_catch = c(Pmat, Popt, Pmega),
    Froese_tar = c(100, 100, 20)
  )

  #2.Making the second barplot of Froese indicators and target
  barplot(rbind(forese_ind_vs_target[[3]], forese_ind_vs_target[[2]]), beside = TRUE,
          names.arg = forese_ind_vs_target[[1]], col = c("#69b3a2" , "#404080"),
          main = "Target vs Catch", xlab = "Froese Length Indicators",
          ylab = "Percentage (%)"
  )
  legend( "topright", legend = c("Target", "Catch"), fill = c("#69b3a2" , "#404080"))


  #3. Main graph with frequency distribution and corresponding lengths
  par(mfrow = c(2,3))

  for(i in 1:6){

    plot(data[[1]], data[[2]], type = "l", lwd = 1.8, main = parameter_names[i] ,
         xlab = "Length Class (cm)", ylab = "Frequency",
         ylim = c(0, max(data[[2]]+100)), xlim = c(0, max(data[[1]]+2)))
    box(col = "white")
    axis(side = 1, lwd = 1.5)
    axis(side = 2, lwd = 1.5)

    abline(v = mean_estimates[i], col = "red", lwd = 2)
    #segments(mean_estimates[i], 0, mean_estimates[i], max(hist(parameter_estimates[, i], plot = FALSE)$counts), col = "red2", lwd = 2)
    segments(lower_bound[i], 0, lower_bound[i], max(hist(parameter_estimates[, i], plot = FALSE)$counts), col = "black", lwd = 2, lty = "dotted" )
    segments(upper_bound[i], 0, upper_bound[i], max(hist(parameter_estimates[, i], plot = FALSE)$counts), col = "black", lwd = 2, lty = "dotted" )
    # Setting the Optimum sized fish as a rectangle box
    if( i == 4){
      rect(xleft = Lopt_m10, ybottom = 0, xright = Lopt_p10, ytop = max(data[[2]]), col = "#69b3a2")

      #Adding text as Lopt
      #text(x = Lopt, y = max(data[[2]]+20), labels = "Lopt", col = "red3", cex = 1)
      #Adding the Optimum size text in Lopt rect
      text(x = Lopt, y = max(data[[2]]), labels = "Optimum\nsize", col = "red3", cex = 0.8)
    }
    #Adding text for Juveniles
    text(x = mean(c(5, Lmat)), y = 100, labels = "Juveniles", col = "red3", cex = 0.8)
    #Adding text for Mega spawners
    text(x = mean(c(Lopt_m10 , Lmax)), y = 100, labels = "Mega-\nspawners", col = "red3", cex = 0.8)
  }


  estimated_freq_par <- data.frame(
    Parameter = c("sumT", "sum_mat", "sum_opt", "sum_mega"),
    Estimate = c(sumT, sum_mat, sum_opt, sum_mega )
  )


  # Return the data frames
  return(list(
    estimated_length_par = estimated_length_par,
    estimated_froese_par = froese_estimates,
    estimated_freq_par = estimated_freq_par,
    forese_ind_vs_target = forese_ind_vs_target,
    LM_ratio = LM_ratio,
    Pobj = Pobj
  ))
}

