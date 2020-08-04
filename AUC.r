### AUC figures and 
# out_train_4 <- out_function("GI", "HEA", info.heat, score="frag_mean", "Cat.2")

#format AUC to % and add 95% CI
format_number <- function(x)
{
  x1 <- format(round(x[1], 1), nsmall = 1)
  x2 <- format(round(x[2], 1), nsmall = 1)
  x3 <- format(round(x[3], 1), nsmall = 1)
  paste0(x2, "% (95% CI, ", x1, "-", x3, ")")
}

# calcuate auc and save specificity, sensitivity, AUC, etc.
out_function <- function(obj, ref, data, score,  Diagnosis_column, main)
{
  data <- data[data[, Diagnosis_column] %in% c(obj, ref), ]
  data[, Diagnosis_column] <- factor(data[, Diagnosis_column], levels = c(ref, obj))
  out_train <- data.frame(score = data[, score], True_respose = data[, Diagnosis_column])
  if (is.null(main)){
    main <- paste(obj, ref, sep="_")
  }
  roc_train <- roc_function(data=out_train, print.auc=T, name="training_set_final", main=main)
  
  format_number(roc_train$ci)

  auc_train <- format_number(roc_train$ci)

  out <- list(roc_train = roc_train, auc_train=auc_train)
  return(out)
}

# roc function to return roc object and plot auc.
roc_function <- function(data, print.auc=T, name, main)
{
  rocobj <- plot.roc(data[,2], data[,1], # data[, 2] is true_respose, data[, 1] is the score
                     cex.main=2,
                     cex.lab=2,
                     cex.axis=1.5,
                     main=main, percent=TRUE,
                     ci=TRUE, # compute AUC (of AUC by default)
                     print.auc=print.auc,
                     print.auc.adj=c(0.5,2),
                     print.auc.cex=2) # print the AUC (will contain the CI)

  return(rocobj)
}
