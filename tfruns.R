library(tfruns)

runs <- tuning_run("NN_tuning.R", sample = 0.7, flags = list(
  dropout1 = c(0.2, 0.3, 0.4),
  dropout2 = c(0.2, 0.3, 0.4),
  lr = c(0.001, 0.0001, 0.00001),
  batch_size = c(512, 256, 128, 64)
))

# saveRDS(object = runs, file = "RDS/runs_padding")
runs_padding = readRDS("RDS/runs_padding") 
View(runs_padding)
