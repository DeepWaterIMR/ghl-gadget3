
if(set_weights) {
  tmb_param[grepl('weight$', tmb_param$switch) & tmb_param$value != 0, c("value", "lower", "upper", "optimise")] <-
    data.frame(value = 1, lower = NA, upper = NA, optimise = FALSE)
}

time_iter_start <- Sys.time()
message("Iteration started ", time_iter_start)

iter_param <- g3_iterative(
  gd = base_dir,
  wgts = "iterative_reweighting",
  model = tmb_model,
  params.in = tmb_param,
  grouping =
    list(SI_EggaN = c('log_EggaN_SI_female',
                      'log_EggaN_SI_male'),
         SI_Juv = c('log_Juv_SI_1',
                    'log_Juv_SI_2'),
         TrawlNor = c('TrawlNor_ldist', 'TrawlNor_sexdist'),
         OtherNor = c('OtherNor_ldist', 'OtherNor_sexdist', 'OtherNor_aldist'),
         TrawlRus = c('TrawlRus_ldist', 'TrawlRus_sexdist'),
         OtherRus = c('OtherRus_ldist', 'OtherRus_sexdist'),
         EcoS = c('EcoS_ldist', 'EcoS_aldist', 'EcoS_sexdist'),
         EggaN = c('EggaN_aldist_female', 'EggaN_aldist_male', 'EggaN_ldist', 'EggaN_matp'),
         EggaS = c('EggaS_aldist', 'EggaS_ldist', 'EggaS_matp')
    ),
  use_parscale = TRUE,
  control = list(maxit = 1000),
  cv_floor = 4e-4, # Gives maximum weight of 1/cv_floor for survey indices
  shortcut = FALSE
)

time_iter_end <- Sys.time()
time_iter <- round(as.numeric(time_iter_end - time_iter_start, units = "mins"), 1)
message("Iteration finished ", time_iter_end, " after ", time_iter, " min")

cat(
  c("Iteration:\n",
    "   started ", as.character(time_iter_start), "\n",
    "   finished ", as.character(time_iter_end), "\n",
    "   time ", time_iter, " min", "\n\n"),
  file = file.path(base_dir, "run_times.txt"), sep = "", append = TRUE)

### Save the model parameters

write.csv(as.data.frame(iter_param), file = file.path(base_dir, "data/Iterated TMB parameters.csv"))
save(iter_param, file = file.path(base_dir, "data/Iterated TMB parameters.rda"), compress = "xz")

### Plots

iter_fit <- g3_fit(model, iter_param)
save(iter_fit, file = file.path(base_dir, "data/Iterated TMB model fit.rda"), compress = "xz")

# gadget_plots(iter_fit, file.path(base_dir, "figures"))

tmppath <- file.path(getwd(), base_dir, "figures")
make_html(iter_fit, path = tmppath, file_name = "model_output_figures_iter.html")
rm(tmppath)

## Save workspace

save.image(file = file.path(base_dir, "data/gadget_workspace.RData"), compress = "xz")
message("Script finished ", Sys.time())
