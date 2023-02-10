# Quali Model(s)

buildQualiModel_rf <- function(model_data = combineData()) {
  # Thin Data
  model_data <- model_data %>%
    dplyr::select(c(
      # ID columns
      "raceId", "circuitId", "driverId", "currentConstructorId",
      # quali data (TARGET)
      "qPosition",
      # driver data
      "driverDOB", "driverAge", "driverGPExperience", "driverSeasonWins",
      "driverSeasonWinsPerc", "driverPoints", "driverPointsPerc", "driverHomeRace",
      # constructor data
      "constructorPoints", "constructorSeasonWins", "constructorPointsPerc",
      "constructorSeasonWinsPerc", "constructorHomeRace",
      # circuit data
      "circuitAltitude", "circuitLength", "circuitType", "circuitDirection",
      # practice data
      "driverPercPracticeLaps", "constructorPercPracticeLaps", "driverPracticeBestPerc", "driverPracticeAvgPerc",
      "constructorPracticeBestPerc", "constructorPracticeAvgPerc", "driverTeamPracticeAvgGapPerc"
    )) %>%
    dplyr::filter(.data$qPosition > 0) %>%
    dplyr::filter(.data$qPosition <= 20) %>%
    dplyr::mutate("qPosition" = as.character(.data$qPosition))

  # Split data
  splitdata <- rsample::initial_split(model_data, 0.8)
  training_data <- rsample::training(splitdata)
  test_data <- rsample::testing(splitdata)

  tune_spec <- parsnip::rand_forest(
    mtry = tune::tune(),
    min_n = tune::tune()
  ) %>%
    parsnip::set_mode("classification") %>%
    parsnip::set_engine("ranger")

  init_recipe <- recipes::recipe(qPosition ~ ., training_data) %>%
    recipes::update_role("raceId", new_role = "ID") %>%
    recipes::update_role("circuitId", new_role = "ID") %>%
    recipes::update_role("driverId", new_role = "ID") %>%
    recipes::update_role("currentConstructorId", new_role = "ID") %>%
    recipes::step_string2factor(c("qPosition", "circuitType", "circuitDirection"))

  tune_wf <- workflows::workflow() %>%
    workflows::add_recipe(init_recipe) %>%
    workflows::add_model(tune_spec)

  cv <- rsample::vfold_cv(training_data)

  logger::log_info("Setting up parallel for rf tuning")
  doParallel::registerDoParallel(cores = parallel::detectCores(logical = F) - 2)
  logger::log_info("Tuning RF model")
  model_res <- tune::tune_grid(tune_wf, resamples = cv, grid = 50)
  logger::log_info("Clean up parallel")
  doParallel::stopImplicitCluster()
  unregister_dopar()

  best_model <- tune::select_best(model_res, "roc_auc")

  final_model <- tune_wf %>%
    tune::finalize_workflow(best_model) %>%
    parsnip::fit(data = training_data)

  logger::log_info("Training final rf model")
  last_fit <- final_model %>%
    tune::last_fit(splitdata)

  # This isn't used but checks for fails.
  test_fit <- final_model %>%
    stats::predict(new_data = test_data)

  train_roc <- tune::show_best(model_res, metric = "roc_auc", n = 1)$mean
  test_roc <- tune::collect_metrics(last_fit)[tune::collect_metrics(last_fit)$.metric == "roc_auc", ]$.estimate

  logger::log_info(glue::glue("Returning random forest model with roc_auc training value of {roc_train} and test roc_auc of {roc_test}.",
    roc_train = round(train_roc, 4), roc_test = round(test_roc, 4)
  ))
  return(final_model)
} # slow. preprocess ok

buildQualiModel_glm <- function(model_data = combineData()) {
  # Thin Data
  model_data <- model_data %>%
    dplyr::select(c(
      # ID columns
      "raceId", "circuitId", "driverId", "currentConstructorId",
      # quali data (TARGET)
      "qPosition",
      # driver data
      "driverAge", "driverGPExperience", "driverSeasonWins",
      "driverSeasonWinsPerc", "driverPoints", "driverPointsPerc", "driverHomeRace",
      # constructor data
      "constructorPoints", "constructorSeasonWins", "constructorPointsPerc",
      "constructorSeasonWinsPerc", "constructorHomeRace",
      # circuit data
      "circuitAltitude", "circuitLength", "circuitType", "circuitDirection",
      # practice data
      "driverPercPracticeLaps", "constructorPercPracticeLaps", "driverPracticeBestPerc", "driverPracticeAvgPerc",
      "constructorPracticeBestPerc", "constructorPracticeAvgPerc", "driverTeamPracticeAvgGapPerc"
    )) %>%
    dplyr::filter(.data$qPosition > 0) %>%
    dplyr::filter(.data$qPosition <= 20) %>%
    dplyr::mutate("qPosition" = as.character(.data$qPosition))

  # Split data
  splitdata <- rsample::initial_split(model_data, 0.8)
  training_data <- rsample::training(splitdata)
  test_data <- rsample::testing(splitdata)

  tune_spec <- parsnip::multinom_reg(
    penalty = tune::tune(),
    mixture = tune::tune()
  ) %>%
    parsnip::set_mode("classification") %>%
    parsnip::set_engine("glmnet")

  init_recipe <- recipes::recipe(qPosition ~ ., training_data) %>%
    recipes::update_role("raceId", new_role = "ID") %>%
    recipes::update_role("circuitId", new_role = "ID") %>%
    recipes::update_role("driverId", new_role = "ID") %>%
    recipes::update_role("currentConstructorId", new_role = "ID") %>%
    recipes::step_string2factor(c("circuitType", "circuitDirection")) %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) # %>%

  tune_wf <- workflows::workflow() %>%
    workflows::add_recipe(init_recipe) %>%
    workflows::add_model(tune_spec)

  cv <- rsample::vfold_cv(training_data)

  grid <- dials::grid_max_entropy(
    dials::penalty(),
    dials::mixture(),
    size = 30
  )

  logger::log_info("Setting up parallel for tuning glm model")
  doParallel::registerDoParallel(cores = parallel::detectCores(logical = F) - 2)
  logger::log_info("Tuning glm model")
  model_res <- tune::tune_grid(tune_wf,
    resamples = cv,
    grid = grid,
    control = tune::control_grid(save_pred = TRUE, verbose = TRUE)
  )
  logger::log_info("Clean up parallel")
  doParallel::stopImplicitCluster()

  best_model <- tune::select_best(model_res, "roc_auc")

  final_model <- tune_wf %>%
    tune::finalize_workflow(best_model) %>%
    parsnip::fit(data = training_data)

  logger::log_info("Training glm model")
  last_fit <- final_model %>%
    tune::last_fit(splitdata)

  # This isn't used but checks for fails.
  test_fit <- final_model %>%
    stats::predict(new_data = test_data)

  train_roc <- tune::show_best(model_res, metric = "roc_auc", n = 1)$mean
  test_roc <- tune::collect_metrics(last_fit)[tune::collect_metrics(last_fit)$.metric == "roc_auc", ]$.estimate

  logger::log_info(glue::glue("Returning logistic model with roc_auc training value of {roc_train} and test roc_auc of {roc_test}.",
    roc_train = round(train_roc, 4), roc_test = round(test_roc, 4)
  ))
  return(final_model)
} # 2 minutes. preprocess ok  ~0.71

buildQualiModel_svm <- function(model_data = combineData()) {
  # Thin Data
  model_data <- model_data %>%
    dplyr::select(c(
      # ID columns
      "raceId", "circuitId", "driverId", "currentConstructorId",
      # quali data (TARGET)
      "qPosition",
      # driver data
      "driverAge", "driverGPExperience", "driverSeasonWins",
      "driverSeasonWinsPerc", "driverPoints", "driverPointsPerc", "driverHomeRace",
      # constructor data
      "constructorPoints", "constructorSeasonWins", "constructorPointsPerc",
      "constructorSeasonWinsPerc", "constructorHomeRace",
      # circuit data
      "circuitAltitude", "circuitLength", "circuitType", "circuitDirection",
      # practice data
      "driverPercPracticeLaps", "constructorPercPracticeLaps", "driverPracticeBestPerc", "driverPracticeAvgPerc",
      "constructorPracticeBestPerc", "constructorPracticeAvgPerc", "driverTeamPracticeAvgGapPerc"
    )) %>%
    dplyr::filter(.data$qPosition > 0) %>%
    dplyr::filter(.data$qPosition <= 20) %>%
    dplyr::mutate("qPosition" = as.character(.data$qPosition))

  # Split data
  splitdata <- rsample::initial_split(model_data, 0.8)
  training_data <- rsample::training(splitdata)
  test_data <- rsample::testing(splitdata)

  tune_spec <- parsnip::svm_linear(cost = tune::tune()) %>%
    parsnip::set_mode("classification") %>%
    parsnip::set_engine("kernlab")

  init_recipe <- recipes::recipe(qPosition ~ ., training_data) %>%
    recipes::update_role("raceId", new_role = "ID") %>%
    recipes::update_role("circuitId", new_role = "ID") %>%
    recipes::update_role("driverId", new_role = "ID") %>%
    recipes::update_role("currentConstructorId", new_role = "ID") %>%
    recipes::step_string2factor(c("circuitType", "circuitDirection")) %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_normalize(recipes::all_numeric()) %>%
    recipes::step_nzv(recipes::all_predictors())

  tune_wf <- workflows::workflow() %>%
    workflows::add_recipe(init_recipe) %>%
    workflows::add_model(tune_spec)

  cv <- rsample::vfold_cv(training_data)

  grid <- dials::grid_max_entropy(
    dials::cost(),
    size = 30
  )

  logger::log_info("Setting up parallel for svm model")
  doParallel::registerDoParallel(cores = max(parallel::detectCores() - 2, 4))
  logger::log_info("Tuning svm model")
  model_res <- tune::tune_grid(tune_wf,
    resamples = cv,
    grid = grid,
    control = tune::control_grid(save_pred = TRUE, verbose = TRUE)
  )
  logger::log_info("Clean up parallel")
  doParallel::stopImplicitCluster()
  unregister_dopar()

  best_model <- tune::select_best(model_res, "roc_auc")

  final_model <- tune_wf %>%
    tune::finalize_workflow(best_model) %>%
    parsnip::fit(data = training_data)

  logger::log_info("Training svm model")
  last_fit <- final_model %>%
    tune::last_fit(splitdata)

  # This isn't used but checks for fails.
  test_fit <- final_model %>%
    stats::predict(new_data = test_data)

  train_roc <- tune::show_best(model_res, metric = "roc_auc", n = 1)$mean
  test_roc <- tune::collect_metrics(last_fit)[tune::collect_metrics(last_fit)$.metric == "roc_auc", ]$.estimate

  logger::log_info(glue::glue("Returning svm model with roc_auc training value of {roc_train} and test roc_auc of {roc_test}.",
    roc_train = round(train_roc, 4), roc_test = round(test_roc, 4)
  ))
  return(final_model)
} # reasonable. preprocess ok ~0.70

buildQualiModel_xgb <- function(model_data = combineData()) {
  # Thin Data
  model_data <- model_data %>%
    dplyr::select(c(
      # ID columns
      "raceId", "circuitId", "driverId", "currentConstructorId",
      # quali data (TARGET)
      "qPosition",
      # driver data
      "driverAge", "driverGPExperience", "driverSeasonWins",
      "driverSeasonWinsPerc", "driverPoints", "driverPointsPerc", "driverHomeRace",
      # constructor data
      "constructorPoints", "constructorSeasonWins", "constructorPointsPerc",
      "constructorSeasonWinsPerc", "constructorHomeRace",
      # circuit data
      "circuitAltitude", "circuitLength", "circuitType", "circuitDirection",
      # practice data
      "driverPercPracticeLaps", "constructorPercPracticeLaps", "driverPracticeBestPerc", "driverPracticeAvgPerc",
      "constructorPracticeBestPerc", "constructorPracticeAvgPerc", "driverTeamPracticeAvgGapPerc"
    )) %>%
    dplyr::filter(.data$qPosition > 0) %>%
    dplyr::filter(.data$qPosition <= 20) %>%
    dplyr::mutate("qPosition" = as.character(.data$qPosition))

  # Split data
  splitdata <- rsample::initial_split(model_data, 0.8)
  training_data <- rsample::training(splitdata)
  test_data <- rsample::testing(splitdata)

  tune_spec <- parsnip::boost_tree(
    trees = tune::tune(), tree_depth = tune::tune(), min_n = tune::tune(), mtry = tune::tune(),
    loss_reduction = tune::tune(), sample_size = tune::tune(), learn_rate = tune::tune()
  ) %>%
    parsnip::set_mode("classification") %>%
    parsnip::set_engine("xgboost", scale_pos_weight = tune::tune(), max_delta_step = 1)

  init_recipe <- recipes::recipe(qPosition ~ ., training_data) %>%
    recipes::update_role("raceId", new_role = "ID") %>%
    recipes::update_role("circuitId", new_role = "ID") %>%
    recipes::update_role("driverId", new_role = "ID") %>%
    recipes::update_role("currentConstructorId", new_role = "ID") %>%
    recipes::step_string2factor(c("circuitType", "circuitDirection")) %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_normalize(recipes::all_numeric()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_center(recipes::all_predictors())

  tune_wf <- workflows::workflow() %>%
    workflows::add_recipe(init_recipe) %>%
    workflows::add_model(tune_spec)

  cv <- rsample::vfold_cv(training_data)

  grid <- dials::grid_latin_hypercube(
    dials::trees(),
    dials::tree_depth(),
    dials::min_n(),
    dials::loss_reduction(),
    sample_size = dials::sample_prop(),
    dials::finalize(dials::mtry(), train_data),
    dials::learn_rate(),
    dials::scale_pos_weight(range = c(8, 10)),
    size = 30
  )

  logger::log_info("Setting up parallel for xgb model")
  doParallel::registerDoParallel(cores = 2)
  logger::log_info("Tuning xgb model")
  model_res <- tune::tune_grid(
    tune_wf,
    resamples = cv,
    grid = grid,
    control = tune::control_grid(save_pred = TRUE, verbose = TRUE)
  )
  logger::log_info("Clean up parallel")
  doParallel::stopImplicitCluster()

  best_model <- tune::select_best(model_res, "roc_auc")

  final_model <- tune_wf %>%
    tune::finalize_workflow(best_model) %>%
    parsnip::fit(data = training_data)

  logger::log_info("Training xgb model")
  last_fit <- final_model %>%
    tune::last_fit(splitdata)

  # This isn't used but checks for fails.
  test_fit <- final_model %>%
    stats::predict(new_data = test_data)

  train_roc <- tune::show_best(model_res, metric = "roc_auc", n = 1)$mean
  test_roc <- tune::collect_metrics(last_fit)[tune::collect_metrics(last_fit)$.metric == "roc_auc", ]$.estimate

  logger::log_info(glue::glue("Returning xgb model with roc_auc training value of {roc_train} and test roc_auc of {roc_test}.",
    roc_train = round(train_roc, 4), roc_test = round(test_roc, 4)
  ))
  return(final_model)
} # SLOW. preprocess ok.

buildQualiModel_knn <- function(model_data = combineData()) {
  # Thin Data
  model_data <- model_data %>%
    dplyr::select(c(
      # ID columns
      "raceId", "circuitId", "driverId", "currentConstructorId",
      # quali data (TARGET)
      "qPosition",
      # driver data
      "driverAge", "driverGPExperience", "driverSeasonWins",
      "driverSeasonWinsPerc", "driverPoints", "driverPointsPerc", "driverHomeRace",
      # constructor data
      "constructorPoints", "constructorSeasonWins", "constructorPointsPerc",
      "constructorSeasonWinsPerc", "constructorHomeRace",
      # circuit data
      "circuitAltitude", "circuitLength", "circuitType", "circuitDirection",
      # practice data
      "driverPercPracticeLaps", "constructorPercPracticeLaps", "driverPracticeBestPerc", "driverPracticeAvgPerc",
      "constructorPracticeBestPerc", "constructorPracticeAvgPerc", "driverTeamPracticeAvgGapPerc"
    )) %>%
    dplyr::filter(.data$qPosition > 0) %>%
    dplyr::filter(.data$qPosition <= 20) %>%
    dplyr::mutate("qPosition" = as.character(.data$qPosition)) %>%
    dplyr::mutate("qPosition" = as.factor(.data$qPosition))

  # Split data
  splitdata <- rsample::initial_split(model_data, 0.8)
  training_data <- rsample::training(splitdata)
  test_data <- rsample::testing(splitdata)

  tune_spec <- parsnip::nearest_neighbor(neighbors = tune::tune()) %>%
    parsnip::set_mode("classification") %>%
    parsnip::set_engine("kknn")

  init_recipe <- recipes::recipe(qPosition ~ ., training_data) %>%
    recipes::update_role("raceId", new_role = "ID") %>%
    recipes::update_role("circuitId", new_role = "ID") %>%
    recipes::update_role("driverId", new_role = "ID") %>%
    recipes::update_role("currentConstructorId", new_role = "ID") %>%
    recipes::step_string2factor(c("circuitType", "circuitDirection")) %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_normalize(recipes::all_predictors()) %>%
    recipes::step_nzv(recipes::all_predictors())

  tune_wf <- workflows::workflow() %>%
    workflows::add_recipe(init_recipe) %>%
    workflows::add_model(tune_spec)

  cv <- rsample::vfold_cv(training_data)

  grid <- dials::grid_latin_hypercube(
    dials::neighbors()
  )

  logger::log_info("Setting up parallel for knn model")
  doParallel::registerDoParallel(cores = parallel::detectCores(logical = F) - 2)
  logger::log_info("Tuning knn model")
  model_res <- tune::tune_grid(
    tune_wf,
    resamples = cv,
    grid = grid,
    control = tune::control_grid(save_pred = TRUE, verbose = TRUE)
  )
  logger::log_info("Clean up parallel")
  doParallel::stopImplicitCluster()

  best_model <- tune::select_best(model_res, "roc_auc")

  final_model <- tune_wf %>%
    tune::finalize_workflow(best_model) %>%
    parsnip::fit(data = training_data)

  logger::log_info("Training knn model")
  last_fit <- final_model %>%
    tune::last_fit(splitdata)

  # This isn't used but checks for fails.
  test_fit <- final_model %>%
    stats::predict(new_data = test_data)

  train_roc <- tune::show_best(model_res, metric = "roc_auc", n = 1)$mean
  test_roc <- tune::collect_metrics(last_fit)[tune::collect_metrics(last_fit)$.metric == "roc_auc", ]$.estimate

  logger::log_info(glue::glue("Returning knn model with roc_auc training value of {roc_train} and test roc_auc of {roc_test}.",
    roc_train = round(train_roc, 4), roc_test = round(test_roc, 4)
  ))
  return(final_model)
} # 20s preprocess ok. ~0.60

buildQualiModel_nb <- function(model_data = combineData()) {
  # Thin Data
  model_data <- model_data %>%
    dplyr::select(c(
      # ID columns
      "raceId", "circuitId", "driverId", "currentConstructorId",
      # quali data (TARGET)
      "qPosition",
      # driver data
      "driverAge", "driverGPExperience", "driverSeasonWins",
      "driverSeasonWinsPerc", "driverPoints", "driverPointsPerc", "driverHomeRace",
      # constructor data
      "constructorPoints", "constructorSeasonWins", "constructorPointsPerc",
      "constructorSeasonWinsPerc", "constructorHomeRace",
      # circuit data
      "circuitAltitude", "circuitLength", "circuitType", "circuitDirection",
      # practice data
      "driverPercPracticeLaps", "constructorPercPracticeLaps", "driverPracticeBestPerc", "driverPracticeAvgPerc",
      "constructorPracticeBestPerc", "constructorPracticeAvgPerc", "driverTeamPracticeAvgGapPerc"
    )) %>%
    dplyr::filter(.data$qPosition > 0) %>%
    dplyr::filter(.data$qPosition <= 20) %>%
    dplyr::mutate("qPosition" = as.character(.data$qPosition))

  # Split data
  splitdata <- rsample::initial_split(model_data, 0.8)
  training_data <- rsample::training(splitdata)
  test_data <- rsample::testing(splitdata)

  require("discrim")

  tune_spec <- parsnip::naive_Bayes(
    smoothness = tune::tune(),
    Laplace = tune::tune()
  ) %>%
    parsnip::set_mode("classification") %>%
    parsnip::set_engine("klaR")

  init_recipe <- recipes::recipe(qPosition ~ ., training_data) %>%
    recipes::update_role("raceId", new_role = "ID") %>%
    recipes::update_role("circuitId", new_role = "ID") %>%
    recipes::update_role("driverId", new_role = "ID") %>%
    recipes::update_role("currentConstructorId", new_role = "ID") %>%
    recipes::step_string2factor(c("circuitType", "circuitDirection")) %>%
    recipes::step_nzv()

  tune_wf <- workflows::workflow() %>%
    workflows::add_recipe(init_recipe) %>%
    workflows::add_model(tune_spec)

  cv <- rsample::vfold_cv(training_data)

  grid <- dials::grid_max_entropy(
    dials::smoothness(),
    dials::Laplace(),
    size = 30
  )

  logger::log_info("Setting up parallel for tuning nb model")
  doParallel::registerDoParallel(cores = parallel::detectCores(logical = F) - 2)
  logger::log_info("Tuning nb model")
  model_res <- tune::tune_grid(tune_wf,
    resamples = cv,
    grid = grid,
    control = tune::control_grid(save_pred = TRUE, verbose = TRUE)
  )
  logger::log_info("Clean up parallel")
  doParallel::stopImplicitCluster()

  best_model <- tune::select_best(model_res, "roc_auc")

  final_model <- tune_wf %>%
    tune::finalize_workflow(best_model) %>%
    parsnip::fit(data = training_data)

  logger::log_info("Training nb model")
  last_fit <- final_model %>%
    tune::last_fit(splitdata)

  # This isn't used but checks for fails.
  test_fit <- final_model %>%
    stats::predict(new_data = test_data)

  train_roc <- tune::show_best(model_res, metric = "roc_auc", n = 1)$mean
  test_roc <- tune::collect_metrics(last_fit)[tune::collect_metrics(last_fit)$.metric == "roc_auc", ]$.estimate

  logger::log_info(glue::glue("Returning naive bayes model with roc_auc training value of {roc_train} and test roc_auc of {roc_test}.",
    roc_train = round(train_roc, 4), roc_test = round(test_roc, 4)
  ))
  return(final_model)
} # 20 minutes - preprocess ok ~0.72

buildQualiModel_dt <- function(model_data = combineData()) {
  # Thin Data
  model_data <- model_data %>%
    dplyr::select(c(
      # ID columns
      "raceId", "circuitId", "driverId", "currentConstructorId",
      # quali data (TARGET)
      "qPosition",
      # driver data
      "driverAge", "driverGPExperience", "driverSeasonWins",
      "driverSeasonWinsPerc", "driverPoints", "driverPointsPerc", "driverHomeRace",
      # constructor data
      "constructorPoints", "constructorSeasonWins", "constructorPointsPerc",
      "constructorSeasonWinsPerc", "constructorHomeRace",
      # circuit data
      "circuitAltitude", "circuitLength", "circuitType", "circuitDirection",
      # practice data
      "driverPercPracticeLaps", "constructorPercPracticeLaps", "driverPracticeBestPerc", "driverPracticeAvgPerc",
      "constructorPracticeBestPerc", "constructorPracticeAvgPerc", "driverTeamPracticeAvgGapPerc"
    )) %>%
    dplyr::filter(.data$qPosition > 0) %>%
    dplyr::filter(.data$qPosition <= 20) %>%
    dplyr::mutate("qPosition" = as.character(.data$qPosition))

  # Split data
  splitdata <- rsample::initial_split(model_data, 0.8)
  training_data <- rsample::training(splitdata)
  test_data <- rsample::testing(splitdata)

  require(rules)

  tune_spec <- parsnip::decision_tree(
    cost_complexity = tune::tune(),
    tree_depth = tune::tune()
  ) %>%
    parsnip::set_mode("classification") %>%
    parsnip::set_engine("rpart")

  init_recipe <- recipes::recipe(qPosition ~ ., training_data) %>%
    recipes::update_role("raceId", new_role = "ID") %>%
    recipes::update_role("circuitId", new_role = "ID") %>%
    recipes::update_role("driverId", new_role = "ID") %>%
    recipes::update_role("currentConstructorId", new_role = "ID") %>%
    recipes::step_string2factor(c("circuitType", "circuitDirection")) %>%
    recipes::step_nzv(recipes::all_predictors())

  tune_wf <- workflows::workflow() %>%
    workflows::add_recipe(init_recipe) %>%
    workflows::add_model(tune_spec)

  cv <- rsample::vfold_cv(training_data)

  grid <- dials::grid_max_entropy(
    dials::cost_complexity(),
    dials::tree_depth(),
    size = 30
  )

  logger::log_info("Setting up parallel for tuning decision tree model")
  doParallel::registerDoParallel(cores = parallel::detectCores(logical = F) - 2)
  logger::log_info("Tuning decision tree model")
  model_res <- tune::tune_grid(tune_wf,
    resamples = cv,
    grid = grid,
    control = tune::control_grid(save_pred = TRUE, verbose = TRUE)
  )
  logger::log_info("Clean up parallel")
  doParallel::stopImplicitCluster()

  best_model <- tune::select_best(model_res, "roc_auc")

  final_model <- tune_wf %>%
    tune::finalize_workflow(best_model) %>%
    parsnip::fit(data = training_data)

  logger::log_info("Training C5.0 rules model")
  last_fit <- final_model %>%
    tune::last_fit(splitdata)

  # This isn't used but checks for fails.
  test_fit <- final_model %>%
    stats::predict(new_data = test_data)

  train_roc <- tune::show_best(model_res, metric = "roc_auc", n = 1)$mean
  test_roc <- tune::collect_metrics(last_fit)[tune::collect_metrics(last_fit)$.metric == "roc_auc", ]$.estimate

  logger::log_info(glue::glue("Returning decision tree model with roc_auc training value of {roc_train} and test roc_auc of {roc_test}.",
    roc_train = round(train_roc, 4), roc_test = round(test_roc, 4)
  ))
  return(final_model)
} # 2 minutes, preprocess ok ~0.74

buildQualiModel_CART <- function(model_data = combineData()) {
  # Thin Data
  model_data <- model_data %>%
    dplyr::select(c(
      # ID columns
      "raceId", "circuitId", "driverId", "currentConstructorId",
      # quali data (TARGET)
      "qPosition",
      # driver data
      "driverAge", "driverGPExperience", "driverSeasonWins",
      "driverSeasonWinsPerc", "driverPoints", "driverPointsPerc", "driverHomeRace",
      # constructor data
      "constructorPoints", "constructorSeasonWins", "constructorPointsPerc",
      "constructorSeasonWinsPerc", "constructorHomeRace",
      # circuit data
      "circuitAltitude", "circuitLength", "circuitType", "circuitDirection",
      # practice data
      "driverPercPracticeLaps", "constructorPercPracticeLaps", "driverPracticeBestPerc", "driverPracticeAvgPerc",
      "constructorPracticeBestPerc", "constructorPracticeAvgPerc", "driverTeamPracticeAvgGapPerc"
    )) %>%
    dplyr::filter(.data$qPosition > 0) %>%
    dplyr::filter(.data$qPosition <= 20) %>%
    dplyr::mutate("qPosition" = as.character(.data$qPosition))

  # Split data
  splitdata <- rsample::initial_split(model_data, 0.8)
  training_data <- rsample::training(splitdata)
  test_data <- rsample::testing(splitdata)

  tune_spec <- parsnip::decision_tree(
    tree_depth = tune::tune(),
    min_n = tune::tune(),
    cost_complexity = tune::tune()
  ) %>%
    parsnip::set_mode("classification") %>%
    parsnip::set_engine("rpart")

  init_recipe <- recipes::recipe(qPosition ~ ., training_data) %>%
    recipes::update_role("raceId", new_role = "ID") %>%
    recipes::update_role("circuitId", new_role = "ID") %>%
    recipes::update_role("driverId", new_role = "ID") %>%
    recipes::update_role("currentConstructorId", new_role = "ID") %>%
    recipes::step_string2factor(c("circuitType", "circuitDirection")) %>%
    recipes::step_dummy(recipes::all_nominal_predictors())

  tune_wf <- workflows::workflow() %>%
    workflows::add_recipe(init_recipe) %>%
    workflows::add_model(tune_spec)

  cv <- rsample::vfold_cv(training_data)

  grid <- dials::grid_max_entropy(
    dials::tree_depth(),
    dials::min_n(),
    dials::cost_complexity(),
    size = 30
  )

  logger::log_info("Setting up parallel for tuning CART model")
  doParallel::registerDoParallel(cores = parallel::detectCores(logical = F) - 2)
  logger::log_info("Tuning CART model")
  model_res <- tune::tune_grid(tune_wf,
    resamples = cv,
    grid = grid,
    control = tune::control_grid(save_pred = TRUE, verbose = TRUE)
  )
  logger::log_info("Clean up parallel")
  doParallel::stopImplicitCluster()

  best_model <- tune::select_best(model_res, "roc_auc")

  final_model <- tune_wf %>%
    tune::finalize_workflow(best_model) %>%
    parsnip::fit(data = training_data)

  logger::log_info("Training CART model")
  last_fit <- final_model %>%
    tune::last_fit(splitdata)

  # This isn't used but checks for fails.
  test_fit <- final_model %>%
    stats::predict(new_data = test_data)

  train_roc <- tune::show_best(model_res, metric = "roc_auc", n = 1)$mean
  test_roc <- tune::collect_metrics(last_fit)[tune::collect_metrics(last_fit)$.metric == "roc_auc", ]$.estimate

  logger::log_info(glue::glue("Returning CART model with roc_auc training value of {roc_train} and test roc_auc of {roc_test}.",
    roc_train = round(train_roc, 4), roc_test = round(test_roc, 4)
  ))
  return(final_model)
} # 2 minutes, preprocess ok ~0.73


buildQualiModel_mix <- function(model_data = combineData()) {
  # ----- Thin Data -----
  model_data <- model_data %>%
    dplyr::select(c(
      # ID columns
      "raceId", "circuitId", "driverId", "currentConstructorId",
      # quali data (TARGET)
      "qPosition",
      # driver data
      "driverDOB", "driverAge", "driverGPExperience", "driverSeasonWins",
      "driverSeasonWinsPerc", "driverPoints", "driverPointsPerc", "driverHomeRace",
      # constructor data
      "constructorPoints", "constructorSeasonWins", "constructorPointsPerc",
      "constructorSeasonWinsPerc", "constructorHomeRace",
      # circuit data
      "circuitAltitude", "circuitLength", "circuitType", "circuitDirection",
      # practice data
      "driverPercPracticeLaps", "constructorPercPracticeLaps", "driverPracticeBestPerc", "driverPracticeAvgPerc",
      "constructorPracticeBestPerc", "constructorPracticeAvgPerc", "driverTeamPracticeAvgGapPerc"
    )) %>%
    dplyr::filter(.data$qPosition > 0) %>%
    dplyr::filter(.data$qPosition <= 20) %>%
    dplyr::mutate("qPosition" = as.character(.data$qPosition))

  # Split data
  splitdata <- rsample::initial_split(model_data, 0.8)
  training_data <- rsample::training(splitdata)
  test_data <- rsample::testing(splitdata)

  folds <- rsample::vfold_cv(training_data)

  # ----- General setup -----
  model_recipe <- recipes::recipe(qPosition ~ ., training_data) %>%
    recipes::update_role("raceId", new_role = "ID") %>%
    recipes::update_role("circuitId", new_role = "ID") %>%
    recipes::update_role("driverId", new_role = "ID") %>%
    recipes::update_role("currentConstructorId", new_role = "ID") %>%
    recipes::step_string2factor(c("qPosition", "circuitType", "circuitDirection")) %>%
    recipes::step_nzv(recipes::all_predictors())

  model_workflow <- workflows::workflow() %>%
    workflows::add_recipe(model_recipe)

  ctrl_grid <- stacks::control_stack_grid()

  # ----- Decision Tree -----
  decision_tree_spec <- parsnip::decision_tree(
    cost_complexity = tune::tune(),
    tree_depth = tune::tune()
  ) %>%
    parsnip::set_mode("classification") %>%
    parsnip::set_engine("rpart")

  decision_tree_wflow <-
    model_workflow %>%
    workflows::add_model(decision_tree_spec)

  decision_tree_res <-
    tune::tune_grid(
      object = decision_tree_wflow,
      resamples = folds,
      grid = 10,
      control = ctrl_grid
    )

  # ----- Naive Bayes -----
  # naive_bayes_spec <- parsnip::naive_Bayes(
  #   smoothness = tune::tune(),
  #   Laplace = tune::tune()
  # ) %>%
  #   parsnip::set_mode("classification") %>%
  #   parsnip::set_engine("klaR")
  #
  # naive_bayes_wflow <-
  #   model_workflow %>%
  #   add_model(naive_bayes_spec)
  #
  # naive_bayes_res <-
  #   tune::tune_grid(
  #     object = naive_bayes_wflow,
  #     resamples = folds,
  #     grid = 10,
  #     control = ctrl_grid
  #   )

  # ----- SVM Linear - s2f, dummy, norm, nzv-----

  # ----- KNN -----
  knn_spec <- parsnip::nearest_neighbor(
    neighbors = tune::tune()
    ) %>%
    parsnip::set_mode("classification") %>%
    parsnip::set_engine("kknn")

  knn_rec <-
    model_recipe %>%
    recipes::step_normalize(recipes::all_numeric_predictors()) %>%
    recipes::step_nzv(recipes::all_predictors())

  knn_workflow <-
    model_workflow %>%
    workflows::add_model(knn_spec) %>%
    workflows::update_recipe(knn_rec)

  knn_res <- tune::tune_grid(
    object = knn_workflow,
    resamples = folds,
    grid = 10,
    control = ctrl_grid
  )

  # ----- GLM - s2f, dummy, nzv  -----
  glm_spec <- parsnip::multinom_reg(
    penalty = tune::tune(),
    mixture = tune::tune()
  ) %>%
    parsnip::set_mode("classification") %>%
    parsnip::set_engine("glmnet")

  glm_rec <-
    model_recipe %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_nzv(recipes::all_predictors())

  glm_workflow <-
    model_workflow %>%
    workflows::add_model(glm_spec) %>%
    workflows::update_recipe(glm_rec)

  glm_res <- tune::tune_grid(
    object = glm_workflow,
    resamples = folds,
    grid = 10,
    control = ctrl_grid
  )
  # ----- CART - s2f, dummy, nzv  -----

  # ----- STACK IT -----
  model_stack <-
    # initialize the stack
    stacks::stacks() %>%
    # add candidate members
    stacks::add_candidates(decision_tree_res) %>%
    #stacks::add_candidates(naive_bayes_res) %>%
    stacks::add_candidates(knn_res) %>%
    stacks::add_candidates(glm_res) %>%
    # determine how to combine their predictions
    stacks::blend_predictions() %>%
    # fit the candidates with nonzero stacking coefficients
    stacks::fit_members()

  model_pred <-
    test_data %>%
    bind_cols(predict(model_stack, ., type = "prob"))

  yardstick::roc_auc(
    model_pred,
    truth = qPosition,
    contains(".pred_")
  )

  return(model_stack)
}
