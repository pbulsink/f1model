buildRaceModel_mix <- function(model_data = combineData()) {
  # ----- Thin Data -----
  model_data <- model_data %>%
    dplyr::select(c(
      # ID columns
      "raceId", "circuitId", "driverId", "currentConstructorId",
      # Race Finish (TARGET) (note: if reused for MC )
      "position",
      #Race Info
      "weather", "grid",
      # quali data
      "qPosition", "qGapPerc",
      # driver data
      "driverDOB", "driverAge", "driverGPExperience", "driverSeasonWins",
      "driverSeasonWinsPerc", "driverPoints", "driverPointsPerc", "driverHomeRace",
      "driverCrashRate", "disqualifiedRate", "tireFailureRate",
      # constructor data
      "constructorPoints", "constructorSeasonWins", "constructorPointsPerc",
      "constructorSeasonWinsPerc", "constructorHomeRace", "carFailureRate",
      # circuit data
      "circuitAltitude", "circuitLength", "circuitType", "circuitDirection",
      # practice data
      "driverPercPracticeLaps", "constructorPercPracticeLaps", "driverPracticeBestPerc", "driverPracticeAvgPerc",
      "constructorPracticeBestPerc", "constructorPracticeAvgPerc", "driverTeamPracticeAvgGapPerc",
    )) %>%
    dplyr::filter(.data$qPosition > 0) %>%
    dplyr::filter(.data$qPosition <= 20) %>%
    dplyr::mutate("position" = as.character(.data$position))

  # Split data
  splitdata <- rsample::initial_split(model_data, 0.8)
  training_data <- rsample::training(splitdata)
  test_data <- rsample::testing(splitdata)

  folds <- rsample::vfold_cv(training_data)

  # ----- General setup -----
  model_recipe <- recipes::recipe(position ~ ., training_data) %>%
    recipes::update_role("raceId", new_role = "ID") %>%
    recipes::update_role("circuitId", new_role = "ID") %>%
    recipes::update_role("driverId", new_role = "ID") %>%
    recipes::update_role("currentConstructorId", new_role = "ID") %>%
    recipes::step_string2factor(c("position", "circuitType", "circuitDirection")) %>%
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
  svm_spec <- parsnip::svm_linear(
    penalty = tune::tune(),
    mixture = tune::tune()
  ) %>%
    parsnip::set_mode("classification") %>%
    parsnip::set_engine("kernlab")

  svm_rec <-
    model_recipe %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_nzv(recipes::all_predictors()) %>%
    recipes::step_normalize(recipes::all_predictors())

  svm_workflow <-
    model_workflow %>%
    workflows::add_model(svm_spec) %>%
    workflows::update_recipe(svm_rec)

  svm_res <- tune::tune_grid(
    object = svm_workflow,
    resamples = folds,
    grid = 10,
    control = ctrl_grid
  )

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

  # ----- STACK IT -----
  model_stack <-
    # initialize the stack
    stacks::stacks() %>%
    # add candidate members
    stacks::add_candidates(decision_tree_res) %>%
    # stacks::add_candidates(naive_bayes_res) %>%
    stacks::add_candidates(knn_res) %>%
    stacks::add_candidates(glm_res) %>%
    stacks::add_candidates(svm_res) %>%
    # determine how to combine their predictions
    stacks::blend_predictions() %>%
    # fit the candidates with nonzero stacking coefficients
    stacks::fit_members()

  model_pred <-
    test_data %>%
    bind_cols(predict(model_stack, ., type = "prob"))

  yardstick::roc_auc(
    model_pred,
    truth = position,
    contains(".pred_")
  )

  return(model_stack)
}
