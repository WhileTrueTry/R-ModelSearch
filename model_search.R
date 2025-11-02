#' Busca los mejores modelos lineales usando un "banco de términos".
#'
#' @param dataset Un data.frame en formato tidy.
#' @param y Un string con el nombre de la variable dependiente.
#' @param k_best Un entero que indica el número de los mejores modelos a devolver.
#' @param cuadratic Booleano (TRUE/FALSE) para incluir términos cuadráticos.
#' @param interactions Booleano (TRUE/FALSE) para incluir términos de interacción.
#' @param log Booleano (TRUE/FALSE) para incluir términos logarítmicos (log(x)).
#' @param results Booleano (TRUE/FALSE) para devolver un data.frame con los resultados.
#' @param max_predictors_in_model Un entero o NULL. Limita el número máximo de términos
#'        en una fórmula. Recomendado para controlar el tiempo de ejecución.
#'
#' @return Una lista con `best_models` (lista de 'lm') y `all_results` (data.frame).
#'
#' @importFrom utils combn
#'
model_search <- function(dataset, y, k_best, 
                         cuadratic = FALSE, 
                         interactions = FALSE,
                         log = FALSE, 
                         results = FALSE,
                         max_predictors_in_model = 3,
                         min_sig_ratio = 0.75,
                         allow_singular = FALSE) {
  
  # --- 1. Funciones Auxiliares (ACTUALIZADO) ---
  
  safe_lm <- function(formula_str, data) {
    
    # 1. Intentar crear el modelo
    model <- tryCatch({
      lm(as.formula(formula_str), data = data, model = TRUE) # Incluye model=TRUE
    }, error = function(e) {
      return(NULL) 
    })
    
    # 2. Si el modelo falló, devolver NULL
    if (is.null(model)) { return(NULL) }
    
    # --- ¡NUEVO BLOQUE V3.7! ---
    # 3. Arreglar el 'call' para que se imprima de forma legible.
    #    Si no hacemos esto, imprime 'as.formula(formula_str)'
    
    # Convertir el string de la fórmula en un objeto formula real
    formula_obj <- as.formula(formula_str)
    
    # Sobrescribir el '$call' del modelo con una versión "limpia"
    # Usamos 'quote(dataset)' para que el 'call' muestre 'data = dataset'
    # en lugar de imprimir el data.frame entero en la consola.
    model$call <- substitute(
      lm(formula = formula_obj, data = quote(dataset), model = TRUE),
      list(formula_obj = formula_obj)
    )
    # --- FIN DEL NUEVO BLOQUE ---
    
    # 4. Devolver el modelo arreglado
    return(model)
  }
  
  get_model_stats <- function(model) {
    if (is.null(model)) {
      return(list(adj_r_squared = NA, f_p_value = NA, is_significant = FALSE, aic = NA, 
                  ratio_significant_params = NA, is_singular = TRUE))
    }
    is_singular <- any(is.na(model$coefficients))
    if (is_singular && !allow_singular) {
      return(list(adj_r_squared = NA, f_p_value = NA, is_significant = FALSE, aic = NA, 
                  ratio_significant_params = NA, is_singular = TRUE))
    }
    sum_model <- summary(model)
    f_p_value <- NA
    if (!is.null(sum_model$fstatistic)) {
      f_stat <- sum_model$fstatistic
      f_p_value <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
    }
    coeffs <- sum_model$coefficients
    num_params <- nrow(coeffs)
    ratio_significant_params <- NA
    has_intercept <- rownames(coeffs)[1] == "(Intercept)"
    num_predictors <- num_params - (if (has_intercept) 1 else 0)
    if (num_predictors > 0) {
      predictor_rows <- if (has_intercept) -1 else TRUE
      coeffs_predictors <- coeffs[predictor_rows, , drop = FALSE]
      p_values <- coeffs_predictors[, "Pr(>|t|)"]
      p_values_no_na <- p_values < 0.05
      p_values_no_na[is.na(p_values_no_na)] <- FALSE
      significant_count <- sum(p_values_no_na, na.rm = TRUE)
      ratio_significant_params <- significant_count / num_predictors
    } else if (num_params > 0) { ratio_significant_params <- 0 }
    list(
      adj_r_squared = sum_model$adj.r.squared, f_p_value = f_p_value,
      is_significant = !is.na(f_p_value) && f_p_value < 0.05, aic = stats::AIC(model),
      ratio_significant_params = ratio_significant_params, is_singular = is_singular
    )
  }
  
  get_anova_pvalue <- function(model_simple, model_complex) {
    if (is.null(model_simple) || is.null(model_complex) || 
        any(is.na(model_simple$coefficients)) || any(is.na(model_complex$coefficients))) {
      return(NA)
    }
    tryCatch({ anova_test <- anova(model_simple, model_complex); return(anova_test$"Pr(>F)"[2])
    }, error = function(e) { return(NA) })
  }
  
  # --- 2. Inicialización y Creación del "Banco de Términos" ---
  dataset <- as.data.frame(dataset) 
  predictors <- setdiff(names(dataset), y)
  numeric_predictors <- names(dataset)[sapply(dataset, is.numeric)]
  numeric_predictors <- setdiff(numeric_predictors, y)
  all_terms <- predictors
  if (cuadratic) { all_terms <- c(all_terms, paste0("I(", numeric_predictors, "^2)")) }
  if (log) {
    for (var in numeric_predictors) {
      if (any(dataset[[var]] <= 0, na.rm = TRUE)) {
        warning(paste("Variable '", var, "' tiene valores <= 0. Se omite log(", var, ")."), call. = FALSE)
      } else { all_terms <- c(all_terms, paste0("log(", var, ")")) }
    }
  }
  all_terms <- unique(all_terms)
  n_terms <- length(all_terms)
  
  # --- 2.5. Configuración del Límite de Modelos y Advertencia ---
  max_i <- n_terms
  if (!is.null(max_predictors_in_model)) {
    if(max_predictors_in_model < 1) { stop("max_predictors_in_model debe ser al menos 1.") }
    max_i <- min(n_terms, max_predictors_in_model)
  }
  n_combinations <- sum(sapply(1:max_i, function(k) choose(n_terms, k)))
  max_models_approx <- n_combinations * (1 + (interactions > 0))
  if (max_models_approx > 1000) {
    warning(paste("Alerta: Se probarán hasta", format(max_models_approx, big.mark=","), "modelos."), call. = FALSE)
  }
  
  all_models_results <- list()
  
  # --- 3. Bucle Principal ---
  for (i in 1:max_i) {
    combinations <- utils::combn(all_terms, i, simplify = FALSE)
    for (combo in combinations) {
      
      formula_base_str <- paste(y, "~", paste(combo, collapse = " + "))
      model_base <- safe_lm(formula_base_str, dataset)
      
      if (is.null(model_base)) {
        all_models_results[[formula_base_str]] <- list(formula = formula_base_str, model_object = NULL, is_singular = TRUE)
        next
      }
      stats_base <- get_model_stats(model_base)
      all_models_results[[formula_base_str]] <- c(list(formula = formula_base_str, model_object = model_base), stats_base,
                                                  list(parent_formula = NA, anova_p_value = NA, passes_anova = NA))
      if (stats_base$is_singular && !allow_singular) { next }
      if (interactions && length(combo) > 1) {
        formula_interact_str <- paste(y, "~", paste(combo, collapse = " * "))
        if (formula_interact_str %in% names(all_models_results)) { next }
        model_interact <- safe_lm(formula_interact_str, dataset)
        if (is.null(model_interact)) {
          all_models_results[[formula_interact_str]] <- list(formula = formula_interact_str, model_object = NULL, is_singular = TRUE, parent_formula = formula_base_str)
          next
        }
        stats_interact <- get_model_stats(model_interact)
        anova_p <- get_anova_pvalue(model_base, model_interact)
        all_models_results[[formula_interact_str]] <- c(list(formula = formula_interact_str, model_object = model_interact), stats_interact,
                                                        list(parent_formula = formula_base_str, anova_p_value = anova_p, 
                                                             passes_anova = !is.na(anova_p) && anova_p < 0.05))
      }
    }
  }
  
  # --- 4. Procesar y Filtrar Resultados ---
  results_df <- do.call(rbind, lapply(all_models_results, function(x) {
    data.frame(
      formula = x$formula,
      adj_r_squared = ifelse(is.null(x$adj_r_squared), NA, x$adj_r_squared),
      aic = ifelse(is.null(x$aic), NA, x$aic),
      f_p_value = ifelse(is.null(x$f_p_value), NA, x$f_p_value),
      is_significant = ifelse(is.null(x$is_significant), FALSE, x$is_significant),
      ratio_significant_params = ifelse(is.null(x$ratio_significant_params), NA, x$ratio_significant_params),
      is_singular = ifelse(is.null(x$is_singular), TRUE, x$is_singular),
      parent_formula = ifelse(is.null(x$parent_formula), NA, x$parent_formula),
      anova_p_value = ifelse(is.null(x$anova_p_value), NA, x$anova_p_value),
      passes_anova = ifelse(is.null(x$passes_anova), NA, x$passes_anova),
      stringsAsFactors = FALSE
    )
  }))
  
  if (!allow_singular) {
    results_df$is_singular[is.na(results_df$is_singular)] <- TRUE
    results_df <- subset(results_df, is_singular == FALSE)
  }
  if (nrow(results_df) == 0) {
    warning("No se pudo ajustar ningún modelo con éxito (o todos eran singulares).", call. = FALSE)
    return(list(best_models = list(), all_results = NULL))
  }
  candidates_df <- subset(results_df, is_significant == TRUE)
  candidates_df$ratio_significant_params[is.na(candidates_df$ratio_significant_params)] <- 0
  candidates_df <- subset(candidates_df, ratio_significant_params >= min_sig_ratio)
  if (nrow(candidates_df) == 0) {
    warning(paste("No se encontraron modelos que pasen los filtros de significancia."), call. = FALSE)
    return(list(best_models = list(), all_results = if(results) results_df else NULL))
  }
  final_candidates <- subset(candidates_df, is.na(parent_formula) | passes_anova == TRUE)
  if (nrow(final_candidates) == 0) {
    warning("Ningún modelo significativo pasó el filtro de complejidad ANOVA.", call. = FALSE)
    return(list(best_models = list(), all_results = if(results) results_df else NULL))
  }
  
  # --- 5. Ranking y Selección Final ---
  ranked_df <- final_candidates[order(final_candidates$adj_r_squared, decreasing = TRUE), ]
  top_k_df <- head(ranked_df, k_best)
  top_k_formulas <- top_k_df$formula
  
  # --- 6. Formatear Salida ---
  best_models_list <- lapply(top_k_formulas, function(f) { all_models_results[[f]]$model_object })
  names(best_models_list) <- top_k_formulas
  
  all_results_output <- NULL
  if (results) {
    all_results_output <- results_df[order(results_df$adj_r_squared, decreasing = TRUE, na.last = TRUE), ]
    all_results_output$is_top_k <- all_results_output$formula %in% top_k_formulas
    all_results_output <- all_results_output[, c("is_top_k", setdiff(names(all_results_output), "is_top_k"))]
    rownames(all_results_output) <- NULL
  }
  
  return(list(best_models = best_models_list, all_results = all_results_output))
}