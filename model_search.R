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
                         interactions = FALSE, # <-- CAMBIO 1
                         log = FALSE, 
                         results = FALSE,
                         max_predictors_in_model = 3) {
  
  # --- 1. Funciones Auxiliares ---
  
  safe_lm <- function(formula_str, data) {
    tryCatch({
      lm(as.formula(formula_str), data = data)
    }, error = function(e) {
      return(NULL) 
    })
  }
  
  get_model_stats <- function(model) {
    if (is.null(model)) {
      return(list(adj_r_squared = NA, f_p_value = NA, is_significant = FALSE, aic = NA))
    }
    sum_model <- summary(model)
    f_p_value <- NA
    if (!is.null(sum_model$fstatistic)) {
      f_stat <- sum_model$fstatistic
      f_p_value <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
    }
    list(
      adj_r_squared = sum_model$adj.r.squared,
      f_p_value = f_p_value,
      is_significant = !is.na(f_p_value) && f_p_value < 0.05,
      aic = stats::AIC(model)
    )
  }
  
  get_anova_pvalue <- function(model_simple, model_complex) {
    if (is.null(model_simple) || is.null(model_complex)) {
      return(NA)
    }
    tryCatch({
      anova_test <- anova(model_simple, model_complex)
      return(anova_test$"Pr(>F)"[2])
    }, error = function(e) {
      return(NA) 
    })
  }
  
  # --- 2. Inicialización y Creación del "Banco de Términos" ---
  
  predictors <- setdiff(names(dataset), y)
  numeric_predictors <- names(dataset)[sapply(dataset, is.numeric)]
  numeric_predictors <- setdiff(numeric_predictors, y)
  
  all_terms <- predictors
  
  if (cuadratic) {
    all_terms <- c(all_terms, paste0("I(", numeric_predictors, "^2)"))
  }
  
  if (log) {
    for (var in numeric_predictors) {
      if (any(dataset[[var]] <= 0, na.rm = TRUE)) {
        warning(paste("Variable '", var, "' tiene valores <= 0. Se omite log(", var, ")."), call. = FALSE)
      } else {
        all_terms <- c(all_terms, paste0("log(", var, ")"))
      }
    }
  }
  
  all_terms <- unique(all_terms)
  n_terms <- length(all_terms)
  
  # --- 2.5. Configuración del Límite de Modelos y Advertencia ---
  
  max_i <- n_terms
  if (!is.null(max_predictors_in_model)) {
    if(max_predictors_in_model < 1) {
      stop("max_predictors_in_model debe ser al menos 1.")
    }
    max_i <- min(n_terms, max_predictors_in_model)
  }
  
  n_combinations <- sum(sapply(1:max_i, function(k) choose(n_terms, k)))
  
  # Aprox. N° de modelos aditivos + N° de modelos con interacción
  max_models_approx <- n_combinations * (1 + (interactions > 0)) # <-- CAMBIO 2
  
  if (max_models_approx > 1000) {
    warning(paste("Alerta: El 'banco de términos' contiene", n_terms, "elementos (base + transformaciones).",
                  "\nSe probarán combinaciones de hasta", max_i, "términos.",
                  "\nTotal estimado:", format(max_models_approx, big.mark=","), "modelos. Esto puede tardar mucho."), call. = FALSE)
  }
  
  all_models_results <- list()
  
  # --- 3. Bucle Principal ---
  
  for (i in 1:max_i) {
    
    combinations <- utils::combn(all_terms, i, simplify = FALSE)
    
    for (combo in combinations) {
      
      # --- Modelo 1: Aditivo ---
      formula_base_str <- paste(y, "~", paste(combo, collapse = " + "))
      model_base <- safe_lm(formula_base_str, dataset)
      stats_base <- get_model_stats(model_base)
      
      all_models_results[[formula_base_str]] <- list(
        formula = formula_base_str,
        model_object = model_base,
        adj_r_squared = stats_base$adj_r_squared,
        aic = stats_base$aic,
        f_p_value = stats_base$f_p_value,
        is_significant = stats_base$is_significant,
        parent_formula = NA,
        anova_p_value = NA,
        passes_anova = NA
      )
      
      # --- Modelo 2: Con Interacción ---
      if (interactions && length(combo) > 1) { # <-- CAMBIO 3
        formula_interact_str <- paste(y, "~", paste(combo, collapse = " * "))
        
        if (formula_interact_str %in% names(all_models_results)) {
          next
        }
        
        model_interact <- safe_lm(formula_interact_str, dataset)
        stats_interact <- get_model_stats(model_interact)
        anova_p <- get_anova_pvalue(model_base, model_interact)
        
        all_models_results[[formula_interact_str]] <- list(
          formula = formula_interact_str,
          model_object = model_interact,
          adj_r_squared = stats_interact$adj_r_squared,
          aic = stats_interact$aic,
          f_p_value = stats_interact$f_p_value,
          is_significant = stats_interact$is_significant,
          parent_formula = formula_base_str,
          anova_p_value = anova_p,
          passes_anova = !is.na(anova_p) && anova_p < 0.05
        )
      }
    }
  }
  
  # --- 4. Procesar y Filtrar Resultados (Sin cambios) ---
  
  results_df <- do.call(rbind, lapply(all_models_results, function(x) {
    data.frame(
      formula = x$formula,
      adj_r_squared = x$adj_r_squared,
      aic = x$aic,
      f_p_value = x$f_p_value,
      is_significant = x$is_significant,
      parent_formula = x$parent_formula,
      anova_p_value = x$anova_p_value,
      passes_anova = x$passes_anova,
      stringsAsFactors = FALSE
    )
  }))
  
  valid_models_mask <- !sapply(all_models_results, function(x) is.null(x$model_object))
  results_df <- results_df[valid_models_mask, ]
  
  if (nrow(results_df) == 0) {
    warning("No se pudo ajustar ningún modelo con éxito.")
    return(list(best_models = list(), all_results = NULL))
  }
  
  candidates_df <- subset(results_df, is_significant == TRUE)
  
  if (nrow(candidates_df) == 0) {
    warning("No se encontraron modelos estadísticamente significativos (p-valor F < 0.05).")
    return(list(best_models = list(), all_results = if(results) results_df else NULL))
  }
  
  final_candidates <- subset(candidates_df, is.na(parent_formula) | passes_anova == TRUE)
  
  if (nrow(final_candidates) == 0) {
    warning("Ningún modelo significativo pasó el filtro de complejidad ANOVA.")
    return(list(best_models = list(), all_results = if(results) results_df else NULL))
  }
  
  # --- 5. Ranking y Selección Final ---
  
  ranked_df <- final_candidates[order(final_candidates$adj_r_squared, decreasing = TRUE), ]
  top_k_df <- head(ranked_df, k_best)
  
  # --- 6. Formatear Salida ---
  
  best_models_list <- lapply(top_k_df$formula, function(f) {
    all_models_results[[f]]$model_object
  })
  names(best_models_list) <- top_k_df$formula
  
  all_results_output <- NULL
  if (results) {
    rownames(results_df) <- NULL
    all_results_output <- results_df
  }
  
  return(list(best_models = best_models_list, all_results = all_results_output))
}