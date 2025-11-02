# Buscador de Modelos Lineales en R (R-ModelSearch)

Este es un módulo de R que automatiza la búsqueda de los mejores modelos lineales (lm) para una variable objetivo, probando múltiples transformaciones y complejidades.

## Características

* Prueba combinaciones de predictores.
* Incluye opcionalmente términos **cuadráticos** (`I(var^2)`).
* Incluye opcionalmente términos **logarítmicos** (`log(var)`).
* Incluye opcionalmente **interacciones** (`var1 * var2`).
* Filtra modelos por significancia estadística (Test F).
* Compara modelos complejos vs. simples usando **ANOVA**.
* Devuelve los `k_best` modelos ordenados por R² Ajustado.

## ¿Cómo usarlo?

1.  **Carga la función** en tu script de R usando `source()`:
    ```R
    source("model_search.R")
    ```

2.  **Prepara tus datos** (ejemplo con `mtcars`):
    ```R
    data(mtcars)
    mi_data <- mtcars[, c("mpg", "wt", "hp")]
    ```

3.  **Ejecuta la búsqueda:**
    ```R
    # (¡Ajusta 'max_predictors_in_model' para controlar el tiempo de ejecución!)
    resultados <- model_search(
      dataset = mi_data,
      y = "mpg",
      k_best = 3,
      cuadratic = TRUE,
      dependency = TRUE,
      log = TRUE,
      results = TRUE,
      max_predictors_in_model = 2 
    )

    # Ver los 3 mejores modelos
    print(resultados$best_models)

    # Ver la tabla completa de todos los modelos probados
    # print(resultados$all_results)
    ```

## Parámetros de la Función `model_search`

* `dataset`: El data.frame.
* `y`: String del nombre de la variable a predecir.
* `k_best`: Cuántos modelos top devolver.
* `cuadratic`: `TRUE` para probar términos `I(x^2)`.
* `dependency`: `TRUE` para probar interacciones `x1 * x2`.
* `log`: `TRUE` para probar términos `log(x)`.
* `results`: `TRUE` para devolver el data.frame con todos los resultados.
* `max_predictors_in_model`: Límite de cuántos términos combinar (ej. 2 = `y ~ A + B` o `y ~ A * B`).
