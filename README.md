# Buscador de Modelos Lineales en R (R-ModelSearch)

Este es un módulo de R que automatiza la búsqueda de los mejores modelos lineales (lm) para una variable objetivo, probando múltiples transformaciones y complejidades.

Su objetivo es facilitar un análisis exploratorio, y es importante recalcar que a menudo un análisis detallado puede encontrar que existen modelos más sencillos, con valores de R² muy cercanos, y que al ser menos complejos pueden ofrecer mayor interpretabilidad. Además, la idea es probar combinaciones sencillas y anidadas, pero otro tipo de combinaciones más complejas pueden ser más útiles

## Características

* Prueba combinaciones de predictores desde un "banco de términos".
* Incluye opcionalmente términos **cuadráticos** (`I(var^2)`).
* Incluye opcionalmente términos **logarítmicos** (`log(var)`).
* Incluye opcionalmente **interacciones** (`var1 * var2`).
* Filtra modelos por significancia estadística global (Test F).
* Compara modelos complejos vs. simples usando **ANOVA** para justificar la complejidad.
* **Filtra modelos "rotos"** (singularidad/multicolinealidad perfecta) que producen coeficientes `NA`.
* **Filtra modelos sobreajustados (overfitting)** exigiendo un ratio mínimo de parámetros estadísticamente significativos.
* Devuelve los `k_best` modelos ordenados por R² Ajustado.
* El data.frame de resultados incluye una columna `is_top_k` para identificar fácilmente a los ganadores.

## ¿Cómo usarlo?

1.  **Carga la función** en tu script de R usando `source()`:
    ```R
    source("model_search.R")
    ```

2.  **Prepara tus datos** (ejemplo con `mtcars`):
    ```R
    data(mtcars)
    mi_data <- mtcars[, c("mpg", "wt", "hp")]
    
    # ¡Importante! Si tus datos son un 'tibble', conviértelos
    # mi_data <- as.data.frame(mi_data)
    ```

3.  **Ejecuta la búsqueda:**
    ```R
    # (Ajusta 'max_predictors_in_model' para controlar el tiempo de ejecución)
    resultados <- model_search(
      dataset = mi_data,
      y = "mpg",
      k_best = 3,
      cuadratic = TRUE,
      interactions = TRUE,  # Corregido de 'dependency'
      log = TRUE,
      results = TRUE,
      max_predictors_in_model = 2,
      min_sig_ratio = 0.75       # (Opcional, 75% por defecto)
    )

    # Ver los 3 mejores modelos
    print(resultados$best_models)

    # Ver la tabla completa de todos los modelos probados
    # La columna 'is_top_k' al principio permite identificar los modelos k_best entre el total
    print(resultados$all_results)
    ```

## Parámetros de la Función `model_search`

* `dataset`: El data.frame. (Nota: Se recomienda un `data.frame` base, no un `tibble`).
* `y`: String del nombre de la variable a predecir.
* `k_best`: Cuántos modelos top devolver.
* `cuadratic`: `TRUE` para probar términos `I(x^2)`.
* `interactions`: `TRUE` para probar interacciones `x1 * x2`.
* `log`: `TRUE` para probar términos `log(x)`.
* `results`: `TRUE` para devolver el data.frame con todos los resultados. Este data.frame incluirá la columna `is_top_k`.
* `max_predictors_in_model`: Límite de cuántos términos combinar
* `min_sig_ratio`: **(Nuevo)** Un decimal (0-1). El ratio mínimo de predictores (p < 0.05) que deben ser significativos. Útil para filtrar modelos sobreajustados. (Por defecto: `0.75`).
* `allow_singular`: **(Nuevo)** Booleano. Si es `FALSE` (por defecto), descarta automáticamente modelos con "singularidad" (multicolinealidad perfecta o coeficientes `NA`).