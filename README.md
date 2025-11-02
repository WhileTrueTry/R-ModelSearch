# Buscador de Modelos Lineales en R

Este es un módulo de R que automatiza la búsqueda de los mejores modelos lineales (lm) para una variable objetivo, probando múltiples transformaciones y complejidades.

Su objetivo es facilitar un análisis exploratorio, y es importante recalcar que a menudo un análisis detallado puede encontrar que existen modelos más sencillos, con valores de R² muy cercanos, y que al ser menos complejos pueden ofrecer mayor interpretabilidad. Además, la idea es probar combinaciones sencillas y anidadas, pero otro tipo de combinaciones más complejas pueden ser más útiles.

## Importante
Se espera que los nombres de las columnas no contengan espacios ni caracteres especiales. De incluirlos, esas columnas no serán consideradas en los modelos.
Ten en cuenta que el número combinatorio de opciones consideradas crece con la cantidad de columnas incluidas en la operación. Intenta reducir al máximo el número de variables independientes de tu dataset. Dada una variable dependiente, todas las otras variables del dataset se considerarán independientes potencialmente explicativas. Asegurate de construir un subset a partir de tu dataset que incluya únicamente las variables que quieres estudiar como potencialmente independientes.

## Características

* **Procesamiento Altamente Paralelizado:** Utiliza los paquetes `foreach` y `doParallel` para dividir la carga de trabajo entre múltiples núcleos de CPU, acelerando drásticamente las búsquedas masivas.
* **Bajo Consumo de Memoria:** Diseñado para evitar colapsos de RAM. Los núcleos paralelos prueban miles de modelos pero solo devuelven sus **estadísticas** (R², AIC, p-values, etc.), no los objetos `lm` (que son pesados). Los objetos `lm` completos se reconstruyen **solo** para los `k_best` modelos ganadores al final del proceso.
* **Banco de Términos:** Prueba combinaciones de predictores, incluyendo opcionalmente términos **cuadráticos** (`I(var^2)`), **logarítmicos** (`log(var)`), e **interacciones** (`var1 * var2`).
* **Filtros de Calidad Robustos:**
    * Filtra modelos por significancia estadística global (Test F).
    * Compara modelos complejos vs. simples usando **ANOVA** para justificar la complejidad.
    * Descarta automáticamente modelos "rotos" (`allow_singular`) con multicolinealidad perfecta (coeficientes `NA`).
    * Filtra modelos sobreajustados (`min_sig_ratio`) exigiendo un ratio mínimo de parámetros estadísticamente significativos.
* **Resultados Claros:** Devuelve los `k_best` modelos (con fórmulas legibles) y un data.frame de `all_results` con la columna `is_top_k` para identificar fácilmente a los ganadores.

## Dependencias

Este módulo requiere los siguientes paquetes para la paralelización. Asegúrate de instalarlos:

```R
install.packages("foreach")
install.packages("doParallel")
```


## ¿Cómo usarlo?

```R
# --- 1. CONFIGURAR EL CLÚSTER ---
library(foreach)
library(doParallel)

# Detectar cuántos núcleos tienes
total_cores <- detectCores()
print(paste("Tu PC tiene", total_cores, "núcleos."))

# Crear un clúster (deja 1 núcleo libre para que tu PC no se congele)
cl <- makeCluster(total_cores - 1)
print(paste("Usando", (total_cores - 1), "núcleos para el trabajo."))

# Registrar el clúster para que 'foreach' sepa dónde trabajar
registerDoParallel(cl)


# --- 2. CARGAR Y EJECUTAR ---

# Carga la función (V3.8+ con las funciones auxiliares AFUERA)
source("model_search.R") 

# Prepara tus datos
data(mtcars)
mi_data <- as.data.frame(mtcars[, c("mpg", "wt", "hp")])

# Ejecuta la búsqueda
print("Iniciando búsqueda paralela...")
start_time <- Sys.time()

resultados <- model_search(
  dataset = mi_data,
  y = "mpg",
  k_best = 3,
  cuadratic = TRUE,
  interactions = TRUE,
  log = TRUE,
  results = TRUE,
  max_predictors_in_model = 2,
  min_sig_ratio = 0.75
)

end_time <- Sys.time()
print(paste("Búsqueda completada en:", round(end_time - start_time, 2), "segundos/minutos"))


# --- 3. ¡IMPORTANTE! DETENER EL CLÚSTER ---
# Si omites esto, los procesos de R se quedarán "zombis" en tu memoria.
stopCluster(cl)
print("Clúster detenido. Recursos liberados.")


# --- 4. REVISAR RESULTADOS ---

# Ver los 3 mejores objetos 'lm' (reconstruidos)
print(resultados$best_models)

# Ver la tabla completa de *estadísticas*
# (¡Revisa la nueva columna 'is_top_k' al principio!)
print(resultados$all_results)

# o directamente
print( search_results$all_results[search_results$all_results$is_top_k == TRUE, ] )

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
* `min_sig_ratio`: Un decimal (0-1). El ratio mínimo de predictores (p < 0.05) que deben ser significativos. Útil para filtrar modelos sobreajustados. (Por defecto: `0.75`).
* `allow_singular`: Booleano. Si es `FALSE` (por defecto), descarta automáticamente modelos con "singularidad" (multicolinealidad perfecta o coeficientes `NA`).