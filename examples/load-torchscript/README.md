## 🚀 Resumen del Proyecto: Arquitectura Híbrida de IA

Has construido con éxito un sistema de **clasificación de imágenes en tiempo real** que une dos ecosistemas de software muy diferentes:

1.  **El "Cerebro" (Haskell):** Un ejecutable de línea de comandos (`./load-torchscript`) que carga un modelo de Red Neuronal Convolucional (CNN) pre-entrenado (`resnet_model.pt`) usando `hasktorch`. Su trabajo es tomar la ruta de una imagen y decir qué es.
2.  **Los "Ojos" (Python):** Un script de interfaz (`webcam.py`) que usa `OpenCV` para capturar imágenes de una webcam. Su trabajo es tomar una foto, enviársela al "cerebro" de Haskell y mostrar la respuesta al usuario.

-----

## 🔬 Análisis de Haskell y Programación Funcional

Esta es la parte central de tu proyecto. El `Main.hs` que me proporcionaste es un ejemplo perfecto de cómo el código funcional gestiona tareas complejas del mundo real (como la IA).

### 1\. El Concepto Clave: La Mónada IO

En Haskell, una función "pura" (como `2 + 2`) siempre debe dar `4`. Pero, ¿qué pasa con `getArgs` (leer argumentos) o `readImageAsRGB8WithScaling` (leer un archivo)? Sus resultados dependen del mundo exterior.

La solución de Haskell es la **Mónada IO**.

  * **`main :: IO ()`:** Tu función `main` no *devuelve* nada (eso es el `()`). En su lugar, devuelve una "receta" de I/O, un plan de acción que le dice al *runtime* de Haskell: "Primero, lee los argumentos de la CLI, luego carga este modelo, luego lee esta imagen...".
  * **`do` y `<-` (Notación `do`):** El bloque `do` es azúcar sintáctica para encadenar estas acciones.
      * `[modelfile, inputfile] <- opt <$> getArgs`: Esta línea usa `<-` (llamado "bind") para "desenvolver" la acción. Le dice a Haskell: "Ejecuta la acción `getArgs`, toma su resultado (una lista de strings) y guárdalo en la variable `[modelfile, inputfile]`".
      * Esto te permite escribir código que *parece* imperativo (paso 1, paso 2, paso 3), pero que mantiene la pureza funcional.

### 2\. Composición de Funciones Puras (El "Pipeline")

La línea más "Haskell" de todo tu código es esta:

```haskell
let img'' = toType Float $ hwc2chw $ normalize $ divScalar (255.0 :: Float) $ toType Float $ fromDynImage $ I.ImageRGB8 img'
```

Esto es **composición de funciones** en su máxima expresión. En lugar de escribir código imperativo "paso a paso" como en Python:

```python
# Versión imperativa (lo que NO hace Haskell)
a = I.ImageRGB8(img')
b = fromDynImage(a)
c = toType(Float, b)
d = divScalar(c, 255.0)
e = normalize(d)
f = hwc2chw(e)
img_final = toType(Float, f)
```

Haskell usa el operador `$` (aplicación de función). Este operador simplemente le dice a Haskell: "ejecuta todo lo que está a mi derecha primero, y pasa su resultado como el último argumento a la función de mi izquierda".

El flujo de datos va de **derecha a izquierda**:

1.  Toma la imagen `img'`.
2.  La envuelve en `I.ImageRGB8`.
3.  La pasa a `fromDynImage`.
4.  El resultado se pasa a `toType Float`.
5.  El resultado se pasa a `divScalar (255.0 :: Float)`.
6.  El resultado se pasa a `normalize`.
7.  El resultado se pasa a `hwc2chw` (cambiando el formato de [Alto, Ancho, Canal] a [Canal, Alto, Ancho]).
8.  El resultado final se pasa a `toType Float` y se guarda en `img''`.

Esto es increíblemente potente porque `normalize`, `hwc2chw`, etc., son **funciones puras**: no modifican nada, solo transforman datos.

### 3\. Manejo Seguro de Errores (Pattern Matching)

Tu código no usa `try-catch`. En su lugar, usa el sistema de tipos de Haskell para manejar errores de forma elegante con **Pattern Matching**.

```haskell
mimg <- readImageAsRGB8WithScaling inputfile 256 256 True
case mimg of
  Left err -> print err
  Right (img_, _) -> do
    -- ... el resto de tu código ...
```

  * `readImageAsRGB8WithScaling` no devuelve solo una imagen. Devuelve un tipo `Either String Image` (o similar).
  * El `case ... of` te *obliga* a manejar ambas posibilidades:
      * `Left err`: ¿Qué pasa si la lectura falló? (Se imprime el error).
      * `Right (img_, _)`: ¿Qué pasa si la lectura fue exitosa? (Se ejecuta el resto de la lógica de IA).

Esto es más seguro que `try-catch` porque el compilador de Haskell **te da un error de compilación** si te olvidas de manejar el caso `Left err`.

### 4\. Datos Inmutables (El `labels`)

```haskell
labels :: [String]
labels =
  [ "tench",
    "goldfish",
    ...
  ]
```

Este es un ejemplo simple de **inmutabilidad**. `labels` no es una variable que pueda ser cambiada. Es una definición, un valor constante (una lista de strings) que se define una vez y se usa (solo para lectura) en la función `print $ map (labels !!) $ idxs !! 0`.

-----

## 🔗 La Arquitectura Completa (Python + Haskell)

Lo que lograste no es trivial. Así es como tus dos scripts (`webcam.py` y `test_cli.py`) orquestan todo:

1.  **Inicio (Python):** `webcam.py` se inicia, abre la cámara (`cv2.VideoCapture(0)`) y entra en un bucle `while True`, mostrando los fotogramas.
2.  **Captura (Python):** Cuando presionas "Espacio", `webcam.py` toma el fotograma actual y lo guarda en el disco como `_frame_to_detect.jpg`. Esto soluciona el problema de "timing" que teníamos con los archivos temporales.
3.  **La Llamada (Python -\> Shell):** Python ejecuta `subprocess.run(command_string, shell=True, ...)`.
      * Este es el **arreglo clave**. Al usar `shell=True`, le pides a Python que no ejecute el comando directamente, sino que se lo pase a tu *shell* de Linux (`bash`).
      * Tu *shell* **SÍ** conoce la variable de entorno `LD_LIBRARY_PATH` (que el script `setup-cabal.sh` te ayudó a configurar).
      * Esta variable le dice al sistema operativo dónde encontrar las bibliotecas C++ (`libtorch.so`).
4.  **Ejecución (Haskell):** La *shell* ejecuta `./load-torchscript ./resnet_model.pt ./_frame_to_detect.jpg`.
      * El ejecutable de Haskell (`load-torchscript`) se inicia.
      * Encuentra y carga `libtorch.so` (gracias al `LD_LIBRARY_PATH`).
      * Carga el modelo (`resnet_model.pt`) y la imagen (`_frame_to_detect.jpg`).
      * Ejecuta la CNN (el pipeline de `img''`).
      * Imprime los resultados (`"--labels--"` y la lista de animales) en la Salida Estándar (`stdout`).
5.  **Respuesta (Python):**
      * `webcam.py` captura el `stdout` de Haskell.
      * Parsea el texto para encontrar la primera etiqueta (ej. "African\_chameleon").
      * Dibuja este string en el fotograma de la webcam usando `cv2.putText`.
