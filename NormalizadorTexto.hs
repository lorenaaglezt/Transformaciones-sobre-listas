-- NORMALIZACIÓN DE TEXTO
module TextNormalizer where

import Data.Char (toLower, toUpper, isAlpha, isSpace)
import Data.List (intercalate)


-- PARTE 1: TRANSFORMACIONES ATÓMICAS


-- Convertir a minúsculas.
-- CONCEPTO: Aplicación parcial
-- La función 'map' está currificada: map :: (a -> b) -> [a] -> [b]
-- Al aplicar solo 'toLower', obtenemos una nueva función: String -> String
-- POINT-FREE: No mencionamos el parámetro String
aMinusculas :: String -> String
aMinusculas = map toLower
{- EXPLICACIÓN:
   map :: (Char -> Char) -> String -> String
   toLower :: Char -> Char
   map toLower :: String -> String (aplicación parcial)
-}

-- | Convertir a mayúsculas
-- CONCEPTO: Aplicación parcial (igual que aMinusculas)
-- Point-free style: definimos la función sin mencionar argumentos
aMayusculas :: String -> String
aMayusculas = map toUpper

-- | Eliminar espacios extra
-- CONCEPTO: Composición de funciones (.)
-- words :: String -> [String] (divide por espacios)
-- unwords :: [String] -> String (une con espacios)
-- Composición: primero words, luego unwords
eliminarEspaciosExtra :: String -> String
eliminarEspaciosExtra = unwords . words
{- EXPLICACIÓN:
   (f . g) x = f (g x)
   (unwords . words) texto = unwords (words texto)
   Point-free: no mencionamos 'texto'
-}

-- | Filtrar solo caracteres alfabéticos y espacios
-- CONCEPTO: Lambda anónima + Aplicación parcial
-- filter :: (a -> Bool) -> [a] -> [a] (currificada)
-- Aplicamos parcialmente filter con una lambda
soloAlfabeticos :: String -> String
soloAlfabeticos = filter (\c -> isAlpha c || isSpace c)
{- EXPLICACIÓN:
   \c -> ... es una lambda anónima (función sin nombre)
   filter toma el predicado y devuelve: String -> String
   Aplicación parcial de filter
-}

-- | Eliminar todas las vocales
-- CONCEPTO: Lambda anónima + Aplicación parcial
-- Similar a soloAlfabeticos pero con lógica diferente
sinVocales :: String -> String
sinVocales = filter (\c -> not (c `elem` "aeiouAEIOU"))
{- EXPLICACIÓN:
   Lambda verifica si el carácter NO está en la lista de vocales
   filter aplica este predicado a cada carácter
-}

-- | Invertir cada palabra individualmente
-- CONCEPTO: Composición triple de funciones
-- Pipeline: words -> map reverse -> unwords
invertirPalabras :: String -> String
invertirPalabras = unwords . map reverse . words
{- EXPLICACIÓN:
   (f . g . h) x = f (g (h x))
   words: divide en palabras
   map reverse: invierte cada palabra
   unwords: une de nuevo
   Composición asociativa: (unwords . (map reverse . words))
-}

-- | Capitalizar primera letra de cada palabra
-- CONCEPTO: Lambda anónima compleja + Composición + Pattern matching
-- Usa case para manejar listas vacías y no vacías
capitalizarPalabras :: String -> String
capitalizarPalabras = unwords . map (\s -> case s of
  [] -> []
  (x:xs) -> toUpper x : map toLower xs) . words
{- EXPLICACIÓN:
   Lambda anónima con pattern matching en case
   Composición: words -> map (lambda) -> unwords
   La lambda transforma cada palabra independientemente
-}

-- ============================================
-- PARTE 2: COMPOSICIONES COMPLEJAS
-- Construcción de pipelines mediante composición
-- ============================================

-- | Normalización básica: minúsculas + sin espacios extra
-- CONCEPTO: Composición binaria de funciones
-- Compone dos transformaciones previamente definidas
normalizacionBasica :: String -> String
normalizacionBasica = eliminarEspaciosExtra . aMinusculas
{- EXPLICACIÓN:
   Primero: aMinusculas convierte a minúsculas
   Segundo: eliminarEspaciosExtra normaliza espacios
   Flujo: texto -> aMinusculas -> eliminarEspaciosExtra -> resultado
-}

-- | Normalización estricta: solo alfabéticos + minúsculas + sin espacios
-- CONCEPTO: Composición ternaria
-- Encadena tres transformaciones en un pipeline
normalizacionEstricta :: String -> String
normalizacionEstricta = eliminarEspaciosExtra . aMinusculas . soloAlfabeticos
{- EXPLICACIÓN:
   Composición asociativa de 3 funciones:
   (f . g . h) = f . (g . h) = (f . g) . h
   Orden de ejecución (derecha a izquierda):
   1. soloAlfabeticos: filtra caracteres
   2. aMinusculas: convierte a minúsculas
   3. eliminarEspaciosExtra: normaliza espacios
-}

-- | Normalización para títulos
-- CONCEPTO: Composición ternaria con orden diferente
-- Demuestra que el orden de composición importa
normalizacionTitulo :: String -> String
normalizacionTitulo = eliminarEspaciosExtra . capitalizarPalabras . soloAlfabeticos
{- EXPLICACIÓN:
   Diferente orden produce diferente resultado
   Pipeline: filtrar -> capitalizar -> limpiar espacios
-}

-- | Codificación divertida: mayúsculas sin vocales de palabras invertidas
-- CONCEPTO: Composición ternaria compleja
-- Demuestra composición de transformaciones no conmutativas
codificacionDivertida :: String -> String
codificacionDivertida = aMayusculas . sinVocales . invertirPalabras
{- EXPLICACIÓN:
   El orden es crucial - diferentes órdenes dan resultados diferentes
   1. invertirPalabras: "Hola" -> "aloH"
   2. sinVocales: "aloH" -> "lH"
   3. aMayusculas: "lH" -> "LH"
-}

-- ============================================
-- PARTE 3: COMBINADORES DE ORDEN SUPERIOR
-- Metaprogramación funcional
-- ============================================

-- | Pipeline: componer dinámicamente una lista de funciones
-- CONCEPTO: Lógica combinatoria + Reducción (foldr)
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- Usa (.) como operador binario y 'id' como caso base
pipeline :: [String -> String] -> String -> String
pipeline = foldr (.) id
{- EXPLICACIÓN PROFUNDA:
   foldr (.) id [f, g, h]
   = f . (g . (h . id))
   = f . g . h
   
   CURRIFICACIÓN:
   foldr :: (a -> b -> b) -> b -> [a] -> b
   foldr (.) :: (String -> String) -> [String -> String] -> (String -> String)
   foldr (.) id :: [String -> String] -> (String -> String)
   
   APLICACIÓN PARCIAL:
   - Aplicamos parcialmente foldr con (.) y id
   - Resultado: función que espera lista de funciones
   - Devuelve la composición de todas ellas
-}

-- | Aplicar transformación N veces
-- CONCEPTO: Recursión + Composición + Lambda
-- Construye f . f . f . ... . f (N veces)
aplicarN :: Int -> (a -> a) -> (a -> a)
aplicarN = \n -> \f -> if n <= 0 
  then id 
  else f . aplicarN (n-1) f
{- EXPLICACIÓN:
   CURRIFICACIÓN EXPLÍCITA con lambdas anidadas:
   \n -> \f -> ... muestra la currificación explícitamente
   
   RECURSIÓN:
   aplicarN 0 f = id (caso base)
   aplicarN n f = f . (aplicarN (n-1) f) (caso recursivo)
   
   COMPOSICIÓN RECURSIVA:
   aplicarN 3 f = f . (f . (f . id))
   
   EJEMPLO:
   aplicarN 2 invertirPalabras "Hola"
   = (invertirPalabras . invertirPalabras) "Hola"
   = invertirPalabras (invertirPalabras "Hola")
   = invertirPalabras "aloH"
   = "Hola" (doble inversión = identidad)
-}

-- | Aplicación condicional
-- CONCEPTO: Lambda de orden superior + Aplicación condicional
-- Combina predicado, transformación y lógica condicional
aplicarSi :: (a -> Bool) -> (a -> a) -> (a -> a)
aplicarSi = \pred -> \trans -> \x -> if pred x then trans x else x
{- EXPLICACIÓN:
   CURRIFICACIÓN EXPLÍCITA con tres lambdas:
   \pred -> (\trans -> (\x -> ...))
   
   Cada lambda devuelve otra función:
   aplicarSi :: (a -> Bool) -> ((a -> a) -> (a -> a))
   aplicarSi pred :: (a -> a) -> (a -> a)
   aplicarSi pred trans :: a -> a
   
   LÓGICA COMBINATORIA:
   Combina un predicado y una transformación
   Si el predicado es verdadero, aplica transformación
   Si no, devuelve el valor sin cambios (id)
-}

-- | Bifurcación: aplicar dos funciones y combinar resultados
-- CONCEPTO: Lambda compleja + Aplicación paralela
-- Patrón común en programación funcional (fanout)
bifurcar :: (b -> c -> d) -> (a -> b) -> (a -> c) -> (a -> d)
bifurcar = \combinar -> \f -> \g -> \x -> combinar (f x) (g x)
{- EXPLICACIÓN:
   CURRIFICACIÓN de 4 argumentos:
   bifurcar combinar f g x = combinar (f x) (g x)
   
   PATRÓN FANOUT:
        x
       / \
      f   g
       \ /
     combinar
   
   EJEMPLO:
   bifurcar (++) (map toUpper) (map toLower) "Hola"
   = (++) (map toUpper "Hola") (map toLower "Hola")
   = (++) "HOLA" "hola"
   = "HOLAhola"
   
   Aplica dos transformaciones al mismo input
   Luego combina los dos resultados
-}

-- ============================================
-- PARTE 4: TRANSFORMACIONES SOBRE LISTAS
-- Aplicación de transformaciones a colecciones
-- ============================================

-- | Normalizar un corpus completo
-- CONCEPTO: Aplicación parcial de map
-- map está currificada, aplicamos parcialmente la transformación
normalizarCorpus :: [String] -> [String]
normalizarCorpus = map normalizacionEstricta
{- EXPLICACIÓN:
   map :: (a -> b) -> [a] -> [b]
   map normalizacionEstricta :: [String] -> [String]
   
   APLICACIÓN PARCIAL:
   normalizarCorpus es simplemente map con el primer argumento fijado
   Point-free: no mencionamos la lista de strings
-}

-- | Procesar con múltiples variantes
-- CONCEPTO: Lambda que construye lista + map
-- Aplica múltiples transformaciones a cada elemento
procesarConVariantes :: [String] -> [[String]]
procesarConVariantes = map (\texto -> [
    normalizacionBasica texto,
    normalizacionEstricta texto,
    normalizacionTitulo texto,
    codificacionDivertida texto
  ])
{- EXPLICACIÓN:
   Lambda genera una lista de resultados para cada texto
   map aplica esta lambda a cada elemento del corpus
   
   RESULTADO: Lista de listas (matriz)
   Cada fila contiene 4 variantes del texto original
-}

-- | Filtrar por longitud mínima con normalización
-- CONCEPTO: Composición de filter y map + Composición de predicado
-- Pipeline: normalizar -> filtrar por longitud
filtrarPorLongitud :: Int -> [String] -> [String]
filtrarPorLongitud = \minLen -> filter ((>= minLen) . length) . map normalizacionBasica
{- EXPLICACIÓN DETALLADA:
   COMPOSICIÓN DEL PREDICADO:
   (>= minLen) :: Int -> Bool (aplicación parcial de >=)
   length :: String -> Int
   (>= minLen) . length :: String -> Bool (composición)
   
   APLICACIÓN PARCIAL:
   filter :: (a -> Bool) -> [a] -> [a]
   filter predicado :: [a] -> [a]
   
   COMPOSICIÓN PRINCIPAL:
   filter predicado . map normalizacionBasica
   Primero: map normaliza cada string
   Segundo: filter elimina strings cortos
   
   CURRIFICACIÓN EXPLÍCITA:
   \minLen -> ... muestra que tomamos el mínimo primero
   Luego devolvemos una función que procesa listas
-}

-- ============================================
-- PARTE 5: EJEMPLOS DE COMPOSICIÓN AVANZADA
-- ============================================

-- | Ejemplo 1: Composición ternaria simple
-- CONCEPTO: Composición point-free de tres funciones
ejemplo1 :: String -> String
ejemplo1 = eliminarEspaciosExtra . aMinusculas . soloAlfabeticos
{- Pipeline claro y legible:
   filtrar -> minúsculas -> limpiar espacios
-}

-- | Ejemplo 2: Uso de pipeline dinámico
-- CONCEPTO: Metaprogramación - composición de lista de funciones
ejemplo2 :: String -> String
ejemplo2 = pipeline [eliminarEspaciosExtra, capitalizarPalabras, soloAlfabeticos]
{- EXPLICACIÓN:
   pipeline usa foldr (.) id para componer la lista
   Equivalente a: eliminarEspaciosExtra . capitalizarPalabras . soloAlfabeticos
   Ventaja: la lista puede construirse dinámicamente en runtime
-}

-- | Ejemplo 3: Aplicación condicional
-- CONCEPTO: Combinador aplicarSi con lambda de predicado
ejemplo3 :: String -> String
ejemplo3 = aplicarSi (\s -> length s > 10) aMayusculas
{- EXPLICACIÓN:
   Solo convierte a mayúsculas si el string tiene más de 10 caracteres
   Si no, devuelve el string sin modificar
   Demuestra composición de lógica condicional
-}

-- | Ejemplo 4: Aplicación repetida
-- CONCEPTO: Recursión controlada mediante aplicarN
ejemplo4 :: String -> String
ejemplo4 = aplicarN 2 invertirPalabras
{- EXPLICACIÓN:
   Invierte palabras dos veces = identidad
   aplicarN 2 f = f . f
   Demuestra propiedades algebraicas de funciones
-}

-- | Ejemplo 5: Bifurcación con combinación
-- CONCEPTO: Procesamiento paralelo con bifurcar
ejemplo5 :: String -> String
ejemplo5 = bifurcar 
  (\normalizado -> \longitud -> normalizado ++ " [" ++ show longitud ++ " chars]")
  normalizacionBasica 
  length
{- EXPLICACIÓN:
   Aplica dos funciones al mismo input:
   - normalizacionBasica: transforma el texto
   - length: calcula la longitud original
   Luego combina ambos resultados en un string anotado
   
   PATRÓN: Enriquecer resultados con metadatos
-}

-- ============================================
-- PARTE 6: COMPOSICIONES EXTREMAS
-- Demostraciones de composición compleja
-- ============================================

-- | Composición de 5 funciones
-- CONCEPTO: Composición n-aria
-- Demuestra que la composición escala a cualquier número de funciones
transformacionCompleja1 :: String -> String
transformacionCompleja1 = 
  eliminarEspaciosExtra . 
  aMayusculas . 
  sinVocales . 
  soloAlfabeticos . 
  invertirPalabras
{- EXPLICACIÓN:
   Pipeline de 5 etapas:
   1. invertirPalabras: "Hola Mundo" -> "aloH odnuM"
   2. soloAlfabeticos: "aloH odnuM" -> "aloH odnuM"
   3. sinVocales: "aloH odnuM" -> "lH dnM"
   4. aMayusculas: "lH dnM" -> "LH DNM"
   5. eliminarEspaciosExtra: "LH DNM" -> "LH DNM"
   
   Demuestra composición asociativa:
   ((((f . g) . h) . i) . j) = f . g . h . i . j
-}

-- | Pipeline dinámico equivalente
-- CONCEPTO: Construcción dinámica vs estática
-- Mismo resultado, diferente construcción
transformacionCompleja2 :: String -> String
transformacionCompleja2 = pipeline [
  eliminarEspaciosExtra,
  aMayusculas,
  sinVocales,
  soloAlfabeticos,
  invertirPalabras
  ]
{- EXPLICACIÓN:
   Funcionalmente idéntico a transformacionCompleja1
   Diferencia: la lista puede construirse en runtime
   Ventaja: más flexible para configuración dinámica
-}

-- | Composición con aplicación condicional anidada
-- CONCEPTO: Composición de combinadores
transformacionCondicionalCompuesta :: String -> String
transformacionCondicionalCompuesta = 
  aplicarSi (\s -> length s > 5) (aMayusculas . eliminarEspaciosExtra)
{- EXPLICACIÓN:
   Condicional con transformación compuesta:
   Si length > 5: aplica (aMayusculas . eliminarEspaciosExtra)
   Si no: devuelve string original
   
   Demuestra composición dentro de combinadores
-}

-- | Bifurcación con composiciones en cada rama
-- CONCEPTO: Composición dentro de bifurcación
transformacionBifurcada :: String -> String
transformacionBifurcada = bifurcar
  (\parte1 -> \parte2 -> parte1 ++ " | " ++ parte2)
  (aMayusculas . soloAlfabeticos)
  (sinVocales . aMinusculas)
{- EXPLICACIÓN:
   Rama 1: soloAlfabeticos . aMayusculas
   Rama 2: aMinusculas . sinVocales
   Combina: concatena con " | " como separador
   
   EJEMPLO:
   "Hola123 Mundo"
   Rama 1: "HOLA MUNDO"
   Rama 2: "hl mnd"
   Resultado: "HOLA MUNDO | hl mnd"
   
   Demuestra composición multinivel
-}

-- ============================================
-- FUNCIÓN PRINCIPAL - DEMOSTRACIÓN
-- ============================================

main :: IO ()
main = do
  putStrLn "=========================================="
  putStrLn "NORMALIZADOR DE TEXTO"
  putStrLn "Composición (.) y Lambdas Anónimas"
  putStrLn "=========================================="
  putStrLn ""
  
  let texto = "  ¡Hola!!!  Mundo123  con    ESPACIOS   extra  "
  
  putStrLn $ "Texto original: \"" ++ texto ++ "\""
  putStrLn ""
  
  putStrLn "--- TRANSFORMACIONES ATÓMICAS ---"
  putStrLn "(Aplicación parcial de map/filter)"
  putStrLn $ "aMinusculas: " ++ aMinusculas texto
  putStrLn $ "aMayusculas: " ++ aMayusculas texto
  putStrLn $ "soloAlfabeticos: " ++ soloAlfabeticos texto
  putStrLn $ "sinVocales: " ++ sinVocales texto
  putStrLn $ "invertirPalabras: " ++ invertirPalabras texto
  putStrLn ""
  
  putStrLn "--- COMPOSICIONES SIMPLES ---"
  putStrLn "(Composición binaria y ternaria)"
  putStrLn $ "normalizacionBasica: " ++ normalizacionBasica texto
  putStrLn $ "normalizacionEstricta: " ++ normalizacionEstricta texto
  putStrLn $ "normalizacionTitulo: " ++ normalizacionTitulo texto
  putStrLn $ "codificacionDivertida: " ++ codificacionDivertida texto
  putStrLn ""
  
  putStrLn "--- COMBINADORES AVANZADOS ---"
  putStrLn "(Lógica combinatoria)"
  putStrLn $ "ejemplo3 (condicional): " ++ ejemplo3 texto
  putStrLn $ "ejemplo4 (aplicar 2 veces): " ++ ejemplo4 "Hola Mundo"
  putStrLn $ "ejemplo5 (bifurcación): " ++ ejemplo5 texto
  putStrLn ""
  
  putStrLn "--- PIPELINE DINÁMICO ---"
  putStrLn "(Metaprogramación con foldr)"
  let miPipeline = pipeline [aMayusculas, sinVocales, eliminarEspaciosExtra]
  putStrLn $ "Pipeline custom: " ++ miPipeline texto
  putStrLn ""
  
  putStrLn "--- COMPOSICIONES COMPLEJAS ---"
  putStrLn "(Composición n-aria)"
  putStrLn $ "transformacionCompleja1 (5 funcs): " ++ transformacionCompleja1 texto
  putStrLn $ "transformacionBifurcada: " ++ transformacionBifurcada texto
  putStrLn ""
  
  putStrLn "--- PROCESAMIENTO DE LISTAS ---"
  putStrLn "(map con aplicación parcial)"
  let corpus = ["HOLA mundo", "  texto  ", "Ejemplo123!!!"]
  putStrLn "Corpus original:"
  mapM_ (putStrLn . ("  - " ++)) corpus
  putStrLn "Corpus normalizado:"
  mapM_ (putStrLn . ("  - " ++)) (normalizarCorpus corpus)
  putStrLn ""
  
  putStrLn "=========================================="
  putStrLn "Todos los conceptos demostrados:"
  putStrLn "  ✓ Currificación (todas las funciones)"
  putStrLn "  ✓ Aplicación parcial (map, filter, etc.)"
  putStrLn "  ✓ Composición (.) (pipelines)"
  putStrLn "  ✓ Lambdas anónimas (predicados, lógica)"
  putStrLn "  ✓ Lógica combinatoria (foldr, pipeline)"
  putStrLn "  ✓ Point-free style (sin parámetros)"
  putStrLn "=========================================="