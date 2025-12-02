-- NORMALIZACIÓN DE TEXTO
-- 

module TextNormalizer where

import Data.Char (toLower, toUpper, isAlpha, isSpace)
import Data.List (intercalate)


--------------------------------------------------------------------------
-- TRANSFORMACIONES ATÓMICAS
--------------------------------------------------------------------------

-- Convertir a minúsculas.
{-
  map :: (Char -> Char) -> String -> String
  toLower :: Char -> Char
  map toLower :: String -> String (aplicación parcial)
-} 
aMinusculas :: String -> String
aMinusculas = map toLower

-- Convertir a mayúsculas
-- Aplicación parcial (igual que aMinusculas)
aMayusculas :: String -> String
aMayusculas = map toUpper

-- Eliminar espacios extra
{-
  words :: String -> [String] (divide por espacios)
  unwords :: [String] -> String (une con espacios)
  Composición: (unwords . words) texto = unwords (words texto)
-}
eliminarEspaciosExtra :: String -> String
eliminarEspaciosExtra = unwords . words

-- Filtrar solo caracteres alfabéticos y espacios
{-
  filter :: (a -> Bool) -> [a] -> [a] (currificada)
  Aplicamos parcialmente filter con una lambda
-}
soloAlfabeticos :: String -> String
soloAlfabeticos = filter (\c -> isAlpha c || isSpace c)

-- Eliminar todas las vocales
{-
  Similar a soloAlfabeticos pero aquí la lambda verifica si el caracter
  no está en la lista de vocales. Se aplica parcialmente filter.
-}
sinVocales :: String -> String
sinVocales = filter (\c -> not (c `elem` "aeiouAEIOU"))

-- Invertir cada palabra individualmente
{-
  words: divide en palabras
  map reverse: invierte cada palabra
  unwords: une de nuevo
  Composición triple de funciones: (unwords . (map reverse . words))
-}
invertirPalabras :: String -> String
invertirPalabras = unwords . map reverse . words

-- Capitalizar primera letra de cada palabra
{-
  Usa case para manejar listas vacías y no vacías
  Composición: words -> map (lambda) -> unwords
-}
capitalizarPalabras :: String -> String
capitalizarPalabras = unwords . map (\s -> case s of
  [] -> []
  (x:xs) -> toUpper x : map toLower xs) . words




------------------------------------------------------------------------
-- COMPOSICIONES
------------------------------------------------------------------------

-- Normalización básica: minúsculas + sin espacios extra
{-
  Composición de funciones aMinuscula y eliminarEspaciosExtra
  texto -> aMinusculas -> eliminarEspaciosExtra -> resultado
-}
normalizacionBasica :: String -> String
normalizacionBasica = eliminarEspaciosExtra . aMinusculas

-- Normalización estricta: solo alfabéticos + minúsculas + sin espacios
{-
  Composición de tres funciones: primero se aplica soloAlfabeticos, 
  después aMinusculas y por último eliminarEspaciosExtra
-}
normalizacionEstricta :: String -> String
normalizacionEstricta = eliminarEspaciosExtra . aMinusculas . soloAlfabeticos

-- Normalización para títulos
{-
  Composición ternaria
  filtrar -> capitalizar -> limpiar espacios
-}
normalizacionTitulo :: String -> String
normalizacionTitulo = eliminarEspaciosExtra . capitalizarPalabras . soloAlfabeticos

-- Reversa sin Vocales: mayúsculas sin vocales de palabras invertidas
-- Composición de funciones ternaria
reversaSinVocales :: String -> String
reversaSinVocales = aMayusculas . sinVocales . invertirPalabras




----------------------------------------------------------------------------
-- COMBINADORES DE ORDEN SUPERIOR
----------------------------------------------------------------------------

-- Pipeline: componer dinámicamente una lista de funciones
{-
  foldr :: (a -> b -> b) -> b -> [a] -> b
  Usa (.) como operador binario y 'id' como caso base
  Aplicamos parcialmente foldr con (.) y id
  Resultado: función que espera lista de funciones
  Devuelve la composición de todas ellas
-}
pipeline :: [String -> String] -> String -> String
pipeline = foldr (.) id

-- Aplicar transformación N veces
{-
  Construye f . f . f . ... . f (N veces)
  Currificación explícita, \n -> \f -> ...

  Implementa recursión:
  aplicarN 0 f = id (caso base)
  aplicarN n f = f . (aplicarN (n-1) f) (caso recursivo)

  Ejemplo:
  aplicarN 2 invertirPalabras "Hola"
   = (invertirPalabras . invertirPalabras) "Hola"
   = invertirPalabras (invertirPalabras "Hola")
   = invertirPalabras "aloH"
   = "Hola" (doble inversión = identidad)
-}
aplicarN :: Int -> (a -> a) -> (a -> a)
aplicarN = \n -> \f -> if n <= 0 
  then id 
  else f . aplicarN (n-1) f

-- Aplicación condicional
{-
  Currificación explícita con tres lambdas: \pred -> (\trans -> (\x -> ...))
  Combina predicados y transformaciones, si el predicado es verdadero entonces
  aplica la transformación, de otro modo, no realiza cambios

  aplicarSi :: (a -> Bool) -> ((a -> a) -> (a -> a))
  aplicarSi pred :: (a -> a) -> (a -> a)
  aplicarSi pred trans :: a -> a
-}
aplicarSi :: (a -> Bool) -> (a -> a) -> (a -> a)
aplicarSi = \pred -> \trans -> \x -> if pred x then trans x else x

-- Bifurcación: aplicar dos funciones y combinar resultados
{-
  Currificación de 4 argumentos
  Aplica dos funciones al mismo tiempo a un valor y combina los resultados
-}
bifurcar :: (b -> c -> d) -> (a -> b) -> (a -> c) -> (a -> d)
bifurcar = \combinar -> \f -> \g -> \x -> combinar (f x) (g x)




-----------------------------------------------------------------------
-- TRANSFORMACIONES SOBRE LISTAS
-----------------------------------------------------------------------

-- Normalizar estrictamente toda la lista
{-
  Aplicación parcial de map (que está currificada), 
  fijamos únicamente el primer argumento
  map :: (a -> b) -> [a] -> [b]
  map normalizacionEstricta :: [String] -> [String]
  Un corpus es una colección de textos, es decir, una lista de cadenas
-}
normalizarCorpus :: [String] -> [String]
normalizarCorpus = map normalizacionEstricta

-- Procesar con múltiples variantes
{-
  Aplica múltiples transformaciones a cada elemento
  La lambda genera una lista de resultados para cada texto
  map aplica la lambda a cada elemento del corpus
  El resultado es una lista de listas (una matriz) y cada fila contiene 4 variantes
  del texto original
  Las variantes son las que definimos: básica, estricya, de título y reversa sin vocales
-}
procesarConVariantes :: [String] -> [[String]]
procesarConVariantes = map (\texto -> [
    normalizacionBasica texto,
    normalizacionEstricta texto,
    normalizacionTitulo texto,
    reversaSinVocales texto
  ])

-- Filtrar por longitud mínima con normalización
{-
  Recibe la longitud de los strings que se deben eliminar, la lista de cadenas
  y devuelve una lista de cadenas normalizadas y que no contienen cadenas menores
  a la longitud definida.

  Con currificación, tomamos el mínimo primero, \minLen -> ...
  Primero map normalizada cada string y luego filter elimina strings cortos
  Aplicación parcial en filter :: (a -> Bool) -> [a] -> [a] con filter predicado :: [a] -> [a]
-}
-- CONCEPTO: Composición de filter y map + Composición de predicado
-- Pipeline: normalizar -> filtrar por longitud
filtrarPorLongitud :: Int -> [String] -> [String]
filtrarPorLongitud = \minLen -> filter ((>= minLen) . length) . map normalizacionBasica




-------------------------------------------------------------------------------
-- EJEMPLOS DE COMPOSICIÓN AVANZADA
--------------------------------------------------------------------------------

-- Ejemplo 1: Composición funcional
{-
  filtrar solo alfabéticos -> minúsculas -> limpiar espacios
-}
ejemplo1 :: String -> String
ejemplo1 = eliminarEspaciosExtra . aMinusculas . soloAlfabeticos

-- Ejemplo 2: Uso de pipeline
{-
  Implementa la composición de lista de funciones
  pipeline usa foldr (.) id para componer la lista
  Equivalente a eliminarEspaciosExtra . capitalizarPalabras . soloAlfabeticos
-}
ejemplo2 :: String -> String
ejemplo2 = pipeline [eliminarEspaciosExtra, capitalizarPalabras, soloAlfabeticos]

-- Ejemplo 3: Aplicación condicional
{-
  Aplicación del combinador aplicarSi con una lambda de predicado
  Solo convierte a mayúsculas si el string tiene más de 10 caracteres
-}
ejemplo3 :: String -> String
ejemplo3 = aplicarSi (\s -> length s > 10) aMayusculas

-- Ejemplo 4: Aplicación repetida
{-
  Invierte palabras dos veces, dejando la misma palabra original (como la identidad)
-}
ejemplo4 :: String -> String
ejemplo4 = aplicarN 2 invertirPalabras

-- Ejemplo 5: Bifurcación con combinación
{-
  Aplica dos funciones a la misma cadena: normalización básica y el cálculo de la longitud original
  Combina los resultados en un string
-}
ejemplo5 :: String -> String
ejemplo5 = bifurcar 
  (\normalizado -> \longitud -> normalizado ++ " [" ++ show longitud ++ " chars]")
  normalizacionBasica 
  length




-------------------------------------------------------------------------------
-- OTRAS COMPOSICIONES
-------------------------------------------------------------------------------

-- Composición de 5 funciones
{-
  La composición escala a cualquier número de funciones
  1. invertirPalabras: "Hola Mundo" -> "aloH odnuM"
  2. soloAlfabeticos: "aloH odnuM" -> "aloH odnuM"
  3. sinVocales: "aloH odnuM" -> "lH dnM"
  4. aMayusculas: "lH dnM" -> "LH DNM"
  5. eliminarEspaciosExtra: "LH DNM" -> "LH DNM"
-}
transformacionCompleja :: String -> String
transformacionCompleja = 
  eliminarEspaciosExtra . 
  aMayusculas . 
  sinVocales . 
  soloAlfabeticos . 
  invertirPalabras

-- Composición con aplicación condicional anidada
{-
  Podemos aplicar la función aplicarSi que toma un predicado y una transformación
  donde la transformación sea una composición de dos funciones.
  Podemos ver la composición dentro de combinadores.
  Esta transformación si la cadena que recibe es de longitud mayor a 5, entonces
  elimina los espacios y luego aplica mayúsculas.
-}
transformacionCondicionalCompuesta :: String -> String
transformacionCondicionalCompuesta = 
  aplicarSi (\s -> length s > 5) (aMayusculas . eliminarEspaciosExtra)

-- Bifurcación con composiciones en cada rama
{-
  Podemos aplicar composiciones dentro de la bifurcación.
  La primera rama aplica la composición soloAlfabeticos . aMayusculas
  La segunda rama aplica la composición aMinusculas . sinVocales
  La lambda indica que los resultados se concatenan con " | " como separador
-}
transformacionBifurcada :: String -> String
transformacionBifurcada = bifurcar
  (\parte1 -> \parte2 -> parte1 ++ " | " ++ parte2)
  (aMayusculas . soloAlfabeticos)
  (sinVocales . aMinusculas)




------------------------------------------------------------------------------
-- FUNCIÓN PRINCIPAL
------------------------------------------------------------------------------

--------------------------------------------------------------------------
-- SUITE DE PRUEBAS (Ejecución y Formato)
--------------------------------------------------------------------------

-- Helper para imprimir
imprimirPrueba :: (Show a, Show b) => String -> String -> a -> b -> IO ()
imprimirPrueba nombreFunc descripcion input output = do
    putStrLn $ ">> PRUEBA: " ++ nombreFunc
    putStrLn $ "   Desc: " ++ descripcion
    putStrLn $ "   Entrada: " ++ show input
    putStrLn $ "   Salida:  " ++ show output
    putStrLn "------------------------------------------------------------"

main :: IO ()
main = do
    putStrLn "------------------------------------------------------------"
    putStrLn "                EJECUCIÓN TEXT NORMALIZER"
    putStrLn "------------------------------------------------------------"
    putStrLn ""

    -- 1. Transformaciones Atómicas
    imprimirPrueba "aMinusculas" 
                   "Convertir todo a minúsculas" 
                   "¡Haskell Es UN LENGUAJE Puramente FUNCIONAL!" 
                   (aMinusculas "¡Haskell Es UN LENGUAJE Puramente FUNCIONAL!")

    imprimirPrueba "aMayusculas" 
                   "Convertir todo a mayúsculas" 
                   "e-mail: usuario@ejemplo.com" 
                   (aMayusculas "e-mail: usuario@ejemplo.com")

    imprimirPrueba "eliminarEspaciosExtra" 
                   "Normalizar espaciado irregular" 
                   "   Este    texto   tiene    demasiados      espacios.   " 
                   (eliminarEspaciosExtra "   Este    texto   tiene    demasiados      espacios.   ")

    imprimirPrueba "soloAlfabeticos" 
                   "Mantener solo letras y espacios (elimina puntuación/nums)" 
                   "ID: 4829-AZ, Fecha: 12/12/2024. ¡Aprobado!" 
                   (soloAlfabeticos "ID: 4829-AZ, Fecha: 12/12/2024. ¡Aprobado!")

    imprimirPrueba "sinVocales" 
                   "Eliminar vocales (estilo abreviado)" 
                   "Murcielago volador ultrasonico" 
                   (sinVocales "Murcielago volador ultrasonico")

    imprimirPrueba "invertirPalabras" 
                   "Invertir cada palabra individualmente" 
                   "Anita lava la tina" 
                   (invertirPalabras "Anita lava la tina")

    imprimirPrueba "capitalizarPalabras" 
                   "Formato Título (Title Case)" 
                   "el ingenioso hidalgo don quijote de la mancha" 
                   (capitalizarPalabras "el ingenioso hidalgo don quijote de la mancha")

    -- 2. Composiciones
    putStrLn "\n--- COMPOSICIONES ---"
    
    imprimirPrueba "normalizacionBasica" 
                   "Trim + Minúsculas" 
                   "  HOLA   MUNDO   " 
                   (normalizacionBasica "  HOLA   MUNDO   ")

    imprimirPrueba "normalizacionEstricta" 
                   "Trim + Minúsculas + Solo Letras" 
                   "¡¡ERROR 404: PÁGINA NO ENCONTRADA!!" 
                   (normalizacionEstricta "¡¡ERROR 404: PÁGINA NO ENCONTRADA!!")

    imprimirPrueba "normalizacionTitulo" 
                   "Limpiar, Solo Letras y Capitalizar" 
                   "--- reporte final: versión 2.0 ---" 
                   (normalizacionTitulo "--- reporte final: versión 2.0 ---")

    imprimirPrueba "reversaSinVocales" 
                   "Mayúsculas, Sin Vocales, Invertido (Criptico)" 
                   "Secret Message" 
                   (reversaSinVocales "Secret Message")

    -- 3. Combinadores de Orden Superior
    putStrLn "\n--- COMBINADORES DE ORDEN SUPERIOR ---"

    imprimirPrueba "pipeline" 
                   "Ejecutar lista: [sinVocales, capitalizar, reverse]" 
                   "Funcional" 
                   (pipeline [sinVocales, capitalizarPalabras, reverse] "Funcional")
                   -- Nota: reverse invierte todo el string, capitalizar arregla el casing, sinVocales quita vocales

    imprimirPrueba "aplicarN" 
                   "Aplicar 3 veces 'capitalizarPalabras' (Idempotencia)" 
                   "texto de prueba" 
                   (aplicarN 3 capitalizarPalabras "texto de prueba")

    imprimirPrueba "aplicarSi (True)" 
                   "Mayúsculas SI longitud > 5" 
                   "palabraLarga" 
                   (aplicarSi (\s -> length s > 5) aMayusculas "palabraLarga")

    imprimirPrueba "aplicarSi (False)" 
                   "Mayúsculas SI longitud > 5" 
                   "corta" 
                   (aplicarSi (\s -> length s > 5) aMayusculas "corta")

    imprimirPrueba "bifurcar" 
                   "Combinar versión en mayúsculas y versión invertida con guión" 
                   "Haskell" 
                   (bifurcar (\a b -> a ++ " --- " ++ b) aMayusculas reverse "Haskell")

    -- 4. Listas y Corpus
    putStrLn "\n--- TRANSFORMACIONES SOBRE LISTAS ---"

    let corpus = ["  Introducción a Haskell  ", "CAPÍTULO 1: Funciones!!", "capítulo 2: Tipos..."]
    
    imprimirPrueba "normalizarCorpus" 
                   "Limpieza estricta de una lista de capítulos" 
                   corpus 
                   (normalizarCorpus corpus)

    imprimirPrueba "procesarConVariantes" 
                   "Generar matriz de variantes para un solo texto" 
                   ["Texto de Ejemplo"] 
                   (procesarConVariantes ["Texto de Ejemplo"])

    imprimirPrueba "filtrarPorLongitud" 
                   "Normalizar y quitar strings menores a 5 chars" 
                   ["Hi", "  Hello  ", "Ok", "Goodbye"] 
                   (filtrarPorLongitud 5 ["Hi", "  Hello  ", "Ok", "Goodbye"])

    -- 5. Ejemplos Avanzados
    putStrLn "\n--- EJEMPLOS AVANZADOS ---"

    imprimirPrueba "ejemplo5 (Bifurcación)" 
                   "Normalizar y anexar metadatos de longitud" 
                   "  Contraseña Secreta  " 
                   (ejemplo5 "  Contraseña Secreta  ")

    imprimirPrueba "transformacionCompleja" 
                   "Pipeline de 5 pasos sobre frase compleja" 
                   "¿Logrará esto funcionar?" 
                   (transformacionCompleja "¿Logrará esto funcionar?")
    
    imprimirPrueba "transformacionBifurcada" 
                   "Dos ramas: (Mayus+Alfa) | (Minus+SinVocales)" 
                   "Input 123 Test" 
                   (transformacionBifurcada "Input 123 Test")