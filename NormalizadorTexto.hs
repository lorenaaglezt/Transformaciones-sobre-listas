-- Normalización de texto usando composición funcional pura
module NormalizadorTexto where

import Data.Char (toLower, toUpper, isAlpha, isSpace)
import Data.List (intercalate)

-- ============================================
-- PARTE 1: Transformaciones atómicas (lambdas)
-- ============================================

-- Convertir a minúsculas
aMinusculas :: String -> String
aMinusculas = map toLower

-- Convertir a mayúsculas
aMayusculas :: String -> String
aMayusculas = map toUpper

-- Eliminar espacios extra
eliminarEspaciosExtra :: String -> String
eliminarEspaciosExtra = unwords . words

-- Eliminar caracteres no alfabéticos (excepto espacios)
soloAlfabeticos :: String -> String
soloAlfabeticos = filter (\c -> isAlpha c || isSpace c)

-- Eliminar vocales
sinVocales :: String -> String
sinVocales = filter (\c -> not (c `elem` "aeiouAEIOU"))

-- Invertir cada palabra
invertirPalabras :: String -> String
invertirPalabras = unwords . map reverse . words

-- Capitalizar primera letra de cada palabra
capitalizarPalabras :: String -> String
capitalizarPalabras = unwords . map capitalizar . words
  where capitalizar [] = []
        capitalizar (x:xs) = toUpper x : map toLower xs

-- ============================================
-- PARTE 2: Composiciones complejas
-- ============================================

-- Normalización básica: minúsculas + sin espacios extra
normalizacionBasica :: String -> String
normalizacionBasica = eliminarEspaciosExtra . aMinusculas

-- Normalización estricta: solo alfabéticos + minúsculas + sin espacios extra
normalizacionEstricta :: String -> String
normalizacionEstricta = eliminarEspaciosExtra . aMinusculas . soloAlfabeticos

-- Normalización para títulos
normalizacionTitulo :: String -> String
normalizacionTitulo = eliminarEspaciosExtra . capitalizarPalabras . soloAlfabeticos

-- Codificación divertida: invertir palabras + sin vocales + mayúsculas
codificacionDivertida :: String -> String
codificacionDivertida = aMayusculas . sinVocales . invertirPalabras

-- ============================================
-- PARTE 3: Transformaciones sobre listas
-- ============================================

-- Aplicar una transformación a cada elemento de una lista
transformarLista :: (String -> String) -> [String] -> [String]
transformarLista = map

-- Composición de transformaciones sobre listas
transformarListaCompuesta :: [(String -> String)] -> [String] -> [String]
transformarListaCompuesta transformaciones = map (foldr (.) id transformaciones)

-- Pipeline personalizable
pipeline :: [String -> String] -> String -> String
pipeline = foldr (.) id

-- ============================================
-- PARTE 4: Combinadores avanzados
-- ============================================

-- Aplicar una transformación N veces
aplicarN :: Int -> (a -> a) -> (a -> a)
aplicarN 0 _ = id
aplicarN n f = f . aplicarN (n-1) f

-- Aplicación condicional
aplicarSi :: (a -> Bool) -> (a -> a) -> (a -> a)
aplicarSi predicado transformacion = \x -> 
  if predicado x then transformacion x else x

-- Bifurcación: aplicar dos transformaciones y combinar resultados
bifurcar :: (b -> c -> d) -> (a -> b) -> (a -> c) -> (a -> d)
bifurcar combinar f g = \x -> combinar (f x) (g x)

-- ============================================
-- PARTE 5: Ejemplos de uso
-- ============================================

-- Ejemplo 1: Pipeline simple
ejemplo1 :: String -> String
ejemplo1 = eliminarEspaciosExtra . aMinusculas . soloAlfabeticos

-- Ejemplo 2: Pipeline complejo usando lista de transformaciones
ejemplo2 :: String -> String
ejemplo2 = pipeline [eliminarEspaciosExtra, capitalizarPalabras, soloAlfabeticos]

-- Ejemplo 3: Transformación condicional
ejemplo3 :: String -> String
ejemplo3 = aplicarSi (\s -> length s > 10) aMayusculas

-- Ejemplo 4: Aplicar transformación múltiples veces
ejemplo4 :: String -> String
ejemplo4 = aplicarN 2 invertirPalabras  -- Invierte dos veces = original

-- Ejemplo 5: Bifurcación - combinar longitud original con texto normalizado
ejemplo5 :: String -> String
ejemplo5 = bifurcar (\normalizado longitud -> 
  normalizado ++ " [" ++ show longitud ++ " caracteres]") 
  normalizacionBasica 
  length

-- ============================================
-- PARTE 6: Procesamiento de listas de textos
-- ============================================

-- Normalizar una lista de textos
normalizarCorpus :: [String] -> [String]
normalizarCorpus = map normalizacionEstricta

-- Aplicar múltiples normalizaciones a cada texto
procesarConVariantes :: [String] -> [[String]]
procesarConVariantes textos = map (\texto -> [
    normalizacionBasica texto,
    normalizacionEstricta texto,
    normalizacionTitulo texto,
    codificacionDivertida texto
  ]) textos

-- Filtrar textos normalizados por longitud
filtrarPorLongitud :: Int -> [String] -> [String]
filtrarPorLongitud minLen = filter ((>= minLen) . length) . map normalizacionBasica

-- ============================================
-- FUNCIÓN PRINCIPAL PARA DEMOSTRACIÓN
-- ============================================

main :: IO ()
main = do
  putStrLn "=== NORMALIZADOR DE TEXTO CON COMPOSICIÓN FUNCIONAL ==="
  putStrLn ""
  
  let textoOriginal = "  ¡Hola!!!  Mundo123  con    ESPACIOS   extra  "
  
  putStrLn $ "Texto original: \"" ++ textoOriginal ++ "\""
  putStrLn ""
  
  putStrLn "--- Transformaciones individuales ---"
  putStrLn $ "Normalización básica: " ++ normalizacionBasica textoOriginal
  putStrLn $ "Normalización estricta: " ++ normalizacionEstricta textoOriginal
  putStrLn $ "Normalización título: " ++ normalizacionTitulo textoOriginal
  putStrLn $ "Codificación divertida: " ++ codificacionDivertida textoOriginal
  putStrLn ""
  
  putStrLn "--- Ejemplos de composición ---"
  putStrLn $ "Ejemplo 1: " ++ ejemplo1 textoOriginal
  putStrLn $ "Ejemplo 2: " ++ ejemplo2 textoOriginal
  putStrLn $ "Ejemplo 3 (condicional): " ++ ejemplo3 textoOriginal
  putStrLn $ "Ejemplo 5 (bifurcación): " ++ ejemplo5 textoOriginal
  putStrLn ""
  
  putStrLn "--- Procesamiento de listas ---"
  let corpus = ["Hola mundo", "TEXTO en mayúsculas", "  espacios  extras  "]
  putStrLn "Corpus original:"
  mapM_ (\t -> putStrLn $ "  - \"" ++ t ++ "\"") corpus
  putStrLn ""
  putStrLn "Corpus normalizado:"
  mapM_ (\t -> putStrLn $ "  - \"" ++ t ++ "\"") (normalizarCorpus corpus)
  putStrLn ""
  
  putStrLn "--- Pipeline personalizado ---"
  let miPipeline = pipeline [aMayusculas, sinVocales, eliminarEspaciosExtra]
  putStrLn $ "Pipeline [mayúsculas . sinVocales . sinEspacios]: " ++ miPipeline textoOriginal