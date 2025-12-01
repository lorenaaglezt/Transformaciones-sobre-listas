# PROYECTO 2 Lenguajes de Programación

**Integrantes**

- Figueroa Rojas Emiliano - 
- Gómez Calva Carlos Manuel - 
- González Téllez Lorena - 321288952

## Implementación práctica

En el presente proyecto, como parte práctica del desarrollo del mismo, presentamos un programa que implementa transformaciones sobre listas en Haskell. El objetivo es plasmar los conceptos de currificación, aplicación parcial y composición funcional en un ejemplo práctico.

## Objetivo

Plasmar cómo se construyen programas complejos combinando pequeñas expresiones lambda mediante composición y aplicación parcial, implementando un conjunto de transformaciones sobre listas de texto usando únicamente el operador de composición (.) y lambdas anónimas.

## Requisitos

La implementación del proyecto fue realizada con Haskell. Requisitos Previos:

- GHC (Glasgow Haskell Compiler) instalado

## Iniciar el interpretador interactivo
ghci TextNormalizer.hs

### Probar funciones individuales
ghci> normalizacionBasica "  HOLA   mundo  "
"hola mundo"

ghci> normalizacionEstricta "Hola123 Mundo!!!"
"hola mundo"

ghci> codificacionDivertida "Hola Mundo"
"LH DNUM"

### Crear composiciones personalizadas
ghci> let miFunc = aMayusculas . eliminarEspaciosExtra
ghci> miFunc "  hola   mundo  "
"HOLA MUNDO"

### Ejecutar el programa de demostración completo
ghci> main