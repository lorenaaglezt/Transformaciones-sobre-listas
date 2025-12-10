# PROYECTO 2 Lenguajes de Programación

**Integrantes**

- Figueroa Rojas Emiliano - 321088628
- Gómez Calva Carlos Manuel - 321281678
- González Téllez Lorena - 321288952

## Implementación práctica

En el presente proyecto, como parte práctica del desarrollo del mismo, presentamos un programa que implementa transformaciones sobre listas en Haskell. El objetivo es plasmar los conceptos de currificación, aplicación parcial y composición funcional en un ejemplo práctico.

## Objetivo

Plasmar cómo se construyen programas complejos combinando pequeñas expresiones lambda mediante composición y aplicación parcial, implementando un conjunto de transformaciones sobre listas de texto usando únicamente el operador de composición (.) y lambdas anónimas. Este programa implementa un normalizador de texto, es decir, aplica distintas transformaciones a una lista de caracteres, ya sea convirtiendolo en mayúsculas, minúsculas, sin números, etc. 

## Ejecución

El programa de transformaciones sobre listas está implementado con Haskell. Se requiere el compilador de Haskell GHC.

Primero cargamos el archivo:
```
ghci NormalizadorTexto.hs
```

Y podemos llamar a la función main que contiene la ejecución de las múltiples transformaciones implementadas, en dicho main se presenta la entrada, la salida y la descripción de la transformación que fue aplicada a la cadena.
```
main
```

Así mismo, se pueden ejecutar las transformaciones sobre distintas cadenas, como:
```
normalizacionTitulo "desde la terminal"
```
-- Resultado: "Desde La Terminal"

```
pipeline [reverse, aMayusculas] "lenguajes"
```
-- Resultado: "SEJAUGNEL"

```
normalizarCorpus [" LECHE ", "Pan..Integral", "Huevos!!"]
```
-- Resultado: ["leche","panintegral","huevos"]

```
let comentarios = ["ok", "excelente servicio", "bye", "muy bueno", "xo"]
filtrarPorLongitud 5 comentarios
```
-- Resultado: ["excelente servicio","muy bueno"]