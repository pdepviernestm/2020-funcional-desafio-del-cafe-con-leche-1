module Library where
import PdePreludat

-- Desafio 1

zippear = implementame

-- Desafio 2

data ArbolBinario = Hoja | Rama ArbolBinario Number ArbolBinario deriving Eq

-- Implementado Show para que se pueda mostrar bonito por consola el arbol
instance Show ArbolBinario where
  show arbol = init $ unlines (showArbol arbol)
    where showArbol (Rama ramaIzquierda valor ramaDerecha)
            = show valor : (showRamas ramaIzquierda ramaDerecha)
                where
                    showRamas izquierda derecha =
                        ((pad "+- " "|  ") (showArbol derecha)) ++ ((pad "`- " "   ") (showArbol izquierda))
                    pad first rest = zipWith (++) (first : repeat rest)
          showArbol (Hoja) = []

ordenado :: ArbolBinario -> Bool
ordenado = implementame