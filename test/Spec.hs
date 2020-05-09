import PdePreludat
import Library
import Test.Hspec
import           Control.Monad (unless)
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
  describe "zippear" $ do
    it "dadas dos listas vacias devuelve una lista vacia" $ do
      zippear [] [] `shouldBe` ([] :: [(Number, Number)])
    it "dadas dos listas, devuelve una lista de tuplas formadas por los elementos de ambas" $ do
      zippear [1, 2, 3, 4, 5] "abcde" `shouldBe` [(1, 'a'), (2, 'b'), (3, 'c'), (4, 'd'), (5, 'e')]
    it "dadas dos listas de diferente tamaño, devuelve una lista de tuplas formadas por los elementos de ambas del tamaño de la mas corta" $ do
      zippear [1, 2, 3] "abcde" `shouldBe` [(1, 'a'), (2, 'b'), (3, 'c')]
      zippear [1, 2, 3, 4, 5] "abc" `shouldBe` [(1, 'a'), (2, 'b'), (3, 'c')]
  
  describe "ordenado" $ do
    it "un arbol que es solo una hoja esta ordenado" $ do
      ordenado Hoja `shouldBe` True
    it "un arbol que es una rama con dos hojas esta ordenado" $ do
      ordenado (Rama Hoja 5 Hoja) `shouldBe` True
    it "un arbol que tiene una rama izquierda esta ordenado si el valor de su rama principal igual al de la rama izquierda" $ do
      ordenado (Rama (Rama Hoja 5 Hoja) 5 Hoja) `shouldBe` True
    it "un arbol que tiene una rama izquierda esta ordenado si el valor de su rama principal es mayor al de la rama izquierda" $ do
      ordenado (Rama (Rama Hoja 3 Hoja) 5 Hoja) `shouldBe` True
    it "un arbol que tiene una rama izquierda NO esta ordenado si el valor de su rama principal es menor al de la rama izquierda" $ do
      ordenado (Rama (Rama Hoja 5 Hoja) 3 Hoja) `shouldBe` False
    it "un arbol que tiene una rama derecha esta ordenado si el valor de su rama principal es menor al de la rama derecha" $ do
      ordenado (Rama Hoja 3 (Rama Hoja 5 Hoja)) `shouldBe` True
    it "un arbol que tiene una rama derecha esta ordenado si el valor de su rama principal es igual al de la rama derecha" $ do
      ordenado (Rama Hoja 5 (Rama Hoja 5 Hoja)) `shouldBe` True
    it "un arbol que tiene una rama derecha NO esta ordenado si el valor de su rama principal es mayor al de la rama derecha" $ do
      ordenado (Rama Hoja 5 (Rama Hoja 3 Hoja)) `shouldBe` False
    it "un arbol esta ordenado solo si el valor de una rama es mayor o igual a todos los valores de su rama izquierda y la misma esta ordenada" $ do
      let ramaIzquierdaOrdenada = Rama (Rama Hoja 3 Hoja)
                                        5
                                       (Rama Hoja 7 Hoja)

      ordenado (Rama ramaIzquierdaOrdenada 7 Hoja) `shouldBe` True
      ordenado (Rama ramaIzquierdaOrdenada 10 Hoja) `shouldBe` True
      ordenado (Rama ramaIzquierdaOrdenada 5 Hoja) `shouldBe` False

      let ramaIzquierdaDesordenada = Rama (Rama Hoja 3 Hoja)
                                          7
                                          (Rama Hoja 5 Hoja)

      ordenado (Rama ramaIzquierdaDesordenada 10 Hoja) `shouldBe` False
    it "un arbol esta ordenado solo si el valor de una rama es menor o igual a todos los valores de su rama derecha y la misma esta ordenada" $ do
      let ramaDerechaOrdenada = Rama (Rama Hoja 3 Hoja)
                                        5
                                       (Rama Hoja 7 Hoja)

      ordenado (Rama Hoja 3 ramaDerechaOrdenada) `shouldBe` True
      ordenado (Rama Hoja 2 ramaDerechaOrdenada) `shouldBe` True
      ordenado (Rama Hoja 5 ramaDerechaOrdenada) `shouldBe` False

      let ramaDerechaDesordenada = Rama (Rama Hoja 3 Hoja)
                                        7
                                        (Rama Hoja 5 Hoja)

      ordenado (Rama Hoja 2 ramaDerechaDesordenada) `shouldBe` False
    it "un arbol solo esta ordenado si todos sus subarboles estan ordenados" $ do
      ordenado (Rama Hoja
                     1
                     (Rama (Rama Hoja 3 Hoja)
                            2
                           (Rama Hoja 4 Hoja))) `shouldBe` False

      ordenado (Rama
                (Rama (Rama Hoja 2 Hoja)
                       1
                      (Rama Hoja 3 Hoja))
                4 Hoja) `shouldBe` False

      ordenado (Rama (Rama (Rama Hoja 1 Hoja)
                            2
                           (Rama Hoja 3 Hoja))
                           4
                           (Rama (Rama Hoja 5 Hoja)
                            6
                           (Rama Hoja 7 Hoja))) `shouldBe` True
