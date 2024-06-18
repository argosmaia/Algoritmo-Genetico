{-|
    Module      : Lib
    Description : A simple example module
    Copyright   : (c) 2024 by aHk4RDlJk2DKK1IX1NWvjA==
    License     : GPL-3
    Maintainer  : 0DbZ3d837keC2NZ2q9JcLg==

|-}
module Lib
    ( someFunc
    ) where

-- Solução: ([Bool], [Int])
-- População: [Solucão]

-- 1. Gerar população inicial
gerarPopulacao :: Int -> Int -> Int -> [([Bool], [Int])]
gerarPopulacao nPopulacao nVariaveis nTermos = [criarSolAleatoria (nVariaveis * nTermos) | n <- [1..nPopulacao]]

-- criar uma solução aleatória
criarSolAleatoria :: Int -> ([Bool], [Int])
criarSolAleatoria n = (vecBool, vecInt)
    where
        vecBool = take n $ cycle [False, True] -- gere umna lista repetindo os elementos de [False, True]
        -- vecBool = [if x == 0 then False else True | x <- take n $ randomRs (0, 1) (mkStdGen 42)]
        vecInt = take n $ cycle [1 .. 5]

-- 2. Aplicar crossover
aplicarCrossover :: [([Bool], [Int])] -> [([Bool], [Int])]
aplicarCrossover populacao = [crossover (choose populacao) (choose populacao) | _ <- [1 .. length populacao]]
    where
        choose populacao = populacao !! (length populacao `div` 3)

-- crossover de dois pais para gerar um filho
crossover :: ([Bool], [Int]) -> ([Bool], [Int]) -> ([Bool], [Int])
crossover (p1Boolean, p1Int) (p2Boolean, p2Int) = (filhoBoolean, filhoInt)
    where
        indice = length p1Boolean `div` 2
        filhoBoolean = take indice p1Boolean ++ drop indice p2Boolean
        filhoInt = take indice p1Int ++ drop indice p2Int
        -- in (filhoBoolean, filhoInt)

-- crossover de dois pais para gerar um filho
p1 = ([True, False, False, True], [1, 1, 3, 5] :: [Int])
p2 = ([False, False, True, False], [3, 1, 4, 2] :: [Int])

-- função que aplica mutação em todos os elementos de uma população
aplicarMutacao :: [([Bool], [Int])] -> [([Bool], [Int])] -- mesma assinatura do crossover
aplicarMutacao populacao = [mutacao p | p <- populacao]

-- mutação: trocar um elemento aleatório de um vetor de booleanos e de um vetor de inteiros
mutacao :: ([Bool], [Int]) -> ([Bool], [Int])
mutacao (vecBool, vecInt) = (vecBool', vecInt')
    where
        b = False -- mudar vecBool se for falsom e vecInt se for verdadeiro
        indice = length vecBool `div` 2
        vecBool' = if not b then mudarAleatorioBool indice vecBool else vecBool
        vecInt' = if b then mudarAleatorioInt indice vecInt else vecInt

-- função que muda um elemento de um vetor
mudar :: Int -> a -> [a] -> [a]
mudar indice x xs = take indice xs ++ (x: drop (indice + 1) xs)

-- função que muda um elemento de um vetor de booleanos
mudarAleatorioBool :: Int -> [Bool] -> [Bool]
mudarAleatorioBool indice vec = mudar indice x vec
    where
        x = not (vec !! indice)

-- função que muda um elemento de um vetor de inteiros
mudarAleatorioInt :: Int -> [Int] -> [Int]
mudarAleatorioInt indice vec = mudar indice x vec
    where
        x = (((vec !! indice) * 10 + 3) `mod` 5) + 1

select :: Int -> [([Bool], [Int])] -> [([Bool], [Int])]
select n populacao = take n populacao

-- função que seleciona os melhores elementos de uma população
ga :: Int -> Int -> Int -> Int -> [([Bool], [Int])]
ga iteracoes nPopulacao nVariaveis nTermos = 
    let populacao = gerarPopulacao nPopulacao nVariaveis nTermos
    in step iteracoes populacao
        where
            step 0 populacao = populacao
            step n populacao = let filho = crossover populacao
                                filho' = aplicarMutacao filho
                                populacao' = select (filho' ++ populacao)
                                in step (n-1) populacao'

someFunc :: IO ()
someFunc = putStrLn "someFunc"