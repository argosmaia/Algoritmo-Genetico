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

gerarPopulacao :: Int -> Int -> Int -> [([Bool], [Int])]
gerarPopulacao nPopulacao nVariaveis nTermos = [criarSolAleatoria (nVariaveis * nTermos) | n <- [1..nPopulacao]]

criarSolAleatoria :: Int -> ([Bool], [Int])
criarSolAleatoria n = (vecBool, vecInt)
    where
        vecBool = [if x == 0 then False else True | x <- take n $ randomRs (0, 1) (mkStdGen 42)]
        vecInt = take n $ randomRs (0, 1) (mkStdGen 42) 

ga :: Int -> Int -> Int -> Int -> [([Bool], [Int])]
ga iteracoes nPopulacao nVariaveis nTermos = 
    let populacao = gerarPopulacao nPopulacao nVariaveis nTermos
    in step it populacao
        where
            step 0 populacao = populacao
            step n populacao = let filho' = mutate filho
                                populacao' = select (filho' ++ populacao)
                                in step (n-1) populacao'

someFunc :: IO ()
someFunc = putStrLn "someFunc"