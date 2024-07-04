-- Define o módulo Matriz que exporta a função substitui2D.
module Matriz (substitui2D) where

-- Define a função substitui2D que substitui um elemento em uma posição específica do tabuleiro.
substitui2D :: [[Int]] -> Int -> Int -> Int -> [[Int]]
substitui2D tabuleiro linha coluna k =
    let (antes, (x:depois)) = splitAt linha tabuleiro  -- Divide o tabuleiro em duas partes na linha especificada.
        (antesColuna, (_:depoisColuna)) = splitAt coluna x  -- Divide a linha especificada na coluna especificada.
    in antes ++ [antesColuna ++ [k] ++ depoisColuna] ++ depois  -- Constrói um novo tabuleiro com o elemento substituído.
