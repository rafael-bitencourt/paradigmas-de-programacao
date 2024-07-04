-- Define o módulo Validacao que exporta a função ehPosicaoValida.
module Validacao (ehPosicaoValida) where

-- Define a função ehPosicaoValida que verifica se um número k pode ser colocado em uma posição específica do tabuleiro.
ehPosicaoValida :: [[Int]] -> [[Int]] -> Int -> Int -> Int -> Bool
ehPosicaoValida tabuleiro regioes linha coluna k =
    let regiao = regioes !! linha !! coluna  -- Obtém a região da posição atual.
        -- Obtém todos os valores na mesma região que a posição atual.
        valoresRegiao = [tabuleiro !! i !! j | i <- [0..length tabuleiro - 1], j <- [0..length tabuleiro - 1], regioes !! i !! j == regiao]
        -- Define as posições dos vizinhos diretos.
        vizinhos = [(linha - 1, coluna), (linha + 1, coluna), (linha, coluna - 1), (linha, coluna + 1)]
        -- Obtém os valores dos vizinhos diretos que estão dentro dos limites do tabuleiro.
        valoresVizinhos = [tabuleiro !! i !! j | (i, j) <- vizinhos, i >= 0, i < length tabuleiro, j >= 0, j < length tabuleiro]
    in
        -- Verifica se k não está presente na região ou entre os vizinhos e cumpre as condições adicionais para as bordas do tabuleiro.
        k `notElem` valoresRegiao &&
        k <= length valoresRegiao &&
        k `notElem` valoresVizinhos &&
        (linha == 0 || regioes !! (linha - 1) !! coluna /= regiao || tabuleiro !! (linha - 1) !! coluna >= k) &&
        (linha == length tabuleiro - 1 || regioes !! (linha + 1) !! coluna /= regiao || tabuleiro !! (linha + 1) !! coluna <= k)
