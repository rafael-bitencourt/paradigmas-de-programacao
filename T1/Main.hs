import Matriz
import Validacao

-- Define a função resolve para encontrar uma solução para o tabuleiro, dado um estado inicial, regiões e posições iniciais de linha e coluna.
resolve :: [[Int]] -> [[Int]] -> Int -> Int -> Maybe [[Int]]
resolve tabuleiro regioes linha coluna
    | linha == length tabuleiro = Just tabuleiro  -- Caso base: se todas as linhas foram processadas, retorna o tabuleiro como solução.
    | coluna == length (head tabuleiro) = resolve tabuleiro regioes (linha + 1) 0  -- Se a coluna excede o limite, passa para a próxima linha.
    | tabuleiro !! linha !! coluna /= 0 = resolve tabuleiro regioes linha (coluna + 1)  -- Se a célula atual já está preenchida, move para a próxima coluna.
    | otherwise = tentaColocacoes [1..length tabuleiro]  -- Caso contrário, tenta encontrar um número válido para colocar na célula.
    where
        tentaColocacoes :: [Int] -> Maybe [[Int]]
        tentaColocacoes [] = Nothing  -- Se não há mais números para tentar, retorna Nothing.
        tentaColocacoes (k:ks)
            | ehPosicaoValida tabuleiro regioes linha coluna k =  -- Verifica se colocar k na posição atual é válido.
                case resolve (substitui2D tabuleiro linha coluna k) regioes linha (coluna + 1) of  -- Tenta resolver o tabuleiro com k na posição atual.
                    Just solucao -> Just solucao  -- Se encontrou solução, retorna.
                    Nothing -> tentaColocacoes ks  -- Se não, continua tentando com o próximo número.
            | otherwise = tentaColocacoes ks  -- Se k não é válido, tenta o próximo número.

-- Define uma função para imprimir o tabuleiro.
imprimeTabuleiro :: [[Int]] -> IO ()
imprimeTabuleiro tabuleiro = mapM_ print tabuleiro  -- Aplica a função print a cada linha do tabuleiro.

-- Função principal que inicializa o tabuleiro e as regiões e chama resolve.
main :: IO ()
main = do
    let tabuleiro =    [[0,0,0,0,0,0,0,0],
                        [0,1,3,0,0,0,0,0],
                        [0,0,0,0,0,3,0,0],
                        [0,0,3,0,0,0,0,0],
                        [0,5,0,3,0,0,0,0],
                        [0,2,0,0,0,0,0,0],
                        [0,0,0,0,0,0,3,0],
                        [0,0,5,3,0,0,0,0]]

        regioes =       [[1,1,2,2,3,4,5,5],
                        [1,1,6,2,7,4,4,5],
                        [6,6,6,8,7,9,10,10],
                        [11,11,11,8,7,9,9,10],
                        [12,8,8,8,8,9,9,10],
                        [12,13,14,14,14,15,16,10],
                        [13,13,13,13,17,15,15,15],
                        [18,17,17,17,17,15,19,19]]

    case resolve tabuleiro regioes 0 0 of
        Just tabuleiroResolvido -> imprimeTabuleiro tabuleiroResolvido 
        Nothing -> putStrLn "Nenhuma solução encontrada."  
