:- use_module(library(clpfd)).

% Definição do tabuleiro com valores iniciais e regiões
tabuleiro([[[1,2],[1,_],[2,_],[2,_],[2,1],[3,_]],
            [[4,_],[4,_],[4,_],[4,3],[4,_],[3,_]],
            [[5,_],[6,3],[6,_],[6,_],[4,5],[7,3]],
            [[5,_],[5,_],[5,_],[6,_],[7,_],[7,_]],
            [[8,_],[8,_],[10,3],[0,_],[0,4],[0,2]],
            [[9,_],[9,_],[10,_],[10,_],[0,_],[0,_]]]).


tamanho_regiao(0,5).
tamanho_regiao(1,2).
tamanho_regiao(2,3).
tamanho_regiao(3,2).
tamanho_regiao(4,6).
tamanho_regiao(5,4).
tamanho_regiao(6,4).
tamanho_regiao(7,3).
tamanho_regiao(8,2).
tamanho_regiao(9,2).
tamanho_regiao(10,3).

% Definir o valor máximo que os valores de cada região podem assumir
valor_maximo_regiao([R,X]) :- 
    tamanho_regiao(R,T),  % Obtém o tamanho da região
    X in 1..T.  % Define o domínio de X entre 1 e o tamanho da região

% Verificar se o vizinho à direita é diferente
vizinhos_diferentes([[_,_]]).
vizinhos_diferentes([[_,X1],[R2,X2]|T]) :-
    X1 #\= X2,  % Garante que os valores vizinhos sejam diferentes
    append([[R2,X2]],T,L),  % Cria uma nova lista com o vizinho atual e o restante
    vizinhos_diferentes(L).  % Chama recursivamente para o restante da lista

% Verificar se o valor acima de outro é maior, se eles fizerem parte do mesmo grupo
maior_acima([[_,_]]).
maior_acima([[R1,X1],[R2,X2]|T]) :-
    ((R1 #\= R2);  % Se os grupos são diferentes
    (X1 #> X2)),  % Ou se o valor de cima é maior que o valor de baixo
    append([[R2,X2]],T,L),  % Cria uma nova lista com o valor atual e o restante
    maior_acima(L).  % Chama recursivamente para o restante da lista

% Agrupar os elementos do mesmo grupo (recebe o grupo)
agrupar(_, [], []).
agrupar(R, [[R1, X1] | T], [X1 | L]) :- 
    R #= R1,  % Se o grupo atual é igual ao grupo R
    agrupar(R, T, L).  % Chama recursivamente para o restante da lista
agrupar(R, [[R1, _] | T], L) :- 
    R #\= R1,  % Se o grupo atual é diferente do grupo R
    agrupar(R, T, L).  % Chama recursivamente para o restante da lista

% Verificar se os membros de uma lista são diferentes
todas_regioes_distintas([H]) :-
    all_distinct(H).  % Garante que todos os elementos da lista são distintos
todas_regioes_distintas([H|T]) :-
    all_distinct(H),  % Garante que todos os elementos da lista são distintos
    todas_regioes_distintas(T).  % Chama recursivamente para o restante das listas

% Obter o segundo elemento de cada matriz
extrair([], []).
extrair([[_, Segundo] | Resto], [Segundo | ValoresSegundo]) :-
    extrair(Resto, ValoresSegundo).  % Chama recursivamente para o restante da lista

% Extrair os valores de cada linha da matriz
extrair_valores([], []).
extrair_valores([Sublista | Resto], [ValoresSegundo | Resultado]) :-
    extrair(Sublista, ValoresSegundo),  % Extrai os valores da sublista
    extrair_valores(Resto, Resultado).  % Chama recursivamente para o restante da lista

% Solucionador do problema
solucionador(Problema) :-
    append(Problema, Lista),  % Cria uma lista com todos os elementos do problema
    maplist(valor_maximo_regiao, Lista),  % Define os valores máximos para cada região
    maplist(vizinhos_diferentes, Problema),  % Verifica vizinhos na linha
    transpose(Problema, Colunas),  % Transpõe a matriz para verificar colunas
    maplist(vizinhos_diferentes, Colunas),  % Verifica vizinhos na coluna
    maplist(maior_acima, Colunas),  % Verifica se os valores acima são maiores
    agrupar(0, Lista, Grupo0),
    agrupar(1, Lista, Grupo1),
    agrupar(2, Lista, Grupo2),
    agrupar(3, Lista, Grupo3),
    agrupar(4, Lista, Grupo4),
    agrupar(5, Lista, Grupo5),
    agrupar(6, Lista, Grupo6),
    agrupar(7, Lista, Grupo7),
    agrupar(8, Lista, Grupo8),
    agrupar(9, Lista, Grupo9),
    agrupar(10, Lista, Grupo10),
    Grupos = [Grupo0,Grupo1,Grupo2,Grupo3,Grupo4,Grupo5,Grupo6,Grupo7,Grupo8,Grupo9,Grupo10], 
    todas_regioes_distintas(Grupos),  % Verifica se todos os grupos têm valores distintos
    !.

% Solução do problema
solucao(Resposta) :-
    tabuleiro(ProblemaTabuleiro),  % Define o tabuleiro inicial
    solucionador(ProblemaTabuleiro),  % Soluciona o problema
    extrair_valores(ProblemaTabuleiro, Resposta).  % Extrai os valores do tabuleiro resolvido
