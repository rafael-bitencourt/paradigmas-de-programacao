camisa(amarela).
camisa(azul).
camisa(branca).
camisa(verde).
camisa(vermelha).

nome(ana).
nome(claudia).
nome(fernanda).
nome(helen).
nome(larissa).

experimentou(granadilho).
experimentou(jambo_rosa).
experimentou(moranga).
experimentou(nespera).
experimentou(pitaya_amarela).

comprou(carnes).
comprou(especiarias).
comprou(queijos).
comprou(peixes).
comprou(vinhos).

gastou(80).
gastou(100).
gastou(150).
gastou(180).
gastou(210).

veio_com(filho).
veio_com(irma).
veio_com(mae).
veio_com(marido).
veio_com(namorado).

% X está à ao lado de Y
aoLado(X,Y,Lista) :- nextto(X,Y,Lista);nextto(Y,X,Lista).

% X está à esquerda de Y (em qualquer posição à esquerda)
aEsquerda(X,Y,Lista) :- nth0(IndexX,Lista,X), nth0(IndexY,Lista,Y), IndexX < IndexY.

% X está à direita de Y (em qualquer posição à direita)
aDireita(X,Y,Lista) :- aEsquerda(Y,X,Lista).

% X está no canto se ele é o primeiro ou o último da lista
noCanto(X,Lista) :- last(Lista,X).
noCanto(X,[X|_]).

todosDiferentes([]).
todosDiferentes([H|T]) :- not(member(H,T)), todosDiferentes(T).

solucao(ListaSolucao) :-
    ListaSolucao = [
        mulher(Nome1, Camisa1, Experimentou1, Comprou1, Gastou1, VeioCom1),
        mulher(Nome2, Camisa2, Experimentou2, Comprou2, Gastou2, VeioCom2),
        mulher(Nome3, Camisa3, Experimentou3, Comprou3, Gastou3, VeioCom3),
        mulher(Nome4, Camisa4, Experimentou4, Comprou4, Gastou4, VeioCom4),
        mulher(Nome5, Camisa5, Experimentou5, Comprou5, Gastou5, VeioCom5)
    ],

    % Hélen veio acompanhada pelo namorado.
    member(mulher(helen, _, _, _, _, namorado), ListaSolucao),

    % Quem veio comprar especiarias está exatamente à esquerda da mulher que veio com a irmã.
    aEsquerda(mulher(_, _, _, especiarias, _, _), mulher(_, _, _, _, _, irma), ListaSolucao),
    aoLado(mulher(_, _, _, especiarias, _, _), mulher(_, _, _, _, _, irma), ListaSolucao),

    % Quem veio com o marido está ao lado da primeira mulher.
    aoLado(mulher(_, _, _, _, _, marido), mulher(_, _, _, _, _, _), ListaSolucao),

    % Quem veio com o filho está em uma das pontas.
    noCanto(mulher(_, _, _, _, _, filho), ListaSolucao),

    % Quem está usando a camisa verde está exatamente à esquerda de quem gastou R$ 100.
    aEsquerda(mulher(_, verde, _, _, _, _), mulher(_, _, _, _, 100, _), ListaSolucao),
    aoLado(mulher(_, verde, _, _, _, _), mulher(_, _, _, _, 100, _), ListaSolucao),

    % A pessoa que gastou R$ 180 veio de blusa branca.
    member(mulher(_, branca, _, _, 180, _), ListaSolucao),

    % Na quarta posição está a mulher que gastou R$ 150.
    nth0(3, ListaSolucao, mulher(_, _, _, _, 150, _)),

    % A pessoa que gastou menos está na segunda posição.
    nth0(1, ListaSolucao, mulher(_, _, _, _, 80, _)),

    % Ana foi ao mercado com a intenção de comprar queijos.
    member(mulher(ana, _, _, queijos, _, _), ListaSolucao),

    % Quem foi comprar peixes está em algum lugar à direita de quem veio com blusa branca.
    aDireita(mulher(_, _, _, peixes, _, _), mulher(_, branca, _, _, _, _), ListaSolucao),

    % A mulher que veio acompanhada pela mãe está de blusa verde.
    member(mulher(_, verde, _, _, _, mae), ListaSolucao),

    % Quem foi comprar carnes está na primeira posição.
    nth0(0, ListaSolucao, mulher(_, _, _, carnes, _, _)),

    % A moça de blusa verde está em algum lugar à esquerda de quem foi experimentar Nêspera.
    aEsquerda(mulher(_, verde, _, _, _, _), mulher(_, _, nespera, _, _, _), ListaSolucao),

    % A mulher que foi comprar vinhos aproveitou para experimentar Pitaya Amarela.
    member(mulher(_, _, pitaya_amarela, vinhos, _, _), ListaSolucao),

    % A moça da segunda posição foi experimentar Moranga.
    nth0(1, ListaSolucao, mulher(_, _, moranga, _, _, _)),

    % Quem experimentou Granadilho foi ao mercado para comprar queijos.
    member(mulher(_, _, granadilho, queijos, _, _), ListaSolucao),

    % A mulher que veio com o marido está exatamente à esquerda de quem experimentou Pitaya Amarela.
    aEsquerda(mulher(_, _, _, _, _, marido), mulher(_, _, pitaya_amarela, _, _, _), ListaSolucao),
    aoLado(mulher(_, _, _, _, _, marido), mulher(_, _, pitaya_amarela, _, _, _), ListaSolucao),

    % A moça que experimentou Pitaya Amarela chama-se Fernanda.
    member(mulher(fernanda, _, pitaya_amarela, _, _, _), ListaSolucao),

    % A Cláudia está exatamente à esquerda de quem experimentou Moranga.
    aEsquerda(mulher(claudia, _, _, _, _, _), mulher(_, _, moranga, _, _, _), ListaSolucao),
    aoLado(mulher(claudia, _, _, _, _, _), mulher(_, _, moranga, _, _, _), ListaSolucao),

    % A moça de camisa azul está na segunda posição.
    nth0(1, ListaSolucao, mulher(_, azul, _, _, _, _)),

    % Na primeira posição está a mulher usando camisa amarela.
    nth0(0, ListaSolucao, mulher(_, amarela, _, _, _, _)),

    % Testa todas as possibilidades...
    camisa(Camisa1), camisa(Camisa2), camisa(Camisa3), camisa(Camisa4), camisa(Camisa5),
    todosDiferentes([Camisa1, Camisa2, Camisa3, Camisa4, Camisa5]),

    nome(Nome1), nome(Nome2), nome(Nome3), nome(Nome4), nome(Nome5),
    todosDiferentes([Nome1, Nome2, Nome3, Nome4, Nome5]),

    experimentou(Experimentou1), experimentou(Experimentou2), experimentou(Experimentou3), experimentou(Experimentou4), experimentou(Experimentou5),
    todosDiferentes([Experimentou1, Experimentou2, Experimentou3, Experimentou4, Experimentou5]),

    comprou(Comprou1), comprou(Comprou2), comprou(Comprou3), comprou(Comprou4), comprou(Comprou5),
    todosDiferentes([Comprou1, Comprou2, Comprou3, Comprou4, Comprou5]),

    gastou(Gastou1), gastou(Gastou2), gastou(Gastou3), gastou(Gastou4), gastou(Gastou5),
    todosDiferentes([Gastou1, Gastou2, Gastou3, Gastou4, Gastou5]),

    veio_com(VeioCom1), veio_com(VeioCom2), veio_com(VeioCom3), veio_com(VeioCom4), veio_com(VeioCom5),
    todosDiferentes([VeioCom1, VeioCom2, VeioCom3, VeioCom4, VeioCom5]).
