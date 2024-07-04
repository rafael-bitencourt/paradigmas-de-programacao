;; Define uma função para verificar se um número k pode ser colocado em uma posição específica (linha, coluna) no tabuleiro.
(defun eh-posicao-valida (tabuleiro regioes linha coluna k)
  (let* ((regiao (nth coluna (nth linha regioes)))  ;; Encontra a região da célula atual.
         (valores-regiao (loop for i from 0 below (length tabuleiro)  ;; Coleta valores da mesma região para checagem.
                              append (loop for j from 0 below (length tabuleiro)
                                           when (= (nth j (nth i regioes)) regiao)
                                           collect (nth j (nth i tabuleiro)))))
         (vizinhos (list (cons (1- linha) coluna) (cons (1+ linha) coluna) (cons linha (1- coluna)) (cons linha (1+ coluna))))  ;; Determina os vizinhos diretos.
         (valores-vizinhos (loop for (i . j) in vizinhos  ;; Coleta valores dos vizinhos para checagem.
                                 when (and (>= i 0) (< i (length tabuleiro))
                                           (>= j 0) (< j (length tabuleiro)))
                                 collect (nth j (nth i tabuleiro)))))
    (and (not (member k valores-regiao))  ;; Verifica se k não está na região.
         (<= k (length valores-regiao))  ;; Verifica se k é menor ou igual ao número de células na região.
         (not (member k valores-vizinhos))  ;; Verifica se k não está entre os vizinhos.
         (or (= linha 0)  ;; Condições adicionais para as bordas e regiões acima e abaixo.
             (/= (nth coluna (nth (1- linha) regioes)) regiao)
             (>= (nth coluna (nth (1- linha) tabuleiro)) k))
         (or (= linha (1- (length tabuleiro)))
             (/= (nth coluna (nth (1+ linha) regioes)) regiao)
             (<= (nth coluna (nth (1+ linha) tabuleiro)) k)))))


;; Define uma função para resolver o tabuleiro começando da posição (linha, coluna).
(defun resolve (tabuleiro regioes linha coluna)
  (cond ((= linha (length tabuleiro)) tabuleiro)  ;; Caso base: se toda a linha foi processada, retorna o tabuleiro.
        ((= coluna (length (car tabuleiro))) (resolve tabuleiro regioes (1+ linha) 0))  ;; Se alcançou o fim de uma linha, passa para a próxima.
        ((/= (nth coluna (nth linha tabuleiro)) 0) (resolve tabuleiro regioes linha (1+ coluna)))  ;; Se a célula atual já está preenchida, passa para a próxima coluna.
        (t (tenta-colocacoes tabuleiro regioes linha coluna (loop for i from 1 to (length tabuleiro) collect i)))))  ;; Caso contrário, tenta colocar um valor válido.


;; Tenta diferentes valores em uma célula específica do tabuleiro.
(defun tenta-colocacoes (tabuleiro regioes linha coluna ks)
  (if (null ks)
      nil  ;; Se não há mais valores para tentar, retorna nil.
      (let ((k (car ks)))  ;; Pega o primeiro valor da lista para tentar.
        (if (eh-posicao-valida tabuleiro regioes linha coluna k)
            (let ((solucao (resolve (substitui-2d tabuleiro linha coluna k) regioes linha (1+ coluna))))  ;; Se o valor é válido, tenta resolver o tabuleiro com esse novo estado.
              (if solucao
                  solucao  ;; Se encontrou solução, retorna.
                  (tenta-colocacoes tabuleiro regioes linha coluna (cdr ks))))  ;; Caso contrário, tenta o próximo valor.
            (tenta-colocacoes tabuleiro regioes linha coluna (cdr ks))))))  ;; Se o valor não é válido, tenta o próximo.


;; Função auxiliar para substituir o valor em uma célula específica do tabuleiro.
(defun substitui-2d (tabuleiro linha coluna k)
  (let* ((antes (subseq tabuleiro 0 linha))  ;; Copia a parte do tabuleiro antes da linha atual.
         (x (nth linha tabuleiro))
         (depois (subseq tabuleiro (1+ linha)))  ;; Copia a parte do tabuleiro após a linha atual.
         (antes2 (subseq x 0 coluna))  ;; Copia a parte da linha antes da coluna atual.
         (depois2 (subseq x (1+ coluna))))  ;; Copia a parte da linha após a coluna atual.
    (append antes (list (append antes2 (list k) depois2)) depois)))  ;; Retorna o novo estado do tabuleiro com o valor substituído.


;; Função para imprimir o tabuleiro.
(defun imprime-tabuleiro (tabuleiro)
  (dolist (linha tabuleiro)
    (print linha)))  ;; Imprime cada linha do tabuleiro.


;; Função principal que prepara o tabuleiro e tenta resolver.
(defun principal ()
  (let ((tabuleiro '((0 0 0 0 0 0 0 0)
                     (0 1 3 0 0 0 0 0)
                     (0 0 0 0 0 3 0 0)
                     (0 0 3 0 0 0 0 0)
                     (0 5 0 3 0 0 0 0)
                     (0 2 0 0 0 0 0 0)
                     (0 0 0 0 0 0 3 0)
                     (0 0 5 3 0 0 0 0)))
        
         (regioes '((1 1 2 2 3 4 5 5)
                    (1 1 6 2 7 4 4 5)
                    (6 6 6 8 7 9 10 10)
                    (11 11 11 8 7 9 9 10)
                    (12 8 8 8 8 9 9 10)
                    (12 13 14 14 14 15 16 10)
                    (13 13 13 13 17 15 15 15)
                    (18 17 17 17 17 15 19 19))))

        

    (let ((tabuleiro-resolvido (resolve tabuleiro regioes 0 0)))  ;; Chama a função de resolução no estado inicial.
      (if tabuleiro-resolvido
          (imprime-tabuleiro tabuleiro-resolvido)  ;; Se houver solução, imprime o tabuleiro resolvido.
          (format t "Nenhuma solução encontrada.")))))  ;; Caso contrário, informa que não encontrou solução.

(principal)
