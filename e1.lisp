; Grupo 33
; Luis Neto 78403
; Miguel Pinto 79060

;(load "utils.fas")

;(load (compile-file "utils.lisp"))

; Tipo Accao

; Representa a accao de um jogador e um tuplo onde
; a primeiro posicao e um numero de um coluna e o segundo elemento
; um array bidimensional que tem o formato da peca


(defun cria-Accao(coluna peca)
	(cons  coluna peca))

(defun accao-coluna(accao)
	(car accao))

(defun accao-peca(accao)
	(cdr accao))

; Tipo Tabuleiro

; Representa o tabuleiro de jogo que tem dimensao 18*10,
; cada posicao do tabuleiro tem de estar preenchido ou nao
; cabe a nos a melhor representacao para este tipo

(defun cria-tabuleiro()
	 (make-array '(18 10)))

(defun copia-tabuleiro(tab)
	(let ((dims (make-array (array-dimensions tab))))
		(dotimes (x (array-dimension dims 0) dims)
			(dotimes (y (array-dimension dims 1))
				(setf (aref dims x y) (aref tab x y))))))


;(defun copia-tabuleiro(tab)
;	(let ((dims (array-dimensions tab)))
;		(adjust-array
;			(make-array dims :displaced-to tab) dims)))

(defun tabuleiro-preenchido-p(tabuleiro linha coluna)
	(not (equal (aref tabuleiro ( - 17 linha ) coluna ) nil)))
  
(defun tabuleiro-altura-coluna(tabuleiro coluna)
	(let ((max 0))
	(dotimes (line 17 max)
		(cond ((< line 0 ) (return-from tabuleiro-altura-coluna max))
			  ((tabuleiro-preenchido-p tabuleiro  line  coluna) (setf max (+ line 1) ))) ; em vez do setf -> (incf max)
	)))

(defun tabuleiro-linha-completa-p(tabuleiro linha) 
	(let ((n t))
	(dotimes(coluna 10)
			(setf n (and (tabuleiro-preenchido-p tabuleiro linha coluna) n)))
	n)) 

(defun tabuleiro-preenche!(tabuleiro linha coluna)
	(if(and (>= linha 0) (< linha 18) (>= coluna 0) (< coluna 10))
		(setf (aref tabuleiro ( - 17 linha)  coluna )t)
		(return-from tabuleiro-preenche! )))
	
(defun tabuleiro-remove-linha!(tabuleiro linha)
	(dotimes(l ( - 17 linha))
		(dotimes(coluna 9)
			(setf (aref tabuleiro ( - ( - 17 linha) l) coluna) 
				(aref tabuleiro ( - ( - ( - 17 linha) l) 1) coluna))))

		(dotimes(coluna 10)
				(setf (aref tabuleiro 0 coluna) nil)))
	

(defun tabuleiro-topo-preenchido-p(tabuleiro) 
	(dotimes(coluna 10)
			(if(tabuleiro-preenchido-p tabuleiro 17 coluna) 
				(return-from tabuleiro-topo-preenchido-p t)))
	nil)

(defun tabuleiros-iguais-p(tabuleiro1 tabuleiro2)
	(equalp tabuleiro1 tabuleiro2))

(defun tabuleiro->array(tab)
	(copia-tabuleiro tab))

(defun array->tabuleiro(tab)
	(copia-tabuleiro tab))

; Tipo Estado

; Representa o estado de um jogo de tetris
; Tem de ser obrigatoriamente uma estrutura 

;definicao de estrutura

(defstruct Estado 
	pontos 
	pecas-por-colocar
	pecas-colocadas
	tabuleiro)

(defun copia-estado(estado1)
	(make-estado 
		:pontos (estado-pontos estado1)
		:pecas-por-colocar (copy-list(estado-pecas-por-colocar estado1))
		:pecas-colocadas (copy-list(estado-pecas-colocadas estado1))
		:tabuleiro (copia-tabuleiro(estado-tabuleiro estado1))
		)
	)

(defun estados-iguais-p(estado1 estado2)
	(equalp estado1 estado2))

(defun estado-final-p(estado)
	(let ((lista_pecas (estado-pecas-por-colocar estado))
		  (tabuleiro (estado-tabuleiro estado)))
		(or(null lista_pecas)
			(tabuleiro-topo-preenchido-p tabuleiro))

		))

; Tipo Problema

; Representa um problema generico de procura
; Tem de ser obrigatoriamente uma estrutura

(defstruct Problema 
	estado-inicial 
	solucao 
	accoes 
	resultado 
	custo-caminho)

;Funcoes do problema de procura

(defun solucao(estado)
	(let ((lista_pecas (estado-pecas-por-colocar estado))
		  (tabuleiro (estado-tabuleiro estado)))
		(and(null lista_pecas)
			(not(tabuleiro-topo-preenchido-p tabuleiro)
				)
			)
		)
	)


; --> Possivel solucao para a funcao accoes
; Ir buscar as pecas por colocar de um dado estado, somar a coluna, onde se pretende por a peca, a 2a dimencao
; verificar se esta dentro do limite de jogo, parar de incluir accaos no vector de de output rodar a peca 
; fazer o mesmo proccedimento


;(defun accoes(estado))

;(defun resultado(estado accao))

;(defun qualidade(estado))

;(defun custo-oportunidade(estado))

    


;Funcoes auxiliares

(defun array-vazio(array)
	(dotimes(linha (car(array-dimensions array)))
		(dotimes(col (car(cdr(array-dimensions array))))
			(if(not(null (aref array linha col)))
				(return-from array-vazio nil))
			)
		)
	t)

(defun 2d-array-to-list (array)
  (loop for i below (array-dimension array 0)
        collect (loop for j below (array-dimension array 1)
                      collect (aref array i j))))

(defun ignore-value (x)
	(declare (ignore x))
	'ignore)
