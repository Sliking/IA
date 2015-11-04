; Procuras
; Parte 2
;segunda parte

(defun pppa (prob)
	(let ((sucessores ()) (estado-actual (problema-estado-inicial prob)))
		(setf sucessores (gera-sucessores (funcall (problema-accoes prob) estado-actual) estado-actual prob))
		(loop
			(if (null sucessores)
				(return-from pppa nil)
				(progn
					(setq estado-actual (first sucessores))
					(if (funcall (problema-solucao? prob) estado-actual)
						(return-from pppa (no-estado estado-actual))
						(progn
							(setf sucessores (append (gera-sucessores (funcall (problema-accoes prob) estado-actual) estado-actual prob) (rest sucessores)))
						)
					)
				)	
			)
		)
	)
)




(defun pA*a (prob heuristica)
	(let ( (estado-actual (problema-estado-inicial prob)) (sucessores ()))
		(setf (no-h estado-actual) (funcall heuristica (no-estado estado-actual) (no-tarefas estado-actual)))
		(setf sucessores (gera-sucessores2 (funcall (problema-accoes prob) estado-actual) estado-actual prob heuristica))
		(loop
			(if (null sucessores)
				(return-from pA*a nil)
				(progn
					(setf estado-actual (first sucessores))
					(if (funcall (problema-solucao? prob) estado-actual)
						(return-from pA*a (no-estado estado-actual))
						(setf sucessores (coloca-por-ordem-A (rest sucessores) (gera-sucessores2 (funcall (problema-accoes prob) estado-actual) estado-actual prob heuristica)))
					)
				)
			)
		)
	)
)
|#