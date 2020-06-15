;-----------------------------------------------------
; EJERCICIOS LISP COMENTADOS
;-----------------------------------------------------

;-----------------------------------------------------
; Escribir una función recursiva impares que dada una lista de números
; retorna una lista con todos los números pares eliminados.
;
; Ejemplo: (impares '(3 1 8 7 4 10)) → (3 1 7)
;-----------------------------------------------------
(defun impares (lista)
     (cond
        ; Si la lista es nula devolver nulo
        ((null lista) nil)
        ; Comprobar si el first de la lista es impar
        ((oddp (car lista))
            ; Impar -> Añade el elemento al inicio de la lista
            (cons (car lista) (impares (cdr lista)))
        )
        ; Recursividad con el resto de la lista
        (t (impares (cdr lista)))
     )
)


;-----------------------------------------------------
; EJERCICIO 2 - julio 2019
;
; Unir dos listas sin append, simular append
;
; Ejemplo: (unir '(3 1 8) '(7 4 10)) → (3 1 8 7 4 10)
;-----------------------------------------------------
(defun unir (lista1 lista2)
    (cond
        ; Caso básico
        ((null lista1) lista2)
        ; True
        (t (cons (car lista1) (unir (cdr lista1) lista2)))
    )
)

;----------------------------------------------------
;  Interseccion de dos listas
;------------------------------------------------------
(defun interseccion (lista1 lista2)
    (cond ; Lista de condiciones
        ; Condicion
        ((or (null lista1) (null lista2)) ())
        ; True
        ((member (car lista1) lista2) (cons (car lista1) (interseccion (cdr lista1) lista2)))
        ; Siempre
        (t (interseccion (cdr lista1) lista2))
    )
)