;-----------------------------------------------------
; INTERFAZ DE USUARIO PARA LA COMPRA DE PRODUCTOS
;-----------------------------------------------------

; Lista de productos
(setq productos '(("Producto 1" 10) ("Producto 2" 20) ("Producto 3" 15) ("Producto 4" 12) ("Producto 5" 4) ("Producto 6" 37) ("Producto 7" 3) ("Producto 8" 8) 
("Producto 9" 124) ("Producto 10" 13) ("Producto 11" 18) ("Producto 12" 17) ("Producto 13" 3) ("Producto 14" 26) ("Producto 15" 35) ("Producto 16" 23) ("Producto 17" 1) 
("Producto 18" 8) ("Producto 19" 12) ("Producto 20" 4) ))

;-----------------------------------------------------
; Pograma principal que llama a todas las funciones
; necesarias.
;-----------------------------------------------------
(defun inicio()
	(cls)
	(ventanaProductos)
	(imagenProductos)
	(infoPedido)
	(detalleProductos)
	(precioPedido)
	(ventanaComunicacion)
)


;----------------------------------------------------
;  
;------------------------------------------------------
(defun ventanaProductos()
	(paralelepipedoRelleno 0 0 0 5 330 430 365)
	(visualizarpalabra "PRODUCTOS" 80 336 2 10)
	(paralelepipedo 5 162 430 325)
	(imprimirProductos)
)

;----------------------------------------------------
;  
;------------------------------------------------------
(defun imagenProductos()
	(visualizador "imagenes/LogoPractica.bmp" 435 165 200)	
)


;----------------------------------------------------
;  
;------------------------------------------------------
(defun infoPedido()
	(paralelepipedoRelleno 0 0 0 5 124 635 160)
)

;----------------------------------------------------
;  
;------------------------------------------------------
(defun detalleProductos()
	(paralelepipedo 5 38 635 121)
)

;----------------------------------------------------
;  
;------------------------------------------------------
(defun precioPedido()
	(paralelepipedoRelleno 0 0 0 325 3 635 35)
	(visualizarpalabra "TOTAL" 332 10 2 5)
	(visualizarpalabra "0000.00" 462 10 2 5)
)

;----------------------------------------------------
;  
;------------------------------------------------------
(defun ventanaComunicacion()
	(menu)
)


;----------------------------------------------------
;  POSIBILITA UN �REA DE COMUNICACI�N CON EL PROGRAMA
;  A TRAV�S DEL CUAL EL USUARIO PUEDE INTRODUCIR DATOS
;------------------------------------------------------

(defun menu ()
	(setq salir 0)
	(loop 
		(if (= salir 1) (return))
		(rectangulo 5 3 320 32) 
		(escribir 23 1 "[] NUMERO DE PRODUCTO:")
		(goto-xy 25 23)
		(setq producto (read)) 
		(borrar 23 21 20)
		(escribir 23 1 "[] UNIDADES PRODUCTO 1:")
		(goto-xy 27 23)
		(setq unidades (read))
		(borrar 23 21 20)
		(escribir 23 1 "[] 150 DE PRODUCTO 1 (S/N):")
		(goto-xy 32 23)
		(setq unidades (read))
		(borrar 23 21 20)
		(escribir 23 1 "[] CONTINUAR PEDIDO (S/N):")
		(goto-xy 30 23)
		(setq salir (read))
		(borrar 23 21 20)
	)
)


; visualiza en pantalla un rent�ngulo
(defun rectangulo (x y dimx dimy)
	(move x y)
	(draw x (+ y dimy) (+ x dimx) (+ y dimy) (+ x dimx) y x y)
)

;borra a partir de la (linea,columna) dada el n�mero de columnas indicada
;de la pantalla en modo texto
(defun borrar (linea columna numcolumnas)
	(goto-xy columna linea)
	(dotimes (i numcolumnas)
		(princ " ")
		(goto-xy (+ i columna) linea)
	)
)

;visualiza el texto dado en la (linea,columna)dada de la pantalla en modo texto
(defun escribir (linea columna TEXTO)
	(goto-xy columna linea)
	(princ TEXTO)
)





;-----------------------------------------------------
; Función que visualiza en pantalla la imagen dada por
; parámetro a partir de la coordenada (x,y) y de
; resolución dimension x dimension
;-----------------------------------------------------
(defun VISUALIZADOR (imagen x y dimension)
	(setq fichero (open imagen :direction :input 
	:element-type 'unsigned-byte))

    ;Lectura de la cabecera del archivo .bmp (54 bytes)
    (dotimes (i 54)
        (read-byte fichero nil)
    )

	(setq pixel 1)
	(setq R 0 G 0 B 0)
	(setq x1 x)
	(move x1 y)
	(loop 
		(setq B (read-byte fichero nil))
		(setq G (read-byte fichero nil))
		(setq R (read-byte fichero nil))
		(if (null B) (return ()) )
		(if (null G) (return ()) )
		(if (null R) (return ()) )
		(color R G B)
		(draw (+ 1 x1) y)
		(setq pixel  (+ pixel  1))
		(setq x1 (+ x1 1))
		(cond  ((> pixel  dimension) (setq pixel  1) (setq x1 x) (setq y (+ y 1)) ) )
		(move x1 y)
	) 
	(color 0 0 0)	
        (close fichero)
)


;-----------------------------------------------------
; Función que dada una palabra la visualiza,
; letra a letra, a partir
; de la coordenada (x,y) con imágenes del tipo dado
; y con un espaciado dado entre ellas.
; tipo 1 --> ".img" (52 píxeles x 52 píxeles)
; tipo 2 --> "_NB.img" (20 píxeles x 20 píxeles)
;-----------------------------------------------------

(defun visualizarpalabra (palabra x y tipo espaciado)
    (dotimes (i (length palabra))
	(VisualizarLetra (string (aref palabra i)) x y tipo)
	(if (= tipo 1) (setq x (+ 52 x espaciado)) (setq x (+ 20 x espaciado))   ) 
    )
)

;-----------------------------------------------------
; Función que dada una letra visualiza dicha letra
; utilizando la imagen del tipo dado
; tipo 1 --> ".img" (52 píxeles x 52 píxeles)
; tipo 2 --> "_NB.img" (20 píxeles x 20 píxeles)
;-----------------------------------------------------
(defun VisualizarLetra (letra x y tipo)
    (cond  ((= tipo 1) 
            (VISUALIZADOR (concatenate 'string "imagenes/" letra ".bmp") x y 52)
           )
	   ((= tipo 2) 
            (VISUALIZADOR (concatenate 'string "imagenes/" letra "_NB.bmp") x y 20)
           )
    )
)

		
;-----------------------------------------------------
; Funciones para pintar paralelepipedos sin relleno
;-----------------------------------------------------
(defun paralelepipedo (x1 y1 x2 y2)
	(move x1 y1)
	(draw x1 y2 x2 y2 x2 y1 x1 y1)
)

;-----------------------------------------------------
; Funciones para pintar paralelepipedos con relleno
;-----------------------------------------------------
(defun paralelepipedoRelleno (r g b x1 y1 x2 y2)
	(color r g b)
	(paralelepipedo x1 y1 x2 y2)
	(dotimes (i (- y2 y1))
		(move x1 (+ i y1))
		(draw x2 (+ i y1))
	)
	(color 0 0 0)
)


;-----------------------------------------------------
; Funciones para pintar paralelepipedos con relleno
;-----------------------------------------------------
(defun imprimirProductos()
	; Guardamos los productos en una lista auxiliar
	(setq auxiliar productos)
	; Contador fila
	(setq i 0)
	; Contador producto
	(setq cont 1)

	(loop
		; Si la lista está vacia salimos
		(if (null (car auxiliar)) (return t) nil)
		; Si ya no caben en la pantalla paramos
		(if (> (+ 4 i) 20) (return t) nil)
		; Tomamos el producto actual
		(setq producto (car auxiliar))


		; Imprimimos por pantalla (lado izquierdo)
		(goto-xy 1 (+ 4 i))
		(princ cont)
		(goto-xy 5 (+ 4 i))
		(princ (car producto))
		(goto-xy 18 (+ 4 i))
		(princ (car (cdr Producto)))

		; Si la lista está vacia salimos
		(if (null (car auxiliar)) (return t) nil)
		; Tomamos el siguiente producto
		(setq auxiliar (cdr auxiliar))
		; Tomamos el producto actual
		(setq producto (car auxiliar))
		; Aumentamos el contador de productos
		(setq cont (+ cont 1))

		; Imprimimos por pantalla (lado derecho)
		(goto-xy 30 (+ 4 i))
		(princ cont)
		(goto-xy 34 (+ 4 i))
		(princ (car producto))		
		(goto-xy 47 (+ 4 i))
		(princ (car (cdr Producto)))

		; Tomamos el siguiente producto
		(setq auxiliar (cdr auxiliar))
		; Aumentamos el contador
		(setq i (+ i 1))
		; Aumentamos el contador de productos
		(setq cont (+ cont 1))
	)


; producto 1
	;(goto-xy 1 4)
	;(princ "01. ")
	;(goto-xy 5 4)
	;(princ "Producto 1")
	;(goto-xy 18 4)
	;(princ "10,00")
; producto 2
	;(goto-xy 30 4)
	;(princ "02. ")
	;(goto-xy 34 4)
	;(princ "Producto 1")
	;(goto-xy 47 4)
	;(princ "100,00")

)

(defun maximo (l)
	(cond ((null (cdr l)) (car l))
		((>= (car l) (maximo (cdr l))) (car l))
		(t (maximo (cdr l)))
	)
)
