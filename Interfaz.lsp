;-----------------------------------------------------
; INTERFAZ DE USUARIO PARA LA COMPRA DE PRODUCTOS
;-----------------------------------------------------

;-----------------------------------------------------
; Pograma principal que llama a todas las funciones
; necesarias.
;-----------------------------------------------------
(defun interfaz()
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
	(paralelepipedo 5 165 430 325)
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
	(paralelepipedoRelleno 0 0 0 5 124 635 162)
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
		(escribir 23 1 "PRODUCTO A SELECCIONAR:")
		(goto-xy 25 23)
		(setq producto (read)) 
		(escribir 23 1 "PRODUCTO SELECCIONADO:")
		(goto-xy 25 21)
		(princ producto)
		(borrar 23 21 20)
		(escribir 23 1 "UNIDADES:")
		(goto-xy 25 23)
		(setq unidades (read))
		(escribir 23 1 "UNIDADES:")
		(goto-xy 25 23)
		(princ unidades)
		(borrar 23 21 20)
		(escribir 23 1 "CONTINUAR (NO->1):")
		(goto-xy 25 23)
		(setq salir (read))
		(borrar 23 21 20)
	)
	(cls)
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
; Funciones para pintar la estética de la pantalla.
;-----------------------------------------------------
(defun paralelepipedo (x1 y1 x2 y2)
	(move x1 y1)
	(draw x1 y2 x2 y2 x2 y1 x1 y1)
)

(defun paralelepipedoRelleno (r g b x1 y1 x2 y2)
	(color r g b)
	(paralelepipedo x1 y1 x2 y2)
	(dotimes (i (- y2 y1))
		(move x1 (+ i y1))
		(draw x2 (+ i y1))
	)
	(color 0 0 0)
)
