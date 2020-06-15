;-----------------------------------------------------
; INTERFAZ DE USUARIO PARA LA COMPRA DE PRODUCTOS
;-----------------------------------------------------

;-----------------------------------------------------
;	VARIABLES GLOBALES
;-----------------------------------------------------

; Lista de productos
(setq productos '(("agua" 10) ("boligrafo" 20) ("cartulina" 15) ("cereza" 12) ("cesta" 4) ("coca cola" 37) ("silla" 3) ("fanta limon" 8) 
("fanta" 124) ("lapiz" 13) ("limon" 18) ("mascarilla" 17) ("naranja" 3) ("pan" 26) ("pelota" 35) ("raqueta" 23) ("tuerca" 1) 
("vendajes" 8) ("zumo" 12) ("tiritas" 4) ))

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
	(precioPedido 0)
	(ventanaComunicacion)
)


;----------------------------------------------------
;  Ventana que muestra los productos
;------------------------------------------------------
(defun ventanaProductos()
	(paralelepipedoRelleno 0 0 0 5 330 430 365)
	(visualizarpalabra "PRODUCTOS" 80 336 2 10)
	(paralelepipedo 5 162 430 325)
	(imprimirProductos)
)

;----------------------------------------------------
;  Ventana que muestra la imagen del producto 
;  seleccionado.
;------------------------------------------------------
(defun imagenProductos()
	(visualizador "imagenes/LogoPractica.bmp" 435 165 200)	
)


;----------------------------------------------------
;  Ventana que muestra cual es el pedido actual
;------------------------------------------------------
(defun infoPedido()
	(paralelepipedoRelleno 0 0 0 5 124 635 160)
)

;----------------------------------------------------
;  Ventana que muestra información sobre los productos
;  de nuestro pedido
;------------------------------------------------------
(defun detalleProductos()
	(paralelepipedo 5 38 635 121)
)

;----------------------------------------------------
;  Ventana que muestra el precio total del pedido
;  actual
;------------------------------------------------------
(defun precioPedido( precio )
	(paralelepipedoRelleno 0 0 0 325 3 635 35)
	(visualizarpalabra (concatenate 'string "TOTAL#" (princ-to-string precio) "#####") 337 10 2 5)
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
	; Inicializamos la x y la y que nos servirán para
	; pintar la pantalla
	(setq x 2 y 17)
	; Inicializamos un contador de pedidos
	(setq contadorPedido 1)
	; Acumulador de precio pedido
	(setq precioTotal 0)
	; Inicializamos una lista que guardará nuestro pedido
	; pedido -> [nombre | precio | cantidad]
	(setq pedido "centinela")	; uso centinela unicamente para inicializar 
	; la lista, no hace falta un centinela porque se puede recorrer con dotimes
	(setq continuar "N")
	(loop 
		(if (= salir 1) (return))
		(rectangulo 5 3 320 32) 
		(loop
			(if (string-equal continuar "N") () (return))
			(escribirMenu "INSERTE UN NUMERO DE PEDIDO: ")
			(setq npedido (read))
			; Chequeamos que introduce un numero
			(if (typep npedido 'integer) 
				(return)
				()
			)
		)
		; Pintamos por pantalla el numero de producto
		(visualizarpalabra (concatenate 'string "PEDIDO#" (princ-to-string npedido) "################") 14 132 2 6)
		; Preguntamos el producto que desea
		(setq seleccion 0)
		(loop
			(escribirMenu "[] NUMERO DE PRODUCTO: ")
			(setq seleccion (read)) 
			; Si no es un integer en el rango correcto no vale
			(if (typep seleccion 'integer) 
				(if (AND (<= seleccion 20) (> seleccion 0)) (return))	
				()
			)
		)
		; Preguntamos cuantas unidades de dicho producto
		(setq unidades 0)
		(loop
			(escribirMenu "[] UNIDADES PRODUCTO 1: ")
			(setq unidades (read)) 
			; Si no es un integer en el rango correcto no vale
			(if (typep unidades 'integer) 
				(return)	
				()
			)
		)
		; Buscamos el producto seleccionado
		(setq producto (encontrarProducto seleccion))
		(escribirMenu (concatenate 'string "[] " (princ-to-string unidades) " DE " (car producto) " (S/N): "))
		(setq confirmar (read))
		; Este loop se utiliza como si fuese un if
		(loop
			(if (string-equal confirmar "S") () (return))
			; Mostramos imagen del producto
			(visualizador (concatenate 'string "productos/" (car producto) ".bmp") 435 165 200)
			; Añadimos el precio al total
			(setq precioTotal (+ precioTotal (* (car (cdr producto)) unidades)))
			(precioPedido precioTotal)
			; Almacenamos los pedidos de la misma manera que los imprimiremos en el fichero
			; \t producto \t unidades \t precio
			(setq elementopedido (concatenate 'string "\t  " (rellenarString (car producto) 15) 
			"\t" (princ-to-string unidades) "\t\t  " (princ-to-string (* (car (cdr producto)) unidades))))
			; Añadimos al array de pedidos
			(setq pedido (cons elementopedido pedido))
			; Escribimos el producto en la pantalla
			(escribirDetalleProductos x y producto unidades)
			; Con este if controlamos que los productos no se pinten 
			; unos encima de otros
			(if(> x 50) (setq x 2 y (+ 1 y)) (setq x (+ x 25)))
			(escribirMenu "[] CONTINUAR PEDIDO (S/N): ")
			(setq continuar (read))
			(if (string-equal continuar "n") () (return))
			(setq x 2)
			(borrarVentanaProductos)
			(añadirAlCarrito (reverse pedido) contadorPedido precioTotal)
			; Precio pedido vuelve a 0
			(setq precioTotal 0)
			(precioPedido precioTotal)
			; Aumentamos el contador de pedido
			(setq contadorPedido (+ contadorPedido 1))
			; El pedido vuelve a estar vacio
			(setq pedido "centinela")
			; Volvemos a la foto inicial
			(visualizador "imagenes/LogoPractica.bmp" 435 165 200)	
			(return t)
		)
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
		(if (null (car auxiliar)) (return) ())
		; Si ya no caben en la pantalla paramos
		(if (> (+ 4 i) 20) (return) ())
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
		(if (null (car auxiliar)) (return) nil)
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

)

;-----------------------------------------------------
; Devuelve el producto de la posición seleccionada
;-----------------------------------------------------
(defun encontrarProducto (posicion)
	; volcamos los productos en una lista auxiliar
	(setq auxiliar productos)
	; Contador
	(setq contador 0)

	(loop
		(if (>= contador posicion) (return producto) ())
		; Guardamos el valor actual
		(setq producto (car auxiliar))
		; Tomamos el siguiente valor
		(setq auxiliar (cdr auxiliar))
		; Aumentamos el contador
		(setq contador (+ contador 1))
	)
)

;-----------------------------------------------------
; Escribe el detalle del producto seleccionado
;-----------------------------------------------------
(defun escribirDetalleProductos (x y producto cantidad)
	(goto-xy x y)
	(princ (concatenate 'string "[" (car producto) "/" (princ-to-string cantidad) "/" (princ-to-string (*(car (cdr producto)) cantidad)) " euros]"))
)

;-----------------------------------------------------
; Borra el detalle de los productos seleccionados.
;-----------------------------------------------------
(defun borrarVentanaProductos ()
	(borrar 17 1 78)
	(borrar 18 1 78)
	(borrar 19 1 78)
	(borrar 20 1 78)
	(borrar 21 1 78)
)

;-----------------------------------------------------
; Escribimos en un fichero el detalle del producto
;-----------------------------------------------------
(defun añadirAlCarrito (pedido nPedido pTotal)
    (setq file (open (concatenate 'string "pedido" (princ-to-string nPedido) ".txt") :direction :output))

    ;Primera línea.
	(princ (concatenate 'string "PEDIDO " (princ-to-string nPedido)) file)
	(write-char #\newline file)

	;Segunda línea.
    (princ "\t PRODUCTOS \t   UNIDADES\t\tIMPORTE" file)
	(write-char #\newline file)

	(dolist (i pedido)
		(princ i file)
		(write-char #\newline file)    
	)

	(write-char #\newline file)    

    ;Última línea.
	(princ (concatenate 'string "TOTAL PEDIDO\t" (princ-to-string pTotal) " euros") file)

)

;-----------------------------------------------------
; Rellena la 'palabra' hasta 'i' caracteres con 
; espacios en blanco
;-----------------------------------------------------
(defun rellenarString (palabra i)
	(setq iterador (- i (length palabra)))
	(dotimes (i iterador)
			(setq palabra (concatenate 'string palabra " "))
	)
    (return-from rellenarString palabra)
)

;-----------------------------------------------------
; Borra el texto anterior y escribe el texto 
; indicado en el menu
;-----------------------------------------------------
(defun escribirMenu (texto)
		(borrar 23 21 20)
		(escribir 23 1 texto)
)
