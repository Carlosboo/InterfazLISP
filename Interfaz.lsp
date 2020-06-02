;-----------------------------------------------------
; Función que visualiza en pantalla la imagen dada por
; parámetro a partir de la coordenada (x,y) y de
; resolución dimension x dimension
;-----------------------------------------------------
(defun VISUALIZADOR (imagen x y dimension)
	(setq fichero (open imagen :direction :input 
	:element-type 'unsigned-byte))
	(setq pixel 1)
	(setq R 0 G 0 B 0)
	(setq x1 x)
	(move x1 y)
	(princ "llega fichero")
	(setq sumatorio 0)
	(dotimes (i 54 sumatorio)
		(setq B (read-byte fichero nil))
		(setq sumatorio (+ sumatorio i))
	)
	(loop 
		(setq B (read-byte fichero nil))
		(setq G (read-byte fichero nil))
		(setq R (read-byte fichero nil))
		(if (null B) (return ()) )
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
    (cls)
    (dotimes (i (length palabra))
	(princ "ENTRA BUCLE")
	(VisualizarLetra (string (aref palabra i)) x y tipo)
	(princ "SALE VISUALIZAR")
	(if (= tipo 1) (setq x (+ 52 x espaciado)) (setq x (+ 20 x espaciado))   ) 
	(princ "SALE BUCLE")
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
            (VISUALIZADOR (concatenate 'string "IMAGENES_CARACTERES_BMP/" letra ".bmp") x y 52)
           )
	   ((= tipo 2) 
            (VISUALIZADOR (concatenate 'string "IMAGENES_CARACTERES_BMP/" letra "_BN.bmp") x y 20)
           )
    )
)
		
