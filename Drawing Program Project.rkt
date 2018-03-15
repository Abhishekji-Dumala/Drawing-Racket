#lang racket
(require (rename-in 2htdp/image (color? color-htdp?)))
(require file/gif)
(require lang/posn)
(require 2htdp/universe) 

(define-struct drawing [l1 l2 l3 l4 l5 l6 l7 l8 l9 l10 current-brush current-color current-layer view current-size])
(define-struct saving [l1 l2 l3 l4 l5 l6 l7 l8 l9 l10])
(define-struct gif [name l1 l2 l3 l4 l5 l6 l7 l8 l9 l10])
(define-struct png [name l1 l2 l3 l4 l5 l6 l7 l8 l9 l10])
(define-struct color-button [l1 l2 l3 l4 l5 l6 l7 l8 l9 l10 current-brush current-color current-layer view current-size])
(define-struct brush-button [l1 l2 l3 l4 l5 l6 l7 l8 l9 l10 current-brush current-color current-layer view current-size])

;---------------------------------------BUTTONS, OPTIONS, AND OTHER DEFINES---------------------------------------------------------------------------

(define INITIAL-STATE (make-drawing
                       empty
                       empty
                       empty
                       empty
                       empty
                       empty
                       empty
                       empty
                       empty
                       empty
                       "pen"
                       (color 0 191 255)
                       10
                       1
                       10))

(define TESTING-PNG (make-png ".png"
                              empty
                              empty
                              empty
                              empty
                              empty
                              empty
                              empty
                              empty
                              empty
                              empty))

;layer-menu :: size->image
;creates an icon for each layer number
(define (layer-menu n)
  (place-image (text (string-append "LAYER "
                                    (number->string n)) 15 "white")
               35 12
               (frame (rectangle 70 24 "solid" "dodgerblue"))))

(define LAYER-MENU (above (layer-menu 1)
                          (layer-menu 2)
                          (layer-menu 3)
                          (layer-menu 4)
                          (layer-menu 5)
                          (layer-menu 6)
                          (layer-menu 7)
                          (layer-menu 8)
                          (layer-menu 9)
                          (layer-menu 10)))

;color-option :: string->image
;creates a custom icon for each given color
(define (color-option c)
  (frame (place-image (square 15 255 c) 10 10
                      (place-image (text c 10 "white") (+ 25 (* .5 (image-width (text c 10 "white")))) 10
                                   (rectangle 120 20 255 "dodgerblue")))))

(define COLOR-MENU (frame (above (color-option "violet red")
                                 (color-option "red")
                                 (color-option "orange")
                                 (color-option "yellow")
                                 (color-option "green yellow")
                                 (color-option "lime green")
                                 (color-option "forest green")
                                 (color-option "RoyalBlue")
                                 (color-option "DodgerBlue")
                                 (color-option "DeepSkyBlue")
                                 (color-option "sky blue")
                                 (color-option "medium slate blue")
                                 (color-option "blue violet")
                                 (color-option "indigo")
                                 (color-option "black")
                                 (color-option "white"))))

;toolbar :: WorldState->Image
;for each given worldstate creates a toolbar also displaying the current brush-size
(define (toolbar ws)
         (overlay/align "left" "middle"
                               (beside (text "  save" 14 "white")
                                       (text "    colors" 14 "white")
                                       (text "    brushes" 14 "white")
                                       (text "    reset" 14 "white")
                                       (text (string-append "    size: " (number->string (cond
                                                                                           [(drawing? ws) (drawing-current-size ws)]
                                                                                           [(color-button? ws) (color-button-current-size ws)]
                                                                                           [(brush-button? ws) (brush-button-current-size ws)]))) 14 "white")
                                       (text (string-append "    view: " (number->string (cond
                                                                                           [(drawing? ws) (drawing-view ws)]
                                                                                           [(color-button? ws) (color-button-view ws)]
                                                                                           [(brush-button? ws) (brush-button-view ws)]))) 14 "white"))
                               (frame (rectangle 1200 20 255 "dodgerblue"))))

(define TOOLBAR
         (overlay/align "left" "middle"
                               (beside (text "  save" 14 "white")
                                       (text "    colors" 14 "white")
                                       (text "    brushes" 14 "white"))
                               (frame (rectangle 1200 20 255 "dodgerblue"))))

;name-tab :: string->image
;creates a custom icon for each input string
(define (make-tab n)
  (frame (place-image (text n 12 "white") (+ 5 (* .5 (image-width (text n 12 "white")))) 9
                      (rectangle 120 20 255 "dodgerblue"))))

(define SAVE-OPTIONS (frame (above (make-tab "png")
                                   (make-tab "gif"))))

(define BRUSH-OPTION (frame (above (make-tab "pen")
                                   (make-tab "paint")
                                   (make-tab "airbrush")
                                   (make-tab "changing")
                                   (make-tab "eraser"))))
               

;-------------------------------------BRUSH-STRUCT RELATED FUNCTIONS----------------------------------------------------------------------------------

; a Brush is one of:
;  - "pen"
;  - "paint"
;  - "airbrush"
;  - "changing"
;  - "eraser"

; brush->opacity :: brush->opacity
; takes in a type of brush and current color and returns an opacity for that brush
(define (brush->opacity b)
  (cond
    [(string=? b "pen") 255]
    [(string=? b "paint") 100]
    [(string=? b "airbrush") 50]
    [(string=? b "changing") (random 255)]
    [(string=? b "eraser") 255]))

; brush struct
(define (make-brush type color size x y vel-x vel-y)
  (lambda (fieldname . extras)
    (cond
      [(symbol=? fieldname 'render) (if (string=? type "eraser")
                                        (circle size (brush->opacity type) "white")
                                        (circle size (brush->opacity type) color))]
      [(symbol=? fieldname 'pos) (make-posn x y)]
      [(symbol=? fieldname 'update) (set! size (if (string=? type "changing")
                                                   (random 200)
                                                   size))
                                    (set! x (+ x vel-x))
                                    (set! y (+ y vel-y))
                                    (set! color (if (string=? type "changing")
                                                    (make-color (random 255) (random 255) (random 255))
                                                    color))]
      [(symbol=? fieldname 'change-velx) (set! vel-x (first extras))]
      [(symbol=? fieldname 'change-vely) (set! vel-y (first extras))])))

;-----------------------------------------------RENDERING--------------------------------------------------------------------------------------------------

;-------------------------------------------COMPILING LAYERS-------------------------------------------------------------------------------------------------------

;compile-layers-X :: worldtstate(X)->layer<brush-points>
;takes in the layers from a worldstateX and compiles into one big layer
(define (compile-layers-d ws)
  (append
   (drawing-l1 ws)
   (drawing-l2 ws)
   (drawing-l3 ws)
   (drawing-l4 ws)
   (drawing-l5 ws)
   (drawing-l6 ws)
   (drawing-l7 ws)
   (drawing-l8 ws)
   (drawing-l9 ws)
   (drawing-l10 ws)))

(define (compile-layers-png ws)
  (append
   (png-l1 ws)
   (png-l2 ws)
   (png-l3 ws)
   (png-l4 ws)
   (png-l5 ws)
   (png-l6 ws)
   (png-l7 ws)
   (png-l8 ws)
   (png-l9 ws)
   (png-l10 ws)))

(define (compile-layers-cb ws)
  (append
   (color-button-l1 ws)
   (color-button-l2 ws)
   (color-button-l3 ws)
   (color-button-l4 ws)
   (color-button-l5 ws)
   (color-button-l6 ws)
   (color-button-l7 ws)
   (color-button-l8 ws)
   (color-button-l9 ws)
   (color-button-l10 ws)))

(define (compile-layers-b ws)
  (append
   (brush-button-l1 ws)
   (brush-button-l2 ws)
   (brush-button-l3 ws)
   (brush-button-l4 ws)
   (brush-button-l5 ws)
   (brush-button-l6 ws)
   (brush-button-l7 ws)
   (brush-button-l8 ws)
   (brush-button-l9 ws)
   (brush-button-l10 ws)))

(define (compile-layers-s ws)
  (append
   (saving-l1 ws)
   (saving-l2 ws)
   (saving-l3 ws)
   (saving-l4 ws)
   (saving-l5 ws)
   (saving-l6 ws)
   (saving-l7 ws)
   (saving-l8 ws)
   (saving-l9 ws)
   (saving-l10 ws)))

; render-layer :: List<Brush-Points> -> Image
; creates an image with each of the visual representations of brushpoints at their given coordinates
(define (render-layer l)
  (place-images
       (map (lambda (brush-point) (brush-point 'render)) l)
       (map (lambda (brush-point) (brush-point 'pos)) l)
       (empty-scene 1200 700)))

; render-background :: drawing->image
; creates an image with each of the images of the layers layered on top of each other
(define (render-background ws)
  (place-images
       (map (lambda (brush-point) (brush-point 'render)) (compile-layers-d ws))
       (map (lambda (brush-point) (brush-point 'pos)) (compile-layers-d ws))
       (empty-scene 1200 700)))

; render-background-png :: drawing->image
; creates an image with each of the images of the layers layered on top of each other
(define (render-background-png ws)
  (place-images
       (map (lambda (brush-point) (brush-point 'render)) (compile-layers-png ws))
       (map (lambda (brush-point) (brush-point 'pos)) (compile-layers-png ws))
       (empty-scene 1200 700)))

; render-background-cb :: color-button->image
; creates an image with each of the images of the layers layered on top of each other
(define (render-background-cb ws)
  (place-images
       (map (lambda (brush-point) (brush-point 'render)) (compile-layers-cb ws))
       (map (lambda (brush-point) (brush-point 'pos)) (compile-layers-cb ws))
       (empty-scene 1200 700)))

; render-background-b :: brush-button->image
; creates an image with each of the images of the layers layered on top of each other
(define (render-background-b ws)
  (place-images
       (map (lambda (brush-point) (brush-point 'render)) (compile-layers-b ws))
       (map (lambda (brush-point) (brush-point 'pos)) (compile-layers-b ws))
       (empty-scene 1200 700)))

; render-background-save :: saving->image
; creates an image with each of the images of the layers layered on top of each other
(define (render-background-save ws)
  (place-images
       (map (lambda (brush-point) (brush-point 'render)) (compile-layers-s ws))
       (map (lambda (brush-point) (brush-point 'pos)) (compile-layers-s ws))
       (empty-scene 1200 700)))

;render-drawing :: drawing->image
;creates the screen showing for the drawing worldtstate with the actual dots and the toolbar and layer menu
(define (render-drawing ws)
  (place-image
   (toolbar ws) 600 10
   (place-image
    LAYER-MENU 60 350
    (scale (drawing-view ws) (render-background ws)))))

;render-png :: png->image
;overlays the name of the file onto a blue background
(define (render-png ws)
  (overlay
   (text (png-name ws) 50 "white")
   (frame (rectangle 1200 700 255 "dodgerblue"))))

;render-gif :: gif->image
;overlays the name of the file onto a blue background
(define (render-gif ws)
  (overlay
   (text (gif-name ws) 50 "white")
   (frame (rectangle 1200 700 255 "dodgerblue"))))

;color-button-render :: color-button->image
;creates the screen showing for the drawing worldtstate with the actual dots and the toolbar, color menu, and layer menu
(define (color-button-render ws)
  (place-image
   COLOR-MENU
   110 180
   (place-image
   (toolbar ws) 600 10
   (place-image
    LAYER-MENU 60 350
    (scale (color-button-view ws) (render-background-cb ws))))))

;brush-button-render :: brush-button->image
;creates the screen showing for the drawing worldtstate with the actual dots and the toolbar, brush menu, and layer menu
(define (brush-button-render ws)
  (place-image
   BRUSH-OPTION
   165 70
   (place-image
   (toolbar ws) 600 10
   (place-image
    LAYER-MENU 60 350
    (scale (brush-button-view ws) (render-background-b ws))))))

;save-render :: saving->image
;creates the screen showing for the drawing worldtstate with the actual dots and the toolbar, saving menu, and layer menu
(define (save-render ws)
  (place-image
   SAVE-OPTIONS
   60 40
   (place-image
   TOOLBAR 600 10
   (place-image
    LAYER-MENU 60 350
    (render-background-save ws)))))

;render :: WorldState->Image  
;creates an image for each of the different worldstates
(define (render ws)
  (cond
    [(drawing? ws) (render-drawing ws)]
    [(png? ws) (render-png ws)]
    [(gif? ws) (render-gif ws)]
    [(color-button? ws) (color-button-render ws)]
    [(saving? ws) (save-render ws)]
    [(brush-button? ws) (brush-button-render ws)]))

;------------------------------------------------------KEY-STROKE HELPER FUNCTIONS---------------------------------------------------------------------------------

; edit-filename-png :: png, keypress -> png
; takes in a keypress and appends it onto the end of the current filename
(define (edit-filename-png ws kp)
  (if (string=? kp "\b")
      (string-append (substring (png-name ws) 0 (- (string-length (png-name ws)) 5))
                     ".png")
      (string-append (substring (png-name ws) 0 (- (string-length (png-name ws)) 4))
                     kp
                     ".png")))

; edit-filename-gif :: gif, keypress->gif
; takes in a keypress and appends it onto the end of the current filename
(define (edit-filename-gif ws kp)
  (if (string=? kp "\b")
      (string-append (substring (gif-name ws) 0 (- (string-length (gif-name ws)) 1))
                     ".gif")
      (string-append (substring (gif-name ws) 0 (- (string-length (gif-name ws)) 4))
                               kp
                               ".gif")))

; pan-left :: brush-stroke->brush-stroke
; moves the screen left (all the dots right)
(define (pan-left bs)
  (begin
    (bs 'change-velx 5)
    bs))

; pan-right :: brush-stroke->brush-stroke
; moves the screen right
(define (pan-right bs)
  (begin
    (bs 'change-velx -5)
    bs))

;pan-up :: brush-stroke->brush-stroke
; moves the screen up
(define (pan-up bs)
  (begin
    (bs 'change-vely 5)
    bs))

;pan-down :: brush-stroke->brush-stroke
;moves the screen down
(define (pan-down bs)
  (begin
    (bs 'change-vely -5)
    bs))

;reset :: brush-stroke, symbol->brush-stroke
; resents a given element of a brush-point to 0
(define (reset sym bs)
  (begin
    (bs sym 0)
    bs))

;map-to-drawing :: drawing, (layer->layer)->drawing
;apply a given functions to all layers of a drawing struct
(define (map-to-drawing ws func)
  (make-drawing
    (map func (drawing-l1 ws))
    (map func (drawing-l2 ws))
    (map func (drawing-l3 ws))
    (map func (drawing-l4 ws))
    (map func (drawing-l5 ws))
    (map func (drawing-l6 ws))
    (map func (drawing-l7 ws))
    (map func (drawing-l8 ws))
    (map func (drawing-l9 ws))
    (map func (drawing-l10 ws))
   (drawing-current-brush ws)
   (drawing-current-color ws)
   (drawing-current-layer ws)
   (drawing-view ws)
  (drawing-current-size ws)))
  

;change-size :: drawing, number->drawing
; changes the current size of the drawing struct
(define (change-size ws n)
  (make-drawing
   (drawing-l1 ws)
   (drawing-l2 ws)
   (drawing-l3 ws)
   (drawing-l4 ws)
   (drawing-l5 ws)
   (drawing-l6 ws)
   (drawing-l7 ws)
   (drawing-l8 ws)
   (drawing-l9 ws)
   (drawing-l10 ws)
   (drawing-current-brush ws)
   (drawing-current-color ws)
   (drawing-current-layer ws)
   (drawing-view ws)
   n))

;change-view :: drawing, number->drawing
; changes the current view size of the drawing struct
(define (change-view ws n)
  (make-drawing
   (drawing-l1 ws)
   (drawing-l2 ws)
   (drawing-l3 ws)
   (drawing-l4 ws)
   (drawing-l5 ws)
   (drawing-l6 ws)
   (drawing-l7 ws)
   (drawing-l8 ws)
   (drawing-l9 ws)
   (drawing-l10 ws)
   (drawing-current-brush ws)
   (drawing-current-color ws)
   (drawing-current-layer ws)
   n
   (drawing-current-size ws)))

;drawing-key-handler :: drawing, keyevent->drawing
;if a correct key is pressed and the worlstate is a make-drawing, performs the corresponding functions
(define (drawing-key-handler ws kp)
  (cond
    [(string=? kp "up") (map-to-drawing ws pan-up)]
    [(string=? kp "down") (map-to-drawing ws pan-down)]
    [(string=? kp "left") (map-to-drawing ws pan-left)]
    [(string=? kp "right") (map-to-drawing ws pan-right)]
    [(string=? kp "1") (change-size ws (max 5 (- (drawing-current-size ws) 2)))]
    [(string=? kp "2") (change-size ws (min 200 (+ (drawing-current-size ws) 2)))]
    [(string=? kp "3") (change-view ws (max 1 (- (drawing-view ws) 0.1)))]
    [(string=? kp "4") (change-view ws (min 5 (+ (drawing-view ws) 0.1)))]
    [else ws]))

;gif-key-handler :: gif, keyevent->gif
;if a correct key is pressed and the worlstate is a make-gif, performs the corresponding functions
(define (gif-key-handler ws kp)
  (if (string=? kp "\r")
      (begin
        (animated-gif (gif-name ws) (map render-layer (list
                                                       (gif-l1 ws)
                                                       (gif-l2 ws)
                                                       (gif-l3 ws)
                                                       (gif-l4 ws)
                                                       (gif-l5 ws)
                                                       (gif-l6 ws)
                                                       (gif-l7 ws)
                                                       (gif-l8 ws)
                                                       (gif-l9 ws)
                                                       (gif-l10 ws))) 5)
        ws)
      (make-gif
       (edit-filename-gif ws kp)
       (gif-l1 ws)
       (gif-l2 ws)
       (gif-l3 ws)
       (gif-l4 ws)
       (gif-l5 ws)
       (gif-l6 ws)
       (gif-l7 ws)
       (gif-l8 ws)
       (gif-l9 ws)
       (gif-l10 ws))))

;png-key-handler :: png,keyevent->png
;if a correct key is pressed and the worlstate is a make-png, performs the corresponding functions
(define (png-key-handler ws kp)
  (if (string=? kp "\r")
      (begin
        (save-image (render-background-png ws) (png-name ws))
        ws)
      (make-png
       (edit-filename-png ws kp)
       (png-l1 ws)
       (png-l2 ws)
       (png-l3 ws)
       (png-l4 ws)
       (png-l5 ws)
       (png-l6 ws)
       (png-l7 ws)
       (png-l8 ws)
       (png-l9 ws)
       (png-l10 ws))))                     

;--------------------------------------------------KEY-STROKE FUNCTIONS-----------------------------------------------------------------------------------------

;on-key-press :: WorldState, keypress->WorldState
;performs each of the keyevent functions corresponding to each worldtsate
(define (on-key-press ws kp)
  (cond
    [(drawing? ws) (drawing-key-handler ws kp)]
    [(gif? ws) (gif-key-handler ws kp)]
    [(png? ws) (png-key-handler ws kp)]
    [else ws]))

;on-key-release :: WordlState, keypress->WorldState
;performs each of the key-release functions corresponding to each worldstate
(define (on-key-release ws kp)
  (if (drawing? ws)
      (cond
        [(or
          (string=? kp "left")
          (string=? kp "right")) (map-to-drawing ws (lambda (brush) (reset 'change-velx brush)))]
        [(or
          (string=? kp "up")
          (string=? kp "down")) (map-to-drawing ws (lambda (brush) (reset 'change-vely brush)))]
        [else ws])
      ws))

;--------------------------------------------------SAVE-IMAGE FUNCTIONS-------------------------------------------------------------------------------------

; img->bytes :: Image -> Bytes
; Returns a list of bytes, each representing the brightness (betw 0 and 255) of a pixel
; in the image.
(define (img->bytes img)
  (list->bytes (map 
                (lambda (col)
                      (round 
                       (/ (+ (color-red col) (color-green col) (color-blue col)) 3)))
                (image->color-list img))))


; make-grayscale-colormap ::  -> ColorMap
; creates a colormap to be used with grayscale images.
; the colormap maps 0 to black, 255 to white, and everything in between to various levels of gray.
(define (make-grayscale-colormap)
  (define (helper starting-at)
    (cond
      [(= starting-at 256) '()]
      [else (cons (make-vector 3 starting-at)
                  (helper (add1 starting-at)))]))
    (helper 0))

; animated-gif :: String List<Image> Number -> Void
; writes an animated gif to the specified filename, containing
; lst-of-imgs as the frames of the animation, with delay 100ths of a second
; between each frame.
(define (animated-gif filename lst-of-imgs delay)
  ; we open the file at filename, specifying 'truncate as the #:exists argument
  ; so that we will overwrite the file if it already exists.
  (define output (open-output-file filename #:exists 'truncate))
  ; we then create a colormap using make-grayscale-colormap
  (define colormap (make-grayscale-colormap))
  ; get some stats about the images we're dealing with
  (define w (image-width (first lst-of-imgs)))
  (define h (image-height (first lst-of-imgs)))
  ; we begin to write the gif to a file, and are returned a gif-stream object
  ; to which we can write images
  (define gif-stream (gif-start output w h 255 colormap))
  ; this is the recursive function that actually writes each image to the gif-stream
  (define (write-images imgs)
    (cond
      ; if no more images to write, close the gif-stream
      [(empty? imgs) (gif-end gif-stream)]
      ; otherwise:
      [else
       ; write that the gif should delay some time after displaying the image we are about to write
       (gif-add-control gif-stream 'any #f delay #f)
       ; and then write the next image to the gif
       (gif-add-image gif-stream 0 0 w h #f #f (img->bytes (first imgs)))
       ; then write the rest of the images
       (write-images (rest imgs))]))
  ; fill in the frames of the gif by calling our helper function
  (write-images lst-of-imgs)
  ; close the .gif file we opened
  (close-output-port output))
 

; count-up-to :: Number Number Number -> List<Number>
; creates a list of numbers at regular intervals, like '(10 20 30 40 50)
(define (count-up-to n starting-at interval)
  (if (>= starting-at n)
      empty
      (cons starting-at (count-up-to n (+ starting-at interval) interval))))

; img-lst is the list of images we'll write to an animated gif
; in this progression, a circle grows and shrinks, getting lighter and lighter
(define img-lst 
  (map (lambda (n)
       (place-image (circle (if (> n 100) (- 100 (- n 100)) n) "solid" (make-color n n n))
                    n n (empty-scene 200 200)))
     (count-up-to 200 0 10)))

;-----------------------------------------------MOUSE STUFF--------------------------------------------------------------------------------------------------

;add-dot
;if a mouseevent is button down or drag, it will add a new dot, if not, and one of the toolbar options
;is selected, changes the worldstate to that button-click
(define (add-dot ws x y m)
  (cond
    [(or (string=? m "button-down")
         (string=? m "drag")) (cond
                                [(= 1 (drawing-current-layer ws)) (make-drawing
                                                                   (cons (make-brush (drawing-current-brush ws)
                                                                                     (drawing-current-color ws)
                                                                                     (drawing-current-size ws)
                                                                                     x y 0 0)
                                                                         (drawing-l1 ws))
                                                                   (drawing-l2 ws)
                                                                   (drawing-l3 ws)
                                                                   (drawing-l4 ws)
                                                                   (drawing-l5 ws)
                                                                   (drawing-l6 ws)
                                                                   (drawing-l7 ws)
                                                                   (drawing-l8 ws)
                                                                   (drawing-l9 ws)
                                                                   (drawing-l10 ws)
                                                                   (drawing-current-brush ws)
                                                                   (drawing-current-color ws)
                                                                   (drawing-current-layer ws)
                                                                   (drawing-view ws)
                                                                   (drawing-current-size ws))]
                                [(= 2 (drawing-current-layer ws)) (make-drawing
                                                                   (drawing-l1 ws)
                                                                   (cons (make-brush (drawing-current-brush ws)
                                                                                     (drawing-current-color ws)
                                                                                     (drawing-current-size ws)
                                                                                     x y 0 0)
                                                                         (drawing-l2 ws))
                                                                   (drawing-l3 ws)
                                                                   (drawing-l4 ws)
                                                                   (drawing-l5 ws)
                                                                   (drawing-l6 ws)
                                                                   (drawing-l7 ws)
                                                                   (drawing-l8 ws)
                                                                   (drawing-l9 ws)
                                                                   (drawing-l10 ws)
                                                                   (drawing-current-brush ws)
                                                                   (drawing-current-color ws)
                                                                   (drawing-current-layer ws)
                                                                   (drawing-view ws)
                                                                   (drawing-current-size ws))]
                                [(= 3 (drawing-current-layer ws)) (make-drawing
                                                                   (drawing-l1 ws)
                                                                   (drawing-l2 ws)
                                                                   (cons (make-brush (drawing-current-brush ws)
                                                                                     (drawing-current-color ws)
                                                                                     (drawing-current-size ws)
                                                                                     x y 0 0)
                                                                         (drawing-l3 ws))
                                                                   (drawing-l4 ws)
                                                                   (drawing-l5 ws)
                                                                   (drawing-l6 ws)
                                                                   (drawing-l7 ws)
                                                                   (drawing-l8 ws)
                                                                   (drawing-l9 ws)
                                                                   (drawing-l10 ws)
                                                                   (drawing-current-brush ws)
                                                                   (drawing-current-color ws)
                                                                   (drawing-current-layer ws)
                                                                   (drawing-view ws)
                                                                   (drawing-current-size ws))]
                                [(= 4 (drawing-current-layer ws)) (make-drawing
                                                                   (drawing-l1 ws)
                                                                   (drawing-l2 ws)
                                                                   (drawing-l3 ws)
                                                                   (cons (make-brush (drawing-current-brush ws)
                                                                                     (drawing-current-color ws)
                                                                                     (drawing-current-size ws)
                                                                                     x y 0 0)
                                                                         (drawing-l4 ws))
                                                                   (drawing-l5 ws)
                                                                   (drawing-l6 ws)
                                                                   (drawing-l7 ws)
                                                                   (drawing-l8 ws)
                                                                   (drawing-l9 ws)
                                                                   (drawing-l10 ws)
                                                                   (drawing-current-brush ws)
                                                                   (drawing-current-color ws)
                                                                   (drawing-current-layer ws)
                                                                   (drawing-view ws)
                                                                   (drawing-current-size ws))]
                                [(= 5 (drawing-current-layer ws)) (make-drawing
                                                                   (drawing-l1 ws)
                                                                   (drawing-l2 ws)
                                                                   (drawing-l3 ws)
                                                                   (drawing-l4 ws)
                                                                   (cons (make-brush (drawing-current-brush ws)
                                                                                     (drawing-current-color ws)
                                                                                     (drawing-current-size ws)
                                                                                     x y 0 0)
                                                                         (drawing-l5 ws))
                                                                   (drawing-l6 ws)
                                                                   (drawing-l7 ws)
                                                                   (drawing-l8 ws)
                                                                   (drawing-l9 ws)
                                                                   (drawing-l10 ws)
                                                                   (drawing-current-brush ws)
                                                                   (drawing-current-color ws)
                                                                   (drawing-current-layer ws)
                                                                   (drawing-view ws)
                                                                   (drawing-current-size ws))]
                                [(= 6 (drawing-current-layer ws)) (make-drawing
                                                                   (drawing-l1 ws)
                                                                   (drawing-l2 ws)
                                                                   (drawing-l3 ws)
                                                                   (drawing-l4 ws)
                                                                   (drawing-l5 ws)
                                                                   (cons (make-brush (drawing-current-brush ws)
                                                                                     (drawing-current-color ws)
                                                                                     (drawing-current-size ws)
                                                                                     x y 0 0)
                                                                         (drawing-l6 ws))
                                                                   (drawing-l7 ws)
                                                                   (drawing-l8 ws)
                                                                   (drawing-l9 ws)
                                                                   (drawing-l10 ws)
                                                                   (drawing-current-brush ws)
                                                                   (drawing-current-color ws)
                                                                   (drawing-current-layer ws)
                                                                   (drawing-view ws)
                                                                   (drawing-current-size ws))]
                                [(= 7 (drawing-current-layer ws)) (make-drawing
                                                                   (drawing-l1 ws)
                                                                   (drawing-l2 ws)
                                                                   (drawing-l3 ws)
                                                                   (drawing-l4 ws)
                                                                   (drawing-l5 ws)
                                                                   (drawing-l6 ws)
                                                                   (cons (make-brush (drawing-current-brush ws)
                                                                                     (drawing-current-color ws)
                                                                                     (drawing-current-size ws)
                                                                                     x y 0 0)
                                                                         (drawing-l7 ws))
                                                                   (drawing-l8 ws)
                                                                   (drawing-l9 ws)
                                                                   (drawing-l10 ws)
                                                                   (drawing-current-brush ws)
                                                                   (drawing-current-color ws)
                                                                   (drawing-current-layer ws)
                                                                   (drawing-view ws)
                                                                   (drawing-current-size ws))]
                                [(= 8 (drawing-current-layer ws)) (make-drawing
                                                                   (drawing-l1 ws)
                                                                   (drawing-l2 ws)
                                                                   (drawing-l3 ws)
                                                                   (drawing-l4 ws)
                                                                   (drawing-l5 ws)
                                                                   (drawing-l6 ws)
                                                                   (drawing-l7 ws)
                                                                   (cons (make-brush (drawing-current-brush ws)
                                                                                     (drawing-current-color ws)
                                                                                     (drawing-current-size ws)
                                                                                     x y 0 0)
                                                                         (drawing-l8 ws))
                                                                   (drawing-l9 ws)
                                                                   (drawing-l10 ws)
                                                                   (drawing-current-brush ws)
                                                                   (drawing-current-color ws)
                                                                   (drawing-current-layer ws)
                                                                   (drawing-view ws)
                                                                   (drawing-current-size ws))]
                                [(= 9 (drawing-current-layer ws)) (make-drawing
                                                                   (drawing-l1 ws)
                                                                   (drawing-l2 ws)
                                                                   (drawing-l3 ws)
                                                                   (drawing-l4 ws)
                                                                   (drawing-l5 ws)
                                                                   (drawing-l6 ws)
                                                                   (drawing-l7 ws)
                                                                   (drawing-l8 ws)
                                                                   (cons (make-brush (drawing-current-brush ws)
                                                                                     (drawing-current-color ws)
                                                                                     (drawing-current-size ws)
                                                                                     x y 0 0)
                                                                         (drawing-l9 ws))
                                                                   (drawing-l10 ws)
                                                                   (drawing-current-brush ws)
                                                                   (drawing-current-color ws)
                                                                   (drawing-current-layer ws)
                                                                   (drawing-view ws)
                                                                   (drawing-current-size ws))]
                                [(= 10 (drawing-current-layer ws)) (make-drawing
                                                                    (drawing-l1 ws)
                                                                    (drawing-l2 ws)
                                                                    (drawing-l3 ws)
                                                                    (drawing-l4 ws)
                                                                    (drawing-l5 ws)
                                                                    (drawing-l6 ws)
                                                                    (drawing-l7 ws)
                                                                    (drawing-l8 ws)
                                                                    (drawing-l9 ws)
                                                                    (cons (make-brush (drawing-current-brush ws)
                                                                                      (drawing-current-color ws)
                                                                                      (drawing-current-size ws)
                                                                                      x y 0 0)
                                                                          (drawing-l10 ws))
                                                                    (drawing-current-brush ws)
                                                                    (drawing-current-color ws)
                                                                    (drawing-current-layer ws)
                                                                    (drawing-view ws)
                                                                    (drawing-current-size ws))])]
    [(string=? m "button-up") (cond
                                [(and (and (< x 40)
                                           (> x 0))
                                      (and (< y 20)
                                           (> y 0))) (make-saving
                                                      (drawing-l1 ws)
                                                      (drawing-l2 ws)
                                                      (drawing-l3 ws)
                                                      (drawing-l4 ws)
                                                      (drawing-l5 ws)
                                                      (drawing-l6 ws)
                                                      (drawing-l7 ws)
                                                      (drawing-l8 ws)
                                                      (drawing-l9 ws)
                                                      (drawing-l10 ws))]
                                [(and (and (< x 94)
                                           (> x 40))
                                      (and (< y 20)
                                           (> y 0))) (make-color-button
                                                      (drawing-l1 ws)
                                                      (drawing-l2 ws)
                                                      (drawing-l3 ws)
                                                      (drawing-l4 ws)
                                                      (drawing-l5 ws)
                                                      (drawing-l6 ws)
                                                      (drawing-l7 ws)
                                                      (drawing-l8 ws)
                                                      (drawing-l9 ws)
                                                      (drawing-l10 ws)
                                                      (drawing-current-brush ws)
                                                      (drawing-current-color ws)
                                                      (drawing-current-layer ws)
                                                      (drawing-view ws)
                                                      (drawing-current-size ws))]
                                [(and (and (< x 161)
                                           (> x 94))
                                      (and (< y 20)
                                           (> y 0))) (make-brush-button
                                                      (drawing-l1 ws)
                                                      (drawing-l2 ws)
                                                      (drawing-l3 ws)
                                                      (drawing-l4 ws)
                                                      (drawing-l5 ws)
                                                      (drawing-l6 ws)
                                                      (drawing-l7 ws)
                                                      (drawing-l8 ws)
                                                      (drawing-l9 ws)
                                                      (drawing-l10 ws)
                                                      (drawing-current-brush ws)
                                                      (drawing-current-color ws)
                                                      (drawing-current-layer ws)
                                                      (drawing-view ws)
                                                      (drawing-current-size ws))]
                                [(and (and (< x 209)
                                           (> x 161))
                                      (and (< y 20)
                                           (> y 0))) INITIAL-STATE]
                                [else (select-layer ws x y m)])]
    [else ws]))

;change-drawing-color :: drawing,color->drawing
;changes the current-color in a drawing struct to a given color
(define (change-drawing-color ws c)
  (make-drawing
   (color-button-l1 ws)
   (color-button-l2 ws)
   (color-button-l3 ws)
   (color-button-l4 ws)
   (color-button-l5 ws)
   (color-button-l6 ws)
   (color-button-l7 ws)
   (color-button-l8 ws)
   (color-button-l9 ws)
   (color-button-l10 ws)
   (color-button-current-brush ws)
   c
   (color-button-current-layer ws)
   (color-button-view ws)
   (color-button-current-size ws)))

;change-brush :: drawing, string->drawing
;changes the current-brush in a drawing struct to a given brush
(define (change-brush ws b)
  (make-drawing
   (brush-button-l1 ws)
   (brush-button-l2 ws)
   (brush-button-l3 ws)
   (brush-button-l4 ws)
   (brush-button-l5 ws)
   (brush-button-l6 ws)
   (brush-button-l7 ws)
   (brush-button-l8 ws)
   (brush-button-l9 ws)
   (brush-button-l10 ws)
   b
   (brush-button-current-color ws)
   (brush-button-current-layer ws)
   (brush-button-view ws)
   (brush-button-current-size ws)))

;change-layer :: drawing, number->drawing
;changes the current-layer in a drawing struct to a given number
(define (change-layer ws l)
  (make-drawing
   (drawing-l1 ws)
   (drawing-l2 ws)
   (drawing-l3 ws)
   (drawing-l4 ws)
   (drawing-l5 ws)
   (drawing-l6 ws)
   (drawing-l7 ws)
   (drawing-l8 ws)
   (drawing-l9 ws)
   (drawing-l10 ws)
   (drawing-current-brush ws)
   (drawing-current-color ws)
   l
   (drawing-view ws)
   (drawing-current-size ws)))
                                                      
;select-color :: color-button, number, number, mouseevent->drawing
;if a color is selected changes to the drawing worldstate with that new color
(define (select-color ws x y m)
  (if (and
       (and (< x 180)
            (> x 60))
       (string=? m "button-up"))
      (cond
        [(and (< y 40)
              (> y 20)) (change-drawing-color ws "violet red")]
        [(and (< y 60)
              (> y 40)) (change-drawing-color ws "red")]
        [(and (< y 80)
              (> y 60)) (change-drawing-color ws "orange")]
        [(and (< y 100)
              (> y 80)) (change-drawing-color ws "yellow")]
        [(and (< y 120)
              (> y 100)) (change-drawing-color ws "green yellow")]
        [(and (< y 140)
              (> y 120)) (change-drawing-color ws "lime green")]
        [(and (< y 160)
              (> y 140)) (change-drawing-color ws "forest green")]
        [(and (< y 180)
              (> y 160)) (change-drawing-color ws "royalblue")]
        [(and (< y 200)
              (> y 180)) (change-drawing-color ws "dodgerblue")]
        [(and (< y 220)
              (> y 200)) (change-drawing-color ws "deepskyblue")]
        [(and (< y 240)
              (> y 220)) (change-drawing-color ws "sky blue")]
        [(and (< y 260)
              (> y 240)) (change-drawing-color ws "medium slate blue")]
        [(and (< y 280)
              (> y 260)) (change-drawing-color ws "blue violet")]
        [(and (< y 300)
              (> y 280)) (change-drawing-color ws "indigo")]
        [(and (< y 320)
              (> y 300)) (change-drawing-color ws "black")]
        [(and (< y 340)
              (> y 320)) (change-drawing-color ws "white")])
      ws))

;select-save :: saving, number, number, mouseevent->saving-state
;if a saving-state is selected changes to that saving-state worldstate
(define (select-save ws x y m)
  (if (and
       (and (< x 120)
            (> x 0))
       (string=? m "button-up"))
      (cond
        [(and (< y 40)
              (> y 20)) (make-png
                        ".png"
                        (saving-l1 ws)
                        (saving-l2 ws)
                        (saving-l3 ws)
                        (saving-l4 ws)
                        (saving-l5 ws)
                        (saving-l6 ws)
                        (saving-l7 ws)
                        (saving-l8 ws)
                        (saving-l9 ws)
                        (saving-l10 ws))]
        [(and (< y 60)
              (> y 40)) (make-gif
                         ".gif"
                         (saving-l1 ws)
                         (saving-l2 ws)
                         (saving-l3 ws)
                         (saving-l4 ws)
                         (saving-l5 ws)
                         (saving-l6 ws)
                         (saving-l7 ws)
                         (saving-l8 ws)
                         (saving-l9 ws)
                         (saving-l10 ws))])
      ws))

;select-layer :: drawing, number, number, mouseevent->drawing
;if a layer is selected changes to the drawing worldstate with that new layer
(define (select-layer ws x y m)
  (if (and
       (and (< x 95)
            (> x 25))
       (string=? m "button-up"))
      (cond
        [(and (< y 254)
              (> y 230)) (change-layer ws 1)]
        [(and (< y 278)
              (> y 254)) (change-layer ws 2)]
        [(and (< y 302)
              (> y 278)) (change-layer ws 3)]
        [(and (< y 326)
              (> y 302)) (change-layer ws 4)]
        [(and (< y 350)
              (> y 326)) (change-layer ws 5)]
        [(and (< y 374)
              (> y 326)) (change-layer ws 6)]
        [(and (< y 398)
              (> y 374)) (change-layer ws 7)]
        [(and (< y 422)
              (> y 398)) (change-layer ws 8)]
        [(and (< y 446)
              (> y 422)) (change-layer ws 9)]
        [(and (< y 470)
              (> y 446)) (change-layer ws 10)])
      ws))
       
;select-brush :: brush-button, number, number, mouseevent->drawing
;if a brush is selected changes to the drawing worldstate with that new brush
(define (select-brush ws x y m)
  (if (and
       (and (< x 235)
            (> x 95))
       (string=? m "button-up"))
      (cond
        [(and (< y 40)
              (> y 20)) (change-brush ws "pen")]
        [(and (< y 60)
              (> y 40)) (change-brush ws "paint")]
        [(and (< y 80)
              (> y 60)) (change-brush ws "airbrush")]
        [(and (< y 100)
              (> y 80)) (change-brush ws "changing")]
        [(and (< y 120)
              (> y 100)) (change-brush ws "eraser")])
      ws))
  

; mouse-handler
; performs the functions asscociated with each of the different types of worldstates
(define (mouse-handler ws x y m)
  (cond
    [(drawing? ws) (add-dot ws x y m)]
    [(color-button? ws) (select-color ws x y m)]
    [(saving? ws) (select-save ws x y m)]
    [(brush-button? ws) (select-brush ws x y m)]
    [else ws]))

; tick
; if the worldtsate is a make-drawing, it will call update on all of the dots, otherwise it does nothing
(define (tick ws)
  (if (drawing? ws)
      (begin
        (map-to-drawing ws (lambda (b) (b 'update)))
        ws)
      ws))

(big-bang INITIAL-STATE
          [to-draw render]
          [on-mouse mouse-handler]
          [on-key on-key-press]
          [on-release on-key-release]
          [on-tick tick])





      