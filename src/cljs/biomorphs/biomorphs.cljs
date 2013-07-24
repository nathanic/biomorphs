(ns biomorphs.biomorphs
  (:require [mondrian.anim :as anim]
            [mondrian.canvas :as canvas]
            [mondrian.color :as color]
            [mondrian.math :as math]
            [mondrian.plot :as plot]
            [mondrian.ui :as ui]
            [monet.canvas :as m])
  (:use-macros [mondrian.macros :only [defmondrian]]))


;; TODO
;;  handle mutation 
;;  show more creatures
;;  allow interactive evolution

(def CX 1024)
(def CY 1024)
(def BIOMORPH-COUNT 9)
(def CXCELL (/ CX BIOMORPH-COUNT))
(def CYCELL (/ CY BIOMORPH-COUNT))
(def MIN-GENE 0)
(def MAX-GENE 19)
(def MAX-DEPTH 10)

; the order of directions we cycle through
(def directions [:n :ne :e :se :s :sw :w :nw])
; and the vectors they represent
(def dir-vectors
  {:n  [ 0  1]
   :ne [ 1  1]
   :e  [ 1  0]
   :se [ 1 -1]
   :s  [ 0 -1]
   :sw [-1 -1]
   :w  [-1  0]
   :nw [-1  1]
   })

;; there's probably a better way to do this
;; but i was relying on a JVMism in my previous impl
(defn index-of [coll item]
  (loop [idx 0, c coll]
    (if-not (empty? c) 
      (if (= item (first c))
        idx
        (recur (inc idx) (rest c)))
      -1)))

(comment
  (index-of [:a :b :c] :b)
  (index-of [:a :b :c] :c)
  (index-of [:a :b :c] :farts)
  )
; make a 45 degree right turn
; based on :left or :right
(defn turn-direction [dir leftright]
  (let [idx (index-of directions dir)
        op  (if (= leftright :left) dec inc)
        ]
    (if-not (= idx -1)
      (nth directions (mod (op idx) (count directions)))
      (throw (js/Error. (str dir " is not a valid direction.")))
      ))) 

(comment
  (turn-direction :n :right)
  (turn-direction :n :left)
  (turn-direction :e :right)
  (turn-direction :e :left)
  (turn-direction "farts" :left)
  )

;; he uses the following genome
;; newgenes = [];
;; newgenes[0] = rand(); // depth
;; newgenes[1] = rand(); // x-scale levels 3, 6, 9
;; newgenes[2] = rand(); // x-scale levels 1, 4, 7
;; newgenes[3] = rand(); // x-scale levels 2, 5, 8
;; newgenes[4] = rand(); // y-scale levels 3, 6, 9
;; newgenes[5] = rand(); // y-scale levels 1, 4, 7
;; newgenes[6] = rand(); // y-scale levels 2, 5, 8
;; newgenes[7] = rand(); // red
;; newgenes[8] = rand(); // green
;; newgenes[9] = rand(); // blue
;; newgenes[10] = rand(); // width 

(def GENES 
  [:depth 
   :xscale369 
   :xscale147 
   :xscale258
   :yscale369 
   :yscale147 
   :yscale258
   :red           ; what about hue here instead?
   :green
   :blue
   :width
   ])
; other ideas: 
; segmentation, 
; segment gradient, 
; symmetries
; drawing primitive (line, ellipse, rect, filled/empty variants)
;
; see also http://www.annanardella.it/biomorph.html 
; http://www.phy.syr.edu/courses/mirror/biomorph/

(defn gene-index [gene-id]
  (let [idx (index-of GENES gene-id)]
    (if-not (= -1 idx)
      idx
      (throw (js/Error. (str gene-id " is not a valid gene id!")))
      )))

(defn get-gene [genome gene-id]
  (get genome (gene-index gene-id)))

(defn random-genome []
  (vec (repeatedly (count GENES) #(+ MIN-GENE (rand-int MAX-GENE)))))
(comment
  (random-genome)
  )

(defn length-genes-for-depth 
  "returns a pair of gene values coding for length at the specified depth"
  [genome depth]
  (let [xidx (+ (gene-index :xscale369) (mod depth 3))]
    [xidx (+ 3 xidx)]))

(defn color-for-depth
  "calculate the color of this branch based on both the genome and the branch depth"
  [genome depth]
  ; TODO impl this for real
  (nth (cycle [[10 10 240]
               [200 10 10]
               [10 200 10]
               [0 0 0]
               [200 200 10]
               ])
       depth))

(comment
  (def mygenome [0 1 2 3 4 5 6 7 8 9 10])
  (def mygenome [1 1 1 1 1 1 1 1 1 1 1])
  ; example taken from the javascript debugger
  (def mygenome [0, 3, 20, 15, 4, 1, 16, 17, 7, 17, 3])
  (get-gene mygenome :depth)
  (get-gene @the-genome :depth)

  (map #(length-genes-for-depth mygenome %1) (range 10))
  )


; i guess cljs doesn't have this
(defn double [x] x)

(defn calc-branch-vector [genome depth dir]
  (let [max-branch-len  200 #_(Math/floor (/ CXCELL 2 8)) 
        [lx ly]         (length-genes-for-depth genome depth)
        [dx dy]         (dir-vectors dir) ]
    ;; var i = x + Math.floor( xoffset*( genes[xGene] * 2579 ) % maxSegmentLen );
    ;; var j = y + Math.floor( yoffset*( genes[yGene] * 5051 ) % maxSegmentLen );
    ;; [(Math/floor (mod (* lx dx 2579) max-branch-len)) 
    ;;  (Math/floor (mod (* ly dy 5051) max-branch-len))]

    (mapv #(Math/floor (double %)) 
          [(* lx dx (/ max-branch-len MAX-GENE ))
           (* ly dy (/ max-branch-len MAX-GENE )) ])
    ;; [(* dx 100) (* dy 100)]
    ))

(comment
  (calc-branch-vector @the-genome 3 :ne)
  (length-genes-for-depth @the-genome 5)
  )

(defn line' 
  "a version of line that works on upside down coordinates (like math rather than like canvas)"
  [ctx x y x' y']
  ;; (line (- CX x) (- CY y) (- CX x') (- CY y'))
  (-> ctx
      (m/save)
      ;; (m/scale 1 -1)
      (m/move-to x y)
      (m/line-to x' y')
      (m/stroke)
      (m/restore))
  ctx)

(defn draw-subtree
  [ctx [x y] genome dir depth-remain]
  (let [[bx by]   (calc-branch-vector genome depth-remain dir)
        [x' y']   [(+ x bx) (+ y by)]
        [r g b]     (color-for-depth genome depth-remain)
        ]
    (when (pos? depth-remain)
      (-> ctx
          ;; (println "draw-subtree depth-remain" depth-remain " line: " x y bx by)
          ;;; there must be a better way than string concatenation
          (m/stroke-style (str "rgb(" r "," g "," b ")"))
          ;; (apply stroke color)
          (line' x y x' y')
          (draw-subtree [x' y'] genome (turn-direction dir :left)  (dec depth-remain))
          (draw-subtree [x' y'] genome (turn-direction dir :right) (dec depth-remain))
          )
      ))
  ctx)

; NB: we could potentially blow the stack
; but the JS impl I saw gets away with it.
(defn genome-depth [genome] 
  (mod (int (get-gene genome :depth)) MAX-DEPTH))

(defn draw-genome [state pos genome]
  ;; (println "BEGIN drawing genome")
  (draw-subtree (:ctx state) pos genome :n (inc (genome-depth genome)))
  ;; (println "END drawing genome")
  )

; no defonce in clojurescript!
(def the-genome (atom [4 1 1 1 1 1 1 1 1 1 1]))

(comment
  (reset! the-genome [4 1 1 1 1 1 1 1 1 1 1])
  (reset! the-genome (random-genome))
  (swap! the-genome #(vec (concat [6] (rest %))))
  (genome-depth @the-genome)
  )



;; ---------------------------------------------------------------------
;; Update pipeline
;;
;; Initial state:
;;    :drawing -- The top-level DOM element that contains the canvas,
;;      the sliders, etc.
;;    :ctx -- The canvas context.
;;    :w -- The width of the canvas.
;;    :h -- The height of the canvas.
;;
;; Configuration values (as defined on the mondrian element):
;;    :persist-image -- Whether or not to clear the background in
;;      between frames.
;;    ... fill this in ...
;;
;; After merge-control-values:
;;    :... -- One key for each control (identified by the control name).
;;      Zero or more controls may be present under the mondrian element,
;;      in which case the missing values must have been provided in as
;;      defaults in the element itself.
;;
;; After ...:

(defn merge-control-values
  "Merge the current values of the controls into state."
  [{:keys [drawing] :as state}]
  (merge state (ui/update-controls drawing)))

(defn update-pipeline
  [state]
  (-> state
      merge-control-values))


;; ---------------------------------------------------------------------
;; Render stack
;;

(defn clear-background
  [{:keys [ctx w h persist-image]}]
  (when-not persist-image
    (-> ctx
        (m/fill-style "rgba(25,29,33,0.75)") ;; Alpha adds motion blur
        (m/fill-rect {:x 0 :y 0 :w w :h h}))))

(defn draw-ball 
  [{:keys [ctx]}]
  (-> ctx
      (m/fill-style "green")
      (m/circle {:x 100, :y 100, :r 30})
      (m/text {:text "repl hacksessss", :x 10, :y 10})
      m/begin-path
      (m/stroke-style "blue")
      (m/fill-style "rgba(10,10,200,0.45)")
      (m/move-to 100 100)
      (m/line-to 200 200)
      (m/line-to 100 200)
      m/close-path
      m/stroke
      m/fill
      ))

(defn render-stack
  [state]
  (clear-background state)
  ; TODO move genome from that atom into the state
  (draw-genome state [100 100] @the-genome)
  ;; (draw-ball state)
  )

;; ---------------------------------------------------------------------
;; Main entry point
;;

(defmondrian biomorphs-anim
  {:foo "bar"}
  update-pipeline
  render-stack)


(comment
  (do (require 'cemerick.piggieback) (cemerick.piggieback/cljs-repl))
  (js/alert "yoyo")
  (+ 2 2)
  (stop-updating)
  (deref the-genome)
  )
