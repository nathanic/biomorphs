(ns biomorphs.graphics
  (:require [mondrian.anim :as anim]
            [mondrian.canvas :as canvas]
            [mondrian.color :as color]
            [mondrian.math :as math]
            [mondrian.plot :as plot]
            [mondrian.ui :as ui]
            [monet.canvas :as m]
            [biomorphs.genetics :as g]
            [biomorphs.utils :refer [log clog]]
            ))


(defn line'
  "draw a line."
  [ctx x y x' y']
  (-> ctx
      ;; (m/save)
      ;; (m/scale 1 -1)
      ;; (m/rotate Math/PI)
      (m/begin-path)
      (m/move-to x y)
      (m/line-to x' y')
      (m/stroke)
      ;; (m/restore)
      )
  ctx)

;;; there must be a better way than string concatenation
;;; but this is how mondrian handles it...
(defn format-color [[r g b a]]
  (if (nil? a)
    (str "rgb(" r "," g "," b ")")
    (str "rgba(" r "," g "," b ",a)")
    ))

(defn draw-subtree
  [ctx [x y] genome dir depth-remain]
  (let [[bx by]   (g/calc-branch-vector genome depth-remain dir)
        [x' y']   [(+ x bx) (+ y by)]
        [r g b]   (g/color-for-depth genome depth-remain)
        ]
    ;; (println "color for" depth-remain "is" (str "rgb(" r "," g "," b ")"))
    (when (pos? depth-remain)
      (-> ctx
          ;; (println "draw-subtree depth-remain" depth-remain " line: " x y bx by)
          ;; (m/stroke-style "green")
          (m/stroke-style (format-color [r g b]))
          (line' x y x' y')
          (draw-subtree [x' y'] genome (g/turn-direction dir :left)  (dec depth-remain))
          (draw-subtree [x' y'] genome (g/turn-direction dir :right) (dec depth-remain))
          )
      ))
  ctx)

; NB: we could potentially blow the stack
; but the JS impl I saw gets away with it.
(defn draw-creature [{:keys [ctx genome pos]}]
  ;; (log "draw-creature" genome "at" pos)
  ;; (println "BEGIN drawing genome")
  (draw-subtree ctx pos genome :n (inc (g/genome-depth genome)))
  ;; (println "END drawing genome")
  )


(comment
  (defn all-creatures
    "return a seq of creatures, with the parent in the middle of the children"
    [parent children]
    (let [[former latter] (split-at (/ (count children) 2) children)]
      (vec (concat former '(parent) latter))))

  )

(defn pos-from-index [n canvas-width cell-width cell-height]
  (let [per-row (int (/ canvas-width cell-width))
        row     (int (/ n per-row))
        col     (int (mod n per-row))]
    [(* col cell-width) (* row cell-height)]))

(comment
  ; TODO fix this, getting NaN and Infinity
  (pos-from-index 5 500)
  (map #(pos-from-index % 500) (range 9))
  (int 42.9)
  )

; maybe set up a one-shot to flick this true after a delay
; giving the browser repl time to set up
(def drawing? (atom false))

(js/setTimeout (fn [] 
                 (log "timeout expired, enabling drawing!")
                 (reset! drawing? true))
               2000)

(comment
  (reset! drawing? true)
  (reset! drawing? false)
  )

(defn bounding-box [ctx x y cx cy]
  (-> ctx
      (m/begin-path)
      (m/move-to x y)
      (m/line-to (+ x cx) y)
      (m/line-to (+ x cx) (+ cy y))
      (m/line-to x (+ cy y))
      (m/line-to x y)
      (m/stroke-style "red")
      (m/stroke)
      )
  ctx)

; TODO: masked drawing so we don't overlap into another cell
(defn draw-creatures
  [{:keys [ctx parent children]}]
  (when @drawing?
    (let [width     (.-width (.-canvas ctx))
          cxcell    (int (/ width 3))
          cycell    cxcell
          creatures (cons parent children)
          ]
      (doseq [[n creature] (map-indexed vector creatures)]
        ;; (log "drawing creature" n "(" creature ") at" (pos-from-index n width) "width: " width)
        (let [[x y] (pos-from-index n width cxcell cycell)]
          (bounding-box ctx x y cxcell cycell)
          (draw-creature {:ctx ctx
                          :genome creature
                          ; TODO HACKISH: need a better way of scaling the creatures
                          ; and aligning them in their cells
                          :pos [(+ x (/ cxcell 2))
                                (+ y (* 3 (/ cycell 4)))
                                ]}))
        ))))


