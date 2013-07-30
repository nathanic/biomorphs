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
  ;; (log "draw-creature")
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

(defn pos-from-index [n canvas-width]
  (let [per-row (-> canvas-width (/ g/CXCELL) Math/floor int)
        row     (/ n per-row)
        col     (mod n per-row)]
    [(* col g/CYCELL) (* row g/CXCELL)]))

(comment
  (map #(pos-from-index % 500) (range 9))
  )

(defn draw-creatures
  [{:keys [ctx parent children width height]}]
  (let [creatures (cons parent children)]
    (doseq [[n creature] (map-indexed vector creatures)]
      (log "drawing creature" creature)
      (draw-creature {:ctx ctx
                      :genome creature
                      :pos (pos-from-index n width)})
      )))


