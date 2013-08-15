(ns biomorphs.graphics
  (:require [monet.canvas :as m]
            [biomorphs.genetics :as gen]
            [biomorphs.utils :refer [log clog]]
            ))

; probably get rid of this one
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
(defn rgb [r g b]
  (str "rgb(" r "," g "," b ")"))

(defn rgba [r g b a]
  (str "rgba(" r "," g "," b "," a ")"))

(defn- to-percent [x]
  (str (* 100.0 x) "%"))

(defn hsl [h s l]
  (str "hsl(" h "," (to-percent s) "," (to-percent l) ")"))

(defn hsla [h s l a]
  (str "hsla(" h "," (to-percent s) "," (to-percent l) "," a ")"))

(defn render-creature [ctx x y creature]
  (let [[center-x center-y] (gen/creature-centroid creature)]
    (-> ctx
        (m/save)
        ; could probably combine the translations
        (m/translate x y)
        (m/rotate Math/PI)
        (m/translate (- center-x) (- center-y))))
  (doseq [{:keys [x0 y0 x1 y1 color]} creature ]
    (-> ctx
        (m/begin-path)
        (m/stroke-style (apply hsla color))
        (m/move-to x0 y0)
        (m/line-to x1 y1)
        (m/stroke)))
  (m/restore ctx)
  ctx)


(comment
  (in-ns 'biomorphs.graphics)
  (in-ns 'biomorphs.biomorphs)
  (defn all-creatures
    "return a seq of creatures, with the parent in the middle of the children"
    [parent children]
    (let [[former latter] (split-at (/ (count children) 2) children)]
      (vec (concat former '(parent) latter))))

  )


(defn canvas-dims [ctx]
  (let [canvas (.-canvas ctx)]
    [(.-width canvas) (.-height canvas)]))

(defn cell-dims [ctx]
  (let [width     (.-width (.-canvas ctx))
        cxcell    (int (/ width 3))
        cycell    cxcell  ;; should probably really base this on height
        ]
    [cxcell cycell]))


(defn pos-from-index [n canvas-width cell-width cell-height]
  (let [per-row (int (/ canvas-width cell-width))
        row     (int (/ n per-row))
        col     (int (mod n per-row))]
    [(* col cell-width) (* row cell-height)]))

; use this for mouse event handlers
(defn pos-to-index [[x y] canvas-width cell-width cell-height]
  (let [per-row (int (/ canvas-width cell-width))
        col     (int (/ x cell-width))
        row     (int (/ y cell-height) )
        ]
    (+ col (* row per-row))
    ))

(defn pos-to-index-from-ctx
  [ctx pos]
  (let [[w _]   (canvas-dims ctx)
        [cx cy] (cell-dims ctx) ]
    (pos-to-index pos w cx cy)))

; mouse event handlers:
  ; on click
    ; determine creature clicked
    ; generate children for creature
    ; redraw
  ; on hover
    ; determine creature clicked
      ; highlight that creature in some way?
        ; how to represent that?
      ; draw it without clipping, maybe in an overlay??
      ; show the genes perhaps?
        ; might just want to do that anyway

(comment
  (pos-from-index 5 300 100 100)
  (pos-to-index [205 105] 300 100 100)
  (map :eq?
       (for [idx (range 10)
             :let [pos (pos-from-index idx 300 100 100)]
             ]
         {:idx idx, :pos pos, :eq? (= idx (pos-to-index pos 300 100 100))}
         ))

  (map #(pos-from-index % 500) (range 9))
  (int 42.9)
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
      (m/stroke))
  ctx)

; TODO: masked drawing so we don't overlap into another cell
(defn draw-creatures
  [{:keys [ctx genomes]}]
  (let [width           (.-width (.-canvas ctx))
        [cxcell cycell] (cell-dims ctx)
        creatures       (map gen/stream-creature genomes) ]
    (doseq [[n creature] (map-indexed vector creatures)]
      (let [[x y] (pos-from-index n width cxcell cycell)]
        (bounding-box ctx x y cxcell cycell)
        (render-creature ctx
                         (+ x (/ cxcell 2))
                         (+ y (/ cycell 2))
                         creature)))))


(defn clear-background [ctx]
  (let [[w h] (canvas-dims ctx)]
    (-> ctx
        ;; (m/fill-style "rgba(25,29,33,0.75)") ;; Alpha adds motion blur
        (m/fill-style "rgb(25,29,33)")
        (m/fill-rect {:x 0 :y 0 :w w :h h}))))

