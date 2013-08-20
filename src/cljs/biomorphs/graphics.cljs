(ns biomorphs.graphics
  (:require [monet.canvas :as m]
            [biomorphs.genetics :as gen]
            [biomorphs.utils :refer [log clog]]
            ))

;;; there must be a better way than string concatenation
;;; but this is how every canvas example i've seen handles it...
(defn rgb [r g b]
  (str "rgb(" r "," g "," b ")"))

(defn rgba [r g b a]
  (str "rgba(" r "," g "," b "," a ")"))

(defn hsl [h s l]
  (str "hsla(" h "," (* 100 s) "%," (* 100 l) "%)"))

(defn hsla [h s l a]
  (str "hsla(" h "," (* 100 s) "%," (* 100 l) "%," a ")"))


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

; i think this one is currently unused
(defn pos-to-index-from-ctx
  [ctx pos]
  (let [[w _]   (canvas-dims ctx)
        [cx cy] (cell-dims ctx) ]
    (pos-to-index pos w cx cy)))


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

(defn clip-box  [ctx x y cx cy]
  (m/begin-path ctx)
  (.rect ctx x y cx cy)
  (.clip ctx))

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


; what about three.js and webgl?
(defn render-creatures
  [{:keys [ctx genomes]}]
  (let [width           (.-width (.-canvas ctx))
        [cxcell cycell] (cell-dims ctx)
        creatures       (map gen/stream-creature genomes) ]
    (doseq [[n creature] (map-indexed vector creatures)]
      (let [[x y] (pos-from-index n width cxcell cycell)]
        (bounding-box ctx x y cxcell cycell)
        (m/save ctx)
        (clip-box ctx x y cxcell cycell)
        (render-creature ctx
                         (+ x (/ cxcell 2))
                         (+ y (/ cycell 2))
                         creature)
        (m/restore ctx)
        ))))


(defn clear-background [ctx]
  (let [[w h] (canvas-dims ctx)]
    (-> ctx
        ;; (m/fill-style "rgba(25,29,33,0.75)") ;; Alpha adds motion blur
        (m/fill-style "rgb(25,29,33)")
        (m/fill-rect {:x 0 :y 0 :w w :h h}))))

