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

(defn clip-box  [ctx x y cx cy]
  (m/begin-path ctx)
  (.rect ctx x y cx cy)
  (.clip ctx))

(defn clear-background [ctx]
  (let [[w h] (canvas-dims ctx)]
    (-> ctx
        (m/fill-style "rgb(25,29,33)")
        (m/fill-rect {:x 0 :y 0 :w w :h h}))))

; what about three.js/webgl instead?
(defn render-creature [ctx creature]
  {:pre [ctx creature]}
  (let [[x0 y0 x1 y1]       (gen/measure-creature creature)
        creature-w          (- x1 x0)
        creature-h          (- y1 y0)
        center-x            (/ (+ x0 x1) 2)
        center-y            (/ (+ y0 y1) 2)
        [canvas-w canvas-h] (canvas-dims ctx)
        ]
    (clear-background ctx)
    (-> ctx
        (m/save)
        (m/translate (/ canvas-w 2) (/ canvas-h 2))
        (m/rotate Math/PI))
    (when (or (> creature-w canvas-w) (> creature-h canvas-h))
      (m/scale ctx (/ canvas-w creature-w) (/ canvas-h creature-h)))
    (doseq [{:keys [x0 y0 x1 y1 color]} creature ]
      (-> ctx
          (m/begin-path)
          (m/stroke-style (apply hsla color))
          (m/move-to (- x0 center-x) (- y0 center-y))
          (m/line-to (- x1 center-x) (- y1 center-y))
          (m/stroke)))
    (m/restore ctx)
    ctx))

(defn render-gene
  [ctx x y w h {:keys [fraction]}]
  (let [pixline (+ y (* fraction h))]
    (-> ctx
        (m/fill-style "blue")
        (m/stroke-rect {:x x, :y y, :w w, :h h})
        (m/fill-rect {:x x, :y pixline, :w w, :h (- h pixline)})
        ;; (m/fill-style "black")
        ;; (m/fill-rect {:x x, :y y, :w w, :h pixline})
        )))

(defn render-genome 
  "render a visualization of the genome itself,
  filling the supplied canvas context completely"
  [ctx genome]
  (let [vizes   (gen/genome-visualization genome)
        [w h]   (canvas-dims ctx)
        gene-w  (/ w (count genome)) ]
    (dotimes [n (count genome)]
      (render-gene ctx (* n gene-w) 0 gene-w h (get vizes n))
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; graphics ideas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; we get asymmetry from depth-first drawing
;   might want to switch to breadth-first
; large :expansion genes lead to lots of the lines falling below nyquist
;   and very faint looking creatures
;   could do scaling myself,
;     and not use (scale),
;     but the varying line width was sometimes interesting visually
;   could just vary line width myself
; could do a more interesting job of picking colors
;   http://devmag.org.za/2012/07/29/how-to-choose-colours-procedurally-algorithms/
;   https://news.ycombinator.com/item?id=6309989


