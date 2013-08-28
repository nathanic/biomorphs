(ns biomorphs.biomorphs
  (:require [monet.canvas :as m]
            [biomorphs.graphics :as gfx]
            [biomorphs.genetics :as gen]
            [biomorphs.utils :refer [log]]
            [goog.events :as ev]
            [goog.dom :as dom]
            [clojure.string :refer [join]]
            ))

;; UI Ideas
;; genome bar plot
;; genome editor
;; genome explorer view
  ;; that is, panes for + and - for each gene
  ;; for directed, non-random mutation
;; more status information
;; back button to revert state
;; completely randomize genome set

; would this benefit from core.async?

; what about morphing the creatures between genomes?
; could render several intermediate states interpolated between the old and new genomes

; what about automatic evolution to match a given input picture?

; protection against too-faint colors?

; would it be faster to expand the genome for measurement
; rather than creating and GC'ing the entire body?

(defn render
  [state]
  (gfx/clear-background (:ctx state))
  (gfx/render-creatures state))

(defn offset-pos
  [evt]
  [(.-offsetX evt)
   (.-offsetY evt)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pushState support
(defn serialize-state [state]
  ; can't serialize canvas context
  (pr-str (dissoc state :ctx)))

(defn deserialize-state [ser-state]
  (cljs.reader/read-string ser-state))

; it would also be cool to support middle-click to open a new tab at the desired state
(defn generate-location-hash [state]
  (str "#"
       (join ","
             (for [g (:genomes state)]
               (str "["
                    (join "," (map (partial format "%3.4f") g))
                    "]")))))

(defn parse-location-hash [state frag]
  (if (empty? frag)
    state
    (assoc state
           :genomes (cljs.reader/read-string
                      (str "[" (subs frag 1) "]")))))


(defn push-state [state]
  (log "saving state:" (serialize-state state))
  (.pushState js/history (serialize-state state) "" (generate-location-hash state)))

(defn on-popstate
  [the-state evt]
  (log "popstate evt fired")
  (cond
    (.-state evt)
    (do (.log js/console "restoring state from pop" (.-state evt))
        (swap! the-state merge (deserialize-state (.-state evt)))
        (render @the-state)),
    ))


(defn on-click-canvas
  [the-state evt]
  (log "canvas has been clicked offset " (offset-pos evt))
  (let [{:keys [ctx genomes]}   @the-state
        pos                     (offset-pos evt)
        idx                     (gfx/pos-to-index-from-ctx ctx pos)
        new-parent              (nth genomes idx) ]
    (swap! the-state
           assoc :genomes (cons new-parent (gen/make-children new-parent))))
  (render @the-state)
  (push-state @the-state)
  )

;; we'll make a button on the html and bind it to randomize all genomes
(defn on-click-randomize [the-state evt]
  (log "randomize button clicked")
  (swap! the-state
         update-in [:genomes]
         (fn [gs] (map #(gen/random-genome) gs)))
  (push-state @the-state)
  (render @the-state)
  )

; called from an inline <script>
(defn ^:export init [canvas]
  (log "init start")
  (let [parent    (gen/default-genome)
        the-state (atom
                    {:genomes  (cons parent (gen/make-children parent))
                     :ctx      (m/get-context canvas "2d")
                     })
        ]
    (when (not-empty (.-hash js/location))
      (let [state @the-state]
        (log "restoring state from location hash")
        (swap! the-state merge (parse-location-hash state (.-hash js/location)))))
    (when (and js/document
               (.-getElementById js/document))
      (ev/listen canvas "click" (partial on-click-canvas the-state))
      (ev/listen js/window "popstate" (partial on-popstate the-state))
      (ev/listen (dom/getElement "randomize")
                 "click"
                 (partial on-click-randomize the-state))
      )
    ;; invoke first draw
    (render @the-state)
    )
  (log "init end")
  )



; temporary debug hooks, for use with single.html
(defn ^:export debug-single-biomorph [canvas genome]
  (let [ctx     (m/get-context canvas "2d")
        [cx cy] (gfx/canvas-dims ctx)
        genome  (or genome
                    [65 25 1 1 1 1 9 0.9 180]
                    #_(gen/default-genome))
        ]
    (gfx/clear-background ctx)
    (m/stroke-style ctx "red")
    (m/stroke-rect ctx {:x 0, :y 0, :w cx, :h cy})
    (gfx/render-creature ctx (/ cx 2) (/ cy 2) (gen/stream-creature genome))
    ))

(defn- pre-draw [ctx]
  (let [[cx cy] (gfx/canvas-dims ctx) ]
    (gfx/clear-background ctx)
    (m/stroke-style ctx "red")
    (m/stroke-rect ctx {:x 0, :y 0, :w cx, :h cy})))

; construct a creature body that's an x in a box
(defn- xbox [x0 y0 x1 y1]
  (let [segs [[x0 y0, x1 y0, [120 50 50 1.0]]
              [x0 y0, x0 y1, [140 50 50 1.0]]
              [x1 y0, x1 y1, [160 50 50 1.0]]
              [x0 y1, x1 y1, [170 50 50 1.0]]
              [x0 y0, x1 y1, [180 50 50 1.0]]
              [x1 y0, x0 y1, [200 50 50 1.0]]
              ]]
    (for [[a b c d col] segs]
      {:color col
       :x0 a, :y0 b,
       :x1 c, :y1 d})))

(defn xbox-at [center-x center-y]
  (let [q 600]
    (xbox (- center-x q) (+ center-y q)
          (+ center-x q) (- center-y q))))

(comment
  (in-ns 'biomorphs.biomorphs)
  (+ 2 2)
  (xbox-at 0 0)
  (xbox-at 1000 1000)
  )

(defn ^:export debug-scaling [canvas canvas-scaled genome]
  (let [ctx         (m/get-context canvas "2d")
        ctx-scaled  (m/get-context canvas-scaled "2d")
        ; NB: lazily assuming identical canvas sizes
        [cx cy]     (gfx/canvas-dims ctx)
        the-state   (atom {:genomes [(or genome
                                         [65 25 1 1 1 1 9 0.9 180])]})
        draw-both  (fn []
                     (pre-draw ctx)
                     (pre-draw ctx-scaled)
                     (let [creature (xbox-at 300 300)
                                     #_(gen/stream-creature (-> @the-state :genomes first))]
                       (gfx/render-creature ctx (/ cx 2) (/ cy 2) creature)
                       (gfx/render-creature-scaled ctx-scaled (/ cx 2) (/ cy 2) creature)))
        ]
    (when (not-empty (.-hash js/location))
      (swap! the-state merge (parse-location-hash @the-state (.-hash js/location))))
    (ev/listen (dom/getElement "redraw") "click" #(draw-both))
    (ev/listen (dom/getElement "randomize") "click"
               (fn []
                 (swap! the-state assoc :genomes [(gen/random-genome)])
                 (push-state @the-state)
                 (draw-both)))
    (draw-both)
    ))

(defn try-genome [genome]
  (debug-single-biomorph (dom/getElementByClass "biomorphs") genome))


(defn now [] (.getTime (js/Date.)))

;; this works, but morphing all genes at once can be  a bit discontinuous-looking
;; my guess is that scaling the gradient is part of that.

;; perhaps we morph one gene at a time for smoothness...
(defn animate-genome [genome-from genome-to duration renderer]
  (let [start-time (now)
        end-time   (+ start-time duration) ]
    (letfn [(handler []
              (let [t     (now)
                    coeff (/ (- t start-time) duration)
                    genome (gen/interpolate-genomes-gradual
                             genome-from
                             genome-to
                             (min 1.0 coeff)) ]
                (renderer genome)
                (if (< coeff 1.0)
                  (.requestAnimationFrame js/window handler)
                  ; maybe then reverse it, and just keep doing that
                  (animate-genome genome-to genome-from duration renderer)
                  ))) ]
      (.requestAnimationFrame js/window handler)
      )))


;; another debugging hook
(def ANIMATION-DURATION 5000)

(defn ^:export debug-animation [canvas genome-a genome-b]
  (let [ctx      (m/get-context canvas "2d")
        [cx cy]  (gfx/canvas-dims ctx)
        genome-a (or genome-a [65 25 1 1 1 1 9 0.9 180])
        genome-b (or genome-b [120 120 1 4 1 1 9 0.9 180])
        the-state (atom
                    {:genomes  [genome-a genome-b]
                     :ctx      (m/get-context canvas "2d") }) ]
    (ev/listen (dom/getElement "randomize")
               "click"
               (fn []
                 (log "randomize button" (:anim-req-id @the-state))
                 (when-let [ari (:anim-req-id @the-state)]
                   (.cancelAnimationFrame js/window ari)
                   (animate-genome (gen/random-genome)
                                   (gen/random-genome)
                                   ANIMATION-DURATION
                                   (fn [genome]
                                     (gfx/clear-background ctx)
                                     (gfx/render-creature ctx (/ cx 2) (/ cy 2)
                                                          (gen/stream-creature genome)))
                                   )
                   )))
    (gfx/clear-background ctx)
    (swap! the-state assoc :anim-req-id
           (animate-genome genome-a genome-b ANIMATION-DURATION
                    (fn [genome]
                      (gfx/clear-background ctx)
                      (gfx/render-creature ctx (/ cx 2) (/ cy 2)
                                           (gen/stream-creature genome)))
                    ))))


(comment
  (in-ns 'biomorphs.biomorphs)
  (map :name gen/GENOTYPE)
  (try-genome [45 45 1 1 1 1 3 0.9 180])
  (try-genome [120 120 1 4 1 1 9 0.9 180]) (try-genome [45 45 2 1 1 1 9 0.8 180])
  (try-genome [45 45 2 1 1 1 2 1.5 180])
  (try-genome [-80 45 -4 2 1 1 6 0.9 180])
  (try-genome [90 45 1 1 1 1 2 2 180])

  (try-genome [65 25 1 2 3 1 9 0.9 180])

  (try-genome [59 45 1 1 1 1 2 2 180])

  (ev/listen js/window "click" (fn [] (try-genome [65 25 1 2 3 1 9 0.9 180])))

  ; let's get some numbers, even if they're kinda shitty
  ; ... oh, i could have used (time)
  (defn cheesey-benchmark [f]
    (let [start   (.getTime (js/Date.))
          result  (f)
          end     (.getTime (js/Date.))
          elapsed (- end start)]
      {:result result, :elapsed elapsed}))

  ; 1575 ms in chrome to generate 20 creatures of complexity 9
  ; 5750 ms in firefox!
  ; fiddling with JIT settings didn't help (disabling JIT hurt very slightly)
  (/ 5760. 1565) ; 3.7
  (:elapsed
    (cheesey-benchmark
      (fn []
        (dotimes [_ 20] (doall (gen/stream-creature [120 120 1 4 1 1 9 0.9 180])))
        )))

  ; 2242 ms in chrome to draw 20 creatures of complexity 9
  ; 8804 ms in firefox.  what the crap.
  (/ 8804. 2242) ; 3.9, so canvas code is also proportionally worse
  (:elapsed
    (cheesey-benchmark
      (fn []
        (dotimes [_ 20] (try-genome [120 120 1 4 1 1 9 0.9 180]))
        )))

  (gen/valid-genome? [120 120 1 4 1 1 9 0.9 180])
  (gen/get-genetic-expansion [120 120 1 4 2 3 9 0.9 180])


  )
; stream-creature using nth in turn-direction 1802 ms
; stream-creature using get in turn-direction 1786 ms
; stream-creature using get
  ; and rewritten util/index-of 1494 ms
;   ; not bad
;   ; enough to significantly smooth out the animation
  ; then i improved turn-direction to a simple nested hashmap thing
    ; now we're at 1176 ms

; profiling suggests i should take a look at measure-creature


