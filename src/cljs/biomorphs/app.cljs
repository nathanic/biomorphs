(ns biomorphs.app
  (:require [monet.canvas :as m]
            [biomorphs.graphics :as gfx]
            [biomorphs.genetics :as gen]
            [biomorphs.utils :refer [index-of log now]]
            [goog.events :as ev]
            [goog.dom :as dom]
            [clojure.string :refer [join]])
  (:require-macros [biomorphs.macros :refer [forloop for-indexes]]))

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



(defn render
  [{:keys [genomes contexts] :as state}]
  {:pre [(vector? genomes)
         (vector? contexts)
         (= (count genomes) (count contexts)) ]}
  (for-indexes [i genomes]
    (gfx/render-creature
      (get contexts i)
      (gen/stream-creature (get genomes i)))))

(defn offset-pos
  [evt]
  [(.-offsetX evt)
   (.-offsetY evt)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pushState support
(defn serialize-state [state]
  ; can't serialize canvas contexts
  (pr-str (dissoc state :contexts)))

(defn deserialize-state [ser-state]
  ; coerce :genomes to be a vector, which we rely on elsewhere
  (update-in (cljs.reader/read-string ser-state)
             [:genomes]
             vec))

; it would also be cool to support middle-click to open a new tab at the desired state
(defn generate-location-hash [state]
  (str "#"
       (join ","
             (for [g (:genomes state)]
               (str "["
                    (join "," (map (partial format "%3.4f") g))
                    "]")))))

(defn parse-location-hash [state frag]
  ; XXX what if we have a mismatch between parsed genomes and available canvi?
  ; TODO generate offspring of first genome to fill gaps
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
        ; TODO see comment in parse-location-hash
        (swap! the-state merge (deserialize-state (.-state evt)))
        (render @the-state)),
    ))



(defn on-click-canvas
  [the-state evt]
  (let [{:keys [contexts genomes]}  @the-state
        pos                         (offset-pos evt)
        target-ctx                  (m/get-context (.-target evt) "2d")
        idx                         (index-of contexts target-ctx)
        new-parent                  (get genomes idx) ]
    (log "canvas" idx "has been clicked at offset" (offset-pos evt))
    (swap! the-state assoc :genomes (gen/reproduce new-parent (count genomes))))
  (render @the-state)
  (push-state @the-state)
  )

;; we'll make a button on the html and bind it to randomize all genomes
(defn on-click-randomize [the-state evt]
  (log "randomize button clicked")
  (swap! the-state
         update-in [:genomes]
         (fn [gs] (mapv #(gen/random-genome) gs)))
  (push-state @the-state)
  (render @the-state)
  )

; called from an inline <script>
(defn ^:export init [canvi]
  (log "init start")
  (let [parent    (gen/default-genome)
        the-state (atom
                    {:genomes  (gen/reproduce parent (count canvi))
                     :contexts (mapv #(m/get-context % "2d") canvi)
                     })
        ]
    (when (not-empty (.-hash js/location))
      (let [state @the-state]
        (log "restoring state from location hash")
        (swap! the-state merge (parse-location-hash state (.-hash js/location)))))
    (when (and js/document (.-getElementById js/document))
      ; will need to bind click handlers to all canvi
      ; could actually just pass in the selector and find the elements here...
      (doseq [canvas canvi]
        (ev/listen canvas "click" (partial on-click-canvas the-state)))
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
(defn ^:export debug-single-biomorph [canvas canvas-genome genome]
  (let [ctx     (m/get-context canvas "2d")
        [cx cy] (gfx/canvas-dims ctx)
        genome  (or genome 
                    [163.8498,308.7442,-2.6838,-4.7789,8.9192,-0.8244,9.8865,1.0608,104.7208]
                    [65 25 1 1 1 1 9 0.9 180]
                    )
        ]
    (gfx/render-creature ctx (gen/stream-creature genome))
    (gfx/render-genome (m/get-context canvas-genome "2d") genome)
    ))

; debug helper
(defn try-genome [genome]
  (debug-single-biomorph (dom/getElementByClass "biomorphs") 
                         (dom/getElementByClass "biomorphs-genome")
                         genome))



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
        genome-a (or genome-a 
                     [109.8498,308.7442,-2.6838,-3.8789,8.0192,-1.7244,9.8865,1.0608,104.7208]
                     [65 25 1 1 1 1 9 0.9 180])
        genome-b (or genome-b 
                     [208.5745,318.0857,-7.4932,-5.5968,-0.2429,3.6921,7.4762,1.0930,140.3989]
                     [120 120 1 4 1 1 9 0.9 180])
        the-state (atom
                    {:genomes  [genome-a genome-b]
                     :contexts [(m/get-context canvas "2d")] }) ]
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
                                     (gfx/render-creature ctx (gen/stream-creature genome)))
                                   )
                   )))
    (swap! the-state assoc :anim-req-id
           (animate-genome genome-a genome-b ANIMATION-DURATION
                    (fn [genome]
                      (gfx/render-creature ctx (gen/stream-creature genome)))
                    ))))


(comment
  (in-ns 'biomorphs.app)
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


