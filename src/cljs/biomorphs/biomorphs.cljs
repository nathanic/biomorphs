(ns biomorphs.biomorphs
  (:require [monet.canvas :as m]
            [biomorphs.graphics :as gfx]
            [biomorphs.genetics :as gen]
            [biomorphs.utils :refer [log]]
            [goog.events :as ev]
            [goog.dom :as dom]
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


; IDEA: encode genome state in URL, for bookmarkable creatures
; and to allow back button
; or just pushState() and stash our state object
; that way we can at least go back
; it would also be cool to support middle-click to open a new tab at the desired state

(defn render
  [state]
  (gfx/clear-background (:ctx state))
  (gfx/draw-creatures state))

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

; TODO: also do some kind of hash encoding for bookmarkability?
(defn push-state [state]
  (.pushState js/history (serialize-state state) "" nil))

(defn on-popstate
  [the-state evt]
  (when (.-state evt)
    (.log js/console "restoring state" (.-state evt))
    (swap! the-state merge (deserialize-state (.-state evt)))
    (render @the-state)))



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

; called from an inline <script>
(defn ^:export init [canvas]
  (log "init start")
  (let [parent    (gen/default-genome)
        the-state (atom
                    {:genomes  (cons parent (gen/make-children parent))
                     :ctx      (m/get-context canvas "2d")
                     })
        ]
    (when (and js/document
               (.-getElementById js/document))
      (ev/listen canvas "click" (partial on-click-canvas the-state))
      (ev/listen js/window "popstate" (partial on-popstate the-state))
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
    #_(gfx/draw-creature {:ctx    ctx
                        :genome genome
                        :pos    [(/ cx 2)
                                 (/ cy 2) ]
                        })
    (gfx/render-creature ctx (/ cx 2) (/ cy 2) (gen/stream-creature genome))

    ))

(defn try-genome [genome]
  (debug-single-biomorph (dom/getElementByClass "biomorphs") genome))


(comment
  (in-ns 'biomorphs.biomorphs)
  (map :name gen/GENOTYPE)
  (try-genome [45 45 1 1 1 1 3 0.9 180])
  (try-genome [120 120 1 4 1 1 9 0.9 180])
  (try-genome [45 45 2 1 1 1 9 0.8 180])
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

  )

