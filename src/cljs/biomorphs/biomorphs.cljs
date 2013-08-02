(ns biomorphs.biomorphs
  (:require [monet.canvas :as m]
            [biomorphs.graphics :as gfx]
            [biomorphs.genetics :as gen]
            [biomorphs.utils :refer [log]]
            [goog.events :as ev]
            [goog.dom :as dom]
            ))

;; plan for doing the evolution stuff
;; multiple canvases or single canvas?
;;  multiple canvases would simplify click handling
;;  and clipping
;;  have to do that myself otherwise
;; when do we abandon mondrian?
;;  it's convenient for testing drawing algorithms
;;  but we don't really need an animation loop
;;  probably not great with multiple canvi
;;  though i guess we could have both
;;  switch back to mondrian & a single genome when needed
;;
;; generate mutant descendents
;; draw them all to canvi
;; on canvas click:
;;  make that genome the new parent
;;  respawn mutants
;;  could save the evo path for an animation or something
;;
;; would also be cool to indicate the genome values

(comment ;; nmap ,e va(<c-c><c-c>

  (browser-repl)
  (in-ns 'biomorphs.biomorphs)

  )


(defn render
  [{:keys [ctx] :as state}]
  (gfx/clear-background ctx)
  (gfx/draw-creatures state)
  )

(defn offset-pos [evt]
  [(.-offsetX evt)
   (.-offsetY evt)])

; IDEA: encode genome state in URL, for bookmarkable creatures
; and to allow back button

; something about this click handler hangs the browser
; it can't be gen/make-children or render
(defn on-click-canvas
  [the-state evt]
  (log "canvas has been clicked offset " (offset-pos evt))
  (let [{:keys [ctx parent children]} @the-state
        pos                           (offset-pos evt)
        idx                           (gfx/pos-to-index-from-ctx ctx pos)
        new-parent                    (nth (cons parent children) idx) ]
    ;; (log "updating state with new children")
    (swap! the-state
           assoc
           :parent   new-parent
           :children (gen/make-children new-parent)))
  ;; (log "rendering the new state")
  (render @the-state)
  (log "end of click handler")
  )


; TODO? just have :genomes and the parent is (first (:genomes state))

; called from an inline <script>
(defn ^:export init [canvas]
  (log "init start")
  (let [parent    (gen/default-genome)
        the-state (atom
                    {:parent   parent
                     :children (gen/make-children parent)
                     :ctx      (m/get-context canvas "2d")
                     })
        ]
    (when (and js/document
               (.-getElementById js/document))
      (ev/listen canvas "click" (partial on-click-canvas the-state))
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
        genome  (or genome (gen/default-genome))
        ]
    (gfx/clear-background ctx)
    (m/stroke-style ctx "red")
    (m/stroke-rect ctx {:x 0, :y 0, :w cx, :h cy})
    (gfx/draw-creature {:ctx    ctx
                        :genome genome
                        :pos    [(/ cx 2)
                                 (/ cy 2) ]
                        })))

(defn try-genome [genome]
  (debug-single-biomorph (dom/getElementByClass "biomorphs") genome))

(comment
  (map :name gen/GENOTYPE)
  (try-genome [45 45 1 1 1 1 3 0.9 180])
  (try-genome [45 45 1 1 1 1 4 0.9 180])
  (try-genome [45 45 2 1 1 1 2 0.8 180])
  (try-genome [45 45 2 1 1 1 2 1.5 180])
  (try-genome [-80 45 -4 2 1 1 6 0.9 180])
  (try-genome [90 45 1 1 1 1 2 2 180])
  (try-genome [65 25 1 1 1 1 9 0.9 180])
  (try-genome [59 45 1 1 1 1 2 2 180])
  )

(comment
  (do (require 'cemerick.piggieback) (cemerick.piggieback/cljs-repl))
  (js/alert "yoyo")
  (.log js/console "logging yo!")
  (+ 2 2)
  (stop-updating)
  )
