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

(defn on-click-canvas
  [the-state evt]
  (log "canvas has been clicked offset " (offset-pos evt))
  (let [{:keys [ctx parent children]} @the-state
        pos                           (offset-pos evt)
        idx                           (gfx/pos-to-index-from-ctx ctx pos)
        new-parent                    (nth (cons parent children) idx) ]
    (swap! the-state
           assoc
           :parent   new-parent
           :children (gen/make-children new-parent)))
  (render @the-state)
  )


; called from an inline <script>
(defn ^:export init [canvas]
  (let [parent [4, 1 2 3, 4 5 6, 7 8 9]
        the-state  (atom
                     {:parent parent
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

(comment
  (do (require 'cemerick.piggieback) (cemerick.piggieback/cljs-repl))
  (js/alert "yoyo")
  (.log js/console "logging yo!")
  (+ 2 2)
  (stop-updating)
  )
