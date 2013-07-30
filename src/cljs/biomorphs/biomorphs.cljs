(ns biomorphs.biomorphs
  (:require [mondrian.anim :as anim]
            [mondrian.canvas :as canvas]
            [mondrian.color :as color]
            [mondrian.math :as math]
            [mondrian.plot :as plot]
            [mondrian.ui :as ui]
            [monet.canvas :as m]
            [biomorphs.graphics :as gfx]
            [biomorphs.genetics :as gen]
            [biomorphs.utils :refer [log]]
            )
  (:use-macros [mondrian.macros :only [defmondrian]]))

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


;; ---------------------------------------------------------------------
;; Update pipeline
;;
;; Initial state:
;;    :drawing -- The top-level DOM element that contains the canvas,
;;      the sliders, etc.
;;    :ctx -- The canvas context.
;;    :w -- The width of the canvas.
;;    :h -- The height of the canvas.
;;
;; Configuration values (as defined on the mondrian element):
;;    :persist-image -- Whether or not to clear the background in
;;      between frames.
;;    ... fill this in ...
;;
;; After merge-control-values:
;;    :... -- One key for each control (identified by the control name).
;;      Zero or more controls may be present under the mondrian element,
;;      in which case the missing values must have been provided in as
;;      defaults in the element itself.
;;
;; After ...:

(defn merge-control-values
  "Merge the current values of the controls into state."
  [{:keys [drawing] :as state}]
  (merge state (ui/update-controls drawing)))

(defn update-pipeline
  [state]
  (-> state
      merge-control-values
      ;; (assoc )

      ))


;; ---------------------------------------------------------------------
;; Render stack
;;

(defn clear-background
  [{:keys [ctx w h persist-image]}]
  (when-not persist-image
    (-> ctx
        (m/fill-style "rgba(25,29,33,0.75)") ;; Alpha adds motion blur
        (m/fill-rect {:x 0 :y 0 :w w :h h}))))

(defn render-stack
  [state]
  (clear-background state)
  (gfx/draw-creatures state)
  )

;; ---------------------------------------------------------------------
;; Main entry point
;;

(let [parent [4, 1 2 3, 4 5 6, 7 8 9]]
  (defmondrian biomorphs-anim
    {:parent   parent
     :children (gen/make-children parent)
     }
    update-pipeline
    render-stack))


; i guess you're supposed to call your init() yourself from js
(defn ^:export init []
  (log "init called")
  ; maybe set up click handlers and such?
  )

(comment
  (do (require 'cemerick.piggieback) (cemerick.piggieback/cljs-repl))
  (js/alert "yoyo")
  (.log js/console "logging yo!")
  (+ 2 2)
  (stop-updating)
  )
