(ns biomorphs.macros)

(defmacro forloop 
  "a traditional imperative for-loop"
  [[init test step] & body]
  `(loop [~@init]
      (when ~test
        ~@body
        (recur ~step))))

(defmacro for-indexes
  "an imperative for-loop optimized for the indexes of a collection"
  [[idxvar coll] & body]
  `(let [cnt# (count ~coll)]
     (loop [~idxvar 0]
       (when (< ~idxvar cnt#)
         ~@body
         (recur (inc ~idxvar))))))

(comment
  (in-ns 'biomorphs.macros)
  (let [c [4 5 6]] 
    (for-indexes [i c]
               (println "i" i "v" (get c i))
               ))

  (macroexpand-1
    '(for-indexes [i c]
               (println "i" i)
               ))

  )
