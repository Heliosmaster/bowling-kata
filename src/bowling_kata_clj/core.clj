(ns bowling-kata-clj.core
  (:require [clojure.string :as str]))

(defn parse-frames [filename]
  (let [frames (rest (str/split (slurp filename) #"\|"))]
    (map (comp #(str/split % #" ")
               str/trim)
         (if (= "\n" (last frames))
           (drop-last frames)
           frames))))


(defn pin-knocked [throws]
  (reduce (fn [acc throw]
            (conj acc
                  (condp = throw
                    "X" 10
                    "/" (- 10 (last acc))
                    (Integer/parseInt throw))))
          []
          throws))

(defn compute-score [frame next-two-throws]
  (let [throws (pin-knocked frame)]
    (+ (reduce + throws)
       (cond
         (empty? next-two-throws) 0
         (contains? (set frame) "X") (reduce + next-two-throws)
         (contains? (set frame) "/") (first next-two-throws)
         :else 0))))

(defn run [frames]
  (loop [coll frames
         sum 0]
    (let [frame (first coll)
          next-two-throws (take 2 (flatten (rest (map pin-knocked coll))))
          score (compute-score frame next-two-throws)]
      (if (seq (rest coll))
        (recur (rest coll)
               (+ sum score))
        (+ sum score)))))

(defn -main [filename]
  (println (run (parse-frames filename))))
