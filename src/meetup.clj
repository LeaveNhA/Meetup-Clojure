(ns meetup)

(def sch->off
  {:first 1
   :second 8
   :third 15
   :fourth 22
   :teenth 13
   :last 30})

(defn- diff-
  "Returns difference schedule and days."
  [sch days]
  (let [sch-d (sch sch->off)]
    (map #(vector (Math/abs (- % sch-d)) %) days)))

(defn- keyword-day->calendar-day
  "Keyword to dayOfWeek with clojure magic."
  [day]
  (->> day name clojure.string/upper-case (symbol "java.time.DayOfWeek") eval))

(defn- monthdays
  "It's obvious!"
  [[year month]]
  (.lengthOfMonth (java.time.YearMonth/of year month)))

(defn meetup [month year day nth]
  (vector
   year
   month
   (->> [year month] monthdays
        inc
        (range 1)
        (map #(java.time.LocalDate/of year month %))
        (filter #(= (.getDayOfWeek %) (keyword-day->calendar-day day)))
        (map #(.getDayOfMonth %))
        (diff- nth)
        (filter (comp (partial <= (if (= nth :last) 0 (nth sch->off))) second))
        (filter (comp (partial <= 0) second))
        (sort-by first)
        first
        second)))