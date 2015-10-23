(ns ^:figwheel-always conway.core
  (:require     [om-tools.core :refer-macros [defcomponent]]
                [om.core :as om :include-macros true]
                [om-tools.dom :as dom :include-macros true]
                [cljs-time.core :as tt]))

(enable-console-print!)

(defonce app-state (atom {:boardsize 20 :cells {} :generation 0 :latest 0 :avg 0 :cycle 0}))

(defonce running? false)

(defn left? [cell boardsize]
  (> cell boardsize))

(defn upper? [cell boardsize]
  (> (mod cell boardsize) 0))

(defn right? [cell boardsize]
  (< cell (* boardsize (dec boardsize))))

(defn lower? [cell boardsize]
  (not= 0 (mod (inc cell) boardsize)))

(defn neighbours [cell boardsize]
  (let [upper (upper? cell boardsize)
        left (left? cell boardsize)
        right (right? cell boardsize)
        lower (lower? cell boardsize)
        ul (when (and upper left) (- cell boardsize 1))
        u (when upper (dec cell))
        ur (when (and upper right) (+ cell boardsize -1))
        le (when left (- cell boardsize))
        r (when right (+ cell boardsize))
        ll (when (and lower left) (- cell boardsize -1))
        l (when lower (inc cell))
        lr (when (and lower right) (+ cell boardsize 1))]
    (filter #(< 0 %) (list ul u ur le r ll l lr))))

(defn alive? [cell cells boardsize]
  (let [n (count (filter #(cells %) (neighbours cell boardsize)))]
    (if (cells cell) (contains? #{2 3} n) (= n 3))))

(defn playround [data]
  (let [board (:boardsize data)
        cells (:cells data)]
  (zipmap (filter #(alive? % cells board) (range 0 (* board board))) (repeat true))))

(defcomponent cell [data _]
  (render-state [_ _]
    (let [{:keys [cell cells]} data
          color (if (cells cell) "red" "black")]
    (dom/div {:style {:margin "1px" :height "10px" :width "10px" :background-color color}
              :onClick (fn [_] (swap! app-state assoc-in [:cells cell] true))}
             ""))))

(defcomponent row [data _]
  (render-state [_ _]
    (let [r (range 0 (:boardsize data))
          f (* (:boardsize data) (:row data))]
      (dom/div {:style {:flex-direction "column" :background-color "yellow"}}
             (map #(om/build cell (assoc data :cell (+ f %))) r)))))

(def header-css {:font-size 20 :color "black" :background-color "#AAAAAA" :padding "2px"})

(defn calc-avg [oldavg cycle nextgen]
  (let [new-avg (if (= 0 nextgen) cycle (quot (+ (* oldavg (dec nextgen)) cycle) nextgen))]
    (prn oldavg cycle nextgen new-avg)
    new-avg))

(defn next-state [s]
  (let [nextgen (inc (:generation s))
        oldavg (:avg s)
        now (tt/now)
        cycle (tt/in-millis (tt/interval (:latest s) now))
        avg (calc-avg oldavg cycle nextgen)]
  (assoc s :cells (playround s) :generation nextgen :latest now :avg avg :cycle cycle)))

(defcomponent board [data _]
  (render-state [_ _]
    (dom/div
    (dom/div {:style {:display "flex" :flex-direction "column"}}
             (dom/div {:style header-css} "Conways life")
             (let [r (range 0 (:boardsize data))]
               (dom/div {:style {:display "flex" :flex-direction "row" :border "1px black solid"}}
                        (map #(om/build row (assoc data :row %)) r))))
    (dom/div {:style {:display "flex" :flex-direction "row"}}
             (dom/div {} (dom/button {:onClick (fn [_] (do
                                                         (set! running? true)
                                                         (swap! app-state assoc :latest (tt/now))))}"spil"))
             (dom/div {} (dom/button {:onClick (fn [_] (set! running? false))}"stop"))
             (dom/div {} (dom/button {:onClick (fn [_] (doall
                                                         (swap! app-state assoc :latest (tt/now))
                                                         (swap! app-state next-state)))}"step"))))))

(defcomponent stats [data owner]
  (render-state [_ _]
    (dom/div {:style {:display "flex" :flex-direction "column"}}
             (dom/div {:style header-css} "Game stats")
             (dom/div {:style {:padding "2px"}}
             (dom/table {:style {:width "100%"}}
               (dom/tr
                 (dom/td "Live cells")(dom/td (count (data :cells))))
               (dom/tr
                 (dom/td "Generation")(dom/td (data :generation)))
               (dom/tr
                 (dom/td "Avg framerate")(dom/td (data :avg)))
               (dom/tr
                 (dom/td "current framerate")(dom/td (data :cycle))))))))

(defcomponent view [data owner]
  (render-state [_ _]
    (dom/div {:style {:display "flex" }}
             (dom/div {:style {:border "1px black solid" :background-color "#CCCCCC" :display "flex" :flex-direction "row"}}
                      (om/build board data)
                      (om/build stats data)))))

(om/root view app-state
         {:target (. js/document (getElementById "app"))})


(js/setInterval (fn [] (when running? (swap! app-state next-state))), 0)

