(ns pendulum.core
  (:require [quil.core :as q] 
            [pendulum.util :as u]
            [org.nfrac.cljbox2d.core :as b]
            [org.nfrac.cljbox2d.vec2d :as v]
            [quil.middleware :refer [fun-mode]]))



(def WIDTH 640)
(def HEIGHT 480)

(def PENPTS 450)



(defn- randi [high & [low]]
  (let [low (or low 0)]
    (+ low (rand-int (- high low)))))

(defn- randf [high & [low]]
  (let [low (or low 0.0)]
    (+ low (* (- high low) (rand)))))


(def RAD 0.5)


(defn setup []
  (q/frame-rate 30)
  (q/color-mode :hsb)

  (let [world (b/new-world)

        posa [0 0]
        anchor (b/body! world
                        {:position posa
                         :type :static}
                        {:shape (b/circle 0.001)
                         :group-index -1})

        ba (b/body! world
                    {:position posa}
                    {:shape (b/circle RAD)
                     :group-index -1})

        pos1 [(randf 8 -8) (randf 9 3)]
        b1 (b/body! world
                    {:position pos1}
                    {:shape (b/circle RAD)
                     :group-index -1})

        pos2 (mapv #(* 2 %) (v/v-scale pos1))
        b2 (b/body! world
                    {:position pos2}
                    {:shape (b/circle RAD)
                     :group-index -1})

        ja (b/joint! {:type :revolute
                      :body-a anchor
                      :body-b ba
                      :anchor-a posa
                      :anchor-b posa
                      :collide-connected false})

        j1 (b/joint! {:type :distance
                      :body-a ba
                      :anchor-a (-> (v/v-sub pos1 posa) v/v-scale (v/v-scale RAD))
                      :body-b b1
                      :anchor-b [0 0]
                      :length (v/v-dist posa pos1)})

        j2 (b/joint! {:type :distance
                      :body-a b1
                      :anchor-a [0 0]
                      :body-b b2
                      :anchor-b [0 0]
                      :length (v/v-dist pos1 pos2)})]

    (assoc u/initial-state
           :world world
           :pen/body b2
           :pen/velocs []
           :pen/points [])))

(defn update-physics [state]
  ; Update sketch state by changing circle color and position.
  (u/world-step state))

(defn update-pen [state]
  (let [pt (-> state :pen/body b/center)
        vel (-> state :pen/body b/linear-velocity v/v-mag)
        points (conj (:pen/points state) pt)
        points (if (> (count points) PENPTS)
                 (vec (rest points))
                 points)
        velocs (conj (:pen/velocs state) vel)
        velocs (if (> (count velocs) PENPTS)
                 (vec (rest velocs))
                 velocs)]
    (assoc state
           :pen/points points
           :pen/velocs velocs)))

(defn draw-pen [state]
  (let [pts (:pen/points state)
        vels (:pen/velocs state)
        cnt (count pts)]
    (doseq [[i xy] (map-indexed vector pts)]
      (let [cam (:camera state)
            [x y] (u/world-to-px cam xy)
            clr (q/color 255 0 255 (* (/ i cnt) 255))
            rad (min 8 (/ (nth vels i) 2.25))]
        (q/no-stroke)
        (q/fill clr)
        (q/ellipse x y rad rad)))))

(defn draw-state [state]
  (q/background 255) 
  (u/draw state)
  (draw-pen state))


;;;;------------------------------------------------------------------------;;;;

(defn draw-sketch [state]
  (draw-state state))

(defn update-sketch [state]
  (if-not (:paused? state)
    (do (update-physics state)
        (update-pen state))
    state))

(defn key-pressed [state evt]
  (update state :paused? not))


(q/defsketch pendulum
  :title "Pendulum Experiments"
  :size [WIDTH HEIGHT]
  :setup setup

  :update update-sketch
  :draw draw-sketch

  :mouse-wheel u/mouse-wheel
  :mouse-dragged u/mouse-dragged
  :mouse-pressed u/mouse-pressed
  :mouse-released u/mouse-released
  :mouse-dragged u/mouse-dragged

  :key-pressed key-pressed

  :features [:keep-on-top]
  :middleware [fun-mode])

