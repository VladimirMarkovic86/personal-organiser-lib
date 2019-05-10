(ns personal-organiser-lib.core
  (:require [personal-organiser-middle.organism.entity :as pomoe]
            [personal-organiser-middle.grocery.entity :as pomge]
            [personal-organiser-middle.meal.entity :as pomme]))

(defn calculate-organism-age
  "Calculate age of organism"
  [organism-birthday]
  (when (and organism-birthday
             (instance?
               java.util.Date
               organism-birthday))
    (let [diff (- (.getTime
                    (java.util.Date.))
                  (.getTime
                    organism-birthday))
          year-diff (int
                      (/ diff
                         1000   ; ms
                         60     ; s
                         60     ; m
                         24     ; h
                         365.25 ; d
                       ))]
      year-diff))
 )

(defn calculate-basal-metabolic-usage
  "Calculate basal metabolic usage"
  [organism]
  (let [organism-height (:height organism)
        organism-weight (:weight organism)
        organism-gender (:gender organism)
        organism-birthday (:birthday organism)
        organism-age (calculate-organism-age
                       organism-birthday)
        bmu (atom 0)]
    (when (and organism-height
               (number?
                 organism-height)
               organism-weight
               (number?
                 organism-weight)
               organism-gender
               (string?
                 organism-gender)
               organism-birthday
               (instance?
                 java.util.Date
                 organism-birthday))
      (when (= organism-gender
               pomoe/gender-male)
        (reset!
          bmu
          (+ 66
             (* organism-weight
                13.7)
             (* organism-height
                5)
             (* organism-age
                6.8
                -1))
         ))
      (when (= organism-gender
               pomoe/gender-female)
        (reset!
          bmu
          (+ 655
             (* organism-weight
                9.6)
             (* organism-height
                1.8)
             (* organism-age
                4.7
                -1))
         ))
     )
    @bmu))

(defn get-physical-activity-coefficient
  "Get physical activity coefficient for particular organism"
  [organism]
  (let [organism-gender (:gender organism)
        organism-activity (first
                            (:activity organism))
        coefficient (atom 1)]
    (when (= organism-gender
             pomoe/gender-male)
      (when (= pomoe/activity-mainly-sitting
               organism-activity)
        (reset!
          coefficient
          1.4))
      (when (= pomoe/activity-easy-physical-labor
               organism-activity)
        (reset!
          coefficient
          1.52))
      (when (= pomoe/activity-medium-physical-labor
               organism-activity)
        (reset!
          coefficient
          1.72))
      (when (= pomoe/activity-hard-physical-labor
               organism-activity)
        (reset!
          coefficient
          1.97))
      (when (= pomoe/activity-very-hard-physical-labor
               organism-activity)
        (reset!
          coefficient
          2.32))
     )
    (when (= organism-gender
             pomoe/gender-female)
      (when (= pomoe/activity-mainly-sitting
               organism-activity)
        (reset!
          coefficient
          1.33))
      (when (= pomoe/activity-easy-physical-labor
               organism-activity)
        (reset!
          coefficient
          1.42))
      (when (= pomoe/activity-medium-physical-labor
               organism-activity)
        (reset!
          coefficient
          1.61))
      (when (= pomoe/activity-hard-physical-labor
               organism-activity)
        (reset!
          coefficient
          1.85))
      (when (= pomoe/activity-very-hard-physical-labor
               organism-activity)
        (reset!
          coefficient
          2.18))
     )
    @coefficient))

(defn get-energy-usage
  "Calculates organism energy usage"
  [organism]
  (let [basal-metabolic-usage (calculate-basal-metabolic-usage
                                organism)
        physical-activity-coefficient (get-physical-activity-coefficient
                                        organism)]
    (* basal-metabolic-usage
       physical-activity-coefficient))
 )

(defn get-training-energy-usage
  "Calculates training energy usage for particular organism"
  [organism]
  0)

(defn calculate-daily-needs
  "Calculate daily needs for particular organism"
  [organism]
  (let [energy-usage (get-energy-usage
                       organism)
        training-energy-usage (get-training-energy-usage
                                organism)]
    (+ energy-usage
       training-energy-usage))
 )

(defn get-body-mass-index
  "Calculates body mass index of particular organism"
  [organism]
  (let [organism-height (:height organism)
        organism-weight (:weight organism)]
    (when (and organism-height
               (number?
                 organism-height)
               (pos?
                 organism-height)
               organism-weight
               (number?
                 organism-weight)
               (pos?
                 organism-weight))
      (let [organism-height-cm (/ organism-height
                                  100)]
        (double
          (/ organism-weight
             (* organism-height-cm
                organism-height-cm))
         ))
     ))
 )

(defn adjust-meal-to-organism-needs
  "Adjusts meal size for particular meal type and particular organism"
  [organism
   meal
   meals
   max-calories]
  (when (and meals
             (instance?
               clojure.lang.Atom
               meals)
             max-calories
             (number?
               max-calories)
             (pos?
               max-calories))
    (let [body-mass-index (get-body-mass-index
                            organism)
          organism-diet (:diet organism)
          meal-diet (:diet meal)]
      (when (or (and (= organism-diet
                        pomge/diet-vegetarian)
                     (= organism-diet
                        meal-diet))
                (= organism-diet
                   pomge/diet-not-vegetarian))
        (let [{mname :mname
               label-code :label-code
               mtype :mtype
               fats-sum :fats-sum
               carbonhydrates-sum :carbonhydrates-sum
               proteins-sum :proteins-sum
               calories-sum :calories-sum} meal
              proportion-coefficient (/ calories-sum
                                        max-calories)
              proportion-fats-sum (/ fats-sum
                                     proportion-coefficient)
              proportion-carbonhydrates-sum (/ carbonhydrates-sum
                                               proportion-coefficient)
              proportion-proteins-sum (/ proteins-sum
                                         proportion-coefficient)
              proportion-calories-sum (/ calories-sum
                                         proportion-coefficient)
              adjusted-meal-size (atom
                                   {:mname mname
                                    :label-code label-code
                                    :fats-sum (double
                                                proportion-fats-sum)
                                    :carbonhydrates-sum (double
                                                          proportion-carbonhydrates-sum)
                                    :proteins-sum (double
                                                    proportion-proteins-sum)
                                    :calories-sum (double
                                                    proportion-calories-sum)})
              ingredients (:ingredients meal)
              adjusted-ingredients (atom [])]
          (doseq [{gname :gname
                   label-code :label-code
                   calories :calories
                   proteins :proteins
                   fats :fats
                   carbonhydrates :carbonhydrates
                   grams :grams} ingredients]
            (swap!
              adjusted-ingredients
              conj
              {:gname gname
               :label-code label-code
               :calories (/ calories
                            proportion-coefficient)
               :proteins (/ proteins
                            proportion-coefficient)
               :fats (/ fats
                        proportion-coefficient)
               :carbonhydrates (/ carbonhydrates
                                  proportion-coefficient)
               :grams (/ grams
                         proportion-coefficient)})
           )
          (swap!
            adjusted-meal-size
            assoc
            :ingredients
            @adjusted-ingredients)
          (when (< body-mass-index
                   18)
            (swap!
              meals
              conj
              @adjusted-meal-size))
          (when (and (>= body-mass-index
                         18)
                     (<= body-mass-index
                         25))
            (swap!
              meals
              conj
              @adjusted-meal-size))
          (when (> body-mass-index
                   25)
            (swap!
              meals
              conj
              @adjusted-meal-size))
         ))
     ))
 )

(defn calculate-meal-recommendations
  "Calculate meal sizes that are recommended to particular organism"
  [meals
   organism
   & [breakfast-calories-share
      lunch-calories-share
      dinner-calories-share]]
  (let [daily-needs (calculate-daily-needs
                      organism)
        breakfast-calories (* daily-needs
                              (or breakfast-calories-share
                                  0.35))
        lunch-calories (* daily-needs
                          (or lunch-calories-share
                              0.40))
        dinner-calories (* daily-needs
                           (or dinner-calories-share
                               0.25))
        breakfast-meals (atom [])
        lunch-meals (atom [])
        dinner-meals (atom [])]
    (doseq [meal meals]
      (let [meal-type (:mtype meal)
            meal-type-set (into
                            #{}
                            meal-type)]
        (when (contains?
                meal-type-set
                pomme/mtype-breakfast)
          (adjust-meal-to-organism-needs
            organism
            meal
            breakfast-meals
            breakfast-calories))
        (when (contains?
                meal-type-set
                pomme/mtype-lunch)
          (adjust-meal-to-organism-needs
            organism
            meal
            lunch-meals
            lunch-calories))
        (when (contains?
                meal-type-set
                pomme/mtype-dinner)
          (adjust-meal-to-organism-needs
            organism
            meal
            dinner-meals
            dinner-calories))
       ))
    {:breakfast-recommendations @breakfast-meals
     :lunch-recommendations @lunch-meals
     :dinner-recommendations @dinner-meals}))

(defn calculate
  "Main function that calculates meals recommendation for particular organism"
  [organism
   meals
   & [breakfast-calories-share
      lunch-calories-share
      dinner-calories-share]]
  (let [results (calculate-meal-recommendations
                  meals
                  organism
                  breakfast-calories-share
                  lunch-calories-share
                  dinner-calories-share)
        {breakfast-recommendations :breakfast-recommendations
         lunch-recommendations :lunch-recommendations
         dinner-recommendations :dinner-recommendations} results]
    results))

