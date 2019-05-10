(ns personal-organiser-lib.core-test
  (:require [clojure.test :refer :all]
            [personal-organiser-lib.core :refer :all]
            [personal-organiser-middle.organism.entity :as pomoe]
            [personal-organiser-middle.grocery.entity :as pomge]))

(deftest test-calculate-organism-age
  (testing "Test calculate organism age"
    
    (let [organism-birthday nil
          result (calculate-organism-age
                   organism-birthday)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [organism-birthday (java.util.Date.)
          result (calculate-organism-age
                   organism-birthday)]
      
      (is
        (= result
           0)
       )
      
     )
    
    (let [organism-birthday (java.util.Date.
                              "October 16, 1986, 04:45:00 GMT")
          result (calculate-organism-age
                   organism-birthday)]
      
      (is
        (= result
           32)
       )
      
     )
    
   ))

(deftest test-calculate-basal-metabolic-usage
  (testing "Test calculate basal metabolic usage"
    
    (let [organism {:height nil
                    :weight nil
                    :gender nil
                    :birthday nil}
          result (calculate-basal-metabolic-usage
                   organism)]
      
      (is
        (= result
           0)
       )
      
     )
    
    (let [organism {:height 182
                    :weight 90
                    :gender pomoe/gender-male
                    :birthday (java.util.Date.
                                "October 16, 1986, 04:45:00 GMT")}
          result (calculate-basal-metabolic-usage
                   organism)]
      
      (is
        (= result
           1991.4)
       )
      
     )
    
   ))

(deftest test-get-physical-activity-coefficient
  (testing "Test get physical activity coefficient"
    
    (let [organism {:gender nil
                    :activity nil}
          result (get-physical-activity-coefficient
                   organism)]
      
      (is
        (= result
           1)
       )
      
     )
    
    (let [organism {:gender pomoe/gender-male
                    :activity [pomoe/activity-mainly-sitting]}
          result (get-physical-activity-coefficient
                   organism)]
      
      (is
        (= result
           1.4)
       )
      
     )
    
    (let [organism {:gender pomoe/gender-male
                    :activity [pomoe/activity-easy-physical-labor]}
          result (get-physical-activity-coefficient
                   organism)]
      
      (is
        (= result
           1.52)
       )
      
     )
    
    (let [organism {:gender pomoe/gender-male
                    :activity [pomoe/activity-medium-physical-labor]}
          result (get-physical-activity-coefficient
                   organism)]
      
      (is
        (= result
           1.72)
       )
      
     )
    
    (let [organism {:gender pomoe/gender-male
                    :activity [pomoe/activity-hard-physical-labor]}
          result (get-physical-activity-coefficient
                   organism)]
      
      (is
        (= result
           1.97)
       )
      
     )
    
    (let [organism {:gender pomoe/gender-male
                    :activity [pomoe/activity-very-hard-physical-labor]}
          result (get-physical-activity-coefficient
                   organism)]
      
      (is
        (= result
           2.32)
       )
      
     )
    
    (let [organism {:gender pomoe/gender-female
                    :activity [pomoe/activity-mainly-sitting]}
          result (get-physical-activity-coefficient
                   organism)]
      
      (is
        (= result
           1.33)
       )
      
     )
    
    (let [organism {:gender pomoe/gender-female
                    :activity [pomoe/activity-easy-physical-labor]}
          result (get-physical-activity-coefficient
                   organism)]
      
      (is
        (= result
           1.42)
       )
      
     )
    
    (let [organism {:gender pomoe/gender-female
                    :activity [pomoe/activity-medium-physical-labor]}
          result (get-physical-activity-coefficient
                   organism)]
      
      (is
        (= result
           1.61)
       )
      
     )
    
    (let [organism {:gender pomoe/gender-female
                    :activity [pomoe/activity-hard-physical-labor]}
          result (get-physical-activity-coefficient
                   organism)]
      
      (is
        (= result
           1.85)
       )
      
     )
    
    (let [organism {:gender pomoe/gender-female
                    :activity [pomoe/activity-very-hard-physical-labor]}
          result (get-physical-activity-coefficient
                   organism)]
      
      (is
        (= result
           2.18)
       )
      
     )
    
   ))

(deftest test-get-energy-usage
  (testing "Test get energy usage"
    
    (let [organism {:height nil
                    :weight nil
                    :gender nil
                    :birthday nil
                    :activity nil}
          result (get-energy-usage
                   organism)]
      
      (is
        (= result
           0)
       )
      
     )
    
    (let [organism {:height 182
                    :weight 90
                    :gender pomoe/gender-male
                    :birthday (java.util.Date.
                                "October 16, 1986, 04:45:00 GMT")
                    :activity [pomoe/activity-mainly-sitting]}
          result (get-energy-usage
                   organism)]
      
      (is
        (= result
           2787.96)
       )
      
     )
    
   ))

(deftest test-get-training-energy-usage
  (testing "Test get training energy usage"
    
    (let [organism nil
          result (get-training-energy-usage
                   organism)]
      
      (= result
         0)
      
     )
    
   ))

(deftest test-calculate-daily-needs
  (testing "Test calculate daily needs"
    
    (let [organism nil
          result (calculate-daily-needs
                   organism)]
      
      (= result
         1)
      
     )
    
    (let [organism {:height 182
                    :weight 90
                    :gender pomoe/gender-male
                    :birthday (java.util.Date.
                                "October 16, 1986, 04:45:00 GMT")
                    :activity [pomoe/activity-mainly-sitting]}
          result (calculate-daily-needs
                   organism)]
      
      (= result
         2787.96)
      
     )
    
   ))

(deftest test-get-body-mass-index
  (testing "Test get body mass index"
    
    (let [organism nil
          result (get-body-mass-index
                   organism)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [organism {:height -182
                    :weight 90}
          result (get-body-mass-index
                   organism)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [organism {:height 182
                    :weight -90}
          result (get-body-mass-index
                   organism)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [organism {:height -182
                    :weight -90}
          result (get-body-mass-index
                   organism)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [organism {:height 182
                    :weight 90}
          result (get-body-mass-index
                   organism)]
      
      (is
        (= result
           27.17063156623596)
       )
      
     )
    
   ))

(deftest test-adjust-meal-to-organism-needs
  (testing "Test adjust meal to organism needs"
  
    (let [organism nil
          meal nil
          meals nil
          max-calories nil
          result (adjust-meal-to-organism-needs
                   organism
                   meal
                   meals
                   max-calories)]
      
      (is
        (nil?
          result)
       )
      
     )
  
    (let [organism {:height 182
                    :weight 90
                    :gender pomoe/gender-male
                    :birthday (java.util.Date.
                                "October 16, 1986, 04:45:00 GMT")
                    :activity [pomoe/activity-mainly-sitting]
                    :diet pomge/diet-not-vegetarian}
          meal {:diet pomge/diet-not-vegetarian
                :mname "Milk with corn flakes"
                :label-code -1
                :mtype ["dinner"]
                :fats-sum 5.7
                :carbonhydrates-sum 66.57
                :proteins-sum 9.06
                :calories-sum 362.5
                :ingredients [{:gname "Cow's milk"
                               :label-code -1
                               :calories 100.5
                               :proteins 5.1
                               :fats 5.4
                               :carbonhydrates 7.2
                               :grams 150}
                              {:gname "Corn flakes"
                               :label-code -1
                               :calories 222
                               :proteins 3.96
                               :fats 0.3
                               :carbonhydrates 49.62
                               :grams 60}
                              {:gname "Sugar"
                               :label-code -1
                               :calories 40
                               :proteins 0
                               :fats 0
                               :carbonhydrates 9.75
                               :grams 10}]}
          meals (atom [])
          max-calories 700
          result (adjust-meal-to-organism-needs
                   organism
                   meal
                   meals
                   max-calories)]
      
      (is
        (= result
           [{:mname "Milk with corn flakes"
             :label-code -1
             :fats-sum 11.006896551724138
             :carbonhydrates-sum 128.54896551724136
             :proteins-sum 17.495172413793103
             :calories-sum 699.9999999999999
             :ingredients [{:gname "Cow's milk"
                            :label-code -1
                            :calories 194.06896551724137
                            :proteins 9.848275862068965
                            :fats 10.427586206896551
                            :carbonhydrates 13.903448275862068
                            :grams 289.6551724137931}
                           {:gname "Corn flakes"
                            :label-code -1
                            :calories 428.6896551724138
                            :proteins 7.646896551724137
                            :fats 0.5793103448275861
                            :carbonhydrates 95.81793103448274
                            :grams 115.86206896551722}
                           {:gname "Sugar"
                            :label-code -1
                            :calories 77.24137931034483
                            :proteins 0.0
                            :fats 0.0
                            :carbonhydrates 18.82758620689655
                            :grams 19.310344827586206}]}])
       )
      
     )
    
   ))

(deftest test-calculate-meal-recommendations
  (testing "Test calculate meal recommendations"
    
    (let [meals nil
          organism nil
          result (calculate-meal-recommendations
                   meals
                   organism)]
      
      (is
        (= result
           {:breakfast-recommendations []
            :lunch-recommendations []
            :dinner-recommendations []})
       )
      
     )
    
    (let [meals [{:diet pomge/diet-not-vegetarian
                  :mname "Milk with corn flakes"
                  :label-code -1
                  :mtype ["dinner"]
                  :fats-sum 5.7
                  :carbonhydrates-sum 66.57
                  :proteins-sum 9.06
                  :calories-sum 362.5
                  :ingredients [{:gname "Cow's milk"
                                 :label-code -1
                                 :calories 100.5
                                 :proteins 5.1
                                 :fats 5.4
                                 :carbonhydrates 7.2
                                 :grams 150}
                                {:gname "Corn flakes"
                                 :label-code -1
                                 :calories 222
                                 :proteins 3.96
                                 :fats 0.3
                                 :carbonhydrates 49.62
                                 :grams 60}
                                {:gname "Sugar"
                                 :label-code -1
                                 :calories 40
                                 :proteins 0
                                 :fats 0
                                 :carbonhydrates 9.75
                                 :grams 10}]}]
          organism {:height 182
                    :weight 90
                    :gender pomoe/gender-male
                    :birthday (java.util.Date.
                                "October 16, 1986, 04:45:00 GMT")
                    :activity [pomoe/activity-mainly-sitting]
                    :diet pomge/diet-not-vegetarian}
          result (calculate-meal-recommendations
                   meals
                   organism)]
      
      (is
        (= result
           {:breakfast-recommendations []
            :lunch-recommendations []
            :dinner-recommendations [{:mname "Milk with corn flakes"
                                      :label-code -1
                                      :fats-sum 10.959566896551724
                                      :carbonhydrates-sum 127.99620496551722
                                      :proteins-sum 17.41994317241379
                                      :calories-sum 696.99
                                      :ingredients [{:gname "Cow's milk"
                                                     :label-code -1
                                                     :calories 193.23446896551724
                                                     :proteins 9.805928275862067
                                                     :fats 10.382747586206897
                                                     :carbonhydrates 13.843663448275862
                                                     :grams 288.40965517241375}
                                                    {:gname "Corn flakes"
                                                     :label-code -1
                                                     :calories 426.84628965517237
                                                     :proteins 7.614014896551724
                                                     :fats 0.5768193103448275
                                                     :carbonhydrates 95.40591393103448
                                                     :grams 115.3638620689655}
                                                    {:gname "Sugar"
                                                     :label-code -1
                                                     :calories 76.90924137931034
                                                     :proteins 0.0
                                                     :fats 0.0
                                                     :carbonhydrates 18.746627586206895
                                                     :grams 19.227310344827586}]}]
            })
       )
      
     )
    
   ))

(deftest test-calculate
  (testing "Test calculate"
    
    (let [meals nil
          organism nil
          result (calculate
                   organism
                   meals)]
      
      (is
        (= result
           {:breakfast-recommendations []
            :lunch-recommendations []
            :dinner-recommendations []})
       )
      
     )
    
    (let [meals [{:diet pomge/diet-not-vegetarian
                  :mname "Milk with corn flakes"
                  :label-code -1
                  :mtype ["dinner"]
                  :fats-sum 5.7
                  :carbonhydrates-sum 66.57
                  :proteins-sum 9.06
                  :calories-sum 362.5
                  :ingredients [{:gname "Cow's milk"
                                 :label-code -1
                                 :calories 100.5
                                 :proteins 5.1
                                 :fats 5.4
                                 :carbonhydrates 7.2
                                 :grams 150}
                                {:gname "Corn flakes"
                                 :label-code -1
                                 :calories 222
                                 :proteins 3.96
                                 :fats 0.3
                                 :carbonhydrates 49.62
                                 :grams 60}
                                {:gname "Sugar"
                                 :label-code -1
                                 :calories 40
                                 :proteins 0
                                 :fats 0
                                 :carbonhydrates 9.75
                                 :grams 10}]}]
          organism {:height 182
                    :weight 90
                    :gender pomoe/gender-male
                    :birthday (java.util.Date.
                                "October 16, 1986, 04:45:00 GMT")
                    :activity [pomoe/activity-mainly-sitting]
                    :diet pomge/diet-not-vegetarian}
          result (calculate
                   organism
                   meals)]
      
      (is
        (= result
           {:breakfast-recommendations []
            :lunch-recommendations []
            :dinner-recommendations [{:mname "Milk with corn flakes"
                                      :label-code -1
                                      :fats-sum 10.959566896551724
                                      :carbonhydrates-sum 127.99620496551722
                                      :proteins-sum 17.41994317241379
                                      :calories-sum 696.99
                                      :ingredients [{:gname "Cow's milk"
                                                     :label-code -1
                                                     :calories 193.23446896551724
                                                     :proteins 9.805928275862067
                                                     :fats 10.382747586206897
                                                     :carbonhydrates 13.843663448275862
                                                     :grams 288.40965517241375}
                                                    {:gname "Corn flakes"
                                                     :label-code -1
                                                     :calories 426.84628965517237
                                                     :proteins 7.614014896551724
                                                     :fats 0.5768193103448275
                                                     :carbonhydrates 95.40591393103448
                                                     :grams 115.3638620689655}
                                                    {:gname "Sugar"
                                                     :label-code -1
                                                     :calories 76.90924137931034
                                                     :proteins 0.0
                                                     :fats 0.0
                                                     :carbonhydrates 18.746627586206895
                                                     :grams 19.227310344827586}]}]
            })
       )
      
     )
    
   ))

