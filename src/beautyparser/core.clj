(ns beautyparser.core
  (:require [clj-http.client :as client]
            [hickory.core :as h]
            [hickory.select :as s]
            [clojure.string :as str]))

(def links "https://beloris.ru/catalog/uhod")
(def link "https://beloris.ru/item/gel-dlya-ruk-antisepticheskiy-6")
(def link2 "https://beloris.ru/item/ton-10-18/child")

(defn parse-link [link]
  (-> (client/get link {:headers {:authority "beloris.ru"
                                  "User-Agent" "whatever"}})
      :body
      (h/parse)
      (h/as-hickory)))

(defn get-product-links [parsed-link]
  (->> (s/select (s/child
                  (s/class "render-catalog-product-list")
                  (s/class "product-flex")
                  (s/class "product")
                  (s/tag :a))
                 parsed-link)
       (map #(get-in % [:attrs :href]))))


(defn create-product-info-keyword [key-str]
  (let [format-key-str (apply str (-> key-str
                                      (-> butlast butlast)))]

    (case format-key-str
      "Бренд" :brand
      "Линия" :line
      "Тип" :type
      "Объем" :volume
      "Количество" :qty
      "Вес" :weight
      "Страна бренда" :brand-country
      :none)))

(defn get-product-info-lines [parsed-link]
  (let [get-product-type (fn [info-line] (-> (:type info-line) second :content first))
        parsed-map {(create-product-info-keyword (-> parsed-link :content second :content second :content first))
                    (-> parsed-link :content (-> butlast last) :content)}]
    (if (contains? parsed-map :type)
      (assoc parsed-map :type (get-product-type parsed-map))
      (update parsed-map (first (keys parsed-map)) first))))

(defn get-product-data [parsed-link]
  (->> (s/select (s/child
                  (s/class "product-preview-info__row"))
                 parsed-link)
       (map get-product-info-lines)
       (reduce merge)
       ;(map get-product-info-lines)
       ))

(defn get-product-images [parsed-link]
  "Get images for product"
  (->> (s/select (s/child
                  (s/class "gallery-big-container")
                  (s/class "gallery-big-img")) parsed-link)
       (map #(get-in % [:attrs :data-src]))))

(defn get-product-price [parsed-link]
  (->> (s/select (s/child
                  (s/class "product-preview-info__price-actual"))
                 parsed-link)
       first
       :content
       first
       (str/trim)
       (butlast)
       (butlast)
       (reduce str)
       (Float/parseFloat)))

(defn get-product-description [parsed-link]
  (->> (s/select (s/child
                  (s/class "product-description__block-info")
                  (s/class "markup")
                  (s/tag :p))
                 parsed-link)
       (map :content)
       (map first)
       (map #(str/replace % #"\r\n\t" ""))))

(defn get-product-howto [parsed-link]
  (->> (s/select (s/child
                  (s/class "product-description__structure"))
                 parsed-link)
       first
       :content
       butlast
       last
       :content
       first
       (str/trim)))

(defn get-product-structure [parsed-link]
  (->> (s/select (s/child
                  (s/class "product-description__structure"))
                 parsed-link)
       second
       :content
       butlast
       last
       :content
       first
       (str/trim)))

(defn get-product-name [parsed-link]
  (->> (s/select (s/child
                  (s/class "product-preview-info__header-title"))
                 parsed-link)
       first
       :content
       last
       (str/trim)))

(defn get-product-short-name [parsed-name]
  (->> (s/select (s/child
                  (s/class "product-preview-sub-offer")
                  (s/class "product-preview-sub-offer__scroll")
                  )
                 parsed-name)
       ))

(comment
  (-> link2
      (parse-link)
      (get-product-short-name)))
