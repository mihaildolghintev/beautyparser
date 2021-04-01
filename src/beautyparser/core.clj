(ns beautyparser.core
  (:require [clj-http.client :as client]
            [hickory.core :as h]
            [hickory.select :as s]))

(def links "https://beloris.ru/catalog/uhod")
(def link "https://beloris.ru/item/gel-dlya-ruk-antisepticheskiy-6")

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

(defn get-product-images [parsed-link]
  (->> (s/select (s/child
                  (s/class "gallery-big-container")
                  (s/class "gallery-big-img")) parsed-link)
       (map #(get-in % [:attrs :data-src]))))

(defn get-product-type [info-line]
  (-> (:type info-line) second :content first))

(defn get-product-info-lines [parsed-link]
  (let [parsed-map {(create-product-info-keyword (-> parsed-link (get-in [:content]) second (get-in [:content]) second (get-in [:content]) first))
                    (-> parsed-link (get-in [:content]) (-> butlast last) (get-in [:content] first))}]
    (if (contains? parsed-map :type )
      (assoc parsed-map :type (get-product-type parsed-map))
      (update parsed-map (first (keys parsed-map)) first))))


(defn create-product-info-keyword [key-str]
  (let [format-key-str (apply str (-> key-str
                  (-> butlast butlast)
                  ))
        ]
    (case format-key-str
      "Бренд" :brand
      "Линия" :line
      "Тип" :type
      "Объем" :volume
      "Количество" :qty
      "Страна бренда" :brand-country
      :none)))


(defn get-product-brand [parse-link]
  (->> (s/select (s/child
                  (s/class "product-preview-info__row"))
                 parse-link)
       (map get-product-info-lines)
       (reduce merge)
       ;(map get-product-info-lines)
       ))

(comment
  (-> link
      (parse-link)
      (get-product-brand)))
