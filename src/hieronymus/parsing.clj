(ns hieronymus.parsing
  (:require [clojure.string :as string]
            [clojure.walk :as walk]
            [clj-time.core :as t]
            [clj-time.coerce :as tc]
            [clj-yaml.core :as yaml]
            [hiccup.core :as h]))

(defn take-to-first [pred coll]
  (if (seq coll)
    (map last
         (take-while
           (comp (complement pred) first)
           (cons (list (first coll)) (partition 2 1 coll))))
    (lazy-seq)))

(defn- reconstitute-fenced-blocks [lines]
  (if-let [fst (first lines)]
    (if (= "```" fst)
      (let [block (vec (take-while #(not= "```" %) (rest lines)))
            rest (drop (+ 2 (count block)) lines)
            joined (str "π:" (string/join "\n" block))]
        (cons joined (reconstitute-fenced-blocks rest)))
      (cons (first lines) (reconstitute-fenced-blocks (rest lines))))))

(def ^:private t-re #"^\|.*")

(defn- reconstitute-tables [lines]
  (if-let [fst (first lines)]
    (if (re-matches t-re fst)
      (let [block (vec (take-while #(re-matches t-re %) lines))
            rest (drop (count block) lines)
            joined (str "†:" (string/join "\n" block))]
        (cons joined (reconstitute-tables rest)))
      (cons (first lines) (reconstitute-tables (rest lines))))))

(defn- str->month [str]
  (let [months ["January" "February" "March" "April" "May"
                "June" "July" "August" "September" "October"
                "November" "December"]]
    (+ 1 (.indexOf months str))))

(defn expand-date [date-str]
  "dates must be of type January 1, 2015"
  (let [date-re #"([A-Z][a-z]{1,}) ([0-9]{1,2}), ([0-9]{4})"
        [_ month-name d yyyy] (re-find date-re date-str)
        [year month day] [(Integer/parseInt yyyy)
                          (str->month month-name)
                          (Integer/parseInt d)]
        date (t/date-time year month day)]
    {:string date-str :date date :unix (tc/to-long date)
     :month-name month-name :year year :month month :day day}))

(defn- expand-table [table]
  (let [lines (->> (string/split table #"\n")
                   (map #(string/replace % #"^\||\|$" ""))
                   (map #(string/split % #"\|"))
                   (walk/postwalk #(if (string? %) (string/trim %) %)))
        header (first lines)
        styler (->> (second lines)
                    (map (fn [col]
                           (let [fore (re-find #"^:" col)
                                 aft (re-find #":$" col)]
                             (cond
                               (and fore aft) :center
                               fore :left
                               aft :right)))))]
    {:header header
     :styler styler
     :content (drop 2 lines)}))

(defn- line->metadatum [line]
  (let [[key & raw-value] (string/split (string/replace line #"^~" "") #":")
        keyword (keyword key)
        value (str (string/trim (string/join ":" raw-value)))]
    (hash-map
      keyword
      (cond
        (= :date keyword) (expand-date value)
        (re-find #"^\[(.*)\]$" value)
        (string/split (string/replace value #"(^\[)|(\]$)" "") #"\s+")
        :else value))))

(defn- tagged-and-edited [p]
  (cond
    (re-find #"^([\s]+)?~" p)
    {:metadata (line->metadatum p)}
    (re-find #"^ƒ" p)
    {:figure (string/replace p #"^ƒ" "")}
    (re-find #"^π:" p)
    {:pre (string/replace p #"^π:" "")}
    (re-find #"^†:" p)
    {:table (expand-table (string/replace p #"^†:" ""))}
    ;(re-find "^Q:")
    ;(re-find "^A:")
    (re-find #"___" p)
    {:line true}
    (re-find #"^>.*" p)
    {:quotation (string/replace p #"^>[\s]+" "")}
    (re-find #"^[-*].*" p)
    {:bullet (string/replace p #"^(-|\*)[\s]+" "")}
    (re-find #"^[0-9]+\." p)
    {:number p}
    (re-find #"^\|\>" p)
    {:aside-right (string/replace p #"^\|\>[\s]+" "")}
    (re-find #"^\<\|" p)
    {:aside-left (string/replace p #"^\<\|[\s]+" "")}
    ;(re-find #"\|" p)
    ;{:table-item (string/replace)}
    (re-find #"^\<" p)
    {:inline-html p}
    (re-find #"^\+\+\+" p)
    {:separator "~~~"}
    (re-find #"^\[\^(.*)\]:(.*)" p)
    (let [parse (re-find #"^\[\^(.*)\]:(.*)" p)]
      {:footnote {:tag (nth parse 1)
                  :text (nth parse 2)}})
    (re-find #"^\{\/.*\}$" p)
    {:section-end {:section (re-find #"[^\{\}/]+" p)}}
    (re-find #"^\{.*\}" p)
    (let [[section title align] (string/split (re-find #"[^\{\}/]+" p) #":")]
      {:section-start {:section section :title title :align (or align "right")}})
    (re-find #"^\[.*\]\(.*\)$" p)
    (let [parse (re-find #"\[(.*)\]\((.*)\)" p)]
      {:link {:tag (nth parse 1)
              :href (nth parse 2)}})
    :else {:p p}))

(defn- query-cache [url]
  (str url "?c=" (rand-int 100000000)))

(defn- soundcloud-url [id]
  (format
    "https://w.soundcloud.com/player/?url=https%3A//api.soundcloud.com/tracks/%s&amp;color=928e8e&amp;auto_play=false&amp;hide_related=false&amp;show_comments=true&amp;show_user=true&amp;show_reposts=false"
    id))

(def ^:private inline-expansions
  [[:bold
    #"[*_]{2}([^\*_]+)[*_]{2}" #(vec [:strong.inlined (nth % 1)])]
   [:italic
    #"[*_]{1}([^\*_\)]+)[*_]{1}" #(vec [:em.inlined (nth % 1)])]
   [:alternate
    #"([|]{1,})([^|]+)[|]{1,}"
    #(vec [:span.alt {:class (str "alt-" (count (nth % 1)))} (nth % 2)])]
   [:code-snippet
    #"`([^`]+)`"
    #(vec [:span.code (nth % 1)])]
   [:inline-link
    #".*{1,}\[([^\]]+)\]\(([^\)]+)\)"
    (fn [[_ text href]]
      (if (re-matches #".*.mp3$|.*.m4a$" href)
        [:span.audio-span
         [:span.sep-title text]
         [:audio {:src (query-cache href)
                  :preload "auto"
                  :controls "true"
                  :title text}]]
        [:a {:href href :target "_blank"} text]))]
   [:embed
    #"«([^:]+):([^»]+)»"
    (fn [[_ type file]]
      (case (keyword type)
        :img [:img {:src (query-cache file)}]
        :iframe [:iframe.embed {:src (query-cache file)}]
        :audio [:strong {:style "color:red"} "YOCHANGETHIS"]
        :soundcloud [:iframe.embed.soundcloud
                     {:width "100%" :height "166"
                      :scrolling "no" :frameborder "no"
                      :src (soundcloud-url file)}]))]])

(defn- expand-all-inlines [text]
  (reduce
    (fn [acc [_ find replace]]
      (string/replace
        acc find
        (fn [& args]
          (h/html (apply replace args)))))
    text
    inline-expansions))

(defn- maybe-newyorkerize-emdashes [str config]
  (if (:newyorkerize config)
    (string/replace str #"[\s]+—[\r\s]+" "—")
    str))

(defn- escape-html-chars [str]
  (string/escape str
                 {\— "&mdash;"
                  \& "&amp;"
                  \“ "&ldquo;"
                  \” "&rdquo;"
                  \‘ "&lsquo;"
                  \’ "&rsquo;"
                  \> "&gt;"
                  \< "lt;"
                  \… "&hellip;"}))

(defn annotate-parentheticals [str]
  (string/replace
    str #"\(([^\)]+)\)"
    (fn [[_ parenthetical]]
      (h/html [:span.parenthesis {} "("]
              [:span.parenthetical {} parenthetical]
              [:span.parenthesis {} ")"]))))

(defn- add-drop-cap [str]
  (string/replace
    str #"∂([A-Z])"
    (fn [[_ cap]]
      (h/html [:span.dropcap {} cap]))))

(defn- hydrate-link [str links colors]
  (string/replace
    str
    #"\[([^\]]+)\]\[([^\]]+)\]"
    (fn [[_ txt key]]
      (let [href (get links key "#")
            data (if-let [color (get colors (keyword href))]
                   {:href href :style (format "color:%s" color)}
                   {:href href})]
        (h/html [:a data txt])))))

(defn- linkify-footnotes [str footnotes]
  (string/replace
    str #"\[\^([^\]]+)\]"
    (fn [[_ tag]]
      (if-let [foot (get footnotes tag)]
        (h/html [:a.footnote
                 {:href (format "#%s" tag)
                  :id (format "back-%s" tag)}
                 [:sup {} (:index foot)]])
        str))))

(defn- enrich-text [text config links footnotes]
  (let [colors (get-in config [:link :colors] {})
        feet-index-pairs (->> (map :footnote footnotes)
                              (map #(vector (:tag %) %))
                              (into {}))]
    (walk/postwalk
      (fn [el]
        (if (or (not (string? el)) (re-matches #"^<(.*)>$" el))
          el
          (-> el
              (maybe-newyorkerize-emdashes config)
              ;(escape-html-chars)
              (hydrate-link links colors)
              (expand-all-inlines)
              (linkify-footnotes feet-index-pairs)
              ;(annotate-parentheticals)
              )))
      text)))

(defn- hydrate-table-html [style table]
  (clojure.pprint/pprint table)
  (let [styler (:styler table)
        col-fn (fn [tag idx col]
                 [tag {:align (nth styler idx)} col])
        header [:thead {} (map-indexed (partial col-fn :th) (:header table))]
        content (->> (:content table)
                     (map
                       (fn [row]
                         (let [diff (- (count row) (count styler))
                               cols (drop-last diff row)
                               call (first (take-last diff row))]
                           [:tr {:class
                                 (case call
                                   "!" "highlight"
                                   "standard")}
                            (map-indexed (partial col-fn :td) cols)]))))]
    [:table
     style
     (cons header content)]))

(defn- tag&content->hiccup [tag style content]
  (case tag
    :p [:p.classic style content]
    :pre [:pre style content]
    :table (hydrate-table-html style content)
    :figure [:figure style content]
    :quotation [:blockquote style content]
    :bullet [:li.bullet style [:span.punct "•"] content]
    :number
    (let [[_ number text] (re-find #"^([0-9]+)\.(.*)" content)]
      [:li.number style [:span.punct number] text])
    :line [:hr]
    :aside-right [:div.aside.aside-right style [:div.aside-inner content]]
    :aside-left [:div.aside.aside-left style [:div.aside-inner content]]
    :inline-html [:div.inline-html style content]
    :separator [:div.separator style content]
    :footnote [:div.footnote
               (assoc style :id (:tag content))
               [:a.footnote-backref
                {:href (format "#back-%s" (:tag content))}
                (:index content)]
               (:text content)]
    :section-end [:div.section-end
                  (merge style {:data-section (:section content)})
                  [:div.spandrel]]
    :section-start [:div.section-start
                    (merge style {:data-section (:section content)
                                  :class (:align content "center")})
                    [:h5 (:title content)]
                    [:div.spandrel]]))

(defn- pulled-style-cues-from-content [content]
  (when (string? content)
    (reduce (fn [[metadata content] [key re]]
              (let [ann (re-find re content)
                    out-content (string/replace content re "")]
                [(if ann
                   (case key
                     :class (assoc metadata :class (str "ann-" (last ann)))
                     :style (assoc metadata :style (last ann)))
                   metadata)
                 out-content]))
            [{} content]
            [[:class #"[\s]{0,}\(§:([^\)]+)\)[\s]{0,}"]
             [:style #"[\s]{0,}\(ß:([^\)]+)\)[\s]{0,}"]])))

(defn- content->style&content [content]
  (let [[style mod-content] (pulled-style-cues-from-content content)]
    [(or style {}) (or mod-content content)]))

(defn- data->hiccup [el]
  (let [tag (first (keys el))
        [style content] (content->style&content (get el tag))]
    (tag&content->hiccup tag style content)))

(defn- contiguous-grouper-fn [selector cb]
  (fn grouper [els]
    (let [head (first els)
          sentinel (first head)]
      (if (keyword? sentinel)
        (let [[el n] (if (= sentinel selector) (cb els) [head 1])
              rest (grouper (drop n els))]
          (cons el rest))))))

(defn- list-grouper [li-selector l-selector]
  (contiguous-grouper-fn
    li-selector
    (fn [els]
      (let [inner (take-while #(= li-selector (first %)) els)]
        [[l-selector {} inner] (count inner)]))))

(def ^:private group-bullets-to-lists
  (list-grouper :li.bullet :ul.bullets))

(def ^:private group-numbers-to-ols
  (list-grouper :li.number :ol.numbers))

(def ^:private group-elements-by-section
  (contiguous-grouper-fn
    :div.section-start
    (fn [els]
      (let [inner (take-to-first #(= :div.section-end (first %)) els)]
        ; pull attributes of section start up to the section itself
        [[:div.section (nth (first els) 1) inner] (count inner)]))))

(defn- add-end-mark [config html]
  (if-let [tombstone (:tombstone config)]
    (let [last (last html)]
      (concat (drop-last 1 html)
              [(conj last [:span.tombstone tombstone])]))
    html))

(defn- group-by-text-function [tagged]
  (reduce
    (fn [groups el]
      (let [group (cond
                    (:link el) :links
                    (:footnote el) :footnotes
                    (:metadata el) :metadata
                    :else :text)]
        (assoc groups group (concat (get groups group) [el]))))
    {}
    tagged))

(defn- extract-preamble [str]
  (let [pieces (string/split str #"\n---")]
    (if (> (count pieces) 1)
      [(yaml/parse-string (first pieces))
       (string/join #"\n---" (rest pieces))]
      [{} str])))

(defn- hiccup->html [lines]
  (h/html lines))

(defn str->data-structure [str config]
  (let [[preamble raw-text] (extract-preamble str)
        cleaned-text (-> raw-text
                         (string/replace #"^[^~]" "")
                         (string/replace #"\r" ""))
        grouped (->> (string/split cleaned-text #"\n")
                     ;(filter (re-matches #"[\w]+" %))
                     (reconstitute-fenced-blocks)
                     (reconstitute-tables)
                     (remove #(= "" %))
                     (map tagged-and-edited)
                     (group-by-text-function))
        links (->> (map :link (:links grouped))
                   (map #(vec [(:tag %) (:href %)]))
                   (into {}))
        footnotes (->> (:footnotes grouped)
                       (map :footnote)
                       (map-indexed
                         (fn [i f]
                           {:footnote (assoc f :index (+ i 1))})))
        html (->> (enrich-text (:text grouped) config links footnotes)
                  (map data->hiccup)
                  (add-end-mark config)
                  (group-bullets-to-lists)
                  (group-numbers-to-ols)
                  (group-elements-by-section)
                  (hiccup->html)
                  (annotate-parentheticals)
                  (add-drop-cap))
        html-footnotes (->> (enrich-text footnotes config links nil)
                            (map data->hiccup)
                            (h/html))
        result (apply merge
                      ; TODO merge preamble here?
                      (map :metadata (:metadata grouped)))]
    (merge result {:html html
                   :metadata preamble
                   :footnotes (if (> (count footnotes) 0)
                                html-footnotes)})))

(defn text->data-structure [file config]
  (str->data-structure (slurp file) config))