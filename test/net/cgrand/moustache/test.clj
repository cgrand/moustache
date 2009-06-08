(ns net.cgrand.moustache.test
  (:use net.cgrand.moustache)
  (:use [clojure.contrib.test-is :as test-is :only [set-test is are]]))


(defn hello [_]
  {:status 200 :headers {"Content-Type" "text/html"} 
   :body "<h3>Hello World</h3>"})

(defn request [handler uri & options]
  (handler (conj {:request-method :get :uri uri} (apply hash-map options))))
  
(defn GET [handler uri & options]
  (apply request handler uri options))

(defn not+found [handler uri & options]
  (-> (apply request handler uri options) :status #{404})) 

(defn not+allowed [handler uri & options]
  (-> (apply request handler uri options) :status #{404})) 

(defn found+content [handler uri & options]
  (let [{:keys [status body]} (apply request handler uri options)]
    (when (= status 200) body))) 

(set-test app
  ;; wrapping
  (is (found+content (app hello) "/random/url"))
  (is (found+content (app (fn [req] (hello req))) "/random/url"))
  
  ;; response literal
  (is (= "literal response"
        (found+content (app {:status 200
                             :body "literal response"}) "/random/url")))
  
  ;; fixed route dispatch      
  (is (found+content (app [] hello) "/"))
  (is (not+found (app [] hello) "/random/url"))
  
  (is (found+content (app ["some" "url"] hello) "/some/url"))
  (is (not+found  (app ["some" "url"] hello) "/"))
  (is (not+found  (app ["some" "url"] hello) "/some/url/"))
  (is (not+found  (app ["some" "url"] hello) "/some/url/foo"))
  
  ;; open route dispatch      
  (is (found+content (app ["some" "url" &] hello) "/some/url"))
  (is (not+found  (app ["some" "url" &] hello) "/"))
  (is (found+content  (app ["some" "url" &] hello) "/some/url/"))
  (is (found+content  (app ["some" "url" &] hello) "/some/url/foo"))

  (is (= "xs=0" (found+content (app ["some" "url" & xs] ["xs=" (count xs)]) "/some/url")))
  (is (= "xs=1" (found+content (app ["some" "url" & xs] ["xs=" (count xs)]) "/some/url/")))
  (is (= "xs=1" (found+content (app ["some" "url" & xs] ["xs=" (count xs)]) "/some/url/bar")))
  (is (= "xs=2" (found+content (app ["some" "url" & xs] ["xs=" (count xs)]) "/some/url/bar/foo")))
  
  ;; segment validators
  (is (= "url" (found+content (app ["some" a] [{:status 200 :body a}]) "/some/url")))
  (is (= "thing" (found+content (app ["some" a] [{:status 200 :body a}]) "/some/thing")))
  (is (not+found (app ["some" a] [{:status 200 :body a}]) "/any/url"))
  (is (not+found (app ["some" a] [{:status 200 :body a}]) "/some/url/"))
  (is (not+found (app ["some" a] [{:status 200 :body a}]) "/some/url/foo"))

  (is (found+content (app [#"(ab)+"] "body") "/abab"))
  (is (not+found (app [#"(ab)+"] "body") "/aba"))
  (is (found+content (app [#"(ab)+"] "body") "/ab"))
  
  (is (= "w=abab" (found+content (app [[w #"(?:ab)+"]] ["w=" w]) "/abab")))
  (is (not+found (app [[w #"(?:ab)+"]] ["w=" w]) "/aba"))
  (is (= "w=ab" (found+content (app [[w #"(?:ab)+"]] ["w=" w]) "/ab")))
  
  (is (= "x=01;y=04" (found+content (app [[[_ x y] #"(\d+)-(\d+)"]] ["x=" x ";y=" y]) "/01-04")))
  (is (found+content (app [[_ #(= 3 (count %))]] "ok") "/abc"))
  (is (not+found (app [[_ #(= 3 (count %))]] "ok") "/abcd"))
  
  ;; method dispatch
  (is (found+content (app :get "ok") "/" :request-method :get))
  (is (= {:status 405, :headers {"Allow" "GET"}} (request (app :get "ok") "/" :request-method :post)))
  (is (= {:status 405, :headers {"Allow" "GET"}} (request (app :get "ok") "/" :request-method :put)))
  (is (= {:status 405, :headers {"Allow" "GET"}} (request (app :get "ok") "/" :request-method :head)))
  
  ;; fallthrough
  (is (= "route1" (found+content (app ["foo" &] "route1" ["foo" "bar"] "route2") "/foo/bar")))
  (is (= "route2" (found+content (app ["foo" &] pass ["foo" "bar"] "route2") "/foo/bar")))
  
  ;; nested apps
  (is (found+content (app ["foo" &] [["bar"] "ok"]) "/foo/bar"))
  (is (not+found (app ["foo" &] [["bar"] "ok"]) "/foo/baz"))
  (is (not+found (app ["foo" &] [["bar"] "ok"]) "/foe/bar"))
  
  ;; middlewares
  (is (= {:status 200, :headers {"Content-Type" "text/html"}, :body "<h3>not text!</h3>"} 
        (request (app (alter-response assoc-in [:headers "Content-Type"] "text/html") "<h3>not text!</h3>") "/")))
  (is (found+content (app (alter-request update-in [:uri] #(str "/foo" %)) ["foo" "bar"] "ok") "/bar"))  
  (is (not+found (app (alter-request update-in [:uri] #(str "/foo" %)) ["foo" "bar"] "ok") "/foo/bar"))  
) 