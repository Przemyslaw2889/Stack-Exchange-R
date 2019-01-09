# Analiza forów *stack.exchange.com

Nasza analiza podsumowana jest w aplikacji w pliku `app.R`. Znajdują się tam wyniki naszych analiz,
w tym interaktywne wizualizacje i wykresy.


### Aplikacja
* `app.R` aplikacja w interaktywny sposób podsumowująca naszą analizę

### Wykorzystywane funkcje
* `tags.R` barploty najczęściej występujących tagów
* `wordcloud_app.R` tworzenie wordcloudów słów w komentarzach
* `maps.R` tworzenie map z zaznaczonymi użytkownikami (wszystkimi, najbardziej aktywnymi oraz piszącymi najlepsze posty)
* `polarity_function_app.R` mierzenie *polarity* (wydźwięku emocjonalnego - pozytywny lub negatywny) danego tekstu
* `boxplot_app_polarity.R` rysowanie boxplotów wielkości polarity w komentarzach

* `df_from_xml` funkcja pobierające dane z XMLa do ramki danych

### Pliki .rds
W plikach *.rds umieściliśmy dane, które wykorzystujemy w aplikacji, a nie chcemy za każdym razem wyliczać ich na nowo.

### Ostatecznie nie wykorzystywane:
* `coffee_predict.R` klasyfikacja sentymentu poprzez model wytrenowany na zewnętrznym zbiorze danych
* `topic_modelling.R` nienadzorowana kategoryzacja postów poprzez metodę *Latent Dirichlet Allocation*
