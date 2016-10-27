data('iris')
head(iris)
str(iris)
iris$Species
iris[iris$Species=='setosa',]
iris[iris$Species=='versicolor',]
iris[iris$Species=='virginica',]
lirios <- iris[c(1:5, 51:55, 101:105),]
# selección de filas: filter
filter(lirios, Species=='setosa')
filter(lirios, Species=='setosa' | Species=='virginica')
filter(lirios, Species=='setosa', Sepal.Length < 5)
# selección de columnas: select
select(lirios, Sepal.Length, Sepal.Width)
select(lirios, Petal.Length:Sepal.Length)
# ordenar: arrange
arrange(lirios, Sepal.Length)
arrange(lirios, -Sepal.Length)
arrange(lirios, Species, Sepal.Length)
# sintaxis en cadena
lirios %>%
  select(contains('Petal'))  %>%
  filter(Petal.Length > 4)   %>%
  arrange(Petal.Length)
# anidado habitual
arrange(filter(select(lirios, contains('Petal')), Petal.Length > 4), Petal.Length)
# añadir nuevas variables
lirios %>% mutate(forma = Petal.Width/Petal.Length)
# resumir subconjuntos de variables
lirios %>% group_by(Species) %>% summarise(mean(Petal.Length))