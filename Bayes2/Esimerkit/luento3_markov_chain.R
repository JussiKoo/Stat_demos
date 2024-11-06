# Esimerkki Markovin ketjusta ja tasapainojakaumasta
# X^{(0)} ~ Tas({1,2,3,4})

# Siirtymämatriisi
P <- matrix(
  c(
    0.0, 0.8, 0.0, 0.1,
    0.9, 0.1, 0.5, 0.0,
    0.1, 0.0, 0.3, 0.0,
    0.0, 0.1, 0.2, 0.9
  ),
  4,
  4
)

# igraph-paketin avulla voidaan piirtää siirtymiä esittävä suunnattu graafi
# Onko ketju pelkistymatön (irreducible)?
g <- igraph::graph_from_adjacency_matrix(P, mode = "directed", weighted = TRUE)
plot(g, vertex.color = "white")
# Tilasta i on mahdollista päästä tilaan j äärellisellä määrällä siirtymiä kaikilla i,j

# Voidaan myös tarkastella m = 1,...,n-1 askeleen siirtymiä siirtymämatriisin avulla:
P             # 1. askeleen siirtymät
P %*% P       # 2. askeleen siirtymät
P %*% P %*% P # 3. askeleen siirtymät
# P sisältää alkioita, jotka ovat nollia, tämä tarkoittaa että yhdellä siirrolla ei ole
# mahdollista siirtyä mistä tahansa tilasta i mihin tahansa tilaan j
# Matriisien P^2 ja P^3 kaikki alkiot ovat positiivisia, joten kahdella tai kolmella siirrolla
# on mahdollista siirtyä mistä tahansa tilasta i mihin tahansa tilaan, eli ketju on pelkistymätön

# Onko ketjulla tasapainojakauma?
T_ <- 1e5
p_X <- matrix(0, T_, 4)
p_X[1, ] <- c(0, 0, 1, 0)
#p_X[1, ] <- rep(0.25, 4)
for (t in seq(2, T_)) {
  p_X[t, ] <- p_X[t - 1, ] %*% P
}

matplot(
  p_X[1:100,], type = "l", lty = 1, col = 1:4,
  lwd = 2, cex.main = 1.5, cex.lab = 1.5, ylim = c(0, 1),
  main = expression("P(" * X^(t) * " = " * i * ")"),
  xlab = expression(t), ylab = "Probability"
)
legend(x = 40, y = 0.90, legend = paste0("i = ", 1:4), col = 1:4, lty = 1, lwd = 2)
# Ketju näyttäisi konvergoivan johonkin tasapainojakaumaan

# Tasapainojakauma
# (Voidaan ratkaista globaalista tasapainoehdosta summarajoitteella)
p_eq <- c(63, 68, 9, 86) / 226
Q <- diag(p_eq)
# approksimaatio
p_X[T_, ]

# Lokaali tasapainoehto
R <- matrix(c(0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0), 4, 4)
P %*% Q
t(P) %*% R %*% Q %*% R
# Huomataan, että ehto ei toteudu

# Globaali tasapainoehto
c(p_eq %*% P)
p_eq
# Huomataan, että ehto toteutuu


# Pelkistyvä Markovin ketju
P2 <- matrix(
  c(
    0, 0, 0, 1,
    1, 0, 0, 0,
    0, 1, 0, 0,
    0, 0, 1, 0
  ),
  4,
  4
)

g2 <- igraph::graph_from_adjacency_matrix(P2, mode = "directed", weighted = TRUE)
plot(g2, vertex.color = "white")

p_X2 <- matrix(0, T_, 4)
#p_X2[1, ] <- rep(0.25, 4) # Todennäköisyys iteraatiolla t riippuu nyt alkuarvosta
p_X2[1, ] <- c(1, 0, 0, 0)
for (t in seq(2, T_)) {
  p_X2[t, ] <- p_X2[t - 1, ] %*% P2
}

matplot(
  p_X2[1000:1010,], type = "l", lty = 1, col = 1:4,
  lwd = 2, cex.main = 1.5, cex.lab = 1.5,
  main = expression("P(" * X^(t) * " = " * i * ")"),
  xlab = expression(t), ylab = "Probability"
)
legend(x = 40, y = 0.20, legend = paste0("i = ", 1:4), col = 1:4, lty = 1, lwd = 2)
