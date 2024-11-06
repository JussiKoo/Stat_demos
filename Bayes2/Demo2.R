#T1

#a) target-"muuttuja" kuvaa log-(posteriori)tiheysfunktiota

#===============================================================================

#b) "~"-notaation jälkeinen lauseke lisätään posterioritiheyteen kertoimena.
#Eli tämä vastaa sikäli lisäämistä lisäystä "target"-muuttujaan.

#Seuraavat rivit vastaavat johtavat siis samaan päättelyyn

#y ~ normal(mu, sigma);

#target += normal_lpdf(y | mu, sigma);

#===============================================================================

#c)

#Normalisoimattomien jakaumien käyttö tekee simuloinnista laskennallisesti
#edullisempaa.

#T3

y <- c(1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0)



#T4

#T5

#T6