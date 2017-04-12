import System.Random

data Cartas = A | Dois | Tres | Quatro | Cinco | Seis | Sete | Oito | Nove | Dez | J | Q | K

entregarCartasDealer :: StdGen -> Int -> [Cartas]
entregarCartasDealer gen total
  |total < 11 && randomNum == 1 = toEnum(randomNum - 1) : entregarCartasDealer novoGen (total + randomNum + 10)
  |total < 17 = toEnum (randomNum - 1) : entregarCartasDealer novoGen (total + randomNum)
  |otherwise = []
  where (randomNum, novoGen) = random(1,13) gen :: (Int, StdGen)
