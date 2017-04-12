import System.Random

data Cartas = A | Dois | Tres | Quatro | Cinco | Seis | Sete | Oito | Nove | Dez | J | Q | K

-- Entrega cartas do dealer. Até o valor ser 17 ou superior.
entregarCartasDealer :: StdGen -> Int -> [Cartas]
entregarCartasDealer gen total
  |total < 11 && randomNum == 1 = toEnum(randomNum - 1) : entregarCartasDealer novoGen (total + randomNum + 10)
  |total < 17 = toEnum (randomNum - 1) : entregarCartasDealer novoGen (total + randomNum)
  |otherwise = []
  where (randomNum, novoGen) = randomR(1,13) gen :: (Int, StdGen)

-- Entrega cartas até jogador parar
iniciarCartasJogador :: StdGen -> Int -> [Cartas]
iniciarCartasJogador gen parar
  |parar \= 0 = toEnum(randomNum - 1) : iniciarCartasJogador novoGen (parar - 1)
  |otherwise = []
  where (randomNum, newGen) = randomR(1,13) gen :: (Int, StdGen)

-- Entrega uma carta para quem chamar
entregarCartaJogador :: StdGen -> [Cartas]
entregarCartasJogador gen = [toEnum (randomNum - 1)]
  where (randomNum, newGen) = randomR(1,13) gen :: (Int, StdGen)
