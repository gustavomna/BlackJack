import Text.Read
import Data.Char
import System.IO
import qualified Data.Text.IO
import Data.List
import System.Environment
import System.Random
import qualified Data.Map

data Cartas = A | Dois | Tres | Quatro | Cinco | Seis | Sete | Oito | Nove | Dez | J | Q | K deriving (Show, Enum, Bounded, Read)

menu :: Double -> Double -> IO()
menu curBalance terBalance = do
  putStrLn "Bem vindo ao BalckJack! digite \"Jogar\" para começar o jogo, \"Regras\"
           ou \"Sair\" para sair do jogo"
  command <- getLine
  if mapToLower command == "regras"
    then do handle <- openFile "regras.txt" ReadMode
            contents <- hGetContents handle
            putStrLn contents
            menu curBalance terBalance
    

jogo :: [Cartas] -> [Cartas] -> Double -> Double -> Double -> IO()
jogo cartasDealer cartasJogador saldoAtual saldoObjetivo aposta = do
  --  confere se o jogador ganhou o jogo
  if saldoAtual >= saldoObjetivo
    then putStrLn $ "Parabens, voce conseguiu " ++ show saldoAtual ++ " e seu objetivo era " ++ show saldoObjetivo
  -- se ainda nao ganhou o jogo
  -- confere se a aposta é valida
  else if aposta == 0
    then do putStrLn $ "Voce tem " ++ show saldoAtual ++ " / " ++ show saldoObjetivo
            putStrLn $ "Faca a aposta para o proximo jogo: \n"
            valorDaAposta <- getLine
            -- valida se a aposta é negativa ou é mais do que o saldo do jogador
            if (read valorDaAposta::Double) < 1 || (read valorDaAposta::Double) > saldoAtual
              then do putStrLn "Aposta invalida, aposte um valor entre 0 e o seu saldo"
                      jogo cartasDealer cartasJogador saldoAtual saldoObjetivo 0
            else do putStrLn $ "Aposta aceita\n"
                    jogo cartasDealer cartasJogador saldoAtual saldoObjetivo (read valorDaAposta::Double)
  -- confere se o dealer passou de 21
  else if (somaCartas $ valorDaMao cartasDealer) > 21
      then do putStrLn $ "Cartas do Dealer: " ++ mostrarCarta cartasDealer ++ "\n"
              putStrLn $ "Suas cartas: " ++ mostrarCarta cartasJogador ++ "\n"
              putStrLn $ "Dealer passou de 21, voce ganhou"
              -- e é isso??
              dealerGen <- newStdGen
              jogadorGen <- newStdGen
              jogo (entregarCartasDealer dealerGen 0) (iniciarCartasJogador jogadorGen 2) (saldoAtual + aposta) saldoObjetivo 0
  -- confere se o jogador passou de 21
  else if (somaCartas $ valorDaMao cartasJogador) > 21
    then do putStrLn $ "Cartas do Dealer: " ++ mostrarCarta cartasDealer ++ "\n"
            putStrLn $ "Suas cartas: " ++ mostrarCarta cartasJogador ++ "\n"
            putStrLn $ "Voce passou de 21, voce perdeu"
            -- confere se o jogador esta abaixo do saldo
            if (saldoAtual - aposta) < 1
              then putStrLn "Voce perdeu todo seu dinheiro. O jogo acabou"
            else do
              dealerGen <- newStdGen
              jogadorGen <- newStdGen
              jogo (entregarCartasDealer dealerGen 0) (iniciarCartasJogador jogadorGen 2) (saldoAtual - aposta) saldoObjetivo 0
  -- se o jogador conseguiu um 21
  else if (somaCartas $ valorDaMao cartasJogador) == 21
    then do putStrLn $ "Cartas do Dealer: " ++ mostrarCarta cartasDealer ++ "\n"
            putStrLn $ "Suas cartas: " ++ mostrarCarta cartasJogador ++ "\n"
            putStrLn $ "Voce conseguiu um BlackJack! Sua aposta vale 1.5 vezes"
            dealerGen <- newStdGen
            jogadorGen <- newStdGen
            jogo (entregarCartasDealer dealerGen 0) (iniciarCartasJogador jogadorGen 2) (saldoAtual + aposta + (aposta / 2)) saldoObjetivo 0
  -- se deu empate
  else if (somaCartas $ valorDaMao cartasJogador) == (somaCartas $ valorDaMao cartasDealer)
    then do putStrLn $ "Cartas do Dealer: " ++ mostrarCarta cartasDealer ++ "\n"
            putStrLn $ "Suas cartas: " ++ mostrarCarta cartasJogador ++ "\n"
            putStrLn $ "Empate! Voce nao ganhou nem perdeu dinheiro"
            dealerGen <- newStdGen
            jogadorGen <- newStdGen
            jogo (entregarCartasDealer dealerGen 0) (iniciarCartasJogador jogadorGen 2) saldoAtual saldoObjetivo 0
  -- se ...
  else do putStrLn $ "Cartas do Dealer: " ++ mostrarCarta cartasDealer ++ "\n"
          putStrLn $ "Suas cartas: " ++ mostrarCarta cartasJogador ++ "\n"
          -- jogador decide se quer mais uma carta ou nao
          putStrLn "Voce quer MAIS ou quer PARAR?\n"
          escolha <- getLine
          -- gera nova carta e chama a funcao jogo
          if map toLower escolha == "mais"
            then do jogadorGen <- newStdGen
                    let novaCarta = entregarCartaJogador jogadorGen
                    let novasCartasDoJogador = cartasJogador ++ novaCarta
                    jogo cartasDealer novasCartasDoJogador saldoAtual saldoObjetivo aposta
          else if map toLower escolha == "parar"
            -- voce ganhou
            then if (apontarTotal cartasDealer cartasJogador) == "Voce Ganhou"
              then do putStrLn $ "Cartas do Dealer: " ++ mostrarCarta cartasDealer ++ "\n"
                      putStrLn $ "Suas cartas: " ++ mostrarCarta cartasJogador ++ "\n"
                      putStrLn "Voce tem valor maior, voce ganhou"
                      dealerGen <- newStdGen
                      jogadorGen <- newStdGen
                      jogo (entregarCartasDealer dealerGen 0) (iniciarCartasJogador jogadorGen 2) (saldoAtual + aposta) saldoObjetivo 0
            -- voce perdeu
            else if (apontarTotal cartasDealer cartasJogador) == "Voce Perdeu"
              then do putStrLn $ "Cartas do Dealer: " ++ mostrarCarta cartasDealer ++ "\n"
                      putStrLn $ "Suas cartas: " ++ mostrarCarta cartasJogador ++ "\n"
                      putStrLn $ "Dealer tem valor maior, voce perdeu"
                      -- confere se o jogador esta abaixo do saldo
                      if (saldoAtual - aposta) < 1
                        then putStrLn "Voce perdeu todo seu dinheiro. O jogo acabou"
                      else do
                        dealerGen <- newStdGen
                        jogadorGen <- newStdGen
                        jogo (entregarCartasDealer dealerGen 0) (iniciarCartasJogador jogadorGen 2) (saldoAtual - aposta) saldoObjetivo 0
            -- empate
            else do putStrLn $ "Cartas do Dealer: " ++ mostrarCarta cartasDealer ++ "\n"
                    putStrLn $ "Suas cartas: " ++ mostrarCarta cartasJogador ++ "\n"
                    putStrLn $ "Empate! Voce nao ganhou nem perdeu dinheiro"
                    dealerGen <- newStdGen
                    jogadorGen <- newStdGen
                    jogo (entregarCartasDealer dealerGen 0) (iniciarCartasJogador jogadorGen 2) saldoAtual saldoObjetivo 0
          --  escolha invalida
          else do putStrLn "Escolha se quer MAIS ou quer PARAR\n"
                  jogo cartasDealer cartasJogador saldoAtual saldoObjetivo aposta


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
  |parar /= 0 = toEnum(randomNum - 1) : iniciarCartasJogador novoGen (parar - 1)
  |otherwise = []
  where (randomNum, novoGen) = randomR(1,13) gen :: (Int, StdGen)

-- Entrega uma carta para quem chamar
entregarCartaJogador :: StdGen -> [Cartas]
entregarCartaJogador gen = [toEnum (randomNum - 1)]
  where (randomNum, novoGen) = randomR(1,13) gen :: (Int, StdGen)

-- recebe um vetor de cartas e retorna um vetor de inteiros contendo os valores
-- dessas cartas
valorDaMao :: [Cartas] -> [Int]
valorDaMao [] = []
valorDaMao (c:cx)
  | fromEnum c >= 10 = fromEnum 10 : valorDaMao cx
  | otherwise = (fromEnum (c) + 1) : valorDaMao cx

-- mostra as cartas do dealer para o usuário
mostrarCarta :: [Cartas] -> String
mostrarCarta [] = ""
mostrarCarta (c:cs)
  | fromEnum c < 10 && fromEnum c > 0 = show (fromEnum (c) + 1) ++ ", " ++ mostrarCarta cs
  | otherwise = show c ++ ", " ++ mostrarCarta cs

-- mapeia os valores das cartas (J = 11, K = 13)
mostrarMao :: [Cartas] -> [Int]
mostrarMao [] = []
mostrarMao (c:cx) = (fromEnum (c) + 1) : mostrarMao cx

-- converte os valores dos inteiros para Cartas (11 =  J, 13 = K)
displayCard :: [Int] -> [Cartas]
displayCard [] = []
displayCard (x:xs) = toEnum (x - 1) : displayCard xs

-- soma os valores das cartas que estao em um vetor de inteiros, se o 'A' estiver nesse vetor e
-- a soma total for menor que 12, entao o 'A' valera 11
somaCartas :: [Int] -> Int
somaCartas cartas
  | sum cartas < 12 && 1 `elem` cartas = sum (cartas) + 10
  | otherwise = sum cartas

-- mostra resultado do jogo
apontarTotal :: [Cartas] -> [Cartas] -> String
apontarTotal cartasDealer cartasJogador
  | somaCartas (valorDaMao cartasDealer) == somaCartas (valorDaMao cartasJogador) = "Jogo Empatado"
  | somaCartas (valorDaMao cartasDealer) > somaCartas (valorDaMao cartasJogador) = "Voce Perdeu"
  | somaCartas (valorDaMao cartasDealer) < somaCartas (valorDaMao cartasJogador) = "Voce Ganhou"
