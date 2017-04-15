import FunCartas
import Text.Read
import Data.Char
import System.IO
import qualified Data.Text.IO
import Data.List
import System.Environment
import System.Random
import qualified Data.Map
import System.Exit

main = do
  putStrLn "Digite o seu objetivo de saldo para ganhar, (O mínimo do saldo limite da casa eh 20 reais, a aposta minima eh de 10 reais) :\n"
  saldo <- getLine
  if (read saldo::Double) > 19
    then menu ((read saldo::Double) / 2::Double) (read saldo::Double)
    else do putStrLn "Entre no minimo um objetivo de 20 reais"
            main

  putStrLn "Game over, deseja jogar novamente? \"Sim\" ou \"Nao\"?"
  escolha <- getLine
  if map toLower escolha == "sim"
    then do main
  else return ()

menu :: Double -> Double -> IO()
menu saldoAtual saldoObjetivo = do
  putStrLn "Bem vindo ao BalckJack! digite \"Jogar\" para começar o jogo, \"Regras\" ou \"Sair\" para sair do jogo"
  escolha <- getLine
  if map toLower escolha == "regras"
    then do handle <- openFile "regras.txt" ReadMode
            contents <- hGetContents handle
            putStrLn contents
            menu saldoAtual saldoObjetivo
  else if map toLower escolha == "jogar"
          then do putStrLn "Iniciando o Jogo...\n"
                  dealerGen <- newStdGen
                  playerGen <- newStdGen
                  let total = 0
                  let aposta = 0
                  jogo (entregarCartasDealer dealerGen total) (iniciarCartasJogador dealerGen total) saldoAtual saldoObjetivo aposta
  else return ()

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

-- mostra resultado do jogo
apontarTotal :: [Cartas] -> [Cartas] -> String
apontarTotal cartasDealer cartasJogador
| somaCartas (valorDaMao cartasDealer) == somaCartas (valorDaMao cartasJogador) = "Jogo Empatado"
| somaCartas (valorDaMao cartasDealer) > somaCartas (valorDaMao cartasJogador) = "Voce Perdeu"
| somaCartas (valorDaMao cartasDealer) < somaCartas (valorDaMao cartasJogador) = "Voce Ganhou"
