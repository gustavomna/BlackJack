-- modulos criados
import FunCartas
import OperacoesArquivo
import Jogadas

-- modulos biblioteca padrao
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
  putStrLn "Digite o seu objetivo de saldo para ganhar, (O mínimo do saldo limite da casa eh 20 reais, a aposta minima eh de 10 reais):"
  saldo <- getLine
  if (read saldo) > 19
    then
      menu ((read saldo) / 2) (read saldo)
    else do
      putStrLn "Entre no minimo um objetivo de 20 reais\n"
      main

  putStrLn "Game over, deseja jogar novamente? \"Sim\" ou \"Nao\"?"
  escolha <- getLine
  if map toLower escolha == "sim"
    then main
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

