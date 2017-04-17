module OperacoesArquivo where

import System.IO
import FunCartas
import Data.Time

escrever :: [Cartas] -> [Cartas] -> String -> String -> String -> IO ()
escrever cartasDealer cartasJogador mensagem saldo aposta = do

	-- abre arquivo
  arq <- openFile "logJogos.txt" AppendMode

  -- escreve no arquivo 
  hPutStr arq $ mensagem ++ "\n"
  hPutStr arq $ "Cartas do Dealer: " ++ mostrarCarta cartasDealer ++ "\n"
  hPutStr arq $ "Cartas do Jogador: " ++ mostrarCarta cartasJogador ++ "\n"
  hPutStr arq $ "Saldo anterior: " ++ saldo ++ " Aposta: " ++ aposta ++ "\n\n" 
    
  putStrLn "Log atualizado"

  --hFlush arq
  hClose arq
