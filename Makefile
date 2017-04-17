run:
	ghc -o exec Blackjack.hs FuncoesCartas.hs operacoesArq.hs AnalisaJogadas.hs
clean: 
	rm *.o *.hi exec logJogos.txt

