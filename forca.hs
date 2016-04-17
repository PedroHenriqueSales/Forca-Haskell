import Data.List 


--Matriz com imagens dos bonecos da forca em uma matriz transposta
imagensBonecoForca :: [[String]]
imagensBonecoForca =
	transpose
	[["   ", " O ", " O ", " O ", " O " , " O " , " O " ]
	,["   ", "   ", " | ", " | ", " | " , "/| " , " |\\" ]
	,["   ", "   ", "   ", "/  ", "/ \\", "/ \\", "/ \\"]]

--Função para renderizar boneco na forca, de acordo com indice, que receberá numero de erros
imagemForca :: Int -> [String]
imagemForca indice = 
	"-----------" :
	"|    |" :
	map ("|   " ++) imagem
	where imagem = imagensBonecoForca !! indice

--Declaração do número máximo de erros
numeroMaxErros :: Int
numeroMaxErros = length imagensBonecoForca - 1

mostrarPalavra :: String -> String
mostrarPalavra palavra = intersperse ' ' [if a `elem` ['a'..'z'] then '_' else a | a <- palavra]

tentarLetra :: String -> Char -> Int -> IO ()
tentarLetra palavra letra tentativas
	| letra `elem` palavra 	= jogo [if letra == a then toUpper letra else a | a <- palavra]
	| otherwise 			= jogo palavra (tentativas -1)

jogo :: String -> Int -> IO ()
jogo palavra tentativas
	| palavra == map toUpper palavra = do
		putStrLn $ mostrarPalavra palavra
		putStrLn "Voce Ganhou"
	| tentativas == 0 = do
		putStrLn $ mostrarPalavra palavra
		putStrLn "Voce Perdeu"
	| otherwise = do
		putStrLn $ "Voce tem " ++ show tentativas ++ "tentativas restantes."
		putStrLn $mostrarPalavra palavra
		putStr "Digite uma letra: "
		tentativaDeLetra <- getLine
		tentarLetra palavra (head tentativaDeLetra) tentativas

		