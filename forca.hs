import Data.List (transpose)

imagensBonecoForca :: [[String]]
imagensBonecoForca =
	transpose
	[["   ", " O ", " O ", " O ", " O " , " O " , " O " ]
	,["   ", "   ", " | ", " | ", " | " , "/| " , " |\\" ]
	,["   ", "   ", "   ", "/  ", "/ \\", "/ \\", "/ \\"]]

imagemForca :: Int -> [String]
imagemForca indice = 
	"-----------" :
	"|    |" :
	map ("|   " ++) imagem
	where imagem = imagensBonecoForca !! indice

