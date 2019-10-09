import Data.Array

--Verifica se um elemento esta no array;

isOnArray :: [Int] -> Int -> String
isOnArray list element 
 |list == [] = "O " ++ (show element) ++ " nao esta na lista"
 |element == (head list) = "O " ++ (show element) ++ " esta na lista"
 |otherwise = isOnArray (tail list) element


-- verifica o maior elemento de uma lista;

getBiggerElement :: [Int] -> Int-> String
getBiggerElement [] bigger = "O maior elemento eh " ++ (show bigger)
getBiggerElement (x:xs) bigger 	| bigger <= x = getBiggerElement xs x
								|otherwise = getBiggerElement xs bigger

-- Outro modo mais correto e otimizado;

getBiggerElement2 :: [Int] -> Int
getBiggerElement2 [x] = x
getBiggerElement2 (x:xs) 	|x > getBiggerElement2 xs = x
							|otherwise = getBiggerElement2 xs


--arePairs :: [Int] -> String
arePairs [] = "Todos os elementos sao pares"
arePairs (x:xs) |(mod x 2) == 0 = arePairs xs
				|otherwise = "Nem todos os elementos sao pares"


--gerando listas de forma mais complexa

--Gera uma lista de pares de '1' até 'n'

listPairs n = [x | x <- [1..n], mod x 2 == 0]

--gera combinacoes de tuplas tal que x vai até n e y vai de n até n ao quadrado
listTuplas n = [(x, y) | x <- [0..n], y <-[n..n^2]]

listChar n = [ x | x <- ['a'..n]]

listChar2 n = [ x | x <- ['a'..n],x == 'a' || x == 'i' || x== 'o']
--listStrings n = [ x | x <- [..n]]




removeElemento :: [Int] -> Int -> [Int] -> [Int]
removeElemento []  smaller newList = newList
removeElemento list smaller newList 	|head list == smaller = removeElemento (tail list) smaller newList
										|otherwise = removeElemento (tail list) smaller (newList ++ ([head list])) 


getSmallerElement :: [Int] -> Int
getSmallerElement [x] = x
getSmallerElement (x:xs) 	|x <= getSmallerElement xs = x
							|otherwise = getSmallerElement xs




--Ordena uma lista de inteiros
sortList :: [Int] -> [Int] -> [Int]
sortList []	sortedList	 = sortedList
sortList list sortedList = sortList (removeElemento list (getSmallerElement list) [] )  (sortedList ++ [getSmallerElement list])


--Ordena uma lista de inteiros de forma mais correta, recebendo apenas a lista como argumento

sortList2 :: [Int] -> [Int]
sortList2 [] = []  
sortList2 list  =  [getSmallerElement list] ++ sortList2 (removeElemento list (getSmallerElement list) [] ) 


---Manipulando vetores

--vetor de 1 linha e 4 colunas
get_vetor = array (1,4) [(1,'A'), (2,'B'),(3,'C'),(4,'D')]

--vetor de 2 lihas e 2 colunas

get_vetor1 = array ((1,1) ,(2,2)) [((1,1), 'A'),((1,2),'B'),((2,1),'C'),((2,2),'D')]


{-
moverSe :: Int-> Int -> Array ((Int,Int),(Int,Int)) -> String 
moverSe x y matriz = "Linha: " ++ show(x) ++ "\n" ++ "Coluna: " ++ show (y) "\n" ++  "Element: " ++ show(matriz ! (x,y))
-}

matriz2x2 = array ((1,1),(2,2)) [((1,1),'A'), ((1,2),'B'),((2,1), 'C'), ((2,2), 'D')]
