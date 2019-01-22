{- |

= Introdução

Nesta tarefa, tivemos que encontrar uma forma de comprimir ao máximo o estado do jogo, por outras palavras, 
arranjar uma forma de fazer com que o estado ocupasse o mínimo de espaço possível, não perdendo nenhuma da
sua informação.

= Objetivos

O nosso principal objetivo, para poder atingir uma taxa de compressão máxima, foi conseguir reduzir um estado 
a um conjunto de dígitos. Estes números poderiam então ser convertidos para números de base superior, ou seja, 
números que são representados por mais do que 10 caracteres. Por exemplo, no formato binário, o número 14 fica 
1110, e no formato hexadecimal fica E. Assim, vemos que, quantos mais caracteres diferentes puderem ser usados 
para representar um número, menos caracteres serão necessários no total. O número máximo de diferentes 
caracteres a que conseguimos chegar foi 1114701. Antes desta mudança de base, foi necessário reduzir ao máximo 
o número de dígitos precisos para representar um estado. Para isso, vimo-nos livres de tudo o que não era 
necessário saber para depois descomprimir o estado. Por exemplo, como a borda do mapa é sempre formada por 
blocos indestrutíveis, essa informação é inútil. Depois, foi simplesmente uma questão de conseguir extrair 
o máximo possível de informação a partir de um número o mais reduzido possível de dígitos, o que requeriu 
alguma criatividade e pensamento fora da caixa.

= Discussão e Conclusão

Em retrospetiva, acreditamos que fizemos o melhor que conseguimos com os conhecimentos que possuímos, sendo que 
até ficamos em primeiro lugar nas classificações quanto à tarefa em questão. Embora, através dos sistemas de 
avaliação fornecidas pelo professor, nos tenhamos apercebido que a qualidade do código precisava de um pouco 
mais de trabalho, decidimo-nos focar no nível de compressão. Podemos assim afirmar que consideramos o facto de 
conseguir equilibrar a compressão/descompressão possível com a qualidade do código como uma tarefa desafiante.

-}

module Tarefa3_2018li1g080 where

import LI11819
import Data.Char
import Data.Maybe
import Data.List
import Tarefa0_2018li1g080
import Tarefa1_2018li1g080


-- * Testes

-- | Testes unitários da Tarefa 3.
--
-- Cada teste é um 'Estado'.
testesT3 :: [Estado]
testesT3 = [Estado (mapaInicial (5,5)) [Jogador (2,2) D 3 5 5] [DisparoCanhao 0 (2,3) D],
            Estado (mapaInicial (7,7)) [Jogador (3,2) C 3 4 5, Jogador (1,3) B 2 3 2] [DisparoCanhao 0 (2,2) D, DisparoChoque 1 5],
            Estado (mapaInicial (12,9)) [Jogador (2,2) C 3 5 5,Jogador (1,2) B 2 0 0,Jogador (1,1) E 0 3 2] [DisparoCanhao 0 (2,3) D, DisparoLaser 2 (1,2) B, DisparoChoque 1 5],
            Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Bloco Destrutivel,Vazia,Bloco Destrutivel,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Bloco Destrutivel,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Bloco Destrutivel,Vazia,Bloco Destrutivel,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Bloco Destrutivel,Vazia,Bloco Destrutivel,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] [Jogador (2,7) C 3 5 5] []
            ]

-- all (==True) (map (\x -> descomprime (comprime x) == x) testesT3)
-- Taxa de compressão dos nossos testes: map (\x -> 100 - fromIntegral (length (comprime x) * 100) / fromIntegral (length (show x))) testesT3
-- Média: sum (map (\x -> 100 - fromIntegral (length (comprime x) * 100) / fromIntegral (length (show x))) testesT3) / fromIntegral (length (testesT3))


-- * Funções principais da Tarefa 3.

-- | Comprime um 'Estado' para formato textual.
--
-- __NB:__ A função 'show' representa um 'Estado' num formato textual facilmente legível mas extenso.
--
-- __NB:__ Uma boa solução deve representar o 'Estado' dado no mínimo número de caracteres possível.
comprime :: Estado -> String
comprime estado = converteMapa (comprimeMapa (mapaEstado estado) lm) ++ "!" ++ converteJD (comprimeJogadores (jogadoresEstado estado)) ++ "?" ++ converteJD (comprimeDisparos (disparosEstado estado))
    where (lm,_) = dimensaoMatriz (mapaEstado estado)

-- | Descomprime um 'Estado' no formato textual utilizado pela função 'comprime'.
--
-- __NB:__ A função 'comprime' é válida de for possível recuperar o 'Estado' utilizando a função 'descomprime', i.e.:
--
-- prop> descomprime . comprime = id
--
-- __NB:__ Esta propriedade é particularmente válida para a solução pré-definida:
--
-- prop> read . show = id
descomprime :: String -> Estado
descomprime str = Estado {mapaEstado = descomprimeMapa (delete '3' mapaStr) (length (takeWhile (/= '3') mapaStr) + 2), jogadoresEstado = descomprimeJogadores str, disparosEstado = descomprimeDisparos str}
    where mapaStr = desconverteMapa str

-- * Funções auxiliares da Tarefa 3

-- ** Funções auxiliares para a compressão

-- | Comprime um 'Mapa' para formato textual.

comprimeMapa :: Mapa -> Int -> String
comprimeMapa [] _ = ""
comprimeMapa [_] _ = [] -- Impede a última linha de ser comprimida, visto que é sempre igual em todos os mapa (x peças indestrutíveis).
comprimeMapa matriz@(m:ms) linhas | linhas == linhasM = comprimeMapa ms linhas -- Isto impede a primeira linha de ser comprimida, visto que é sempre igual em todos os mapas.
                                  | linhas == linhasM + 1 = comprimeLinha (init (tail m)) ++ "33" ++ comprimeMapa ms linhas -- Isto é para converter a segunda linha, a função põe "33" depois desta linha para ao descomprimir saber o número de colunas do mapa
                                  | otherwise = comprimeLinha (init (tail m)) ++ comprimeMapa ms linhas -- Comprime todas as outras linhas por recursão, menos a última.
    where (linhasM,_) = dimensaoMatriz matriz

-- | Comprime uma linha de um 'Mapa' (uma lista de 'Peca') para formato textual.
--
-- __NB:__ Esta função é usada como função auxiliar da 'comprimeMapa', para comprimir uma linha do mapa de cada vez.

comprimeLinha :: [Peca] -> String
comprimeLinha = foldr (\e -> (:) (case e of Vazia -> '0'
                                            Bloco Destrutivel -> '2'
                                            Bloco Indestrutivel -> '1')) ""

-- | Converte um 'Mapa' comprimido numa 'String' ainda mais comprimida, usando a base 1114071 para representar o resultado da função 'compactaMapa'.
--
-- __NB:__ Desta forma, o nosso 'Mapa' comprimido passa a ser representado por um conjunto de 1114071 diferentes caracteres, em vez de 4.

converteMapa :: String -> String
converteMapa [] = []
converteMapa str = base4PraBase1114071 ('1': reverse (compactaMapa str))

-- | Comprime ainda mais o 'Mapa', após a função 'comprimeMapa', usando o caracter '3' e outro(s) caracter(es) /= '3' à frente para indicar que uma dada 'Peca' se repete várias vezes seguidas.
--
-- __Ex:__ "31012301" é o mesmo que "1111111111112111111"

compactaMapa :: String -> String
compactaMapa [] = []
compactaMapa (h:t) | h:take 29 t == replicate 30 h = "322" ++ h:compactaMapa (drop 29 t)
                   | h:take 26 t == replicate 27 h = "321" ++ h:compactaMapa (drop 26 t)
                   | h:take 23 t == replicate 24 h = "320" ++ h:compactaMapa (drop 23 t)
                   | h:take 20 t == replicate 21 h = "312" ++ h:compactaMapa (drop 20 t)
                   | h:take 17 t == replicate 18 h = "311" ++ h:compactaMapa (drop 17 t)
                   | h:take 14 t == replicate 15 h = "310" ++ h:compactaMapa (drop 14 t)
                   | h:take 11 t == replicate 12 h = "302" ++ h:compactaMapa (drop 11 t)
                   | h:take 8 t == replicate 9 h = "301" ++ h:compactaMapa (drop 8 t) 
                   | h:take 5 t == replicate 6 h = "300" ++ h:compactaMapa (drop 5 t)
                   | otherwise = h:compactaMapa t
          

-- | Comprime uma lista de 'Jogador' para formato textual.
--
-- __NB:__ Esta função usa a função 'comprimeJogador' para comprimir um 'Jogador' de cada vez.

comprimeJogadores :: [Jogador] -> String
comprimeJogadores = concatMap comprimeJogador

-- | Comprime um 'Jogador' para formato textual.
--
-- __NB:__ Os valores auxiliares lenVLS e yxlen servem para a função descomprimeJogador saber se as coordenadas / vidas e munições ocupam um ou dois caracteres.

comprimeJogador :: Jogador -> String
comprimeJogador (Jogador (y,x) d v l s) = dir ++ ys ++ xs ++ vs ++ ls ++ ss
    where ys = if length (show y) == 1 && null yxlen then '0':show y else show y
          xs = if length (show x) == 1 && null yxlen then '0':show x else show x
          dir = yxlen ++ show (lenVLS + case d of C -> 1
                                                  D -> 2
                                                  B -> 3
                                                  E -> 4)
          auxVLS vls = if length vls == 1 && lenVLS == 4 then '0':vls else vls
          vs = auxVLS (show v)
          ls = auxVLS (show l)
          ss = auxVLS (show s)
          lenVLS :: Int
          lenVLS | length (show v) == 1  && length (show l) == 1 && length (show s) == 1 = 0
                 | otherwise = 4
          yxlen :: String
          yxlen | length (show y) /= 1 || length (show x) /= 1 = []
                | otherwise = ['9']

-- | Converte as listas dos 'Jogador'es ou dos 'Disparo's comprimidas, da base 10 para a base 1114071.

converteJD :: String -> String
converteJD [] = []
converteJD str = base10PraBase1114071 (read str)

-- | Comprime uma lista de 'Disparo' para formato textual.
--
-- __NB:__ Esta função usa a função 'comprimeDisparo' para comprimir um 'Disparo' de cada vez.

comprimeDisparos :: [Disparo] -> String
comprimeDisparos = concatMap comprimeDisparo

-- | Comprime um 'Disparo' para formato textual.
--
-- __NB:__ Esta função usa alguns valores auxiliares, que adiciona à direção, para ao descomprimir poder determinar a direçao, o jogador que disparou, o tipo de disparo ou os ticks apenas com um número. 
-- Também é usado um valor auxiliar para verificar ao descomprimir se a posição do disparo ocupa dois ou quatro caracteres (ex: (4,3) vs (12,14)).

comprimeDisparo :: Disparo -> String
comprimeDisparo d = case d of DisparoCanhao numJ _ di ->  dirAdj di numJ ++ ys ++ xs
                              DisparoLaser numJ _ di -> dirAdj di numJ ++ ys ++ xs
                              DisparoChoque numJ td -> auxChoque numJ td
    where (y,x) = posicaoDisparo d
          ys = if length (show y) == 1 && lyx == 32 then '0':show y else show y
          xs = if length (show x) == 1 && lyx == 32 then '0':show x else show x
          dir dire nJ = case dire of C -> show ((1 + 4 * nJ + n) + lyx)
                                     D -> show ((2 + 4 * nJ + n) + lyx)
                                     B -> show ((3 + 4 * nJ + n) + lyx)
                                     E -> show ((4 + 4 * nJ + n) + lyx)
          dirAdj :: Direcao -> Int -> String -- Caso dir < 10, dirAdj acrescenta-lhe um '7' antes, para não causar problemas na conversão de base.
          dirAdj a j | length (dir a j) == 1 = '7':dir a j
                     | otherwise = dir a j 
          n = case d of DisparoCanhao{} -> 0
                        _ -> 16
          lyx :: Int -- Se == 0, significa que as coordenadas do disparo são de apenas um caracter (ex: (4,3)). Se == 32, significa que pelo menos uma das coordenadas ocupa mais que um caracter.
          lyx | length (show y) == 1 && length (show x) == 1 = 0
              | otherwise = 32
          auxChoque nJ td = show (80 + nJ + (4 * (td - 1))) 

-- ** Funções auxiliares para a descompressão

-- | Converte um 'Mapa' convertido na base 1114071 para uma 'String' de dígitos na base 4, podendo depois ser descomprimido pela função 'descomprimeMapa'.

desconverteMapa :: String -> String
desconverteMapa [] = []
desconverteMapa str = if head str == '!' then [] else init (descompactaMapa (reverse (base1114071PraBase4 (takeWhile (/= '!') str))))

-- | Descomprime as 'Peca's repetidas representadas pelo caracter '3', para o 'Mapa' poder ser corretamente descomprimido.

descompactaMapa :: String -> String
descompactaMapa (w:x:y:z:t) | w == '3' = case x of '2' -> replicate (24 + 3 * digitToInt y) z ++ descompactaMapa t
                                                   '1' -> replicate (15 + 3 * digitToInt y) z ++ descompactaMapa t
                                                   '0' -> replicate (6 + 3 * digitToInt y) z ++ descompactaMapa t
                                                   _ -> w:descompactaMapa (y:z:t)
                            | otherwise = w:descompactaMapa (x:y:z:t)
descompactaMapa m = m

-- | Descomprime um 'Mapa' em formato textual.
--
-- __NB:__ Esta função usa a função 'descomprimeLinha' para descomprimir uma linha de cada vez.

descomprimeMapa :: String -> Int -> Mapa
descomprimeMapa [] _ = []
descomprimeMapa ('!':_) _ = []
descomprimeMapa strM len = [replicate len (Bloco Indestrutivel)] ++ descomprimeMapaAux strM len ++ [replicate len (Bloco Indestrutivel)]
    where descomprimeMapaAux :: String -> Int -> Mapa
          descomprimeMapaAux strMap leng = if not $ null strMap
                                           then ([Bloco Indestrutivel] ++ descomprimeLinha (take (leng - 2) strMap) ++ [Bloco Indestrutivel]):descomprimeMapaAux (drop (leng - 2) strMap) leng
                                           else []

-- | Descomprime uma linha do 'Mapa' (uma lista de 'Peca') em formato textual.

descomprimeLinha :: String -> [Peca]
descomprimeLinha [] = [] 
descomprimeLinha (c:cs) | c == '0' = Vazia:descomprimeLinha cs
                        | c == '1' = Bloco Indestrutivel:descomprimeLinha cs
                        | otherwise = Bloco Destrutivel:descomprimeLinha cs

-- | Descomprime uma lista de 'Jogador'es em formato textual.
--
-- __NB:__ Esta função usa a função 'descomprimeJogador' para descomprimir um 'Jogador' de cada vez.

descomprimeJogadores :: String -> [Jogador]
descomprimeJogadores [] = []
descomprimeJogadores strJ = if null strJogadores then [] else descomprimeJogador (take n strJogadores):descomprimeJogadores (drop n strJogadores)
    where strJogadores = if '!' `elem` strJ then desconverteJD (takeWhile (/= '?') (tail (dropWhile (/= '!') strJ))) else strJ
          n | head strJogadores `elem` ['1'..'4'] = 8
            | head strJogadores `elem` ['5'..'8'] = 11
            | otherwise = if head (tail strJogadores) `elem` ['1'..'4'] 
                          then 7
                          else 10

-- | Descomprime um 'Jogador' em formato textual.

descomprimeJogador :: String -> Jogador
descomprimeJogador strJogador = Jogador (y,x) (dir (head strJogador)) vidas nLasers nChoques
    where y = read (take (2 - p) (drop (1 + p) strJogador))
          x = read (take (2 - p) (drop 3 strJogador))
          dir hJ | hJ == '1' ||  hJ == '5' = C
                 | hJ == '2' ||  hJ == '6' = D
                 | hJ == '3' ||  hJ == '7' = B
                 | hJ == '4' ||  hJ == '8' = E
                 | otherwise = dir (head (tail strJogador))
          vidas = read (take nh (drop (5 - p) strJogador))
          nLasers = read (take nh (drop (5 + nh - p) strJogador))
          nChoques = read (drop (5 + 2 * nh - p) strJogador)
          nh = n (head strJogador)
          n hJ | hJ `elem` ['1'..'4'] = 1
               | hJ `elem` ['5'..'8'] = 2
               | otherwise = n (head (tail strJogador))
          p | head strJogador == '9' = 1
            | otherwise = 0

-- | Converte a lista dos 'Jogador'es e dos 'Disparo's comprimidas na base 1114071 para una 'String' equivalente na base 10, para depois poderem ser descomprimidas.

desconverteJD :: String -> String
desconverteJD [] = ""
desconverteJD str = show (base1114071PraBase10 str)
          
-- | Descomprime uma lista de 'Disparo' em formato textual.
--
-- __NB:__ Esta função usa a função 'descomprimeDisparo' para descomprimir um 'Disparo' de cada vez.

descomprimeDisparos :: String -> [Disparo]
descomprimeDisparos [] = []
descomprimeDisparos strD = if last strD == '?' then [] else descomprimeDisparo (take n strDisparos) : descomprimeDisparos (drop n strDisparos)
    where strDisparos' = if '?' `elem` strD then desconverteJD (tail (dropWhile (/= '?') strD)) else strD
          strDisparos = if head strDisparos' == '7' then '0':tail strDisparos' else strDisparos'
          n | head strDisparos == '9' || head strDisparos == '8' = 2
            | read (take 2 strDisparos) <= 32 = 4
            | otherwise = 6

-- | Descomprime um 'Disparo' em formato textual.

descomprimeDisparo :: String -> Disparo
descomprimeDisparo [] = error "Disparo não encontrado!"
descomprimeDisparo strDisparo | read (take 2 strDisparo) `elem` [1..16] ++ [33..48] = DisparoCanhao (numJ (take 2 strDisparo)) (y,x) (dir (take 2 strDisparo))
                              | read (take 2 strDisparo) `elem` [17..32] ++ [49..64] = DisparoLaser (numJ (take 2 strDisparo)) (y,x) (dir (take 2 strDisparo))
                              | head strDisparo == '8' || head strDisparo == '9' = DisparoChoque numJS tempo
    where y = read (take (1 + n) (drop 2 strDisparo))
          x = read (drop (3 + n) strDisparo)
          dir t2D | t2D `elem` ["01","05","09","13","17","21","25","29"] ++ [show a | a <- [33,37..61]] = C
                  | t2D `elem` ["02","06","10","14","18","22","25","30"] ++ [show a | a <- [34,38..62]] = D
                  | t2D `elem` ["03","07","11","15","19","23","27","31"] ++ [show a | a <- [35,39..63]] = B
                  | otherwise = E
          numJ t2D | head t2D == '9' = digitToInt (last t2D)
                   | read t2D `elem` [1..4] ++ [17..20] ++ [33..36] ++ [49..52] = 0
                   | read t2D `elem` [5..8] ++ [21..24] ++ [37..40] ++ [53..56] = 1
                   | read t2D `elem` [9..12] ++ [25..28] ++ [41..44] ++ [57..60] = 2
                   | otherwise = 3
          n | read (take 2 strDisparo) <= 32 = 0
            | otherwise = 1
          numJS = read strDisparo `mod` 4
          tempo = (read strDisparo - 80) `div` 4 + 1 

-- ** Funções auxiliares numéricas

-- | Converte um número na base 4 (ex: 1202120) num número na base 1114071 (ex: 5n-F).
--
-- __NB:__ Ambos os números têm que estar dentro de uma 'String'.

base4PraBase1114071 :: String -> String
base4PraBase1114071 n = base10PraBase1114071 (base4PraBase10 n)

-- | Converte um número na base 4 (ex: 1202120) num número na base 10 (ex: 7323).
--
-- __NB:__ O input tem que estar em formato 'String'.
--
-- __NB:__ O output é do tipo 'Integer' para evitar as limitações de tamanho do 'Int'.

base4PraBase10 :: String -> Integer
base4PraBase10 n = aux n (length n - 1)
    where aux :: String -> Int -> Integer
          aux [] _ = 0
          aux [x] 0 = fromIntegral (digitToInt x)
          aux (h:t) x = fromIntegral (digitToInt h) * (4 ^ x) + aux t (x - 1)

-- | Converte um número na base 10 (ex: 7323) num número na base 1114071 (ex: 5n-F).
--
-- __NB:__ O output é dado numa 'String'.

base10PraBase1114071 :: Integer -> String
base10PraBase1114071 0 = ""
base10PraBase1114071 n = base10PraBase1114071 quociente ++ restoC
    where quociente = n `div` 1114071
          resto = n `mod` 1114071
          restoC | resto `elem` [0..9] = show resto
                 | resto `elem` [10..35] = [['a'..'z'] !! fromInteger (resto - 10)]
                 | resto `elem` [36..61] = [['A'..'Z'] !! fromInteger (resto - 36)]
                 | resto `elem` [62..82] = [['+','-','*','[',']','<','>',',',';',':','_','#','$','=','%','&','(',')','@','^','`'] !! fromInteger (resto - 62)]
                 | otherwise = [chr (fromInteger resto + 40)]

-- | Converte um número na base 1114071 (ex: 5n-F) num número na base 4 (ex: 1202120).
--
-- __NB:__ Ambos os números têm que estar dentro de uma 'String'

base1114071PraBase4 :: String -> String
base1114071PraBase4 n = base10PraBase4 (base1114071PraBase10 n)

-- | Converte um número na base 1114071 (ex: 5n-F) num número na base 10 (ex: 7323).
--
-- __NB:__ O imput tem que estar no formato 'String'.

base1114071PraBase10 :: String -> Integer
base1114071PraBase10 s = aux s 0
    where aux [] _ = 0
          aux l n = fromIntegral (aux2 (last l)) * (1114071 ^ n) + aux (init l) (n + 1)
          aux2 c | c `elem` ['0'..'9'] = digitToInt c
                 | c `elem` ['a'..'z'] ++ ['A'..'Z'] = fromJust (c `elemIndex` (['a'..'z'] ++ ['A'..'Z'])) + 10
                 | c `elem` ['+','-','*','[',']','<','>',',',';',':','_','#','$','=','%','&','(',')','@','^','`'] = fromJust (c `elemIndex` ['+','-','*','[',']','<','>',',',';',':','_','#','$','=','%','&','(',')','@','^','`']) + 62
                 | otherwise = ord c - 40
                 

-- | Converte um número na base 10 (ex: 7323) num número na base 4 (ex: 1202120).
--
-- __NB:__ O output é dado em formato 'String'.

base10PraBase4 :: Integer -> String
base10PraBase4 0 = ""
base10PraBase4 n = base10PraBase4 quociente ++ resto
    where quociente = div n 4
          resto = show (mod n 4)