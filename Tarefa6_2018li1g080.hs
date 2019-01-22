{- |

= Introdução

Esta tarefa consistiu na criação de um bot, isto é, de um "jogador" capaz de jogar sem qualquer input humano, 
uma espécie de inteligência artificial primitiva. Esta tarefa foi talvez a mais complicada, no sentido em que nos 
obrigou a pensar em muitas variáveis e possibilidades para o comportamento do bot.

= Objetivos

Para nós, o objetivo foi criar um bot que conseguisse fazer tudo o que um jogador humano consegue, que fosse o melhor
possível, mas que conseguisse ser derrotado por um humano, senão o jogo ia passar a ser frustrante, em vez de divertido.
Para cumprir esse objetivo, definimos várias funções para o bot saber o que fazer. Por exemplo, se existir um disparo
a dirigir-se para o bot, este tenta desviar-se. Se não houver nenhum perigo iminente, este tenta encontrar outros
jogadores ou blocos destrutíveis, para tentar ganhar pontos. A ordem pela qual estas funções aparecem na função 'bot'
define as prioridades do mesmo. No nosso caso, as prioridades do nosso bot são, mais ou menos por esta ordem: 

* Desviar-se de disparos iminentes;
* Disparar em direção a um jogador que se encontre perto do bot;
* Disparar em direção a blocos destrutíveis que se encontrem perto do bot;
* Verificar se existem, e, caso existam, mover-se para um local onde existam blocos destrutíveis perto;
* Mover-se para perto de outros jogadores, para os poder matar.

Se o bot não conseguir fazer nenhuma destas coisas, move-se pelo mapa sem um destino concreto, mas na grande maioria 
dos casos ele consegue sempre fazer pelo menos uma coisa.

= Discussão e conclusão

Infelizmente, tivemos que fazer com que o bot apenas detetasse blocos destrutíveis a um raio de 8 blocos dele, já que
um número mais elevado fazia com que o jogo ficasse muito lento, principalmente em mapas grandes.
Esta limitação impede que o nosso bot tenha o potencial máximo que podia ter, mas da forma que está já consegue ganhar
contra jogadores humanos, em quase todas as partidas, e já ficou em primeiro lugar no ranking da tarefa 6.

Em conclusão, apesar de não termos conseguido fazer tudo o que pretendíamos, devido sobretudo às limitações do haskell,
fomos capazes de cumprir o nosso objetivo, e estamos orgulhosas da forma como o nosso bot ficou. Esta tarefa foi
um grande desafio, mas no bom sentido, já que nos permitiu testar as nossas capacidades de formas que as outras tarefas
não o fizeram, e obrigou-nos a pensar um pouco na forma como a inteligência artificial funciona, e a valorizar bastante
os avanços nessa área.

-}


module Tarefa6_2018li1g080 where

import LI11819
import Tarefa0_2018li1g080
import Tarefa4_2018li1g080
import Data.List ( (\\)
                 , sortBy
                 , minimumBy
                 )
import Data.Maybe

-- * Funções principais da Tarefa 6.

-- | Define um ro'bot' capaz de jogar autonomamente o jogo.
bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot nB estado@(Estado mapa jogadores disparos) | vBot == 0                                                                     = Nothing
                                               | muitosCanhoesAFrente posBot dBot estado && lBot > 0                           = Just $ Dispara Laser
                                               | canhaoPerto posBot nB mapa disparos                                           = Just $ desviar posBot dBot lBot estado
                                               | jogadorAdjacente posBot jogadores                                             = dispararAdjacente posBot dBot jogadores
                                               | jogadorPertoChoque posBot mapa jogadores && cBot > 0 && semChoque nB disparos = Just $ Dispara Choque
                                               | jogadorAVolta posBot estado                                                   = disparar posBot dBot estado
                                               | destrutiveisAVolta posBot estado                                              = Just $ destruir posBot dBot lBot estado
                                               | destrutiveisNoMapa posBot estado                                              = moverPraDestrutivel posBot dBot nB estado
                                               | otherwise                                                                     = moverPraJogador posBot dBot nB estado
    where (Jogador posBot dBot vBot lBot cBot) = encontraIndiceLista nB jogadores

-- | Verifica se o 'bot' tem algum 'Jogador' encostado a ele, mas que não se encontra diretamente de um dos seus lados.
jogadorAdjacente :: PosicaoGrelha -> [Jogador] -> Bool
jogadorAdjacente _ [] = False
jogadorAdjacente (y,x) (Jogador (a,b) _ v _ _:js) = v > 0 && distMan (y,x) (a,b) == 3 && y /= a && x /= b || jogadorAdjacente (y,x) js 


-- | Calcula a distância de Manhattan entre duas 'Posicao'es.
distMan :: (Int,Int) -> (Int,Int) -> Int
distMan (y,x) (a,b) = abs (y - a) + abs (x - b)


-- | 'Dispara' contra um 'Jogador' que se encontre adjacente ao 'bot'. 
dispararAdjacente :: PosicaoGrelha -> Direcao -> [Jogador] -> Maybe Jogada
dispararAdjacente _ _ [] = Nothing
dispararAdjacente (y,x) dir (j@(Jogador (a,b) _ _ _ _):js) | jogadorAdjacente (y,x) [j] = Just virarOuDisparar
                                                           | otherwise = dispararAdjacente (y,x) dir js
    where virarOuDisparar | (a,b) `elem` [(y-2,x-1),(y-2,x+1)] = if dir == C then Dispara Canhao else Movimenta C
                          | (a,b) `elem` [(y+2,x-1),(y+2,x+1)] = if dir == B then Dispara Canhao else Movimenta B
                          | (a,b) `elem` [(y-1,x-2),(y+1,x-2)] = if dir == E then Dispara Canhao else Movimenta E
                          | otherwise = if dir == D then Dispara Canhao else Movimenta D
                          
                          
-- | Verifica se existe um disparo de canhão a menos de 4 blocos do bot, que não seja do próprio bot, e a mover-se na direção do bot.
canhaoPerto :: PosicaoGrelha -> Int -> Mapa -> [Disparo] -> Bool
canhaoPerto _ _ _ [] = False
canhaoPerto (y,x) nB mapa (DisparoCanhao n (a,b) d:disps) = nB /= n && (abs (y - a) <= 3 ||
                                                                        abs (x - b) <= 3) && (case d of C -> y < a && abs (x - b) <= 2
                                                                                                        D -> x > b && abs (y - a) <= 2
                                                                                                        B -> y > a && abs (x - b) <= 2
                                                                                                        E -> x < b && abs (y - a) <= 2) && vaziasAte (a,b) [(y,x)] mapa || canhaoPerto (y,x) nB mapa disps
canhaoPerto p nB mapa (_:ds) = canhaoPerto p nB mapa ds


-- | Tenta desviar o bot de um disparo de canhão iminente, ou disparar outro canhão para destruir o que se aproxima.
desviar :: PosicaoGrelha -> Direcao -> Int -> Estado -> Jogada
desviar (y,x) d lasers e@(Estado m j (DisparoCanhao _ (a,b) dir:disps)) | canhaoAFrente (y,x) d (a,b) dir && not (muitosCanhoesAFrente (y,x) d e) = Dispara Canhao
                                                                        | canhaoVemDe C (a,b) dir (y,x) = desviarDe (C,(a,b)) ((y,x),d) e
                                                                        | canhaoVemDe D (a,b) dir (y,x) = desviarDe (D,(a,b)) ((y,x),d) e
                                                                        | canhaoVemDe B (a,b) dir (y,x) = desviarDe (B,(a,b)) ((y,x),d) e
                                                                        | canhaoVemDe E (a,b) dir (y,x) = desviarDe (E,(a,b)) ((y,x),d) e
                                                                        | otherwise                     = desviar (y,x) d lasers (Estado m j disps)
desviar p dir l (Estado mapa jogadores (_:disps)) = desviar p dir l (Estado mapa jogadores disps)
desviar _ _ _ _ = Dispara Canhao


-- | Verifica se existem vários canhões a dirigir-se em direção ao 'bot'.
muitosCanhoesAFrente :: PosicaoGrelha -> Direcao -> Estado -> Bool
muitosCanhoesAFrente (y,x) d (Estado m _ disps) = foldl (\acc disparo -> case disparo of DisparoCanhao _ (a,b) dir -> if canhaoEmFrente (a,b) dir && vaziasAte (a,b) [(y,x)] m then acc + 1 else acc; _ -> acc) 0 disps >= 3
    where canhaoEmFrente (a,b) dir | d == C = y > a && dir == B && b == x
                                   | d == D = x < b && dir == E && a == y
                                   | d == B = y < a && dir == C && b == x
                                   | otherwise = x > b && dir == D && a == y


-- | Verifica se existe um 'DisparoCanhao' à frente do 'bot'.
canhaoAFrente :: PosicaoGrelha -> Direcao -> PosicaoGrelha -> Direcao -> Bool
canhaoAFrente (y,x) d (a,b) dir = (y == a || x == b) && canhaoVemDe d (a,b) dir (y,x)

-- | Dada uma 'Direcao', a função retorna True se existir um 'DisparoCanhao' a dirigir-se para o 'bot' vindo dessa direção.
canhaoVemDe :: Direcao -> PosicaoGrelha -> Direcao -> PosicaoGrelha -> Bool
canhaoVemDe C (a,b) d (y,x) = y > a && d /= C && b `elem` [(x-2)..(x+2)]
canhaoVemDe D (a,b) d (y,x) = x < b && d /= D && a `elem` [(y-2)..(y+2)]
canhaoVemDe B (a,b) d (y,x) = y < a && d /= B && b `elem` [(x-2)..(x+2)]
canhaoVemDe E (a,b) d (y,x) = x > b && d /= E && a `elem` [(y-2)..(y+2)]

-- | Tenta desviar o 'bot' de um 'DisparoCanhao' iminente.
desviarDe :: (Direcao,PosicaoGrelha) -> (PosicaoGrelha,Direcao) -> Estado -> Jogada         
desviarDe (dir,(a,b)) ((y,x),d) estado | noCanto (a,b) (y,x)             = sairDoCanto (a,b) (y,x) d
                                       | livrePara dir1 (y,x) estado     = Movimenta dir1
                                       | livrePara dir2 (y,x) estado     = Movimenta dir2
                                       | livrePara dir3 (y,x) estado     = Movimenta dir3
                                       | semilivrePara dir1 (y,x) estado = Movimenta dir1
                                       | semilivrePara dir2 (y,x) estado = Movimenta dir2
                                       | semilivrePara dir3 (y,x) estado = Movimenta dir3
                                       | viradoParaCanhao (y,x) d estado = Dispara Laser
                                       | otherwise                       = virarParaCanhao (y,x) estado
    where dir1 | dir == C || dir == B = if b < x then D else E
               | otherwise            = if a < y then B else C
          dir2 = inverteDir dir1
          dir3 = inverteDir dir


-- | Devolve True caso exista um 'DisparoCanhao' a tocar num dos cantos do tanque do 'bot'.
noCanto :: PosicaoGrelha -> PosicaoGrelha -> Bool
noCanto (a,b) (y,x) = abs (a - y) == 1 && abs (b - x) == 1


-- | Desvia o 'bot' de um 'DisparoCanhao' que esteja a tocar num dos seus cantos.
sairDoCanto :: PosicaoGrelha -> PosicaoGrelha -> Direcao -> Jogada
sairDoCanto (a,b) (y,x) d | (a,b) == (y+1,x+1) && d == C = Movimenta C
                          | (a,b) == (y+1,x+1) && d == E = Movimenta E
                          | (a,b) == (y+1,x-1) && d == C = Movimenta C
                          | (a,b) == (y+1,x-1) && d == D = Movimenta D
                          | (a,b) == (y-1,x+1) && d == B = Movimenta B
                          | (a,b) == (y-1,x+1) && d == E = Movimenta E
                          | (a,b) == (y-1,x-1) && d == B = Movimenta B
                          | (a,b) == (y-1,x-1) && d == D = Movimenta D
                          | otherwise                    = Movimenta d


-- | Verifica se uma dada 'Direcao' se encontra livre para o 'bot' nela se deslocar.
livrePara :: Direcao -> PosicaoGrelha -> Estado -> Bool
livrePara d (y,x) (Estado mapa _ disparos) = semDisparosPara d (y,x) disparos && vaziasPara d (y,x) mapa


-- | Semelhante à função 'livrePara', mas apenas verifica os dois blocos mais próximos do 'bot', em vez de quatro.
semilivrePara :: Direcao -> PosicaoGrelha -> Estado -> Bool
semilivrePara d (y,x) (Estado mapa _ disparos) = semDisparosPara d (y,x) disparos && semivaziasPara d (y,x) mapa


-- | Verifica se existe algum 'DisparoCanhao' numa dada 'Direcao', se não existir nenhum devolve 'True'.
semDisparosPara :: Direcao -> PosicaoGrelha -> [Disparo] -> Bool
semDisparosPara _ _ [] = True
semDisparosPara di (y,x) (DisparoCanhao _ (a,b) dir:disps) | di == C = not ((a,b) `elem` [(c,d) | c <- [(y - 5)..(y - 1)], d <- [(x - 2)..(x + 2)]] && dir /= C) && semDisparosPara di (y,x) disps
                                                           | di == D = not ((a,b) `elem` [(c,d) | c <- [(y - 2)..(y + 2)], d <- [(x + 1)..(x + 5)]] && dir /= D) && semDisparosPara di (y,x) disps
                                                           | di == B = not ((a,b) `elem` [(c,d) | c <- [(y + 1)..(y + 5)], d <- [(x - 2)..(x + 2)]] && dir /= B) && semDisparosPara di (y,x) disps
                                                           | otherwise = not ((a,b) `elem` [(c,d) | c <- [(y - 2)..(y + 2)], d <- [(x - 5)..(x - 1)]] && dir /= E) && semDisparosPara di (y,x) disps 
semDisparosPara d (y,x) (_:disps) = semDisparosPara d (y,x) disps


-- | Verifica se existem 'Peca' não 'Vazia's nos 4 blocos adjacentes ao 'bot' numa dada 'Direcao'.
vaziasPara :: Direcao -> PosicaoGrelha -> Mapa -> Bool
vaziasPara d (y,x) mapa = all (\p -> encontraPosicaoMatriz p mapa == Vazia) pecas && case d of C -> y > 1
                                                                                               D -> x < snd (dimensaoMatriz mapa) - 1
                                                                                               B -> y < fst (dimensaoMatriz mapa) - 1
                                                                                               E -> x > 1
    where pecas | d == C = [(y-1,x),(y-1,x+1),(y-2,x),(y-2,x+1)]
                | d == D = [(y,x+2),(y+1,x+2),(y,x+3),(y+1,x+3)]
                | d == B = [(y+2,x),(y+2,x+1),(y+3,x),(y+3,x+1)]
                | otherwise = [(y,x-1),(y+1,x-1),(y,x-2),(y+1,x-2)]


-- | Verifica se existem 'Peca' não 'Vazia's nos 2 blocos adjacentes ao 'bot' numa dada 'Direcao'.
semivaziasPara :: Direcao -> PosicaoGrelha -> Mapa -> Bool
semivaziasPara d (y,x) mapa = all (\p -> encontraPosicaoMatriz p mapa == Vazia) pecas && case d of C -> y > 1
                                                                                                   D -> x < snd (dimensaoMatriz mapa) - 1
                                                                                                   B -> y < fst (dimensaoMatriz mapa) - 1
                                                                                                   E -> x > 1
    where pecas | d == C = [(y-1,x),(y-1,x+1)]
                | d == D = [(y,x+2),(y+1,x+2)]
                | d == B = [(y+2,x),(y+2,x+1)]
                | otherwise = [(y,x-1),(y+1,x-1)]


-- | Verifica se o 'bot' está virado para um 'DisparoCanhao'.
viradoParaCanhao :: PosicaoGrelha -> Direcao -> Estado -> Bool
viradoParaCanhao (y,x) d (Estado _ _ disparos) = not $ semDisparosPara d (y,x) disparos


-- | Vira o 'bot' na 'Direcao' de um 'DisparoCanhao'.
virarParaCanhao :: PosicaoGrelha -> Estado -> Jogada
virarParaCanhao (y,x) (Estado _ _ disparos) | not (semDisparosPara C (y,x) disparos) = Movimenta C
                                            | not (semDisparosPara D (y,x) disparos) = Movimenta D
                                            | not (semDisparosPara B (y,x) disparos) = Movimenta B
                                            | otherwise = Movimenta E


-- | Verifica se existe algum 'Jogador' perto o suficiente do 'bot' para ser afetado pelo 'DisparoChoque'.
jogadorPertoChoque :: PosicaoGrelha -> Mapa -> [Jogador] -> Bool
jogadorPertoChoque (y,x) mapa = foldl (\acc (Jogador (a,b) _ v _ _) -> (v > 0 && (a,b) /= (y,x) && abs (y - a) < 4 && abs (x - b) < 4 && vaziasAte (a,b) [(y,x)] mapa) || acc) False


-- | Verifica se existe um 'Jogador' contra o qual o 'bot' pode disparar imediatamente (ou após virar-se).
jogadorAVolta :: PosicaoGrelha -> Estado -> Bool
jogadorAVolta (y,x) (Estado mapa jogadores _) = vaziasAte (y,x) (posJogadoresAVolta (y,x) jogadores) mapa


-- | Verifica se o 'bot' tem algum 'DisparoChoque' disparado por si atualmente no mapa.
semChoque :: Int -> [Disparo] -> Bool
semChoque n = foldl (\acc disp -> case disp of DisparoChoque nj _ -> nj /= n && acc ; _ -> acc) True


-- | Verifica se o caminho entre uma 'PosicaoGrelha' e outras 'Posicao'es se encontra livre.
vaziasAte :: PosicaoGrelha -> [Posicao] -> Mapa -> Bool
vaziasAte _ [] _ = False
vaziasAte (y,x) ((a,b):ps) mapa | abs (x - b) <= 1 = all (\p -> encontraPosicaoMatriz p mapa /= Bloco Indestrutivel) [(c,d) | c <- l1, d <- [x,x+1]] || vaziasAte (y,x) ps mapa
                                | otherwise = all (\p -> encontraPosicaoMatriz p mapa /= Bloco Indestrutivel) [(c,d) | d <- l2, c <- [y,y+1]] || vaziasAte (y,x) ps mapa
    where l1 | a < y = [a..y]
             | otherwise = [y..a]
          l2 | b < x = [b..x]
             | otherwise = [x..b]


-- | Devolve as posicões dos 'Jogador'es à volta do 'bot'.
posJogadoresAVolta :: PosicaoGrelha -> [Jogador] -> [PosicaoGrelha]
posJogadoresAVolta _ [] = []
posJogadoresAVolta (y,x) (Jogador (a,b) _ v _ _:js) = if (a,b) /= (y,x) && v > 0 && (abs (y - a) <= 1 || abs (x - b) <= 1) && (abs (y - a) + abs (x - b) < 9) then (a,b):posJogadoresAVolta (y,x) js else posJogadoresAVolta (y,x) js


-- | Dispara um 'DisparoCanhao' contra um 'Jogador'.
disparar :: PosicaoGrelha -> Direcao -> Estado -> Maybe Jogada
disparar (y,x) d (Estado mapa jogadores _) | jogadorAFrente (y,x) d jogadores mapa = Just (Dispara Canhao)
                                                  | otherwise = if dirV /= "X" then Just (Movimenta (read dirV)) else Nothing
    where dirV = virarParaJogador (y,x) d jogadores mapa


-- | Verifica se existe um 'Jogador' à frente do 'bot'.
jogadorAFrente :: PosicaoGrelha -> Direcao -> [Jogador] -> Mapa -> Bool
jogadorAFrente _ _ [] _ = False
jogadorAFrente (y,x) d (Jogador (a,b) _ v _ _:js) mapa = (v > 0 && (case d of C -> x `elem` [(b-1)..(b+1)] && a < y
                                                                              D -> y `elem` [(a-1)..(a+1)] && b > x
                                                                              B -> x `elem` [(b-1)..(b+1)] && a > y
                                                                              E -> y `elem` [(a-1)..(a+1)] && b < x) && vaziasAte (y,x) [(a,b)] mapa) || jogadorAFrente (y,x) d js mapa


-- | Devolve a 'Direcao' para a qual o 'bot' se deve virar para estar de frente a um 'Jogador', em forma de 'String'.
--
-- __NB:__Caso não haja jogadores para os quais o 'bot' se possa virar, a função devolve "X", que será interpretado pela função 'disparar' como 'Nothing'.
virarParaJogador :: PosicaoGrelha -> Direcao -> [Jogador] -> Mapa -> String
virarParaJogador (y,x) d (Jogador (a,b) _ v _ _:js) mapa = if (a,b) /= (y,x) && v > 0 && (y `elem` [(a-1)..(a+1)] || x `elem` [(b-1)..(b+1)]) && vaziasAte (y,x) [(a,b)] mapa then dirAux else virarParaJogador (y,x) d js mapa
    where dirAux | a >= y + 1 && d /= B = "B"
                 | a <= y - 1 && d /= C = "C"
                 | b <= x - 1 && d /= E = "E"
                 | b >= x + 1 && d /= D = "D"
                 | otherwise = "X"
virarParaJogador _ _ _ _ = "X"


-- | Verifica se existem 'Bloco's 'Destrutivel'is que o 'bot' possa destruir ao seu redor.
destrutiveisAVolta :: PosicaoGrelha -> Estado -> Bool
destrutiveisAVolta (y,x) (Estado mapa _ _) = foldl (\acc d -> not (null (bateEmBlocosX (y,x) d mapa)) || acc) False [C,D,B,E]


-- | Igual à função 'bateEmBlocos' da 'Tarefa4_2018li1g080', mas esta recebe uma 'PosicaoGrelha' e uma 'Direcao' em vez de um 'Disparo'. Devolve as 'Posicao'es dos 'Bloco's 'Destrutivel'is nessa 'Direcao'.
bateEmBlocosX :: PosicaoGrelha -> Direcao -> Mapa -> [PosicaoGrelha]
bateEmBlocosX (y,x) d mapa | any (\pos -> encontraPosicaoMatriz pos mapa == Bloco Indestrutivel) (blocosOndeBate (y,x) d) = filter (\pos -> encontraPosicaoMatriz pos mapa == Bloco Destrutivel) (blocosOndeBate (y,x) d)
                           | otherwise = filter (\pos -> encontraPosicaoMatriz pos mapa == Bloco Destrutivel) (blocosOndeBate (y,x) d) ++ bateEmBlocosX posNova d mapa
    where posNova = somaVetores (y,x) (direcaoParaVetor d)


-- | Destroi o(s) 'Bloco'(s) 'Destrutivel'(eis) à volta do 'bot'.
destruir :: PosicaoGrelha -> Direcao -> Int -> Estado -> Jogada
destruir (y,x) d l (Estado mapa _ _) | length (bateEmBlocosX (y,x) d mapa) > 5 && l > 0 = Dispara Laser
                                     | length (bateEmBlocosX (y,x) d mapa) > 0 = Dispara Canhao
                                     | otherwise = foldl (\acc dir -> if not (null (bateEmBlocosX (y,x) dir mapa)) then Movimenta dir else acc) (Movimenta d) ([C,D,B,E] \\ [d])


-- | Verifica se existem 'Bloco's 'Destrutivel'is no mapa.
destrutiveisNoMapa :: PosicaoGrelha -> Estado -> Bool
destrutiveisNoMapa _ (Estado [] _ _) = False
destrutiveisNoMapa _ (Estado [_] _ _) = False
destrutiveisNoMapa (y,x) (Estado (l:mapa) j d) = Bloco Destrutivel `elem` l || destrutiveisNoMapa (y,x) (Estado mapa j d)


-- | Movimenta o 'bot' para onde possa destruir 'Bloco's 'Destrutivel'is.
moverPraDestrutivel :: PosicaoGrelha -> Direcao -> Int -> Estado -> Maybe Jogada
moverPraDestrutivel (y,x) d nB e@(Estado mapa _ disps) | not (null posO) = moverPara (minimumBy (\pos1 pos2 -> compare (distMan (y,x) pos1) (distMan (y,x) pos2)) posO) d (y,x) mapa
                                                       | otherwise = moverPraJogador (y,x) d nB e
    where posO = posicoesOtimas (y,x) mapa disps


-- | Devolve as 'PosicaoGrelha's de onde o 'bot' consegue destruir 'Bloco's.
posicoesOtimas :: PosicaoGrelha -> Mapa -> [Disparo] -> [PosicaoGrelha]
posicoesOtimas (a,b) mapa disparos = foldl (\acc (y,x) -> if not (any (\pos -> encontraPosicaoMatriz pos mapa == Bloco Indestrutivel) [(y,x),(y+1,x),(y,x+1),(y+1,x+1)]) && destrutiveisAVolta (y,x) (Estado mapa [] disparos) then (y,x):acc else acc) [] posicoesMapa
    where posicoesMapa = [(y,x) | y <- [yMin..yMax], x <- [xMin..xMax]]
          (linesM,rows) = dimensaoMatriz mapa
          yMin = max 1 (a - 7)
          yMax = min (linesM - 3) (a + 7)
          xMin = max 1 (b - 7)
          xMax = min (rows - 3) (b + 7) 


-- | Movimenta o 'bot' para uma 'PosicaoGrelha' onde esteja outro 'Jogador'.
moverPraJogador :: PosicaoGrelha -> Direcao -> Int -> Estado -> Maybe Jogada
moverPraJogador (y,x) dir nB (Estado mapa jogadores disps) | not (null posJ) && isJust moveHead = moveHead
                                                           | length posJ > 1 && isJust move2 = move2
                                                           | length posJ > 2 && isJust move3 = move3
                                                           | otherwise = deambular (y,x) dir (Estado mapa jogadores disps)
    where posJ = sortBy (\(a,b) (c,d) -> compare (abs (a-y) + abs (b-x)) (abs (c-y) + abs (d-x))) (posicoesJog jogadores (y,x) nB 0)
          moveHead = moverPara (head posJ) dir (y,x) mapa
          move2 = moverPara (posJ !! 1) dir (y,x) mapa
          move3 = moverPara (posJ !! 2) dir (y,x) mapa    
    

-- | Devolve as 'PosicaoGrelha's dos 'Jogador'es que ainda estão vivos e que não são o próprio 'bot'.
posicoesJog :: [Jogador] -> PosicaoGrelha -> Int -> Int -> [PosicaoGrelha]
posicoesJog [] _ _ _ = []
posicoesJog (Jogador (a,b) _ v _ _:js) (y,x) nB n | nB /= n && v > 0 = (a,b) : posicoesJog js (y,x) nB (n + 1)
                                                  | otherwise = posicoesJog js (y,x) nB (n + 1)


-- | Move o 'bot' de uma 'PosicaoGrelha' para outra 'PosicaoGrelha', se encontrar um caminho possível.
moverPara :: PosicaoGrelha -> Direcao -> PosicaoGrelha -> Mapa -> Maybe Jogada
moverPara (a,b) d (y,x) mapa | diretoPara (head dirs) (y,x) poss mapa = Just $ Movimenta (head dirs)
                             | diretoPara (dirs !! 1) (y,x) poss mapa = Just $ Movimenta (dirs !! 1)
                             | diretoPara (dirs !! 2) (y,x) poss mapa = Just $ Movimenta (dirs !! 2)
                             | diretoPara (dirs !! 3) (y,x) poss mapa = Just $ Movimenta (dirs !! 3)
                             | indiretoPara (head dirs) (y,x) poss mapa = Just $ Movimenta (head dirs)
                             | indiretoPara (dirs !! 1) (y,x) poss mapa = Just $ Movimenta (dirs !! 1)
                             | indiretoPara (dirs !! 2) (y,x) poss mapa = Just $ Movimenta (dirs !! 2)
                             | indiretoPara (dirs !! 3) (y,x) poss mapa = Just $ Movimenta (dirs !! 3)
                             | otherwise = deambular (y,x) d (Estado mapa [] [])
    where poss = posAVolta (a,b) mapa
          dirs = [d] ++ (if d == C || d == B then if b < x then [E,D] else [D,E] else if a < y then [C,B] else [B,C]) ++ [inverteDir d]

-- | Verifica se existe um caminho direto entre duas 'PosicaoGrelha's.
diretoPara :: Direcao -> PosicaoGrelha -> [PosicaoGrelha] -> Mapa -> Bool
diretoPara dir (y,x) poss mapa = foldl (\acc (a,b) -> (a,b) `elem` poss || acc) False pDir
    where pDir | dir == C = posAcima (y,x) mapa
               | dir == D = posDir (y,x) mapa
               | dir == B = posAbaixo (y,x) mapa
               | otherwise = posEsq (y,x) mapa


-- | Verifica se existe um caminho indireto (ou seja, um caminho que envolve mudar de 'Direcao') entre duas 'PosicaoGrelha's.
indiretoPara :: Direcao -> PosicaoGrelha -> [PosicaoGrelha] -> Mapa -> Bool
indiretoPara dir (y,x) poss mapa = foldl (\acc (a,b) -> ((dir == C || dir == B) && foldl (\acc2 (c,d) -> c == a && vaziasAte (c,d) [(a,b)] mapa || acc2) False poss) || ((dir == E || dir == D) && foldl (\acc2 (c,d) -> d == b && vaziasAte (c,d) [(a,b)] mapa || acc2) False poss) || acc) False pDir
    where pDir | dir == C = posAcima (y,x) mapa
               | dir == D = posDir (y,x) mapa
               | dir == B = posAbaixo (y,x) mapa
               | otherwise = posEsq (y,x) mapa


-- | Devolve todas as 'PosicaoGrelha's à volta de uma 'PosicaoGrelha' que se encontrem livres.
posAVolta :: PosicaoGrelha -> Mapa -> [Posicao]
posAVolta (y,x) mapa = posAcima (y,x) mapa ++ posAbaixo (y,x) mapa ++ posEsq (y,x) mapa ++ posDir (y,x) mapa ++ [(y,x)]


-- | Devolve todas as 'PosicaoGrelha's acima de uma 'PosicaoGrelha' que se encontrem livres.
posAcima :: PosicaoGrelha -> Mapa -> [Posicao]
posAcima (y,x) mapa = posAcima1
    where (posAcima1,_) = foldl (\(pp,b) n -> if b && encontraPosicaoMatriz (n,x) mapa /= Bloco Indestrutivel && encontraPosicaoMatriz (n,x+1) mapa /= Bloco Indestrutivel then ((n,x):pp,b) else (pp,False)) ([],True) (reverse [1..(y - 1)])


-- | Devolve todas as 'PosicaoGrelha's abaixo de uma 'PosicaoGrelha' que se encontrem livres.
posAbaixo :: PosicaoGrelha -> Mapa -> [Posicao]
posAbaixo (y,x) mapa = posAbaixo1
    where (posAbaixo1,_) = foldl (\(pp,b) n -> if b && encontraPosicaoMatriz (n+1,x) mapa /= Bloco Indestrutivel && encontraPosicaoMatriz (n+1,x+1) mapa /= Bloco Indestrutivel then ((n,x):pp,b) else (pp,False)) ([],True) [(y + 1)..(linesM - 3)]
          (linesM,_) = dimensaoMatriz mapa


-- | Devolve todas as 'PosicaoGrelha's à esquerda de uma 'PosicaoGrelha' que se encontrem livres.
posEsq :: PosicaoGrelha -> Mapa -> [Posicao]
posEsq (y,x) mapa = posEsq1
    where (posEsq1,_) = foldl (\(pp,b) n -> if b && encontraPosicaoMatriz (y,n) mapa /= Bloco Indestrutivel && encontraPosicaoMatriz (y+1,n) mapa /= Bloco Indestrutivel then ((y,n):pp,b) else (pp,False)) ([],True) (reverse [1..(x - 1)])


-- | Devolve todas as 'PosicaoGrelha's à direita de uma 'PosicaoGrelha' que se encontrem livres.
posDir :: PosicaoGrelha -> Mapa -> [Posicao]
posDir (y,x) mapa = posDir1
    where (posDir1,_) = foldl (\(pp,b) n -> if b && encontraPosicaoMatriz (y,n+1) mapa /= Bloco Indestrutivel && encontraPosicaoMatriz (y+1,n+1) mapa /= Bloco Indestrutivel then ((y,n):pp,b) else (pp,False)) ([],True) [(x + 1)..(rows - 3)]
          (_,rows) = dimensaoMatriz mapa


-- | Faz com que o 'bot' ande pelo 'Mapa' aleatoriamente, sem destino definido. Esta função só é executada se o 'bot' não se conseguir deslocar para nenhum 'Jogador' / 'Bloco Destrutivel'.
deambular :: PosicaoGrelha -> Direcao -> Estado -> Maybe Jogada
deambular (y,x) d estado | semilivrePara (head dirs) (y,x) estado = Just $ Movimenta (head dirs)
                         | semilivrePara (dirs !! 1) (y,x) estado = Just $ Movimenta (dirs !! 1)
                         | semilivrePara (dirs !! 2) (y,x) estado = Just $ Movimenta (dirs !! 2)
                         | semilivrePara (dirs !! 3) (y,x) estado = Just $ Movimenta (dirs !! 3)
                         | otherwise = Nothing
    where dirs = [d] ++ (if d == C || d == B then [E,D] else [C,B]) ++ [inverteDir d]