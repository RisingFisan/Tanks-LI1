-- | Este módulo define funções comuns da Tarefa 4 do trabalho prático.
module Tarefa4_2018li1g080 where

import LI11819
import Tarefa0_2018li1g080
import Tarefa1_2018li1g080
import Data.List (delete)

-- * Testes
-- | Testes unitários da Tarefa 4.
--
-- Cada teste é um 'Estado'.
testesT4 :: [Estado]
testesT4 = [Estado mapaTestes [Jogador (3,3) D 5 3 3,Jogador (3,6) B 4 3 2] [DisparoLaser 0 (3,4) D],
            Estado mapaTestes [Jogador (7,7) C 5 3 3,Jogador (6,3) D 4 3 2,Jogador (3,6) E 3 2 1] [DisparoLaser 1 (6,4) D,DisparoLaser 0 (6,7) C,DisparoChoque 2 3],
            Estado mapaTestes [Jogador (7,7) C 5 3 3,Jogador (6,3) D 4 3 2,Jogador (3,6) E 3 2 1] [DisparoCanhao 1 (6,4) D,DisparoCanhao 0 (6,7) C,DisparoLaser 2 (3,5) E],
            Estado {mapaEstado = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]], jogadoresEstado = [Jogador {posicaoJogador = (3,3), direcaoJogador = D, vidasJogador = 5, lasersJogador = 3, choquesJogador = 3},Jogador {posicaoJogador = (4,6), direcaoJogador = B, vidasJogador = 4, lasersJogador = 3, choquesJogador = 2}], disparosEstado = [DisparoCanhao {jogadorDisparo = 0, posicaoDisparo = (3,5), direcaoDisparo = D}, DisparoCanhao {jogadorDisparo = 1, posicaoDisparo = (5,6), direcaoDisparo = B}]},
            Estado {mapaEstado = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]], jogadoresEstado = [Jogador {posicaoJogador = (3,3), direcaoJogador = D, vidasJogador = 5, lasersJogador = 3, choquesJogador = 3},Jogador {posicaoJogador = (3,6), direcaoJogador = E, vidasJogador = 4, lasersJogador = 3, choquesJogador = 2}], disparosEstado = [DisparoCanhao {jogadorDisparo = 0, posicaoDisparo = (3,4), direcaoDisparo = D},DisparoCanhao {jogadorDisparo = 1, posicaoDisparo = (3,5), direcaoDisparo = E}]},
            Estado (mapaInicial (10,10)) [Jogador (3,2) D 5 3 3,Jogador (7,1) C 4 3 2,Jogador (7,7) B 5 4 3] [DisparoLaser 0 (3,3) D,DisparoCanhao 1 (3,6) E, DisparoCanhao 1 (3,7) E, DisparoCanhao 2 (5,2) C],
            Estado (mapaInicial (10,10)) [Jogador (3,2) D 5 3 3,Jogador (7,1) C 4 3 2,Jogador (7,7) B 5 4 3] [DisparoCanhao 0 (3,6) E, DisparoCanhao 1 (3,6) D, DisparoCanhao 2 (5,2) C],
            Estado (mapaInicial (10,10)) [Jogador (3,2) D 5 3 3,Jogador (7,1) C 4 3 2,Jogador (7,7) B 5 4 3] [DisparoCanhao 0 (3,6) D, DisparoCanhao 1 (3,5) E, DisparoCanhao 2 (5,2) C]
            ]
-- Também usámos a tarefa 5 para testar a tarefa 4, por isso a quantidade "reduzida" de testes não significa que não testámos devidamente a tarefa.

-- | Mapa usado para alguns dos testes desta tarefa.
mapaTestes :: Mapa
mapaTestes = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],
             [Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel],
             [Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel],
             [Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel],
             [Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel],
             [Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel],
             [Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel],
             [Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],
             [Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],
             [Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]

-- * Funções principais da Tarefa 4.

-- | Avança o 'Estado' do jogo um 'Tick' de tempo.
--
-- __NB:__ Apenas os 'Disparo's afetam o 'Estado' do jogo com o passar do tempo.
--
-- __NB:__ Deve chamar as funções 'tickChoques', 'tickCanhoes' e 'tickLasers' pela ordem definida.
tick :: Estado -- ^ O 'Estado' anterior.
     -> Estado -- ^ O 'Estado' após um 'Tick'.
tick = tickChoques . tickCanhoes . tickLasers

-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos dos tiros de 'Laser' disparados.
tickLasers :: Estado -> Estado
tickLasers estado@(Estado _ _ disparos) = foldl tickLaser estado disparos 


-- | Função auxiliar da 'tickLasers', avança apenas um 'DisparoLaser' no tempo.
tickLaser :: Estado -> Disparo -> Estado
tickLaser estado@(Estado mapa jogadores disparos) disp@DisparoLaser{} = estado{mapaEstado = foldl (\acc x -> atualizaPosicaoMatriz x Vazia acc) mapa (init blocosQBate),
                                                                                         jogadoresEstado = map (\jog@(Jogador _ _ vidas _ _) -> if vidas > 0 && bateNoJogador disp jog (last blocosQBate) then jog{vidasJogador = vidas - 1} else jog) jogadores,
                                                                                         disparosEstado = delete disp (apagaCanhoes disp disparos (last blocosQBate))}
    where blocosQBate = bateEmBlocos disp mapa
tickLaser estado _ = estado

-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos das balas de 'Canhao' disparadas.
tickCanhoes :: Estado -> Estado
tickCanhoes estado@(Estado mapa _ disparos) = foldr (\x acc -> tickCanhao acc disparos mapa x) estado disparos


-- | Função auxiliar da 'tickCanhoes', avança apenas um 'DisparoCanhao' no tempo.
tickCanhao :: Estado -> [Disparo] -> Mapa -> Disparo -> Estado
tickCanhao estado@(Estado mapa jogadores disparos) disparosI mapaI disp@(DisparoCanhao _ (y,x) d) = if any (\disp2 -> case disp2 of DisparoCanhao _ (a,b) dir -> colisao (a,b) dir; _ -> False) (delete disp disparosI)
                                                                                                    then estado{disparosEstado = foldr (\disp2 acc -> case disp2 of DisparoCanhao _ (a,b) dir -> if colisao (a,b) dir then acc else disp2:acc;_ -> disp2:acc) [] (delete disp disparos)}
                                                                                                    else estado{mapaEstado = tickCanhaoMapa mapa (y,x) d,
                                                                                                                jogadoresEstado = map (\jog@(Jogador _ _ vidas _ _) -> if vidas > 0 && bateNoJogador disp jog (y,x) then jog{vidasJogador = vidas - 1} else jog) jogadores,
                                                                                                                disparosEstado = if not (all (\pos -> encontraPosicaoMatriz pos mapaI == Vazia) (blocosOndeBate (y,x) d)) || any (\jog@(Jogador _ _ vidas _ _) -> vidas > 0 && bateNoJogador disp jog (y,x)) jogadores then delete disp disparos else disp{posicaoDisparo = somaVetores (y,x) (direcaoParaVetor d)}:delete disp disparos}
    where colisao (a,b) dir = (a,b) == (y,x) || (a,b) == somaVetores (y,x) (direcaoParaVetor (inverteDir d)) && inverteDir d == dir
tickCanhao estado _ _ _ = estado 


-- | Processa os efeitos de um 'DisparoCanhao' no 'Mapa', ou seja, destroi os 'Bloco's 'Destrutivel'is com os quais o 'DisparoCanhao' colide.
tickCanhaoMapa :: Mapa -> PosicaoGrelha -> Direcao -> Mapa
tickCanhaoMapa mapa (y,x) d = foldl (\acc bl -> case encontraPosicaoMatriz bl mapa of Bloco Destrutivel -> atualizaPosicaoMatriz bl Vazia acc; _ -> acc) mapa (blocosOndeBate (y,x) d)

-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos dos campos de 'Choque' disparados.
tickChoques :: Estado -> Estado
tickChoques estado@(Estado _ _ disparos) = estado{disparosEstado = foldl (\acc d -> case d of DisparoChoque _ t -> if t > 0 then acc ++ [d{tempoDisparo = t - 1}] else acc; _ -> acc ++ [d]) [] disparos}

-- * Funções auxiliares da Tarefa 4.

-- | Verifica se um 'Disparo' embate num 'Jogador'
bateNoJogador :: Disparo -> Jogador -> PosicaoGrelha -> Bool
bateNoJogador (DisparoCanhao _ (y,x) d) (Jogador (a,b) _ _ _ _) _ = (a,b) `elem` posJogadoresOndeBate (y,x) d
bateNoJogador (DisparoLaser n (y,x) d) j@(Jogador (a,b) _ _ _ _) fim | (y,x) == fim = (a,b) `elem` posJogadoresOndeBate (y,x) d
                                                                     | otherwise = (a,b) `elem` posJogadoresOndeBate (y,x) d || bateNoJogador (DisparoLaser n (somaVetores (y,x) (direcaoParaVetor d)) d) j fim


-- | Devolve os 'Bloco's 'Destrutivel'is que um 'DisparoLaser' destroi.
bateEmBlocos :: Disparo -> Mapa -> [PosicaoGrelha]
bateEmBlocos dl@(DisparoLaser _ (y,x) d) mapa | any (\pos -> encontraPosicaoMatriz pos mapa == Bloco Indestrutivel) (blocosOndeBate (y,x) d) = filter (\pos -> encontraPosicaoMatriz pos mapa == Bloco Destrutivel) (blocosOndeBate (y,x) d) ++ [(y,x)]
                                              | otherwise = filter (\pos -> encontraPosicaoMatriz pos mapa == Bloco Destrutivel) (blocosOndeBate (y,x) d) ++ bateEmBlocos dl{posicaoDisparo = posNovaDisp} mapa
    where posNovaDisp = somaVetores (y,x) (direcaoParaVetor d)


-- | Destroi os 'DisparoCanhao' que colidem com um 'DisparoLaser'.
apagaCanhoes :: Disparo -> [Disparo] -> PosicaoGrelha -> [Disparo]
apagaCanhoes _ [] _ = []
apagaCanhoes dl@(DisparoLaser _ (y,x) d) (dc@(DisparoCanhao _ (a,b) _):t) (yf,xf) = if (a,b) `elem` listaP then apagaCanhoes dl t (yf,xf) else dc:apagaCanhoes dl t (yf,xf)
    where listaP = case d of C -> [(yt,x) | yt <- [yf..y]]
                             D -> [(y,xt) | xt <- [x..xf]]
                             B -> [(yt,x) | yt <- [y..yf]]
                             E -> [(y,xt) | xt <- [xf..x]]
apagaCanhoes dl (h:t) fim = h:apagaCanhoes dl t fim


-- | Inverte a 'Direcao' dada.
--
-- __Eg:__ inverteDir B == C
inverteDir :: Direcao -> Direcao
inverteDir B = C
inverteDir E = D
inverteDir C = B
inverteDir D = E


-- | Devolve as 'Posicao'es dos 'Bloco's com os quais um 'DisparoCanhao' com uma dada 'PosicaoGrelha' e 'Direcao' embate.
blocosOndeBate :: PosicaoGrelha -> Direcao -> [Posicao]
blocosOndeBate (y,x) C = [(y,x),(y,x+1)]
blocosOndeBate (y,x) D = [(y,x+1),(y+1,x+1)]
blocosOndeBate (y,x) B = [(y+1,x),(y+1,x+1)]
blocosOndeBate (y,x) E = [(y,x),(y+1,x)]


-- | Devolve as 'Posicao'es dos 'Jogador'es com os quais um 'DisparoCanhao' com uma dada 'PosicaoGrelha' e 'Direcao' embate.
posJogadoresOndeBate :: PosicaoGrelha -> Direcao -> [PosicaoGrelha]
posJogadoresOndeBate (y,x) C = [(y-1,x),(y-1,x-1),(y-1,x+1)]
posJogadoresOndeBate (y,x) D = [(y,x+1),(y+1,x+1),(y-1,x+1)]
posJogadoresOndeBate (y,x) B = [(y+1,x),(y+1,x-1),(y+1,x+1)]
posJogadoresOndeBate (y,x) E = [(y,x-1),(y+1,x-1),(y-1,x-1)]