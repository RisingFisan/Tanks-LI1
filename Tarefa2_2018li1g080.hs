-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa2_2018li1g080 where

import LI11819
import Tarefa0_2018li1g080
import Tarefa1_2018li1g080

-- * Testes

-- | Testes unitários da Tarefa 2.
--
-- Cada teste é um triplo (/identificador do 'Jogador'/,/'Jogada' a efetuar/,/'Estado' anterior/).
testesT2 :: [(Int,Jogada,Estado)]
testesT2 = [(0,Movimenta B,(Estado {mapaEstado = mapaInicial (7,7), jogadoresEstado = [Jogador (3,3) E 3 5 5],disparosEstado = []})),
            (0,Movimenta B,(Estado {mapaEstado = mapaInicial (7,7), jogadoresEstado = [Jogador (3,3) B 3 5 5],disparosEstado = []})),
            (1,Movimenta E,(Estado {mapaEstado = mapaInicial (7,7), jogadoresEstado = [Jogador (3,3) B 3 5 5, Jogador (1,1) E 0 0 0],disparosEstado = []})),
            (0,Movimenta C,(Estado {mapaEstado = mapaInicial (7,7), jogadoresEstado = [Jogador (1,1) E 3 5 5],disparosEstado = []})),
            (0,Dispara Laser,(Estado {mapaEstado = mapaInicial (7,7), jogadoresEstado = [Jogador (3,3) E 3 5 5],disparosEstado = []})),
            (0,Dispara Laser,(Estado {mapaEstado = mapaInicial (7,7), jogadoresEstado = [Jogador (3,3) E 3 0 0],disparosEstado = []})),
            (0,Dispara Canhao,(Estado {mapaEstado = mapaInicial (7,7), jogadoresEstado = [Jogador (3,3) E 3 5 5],disparosEstado = []})),
            (0,Dispara Choque,(Estado {mapaEstado = mapaInicial (7,7), jogadoresEstado = [Jogador (3,3) E 3 0 0],disparosEstado = []})),
            (0,Movimenta C,Estado (mapaInicial (12,12)) [Jogador (5,5) C 5 3 3] [DisparoChoque 0 5]),
            (0,Movimenta C,Estado (mapaInicial (12,12)) [Jogador (5,5) C 5 3 3,Jogador (7,6) B 3 2 1] [DisparoChoque 0 5,DisparoChoque 1 3]),
            (1,Dispara Laser,Estado (mapaInicial (12,12)) [Jogador (6,6) D 4 3 2,Jogador (8,6) C 5 3 2] [DisparoCanhao 0 (6,9) D,DisparoChoque 0 3,DisparoChoque 1 4]),
            (0,Movimenta B,Estado (mapaInicial (12,12)) [Jogador (6,6) B 5 3 3,Jogador (8,6) C 5 3 3] []),
            (0,Movimenta E,Estado (mapaInicial (12,12)) [Jogador (9,1) B 5 3 3,Jogador (8,6) C 5 3 3] [])
            ]

-- * Funções principais da Tarefa 2.

-- | Efetua uma jogada.
jogada :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
       -> Jogada -- ^ A 'Jogada' a efetuar.
       -> Estado -- ^ O 'Estado' anterior.
       -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada.

jogada jogadorNum (Movimenta dir) estado = if temVidas jogador 
                                           then if dir == direcaoJogador jogador && posicaoLivre jogador dir estado && foraDeChoque jogador estado && semTanques (posicaoNova jogador dir) jogadorNum (jogadoresEstado estado) 0
                                                then estado {jogadoresEstado = atualizaIndiceLista jogadorNum (jogador {posicaoJogador = posicaoNova jogador dir}) (jogadoresEstado estado) }
                                                else estado {jogadoresEstado = atualizaIndiceLista jogadorNum (jogador {direcaoJogador = dir}) (jogadoresEstado estado) }
                                           else estado
    where jogador = encontraIndiceLista jogadorNum (jogadoresEstado estado)
          
jogada jogadorNum (Dispara Canhao) estado = if temVidas jogador
                                            then estado {disparosEstado = DisparoCanhao jogadorNum (posicaoNova jogador (direcaoJogador jogador)) (direcaoJogador jogador):disparosEstado estado }
                                            else estado
    where jogador = encontraIndiceLista jogadorNum (jogadoresEstado estado)
jogada jogadorNum (Dispara Laser) estado = if lasersJogador jogador > 0 && temVidas jogador
                                           then estado {jogadoresEstado = atualizaIndiceLista jogadorNum (jogador {lasersJogador = lasersJogador jogador - 1}) (jogadoresEstado estado), disparosEstado = DisparoLaser jogadorNum (posicaoNova jogador (direcaoJogador jogador)) (direcaoJogador jogador):disparosEstado estado }
                                           else estado
    where jogador = encontraIndiceLista jogadorNum (jogadoresEstado estado)
jogada jogadorNum (Dispara Choque) estado = if choquesJogador jogador > 0 && temVidas jogador
                                            then estado {jogadoresEstado = atualizaIndiceLista jogadorNum (jogador {choquesJogador = choquesJogador jogador - 1}) (jogadoresEstado estado), disparosEstado = DisparoChoque jogadorNum 5 : disparosEstado estado }
                                            else estado
    where jogador = encontraIndiceLista jogadorNum (jogadoresEstado estado)

-- | Dados um 'Jogador' e uma 'Direcao', devolve a 'Posicao' do 'Jogador' movido nessa direção.

posicaoNova :: Jogador -- ^ O 'Jogador' que efetuou a 'Jogada'.
            -> Direcao -- ^ A 'Direcao' na qual o 'Jogador' se pretende mover.
            -> Posicao -- ^ A nova 'Posicao' do 'Jogador'.
posicaoNova jogador dir = somaVetores (posicaoJogador jogador) (direcaoParaVetor dir)

-- | Verifica se um dado 'Jogador' ainda tem vidas.

temVidas :: Jogador -- ^ O 'Jogador' cujo número de vidas queremos verificar.
         -> Bool -- ^ 'True' se o 'Jogador' tiver vidas, 'False' se não tiver.
temVidas jogador = vidasJogador jogador > 0

-- | Verifica se a 'Posicao' para a qual um 'Jogador' se pretende mover se encontra livre de 'Peca's.

posicaoLivre :: Jogador -- ^ O 'Jogador' que efetuou a 'Jogada'.
             -> Direcao -- ^ A 'Direcao' na qual o 'Jogador' se pretende mover.
             -> Estado -- ^ O 'Estado' anterior à 'Jogada'.
             -> Bool -- ^ 'True' se as 'Peca's à frente do 'Jogador' estiverem 'Vazia's, 'False' se não estiverem.
posicaoLivre jogador C (Estado mapa _ _) = encontraPosicaoMatriz (y,x) mapa == Vazia && encontraPosicaoMatriz (y,x + 1) mapa == Vazia
    where (y,x) = posicaoNova jogador C
posicaoLivre jogador D (Estado mapa _ _) = encontraPosicaoMatriz (y,x + 1) mapa == Vazia && encontraPosicaoMatriz (y + 1,x + 1) mapa == Vazia
    where (y,x) = posicaoNova jogador D
posicaoLivre jogador B (Estado mapa _ _) = encontraPosicaoMatriz (y + 1,x) mapa == Vazia && encontraPosicaoMatriz (y + 1,x + 1) mapa == Vazia
    where (y,x) = posicaoNova jogador B
posicaoLivre jogador E (Estado mapa _ _) = encontraPosicaoMatriz (y,x) mapa == Vazia && encontraPosicaoMatriz (y + 1,x) mapa == Vazia
    where (y,x) = posicaoNova jogador E

-- | Verifica se um dado 'Jogador' se encontra numa zona de 'DisparoChoque'.

foraDeChoque :: Jogador -- ^ O 'Jogador' que efetuou a 'Jogada'.
             -> Estado -- ^ O 'Estado' anterior à 'Jogada'.
             -> Bool -- ^ 'True' se o 'Jogador' não estiver dentro de um 'DisparoChoque' de outro 'Jogador', 'False' se estiver.
foraDeChoque jogador estado@(Estado _ jogadores disparos) = null disparos || case head disparos of DisparoChoque njds _ -> (abs (y - yj) > 3 || abs (x - xj) > 3) || jogador == encontraIndiceLista njds (jogadoresEstado estado) && foraDeChoque jogador (estado {disparosEstado = tail disparos})
                                                                                                   _ -> foraDeChoque jogador (estado {disparosEstado = tail disparos})
    where (y,x) = posicaoJogador (encontraIndiceLista (jogadorDisparo (head disparos)) jogadores)
          (yj,xj) = posicaoJogador jogador

-- | Verifica se a 'Posicao' para a qual um 'Jogador' se pretende mover se encontra livre de outros 'Jogador'es.

semTanques :: Posicao -- ^ A 'Posicao' para a qual o 'Jogador' que efetuou a 'Jogada' se pretende mover.
           -> Int -- ^ O número do 'Jogador' que efetuou a 'Jogada'.
           -> [Jogador] -- ^ A lista dos 'Jogador'es no 'Estado' atual.
           -> Int -- ^ O número do 'Jogador' cuja 'Posicao' estamos a comparar com a 'Posicao' para a qual o 'Jogador' que efetuou a 'Jogada' se pretende mover.
           -> Bool -- ^ 'True' se o 'Jogador' não tiver outros tanques à sua frente, 'False' se tiver.
semTanques (_,_) _ [] _ = True
semTanques (y,x) nJ (j:js) nlJ | nJ == nlJ = semTanques (y,x) nJ js (nlJ + 1) 
                               | abs (y - yt) < 2 && abs (x - xt) < 2 && temVidas j = False
                               | otherwise = semTanques (y,x) nJ js (nlJ + 1)
    where (yt,xt) = posicaoJogador j