{- |

= Introdução

A tarefa 5 foi a tarefa na qual criámos o jogo em si, isto é, enquanto que nas outras tarefas definimos o código por
detrás do jogo, nesta tarefa tratámos do aspeto visual e do input/output, aquilo que transforma o jogo em algo mais 
que meras linhas de código.

= Objetivos

O nosso principal objetivo era criar um jogo com design intuitivo e minimalista, mas apelativo. Tentámos usar o
mínimo de texto possível, e usar imagens que representassem de forma ideal cada objeto, como os tanques, os disparos
ou os contadores de vidas e munições. Assim, uma pessoa que nunca tenha jogado o nosso jogo pode aprender a jogar
sem ter que ler grandes quantidades de texto, e percebe imediatamente o que cada coisa é ou o que faz.

Escolhemos um tema futurístico para o nosso jogo, sendo a cor principal do jogo o roxo, e demos um efeito neon
ao título. Todas as imagens que aparecem no nosso jogo foram criadas por nós ou retiradas da internet, mas 
tivemos o cuidado de apenas escolher imagens sem direitos de autor, e que pudessem ser reutilizadas. 

Queríamos também que o jogador fosse capaz de escolher o mapa onde quer jogar, podendo até criar o seu próprio mapa,
se não gostasse de nenhum dos mapas que criámos para o jogo, e que pudesse escolher a cor do seu tanque. Cada tanque 
tem canhões e lasers com cores próprias, baseadas na cor do próprio tanque. Também demos ao jogador a possibilidade 
de jogar contra outras pessoas, ou de jogar contra bots, podendo escolher quantos humanos/bots existem em cada partida.

= Discussão e conclusão

Gostamos bastante da forma como o nosso jogo ficou, tínhamos uma ideia em mente e acreditamos que fomos capazes de
transformar essa ideia num jogo que funciona e é divertido. O haskell não é uma linguagem ideal para criar
jogos, mesmo com o gloss, o melhor seria usar algo como o Unity ou o Unreal Engine, ferramentas feitas de raiz
para criar jogos, mas mesmo assim este jogo funciona perfeitamente bem, e fomos capazes de fazer tudo o que
queríamos.

Em conclusão, esta tarefa foi bastante divertida, já que pudemos dar asas à nossa imaginação e criar algo que
outras pessoas pudessem jogar e desfrutar. Foi uma das nossas tarefas preferidas de fazer, precisamente por causa
disso. A parte mais difícil foi ter que aprender a usar o gloss, que é um pouco diferente do que tínhamos feito 
com haskell até ao momento, mas não foi muito difícil apanhar-lhe o jeito, e a partir daí tornou-se fácil de usar.

-}

module Tarefa5_2018li1g080 where

import LI11819
import Tarefa0_2018li1g080
import Tarefa1_2018li1g080
import Tarefa2_2018li1g080
import Tarefa4_2018li1g080
import Tarefa6_2018li1g080
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.Pure.Game
import Data.List (elemIndex)
import Data.Maybe
import qualified Data.Text as Tx

-- Comando para compilar a tarefa: ghc -main-is Tarefa5_2018li1g080 -outputdir Tarefa5 Tarefa5_2018li1g080
-- | O estado que usámos para guardar todas as variáveis do nosso jogo.

data State = State 
    { estadoState :: Estado                 -- ^ O 'Estado' do jogo.
    , playersBotsList :: [(PlBt,Int,Points)]   -- ^ A lista dos jogadores / bots que estão a jogar, o seu tanque (índice da sua imagem) e os seus pontos.
    , currentMenu :: Menu                   -- ^ O 'Menu' atual.
    , isMoving :: [(Int,Direcao,Int)]       -- ^ Os jogadores que se estão a mover de momento. Isto permite que um clique longo de uma tecla para mover mova o tanque continuamente.
    , currentTick :: Int                    -- ^ O tick atual.
    , selectedOption :: Options             -- ^ A 'Option' atualmente selecionada.
    , drawnLasers :: [(Disparo,Int)]        -- ^ Os lasers atualmente no mapa.
    , cleanMap :: Mapa                      -- ^ O 'Mapa' igual ao do 'Estado', mas com os blocos destrutíveis intactos, para poder reiniciar o jogo no mesmo mapa.
    , mapInsts :: [Instrucao]               -- ^ As instruções necessárias para construir o mapa, caso o mapa seja personalizado, isto é, criado pelo jogador.
    } deriving Show

-- | Os vários menus que o nosso jogo possui.

data Menu = MainMenu | MapMenu | PlayerMenu | GameMenu | GameOver | PlayMenu | MapEditor | CreditsMenu deriving (Eq, Show)

-- | As opções que é possível selecionar em cada 'Menu'.

data Options = VoltarAoMenu | JogarNovamente | Jogar | IniciarJogo | AlterarJogadores | AlterarMapa
               | Mapa1 | Mapa2 | Mapa3 | Mapa4 | MapaP 
               | Player1 | Player2 | Player3 | Player4 | Bot2 | Bot3 | Bot4 | OKPlayer | Creditos | SairCreditos deriving (Eq, Show, Read)

-- | Apenas usado para definir se um jogador é humano ou bot.    

data PlBt = HumanPlayer | BotPlayer deriving (Eq,Show)

-- | Os pontos de um jogador.

type Points = Int

-- | As definições da janela do jogo.

mainDisplay :: Display
mainDisplay = InWindow "Tanks" displayDimension (0,0)

-- | A dimensão da janela do jogo.

displayDimension :: (Int,Int)
displayDimension = (1280,640)

-- | O framerate do jogo, isto é, quantas vezes a função 'timeReact' é executada num segundo.

fps :: Int
fps = 15

-- | Função que permite aos bots efetuar a sua jogada, usando para isso a função 'bot' da 'Tarefa6_2018li1g080'

playBots :: Estado -> [(PlBt,Int,Points)] -> Int -> Estado
playBots estado [] _ = estado
playBots estado ((h,_,_):t) n = if h == BotPlayer then playBots jogaBot t (n + 1) else playBots estado t (n + 1)
    where jogadaBot = bot n estado
          jogaBot | isNothing jogadaBot = estado
                  | otherwise = jogada n (fromJust (bot n estado)) estado

-- | Função que adiciona pontos a cada jogador com base no seguinte sistema:
--
-- * Bloco destruído: 1 ponto;
-- * Vida retirada a jogador: 4 pontos;
-- * Vidas restantes após fim do jogo: 4 pontos por vida.

addPoints :: [(PlBt,Int,Points)] -> Int -> Estado -> [(PlBt,Int,Points)]
addPoints [] _ _ = []
addPoints ((a,b,points):l) n e@(Estado _ _ disparos) = (a,b,points + addBlockP e n + addPlayerP e n disparos):addPoints l (n + 1) e

-- | Função auxiliar da função 'addPoints', apenas adiciona os pontos relativos à destruição de blocos.

addBlockP :: Estado -> Int -> Int
addBlockP (Estado _ _ []) _ = 0
addBlockP (Estado mapa j (d:disparos)) n = case d of DisparoCanhao nj (y,x) dir -> if nj /= n 
                                                                                   then addBlockP (Estado mapa j disparos) n 
                                                                                   else foldl (\acc bl -> case encontraPosicaoMatriz bl mapa of Bloco Destrutivel -> acc + 1; _ -> acc) 0 (blocosOndeBate (y,x) dir) + addBlockP (Estado mapa j disparos) n 
                                                     DisparoLaser nj _ _ -> if nj /= n 
                                                                            then addBlockP (Estado mapa j disparos) n 
                                                                            else length (init (bateEmBlocos d mapa)) + addBlockP (Estado mapa j disparos) n 
                                                     _ -> addBlockP (Estado mapa j disparos) n 

-- | Função auxiliar da função 'addPoints', apenas adiciona os pontos relativos ao dano a outros jogadores.

addPlayerP :: Estado -> Int -> [Disparo] -> Int
addPlayerP (Estado _ _ []) _ _ = 0
addPlayerP (Estado mapa j (d:disparos)) n disps = case d of DisparoCanhao nj (y,x) dir -> sum (map (\jog@(Jogador _ _ vidas _ _) -> if nj == n && vidas > 0 && bateNoJogador d jog (y,x) && (null disps || all (\dsp -> case dsp of DisparoCanhao _ (a,b) dire -> (a,b) /= (y,x) && ((a,b) /= somaVetores (y,x) (direcaoParaVetor (inverteDir dir)) || inverteDir dir /= dire); _ -> True) disps) then 4 else 0) j) + addPlayerP (Estado mapa j disparos) n disps
                                                            DisparoLaser nj _ _ -> sum (map (\jog@(Jogador _ _ vidas _ _) -> if nj == n && vidas > 0 && bateNoJogador d jog (last (bateEmBlocos d mapa)) then 4 else 0) j) + addPlayerP (Estado mapa j disparos) n disps
                                                            _ -> addPlayerP (Estado mapa j disparos) n disps

-- | Função que coloca os jogadores nos seus lugares iniciais, que no nosso caso é nos cantos dos mapas para todos os mapas.

colocarJogadores :: [Jogador] -> Mapa -> Int -> [Jogador]
colocarJogadores [] _ _ = []
colocarJogadores (j:jog) mapa n = j{posicaoJogador = posN,direcaoJogador = dirN} : colocarJogadores jog mapa (n + 1)
    where posN | n == 0 = (1,1)
               | n == 1 = (1,mapRows - 3)
               | n == 2 = (mapLines - 3,1)
               | n == 3 = (mapLines - 3,mapRows - 3)
          dirN = [D,B,E,C] !! n
          (mapLines,mapRows) = dimensaoMatriz mapa

-- | Função que permite fazer "reset" do jogo, para quando o jogador pretende jogar novamente.

resetGame :: Estado -> Mapa -> Estado
resetGame (Estado _ jogadores _) mn = Estado mn (map (\(Jogador pos d _ _ _) -> Jogador pos d 6 3 3) (colocarJogadores jogadores mn 0)) []

-- | Função que faz "reset" da pontuação.

resetScore :: [(PlBt,Int,Points)] -> [(PlBt,Int,Points)]
resetScore [] = []
resetScore ((a,b,_):r) = (a,b,0):resetScore r

-- | Função que dá os devidos pontos aos jogadores com base nas suas vidas quando o jogo acaba.

gameOverPoints :: Estado -> [(PlBt,Int,Points)] -> [(PlBt,Int,Points)]
gameOverPoints (Estado m (Jogador _ _ v _ _ :js) d) ((a,b,c):r) = (if v > 0 then (a,b,c + (4 * v)) else (a,b,c)) : gameOverPoints (Estado m js d) r
gameOverPoints _ _ = []

-- | Função que verifica se um dado jogador (identificável pelo seu número) é um bot ou não.

isBot :: Int -> [(PlBt,Int,Points)] -> Bool
isBot n l = a == BotPlayer
    where (a,_,_) = l !! n

-- | Função que altera o tipo de jogador de bot para humano, e vice-versa.

switchBot :: Int -> [(PlBt,Int,Points)] -> [(PlBt,Int,Points)]
switchBot n pbl = atualizaIndiceLista n (a',b,c) pbl 
    where (a,b,c) = pbl !! n
          a' | a == BotPlayer = HumanPlayer
             | otherwise = BotPlayer

-- | Esta função já estava definida na tarefa 2, mas como queríamos que no nosso jogo os disparos de choque durassem mais tempo, sem interferir com o resto, voltámos a defini-la aqui.

jogada' :: Int -> Jogada -> Estado -> Estado
jogada' jogadorNum (Dispara Choque) estado = if choquesJogador jogador > 0 && temVidas jogador
                                             then estado {jogadoresEstado = atualizaIndiceLista jogadorNum (jogador {choquesJogador = choquesJogador jogador - 1}) (jogadoresEstado estado), disparosEstado = DisparoChoque jogadorNum 30 : disparosEstado estado }
                                             else estado
    where jogador = encontraIndiceLista jogadorNum (jogadoresEstado estado)
jogada' a b c = jogada a b c

-- * Funções principais da tarefa 5

-- | Função 'main', permite que o jogo corra, e permite importar as imagens para o jogo.

main :: IO ()
main = do
    Just v         <- loadJuicyPNG $ t5r ++ "Map/Vazia.png"
    Just pin       <- loadJuicyPNG $ t5r ++ "Map/Indestrutivel.png"
    Just pd        <- loadJuicyPNG $ t5r ++ "Map/Destrutivel.png"
    Just rv        <- loadJuicyPNG $ t5r ++ "Map/RedVazia.png"
    Just ri        <- loadJuicyPNG $ t5r ++ "Map/RedIndes.png"
    Just rd        <- loadJuicyPNG $ t5r ++ "Map/RedDes.png"
    Just tank1     <- loadJuicyPNG $ t5r ++ "Players/tankPink.png"
    Just tank2     <- loadJuicyPNG $ t5r ++ "Players/tankBlue.png"
    Just tank3     <- loadJuicyPNG $ t5r ++ "Players/tankGreen.png"
    Just tank4     <- loadJuicyPNG $ t5r ++ "Players/tankYellow.png"
    Just tank5     <- loadJuicyPNG $ t5r ++ "Players/tankRed.png"
    Just tank6     <- loadJuicyPNG $ t5r ++ "Players/tankPT.png"
    Just tank7     <- loadJuicyPNG $ t5r ++ "Players/tankLGBT.png"
    Just tank8     <- loadJuicyPNG $ t5r ++ "Players/tankTrans.png"
    Just cb        <- loadJuicyPNG $ t5r ++ "Shots/CannonB.png"
    Just cg        <- loadJuicyPNG $ t5r ++ "Shots/CannonG.png"
    Just cy        <- loadJuicyPNG $ t5r ++ "Shots/CannonY.png"
    Just cr        <- loadJuicyPNG $ t5r ++ "Shots/CannonR.png"
    Just cp        <- loadJuicyPNG $ t5r ++ "Shots/CannonP.png"
    Just lb        <- loadJuicyPNG $ t5r ++ "Shots/laserB.png"
    Just lg        <- loadJuicyPNG $ t5r ++ "Shots/laserG.png"
    Just ly        <- loadJuicyPNG $ t5r ++ "Shots/laserY.png"
    Just lr        <- loadJuicyPNG $ t5r ++ "Shots/laserR.png"
    Just lp        <- loadJuicyPNG $ t5r ++ "Shots/laserP.png"
    Just s         <- loadJuicyPNG $ t5r ++ "Shots/Shock.png"
    Just pt1       <- loadJuicyPNG $ t5r ++ "GUI/playerThing1.png" -- 204*210
    Just pt2       <- loadJuicyPNG $ t5r ++ "GUI/playerThing2.png"
    Just pt3       <- loadJuicyPNG $ t5r ++ "GUI/playerThing3.png"
    Just pt4       <- loadJuicyPNG $ t5r ++ "GUI/playerThing4.png"
    Just ptb1      <- loadJuicyPNG $ t5r ++ "GUI/playerThingB1.png"
    Just ptb2      <- loadJuicyPNG $ t5r ++ "GUI/playerThingB2.png"
    Just num6      <- loadJuicyPNG $ t5r ++ "GUI/number_6.png"
    Just num5      <- loadJuicyPNG $ t5r ++ "GUI/number_5.png"
    Just num4      <- loadJuicyPNG $ t5r ++ "GUI/number_4.png"
    Just num3      <- loadJuicyPNG $ t5r ++ "GUI/number_3.png"
    Just num2      <- loadJuicyPNG $ t5r ++ "GUI/number_2.png"
    Just num1      <- loadJuicyPNG $ t5r ++ "GUI/number_1.png"
    Just num0      <- loadJuicyPNG $ t5r ++ "GUI/number_0.png"
    Just gameOver1 <- loadJuicyPNG $ t5r ++ "GUI/gameOver1.png"
    Just gameOver2 <- loadJuicyPNG $ t5r ++ "GUI/gameOver2.png"
    Just mainBG    <- loadJuicyPNG $ t5r ++ "Menu/background.png"
    Just title     <- loadJuicyPNG $ t5r ++ "Menu/title.png"
    Just mmopt1    <- loadJuicyPNG $ t5r ++ "Menu/mainMOpt1.png"
    Just mmopt2    <- loadJuicyPNG $ t5r ++ "Menu/mainMOpt2.png"
    Just pmopt1    <- loadJuicyPNG $ t5r ++ "Menu/playMOpt1.png"
    Just pmopt2    <- loadJuicyPNG $ t5r ++ "Menu/playMOpt2.png"
    Just pmopt3    <- loadJuicyPNG $ t5r ++ "Menu/playMOpt3.png"
    Just mamopt1   <- loadJuicyPNG $ t5r ++ "Menu/mapMOpt1.png"
    Just mamopt2   <- loadJuicyPNG $ t5r ++ "Menu/mapMOpt2.png"
    Just mamopt3   <- loadJuicyPNG $ t5r ++ "Menu/mapMOpt3.png"
    Just mamopt4   <- loadJuicyPNG $ t5r ++ "Menu/mapMOpt4.png"
    Just mamopt5   <- loadJuicyPNG $ t5r ++ "Menu/mapMOpt5.png"
    Just mpa1      <- loadJuicyPNG $ t5r ++ "Menu/mapa1.png"
    Just mpa2      <- loadJuicyPNG $ t5r ++ "Menu/mapa2.png"
    Just mpa3      <- loadJuicyPNG $ t5r ++ "Menu/mapa3.png"
    Just mpa4      <- loadJuicyPNG $ t5r ++ "Menu/mapa4.png"
    Just plmopt1   <- loadJuicyPNG $ t5r ++ "Menu/playerMOpt1.png"
    Just plmopt2   <- loadJuicyPNG $ t5r ++ "Menu/playerMOpt2.png"
    Just plmopt3   <- loadJuicyPNG $ t5r ++ "Menu/playerMOpt3.png"
    Just plmopt4   <- loadJuicyPNG $ t5r ++ "Menu/playerMOpt4.png"
    Just plmopt5   <- loadJuicyPNG $ t5r ++ "Menu/playerMOpt5.png"
    Just plmopt6   <- loadJuicyPNG $ t5r ++ "Menu/playerMOpt6.png"
    Just plmopt7   <- loadJuicyPNG $ t5r ++ "Menu/playerMOpt7.png"
    Just plmopt8   <- loadJuicyPNG $ t5r ++ "Menu/playerMOpt8.png"
    Just x         <- loadJuicyPNG $ t5r ++ "Menu/cross.png"
    Just creditos  <- loadJuicyPNG $ t5r ++ "Menu/creditos.png"
    Just editInsts <- loadJuicyPNG $ t5r ++ "Menu/editInsts.png"
    play mainDisplay
         (greyN 0.25)
         fps 
         initialState 
         (drawState ([[v,pin,pd],[rv,ri,rd]],[[tank1,tank2,tank3,tank4,tank5,tank6,tank7,tank8],[pt1,pt2,pt3,pt4,ptb1,ptb2,num0,num1,num2,num3,num4,num5,num6]],[[cp,cb,cg,cy,cr],[lp,lb,lg,ly,lr,s]],[[gameOver1,gameOver2],[mainBG,title,mmopt1,mmopt2,pmopt1,pmopt2,pmopt3,mamopt1,mamopt2,mamopt3,mamopt4,mamopt5,plmopt1,plmopt2,plmopt3,plmopt4,plmopt5,plmopt6,plmopt7,plmopt8,x,creditos,editInsts],[mpa1,mpa2,mpa3,mpa4]])) 
         inputReact 
         timeReact

-- | Função que executa algo no jogo com base na tecla premida pelo utilizador.

inputReact :: Event -> State -> State
inputReact (EventKey (Char 'w') Down _ _) state@(State _ _ GameMenu m _ _ _ _ _) = state{isMoving = (0,C,0):m}
inputReact (EventKey (Char 'w') Up _ _) state@(State _ _ GameMenu m _ _ _ _ _) = state{isMoving = filter (\(a,b,_) -> a /= 0 || b /= C) m}
inputReact (EventKey (Char 'a') Down _ _) state@(State _ _ GameMenu m _ _ _ _ _) = state{isMoving = (0,E,0):m}
inputReact (EventKey (Char 'a') Up _ _) state@(State _ _ GameMenu m _ _ _ _ _) = state{isMoving = filter (\(a,b,_) -> a /= 0 || b /= E) m}
inputReact (EventKey (Char 's') Down _ _) state@(State _ _ GameMenu m _ _ _ _ _) = state{isMoving = (0,B,0):m}
inputReact (EventKey (Char 's') Up _ _) state@(State _ _ GameMenu m _ _ _ _ _) = state{isMoving = filter (\(a,b,_) -> a /= 0 || b /= B) m}
inputReact (EventKey (Char 'd') Down _ _) state@(State _ _ GameMenu m _ _ _ _ _) = state{isMoving = (0,D,0):m}
inputReact (EventKey (Char 'd') Up _ _) state@(State _ _ GameMenu m _ _ _ _ _) = state{isMoving = filter (\(a,b,_) -> a /= 0 || b /= D) m}
inputReact (EventKey (Char '1') Down _ _) state@(State estado _ GameMenu _ _ _ _ _ _) = state{estadoState = jogada 0 (Dispara Canhao) estado}
inputReact (EventKey (Char '2') Down _ _) state@(State estado _ GameMenu _ _ _ _ _ _) = state{estadoState = jogada 0 (Dispara Laser) estado}
inputReact (EventKey (Char '3') Down _ _) state@(State estado _ GameMenu _ _ _ _ _ _) = state{estadoState = jogada' 0 (Dispara Choque) estado}
inputReact (EventKey (SpecialKey KeyUp) Down _ _) state@(State _ pbl GameMenu m _ _ _ _ _) = if isBot 1 pbl then state else state{isMoving = (1,C,0):m}
inputReact (EventKey (SpecialKey KeyUp) Up _ _) state@(State _ pbl GameMenu m _ _ _ _ _) = if isBot 1 pbl then state else state{isMoving = filter (\(a,b,_) -> a /= 1 || b /= C) m}
inputReact (EventKey (SpecialKey KeyLeft) Down _ _) state@(State _ pbl GameMenu m _ _ _ _ _) = if isBot 1 pbl then state else state{isMoving = (1,E,0):m}
inputReact (EventKey (SpecialKey KeyLeft) Up _ _) state@(State _ pbl GameMenu m _ _ _ _ _) = if isBot 1 pbl then state else state{isMoving = filter (\(a,b,_) -> a /= 1 || b /= E) m}
inputReact (EventKey (SpecialKey KeyDown) Down _ _) state@(State _ pbl GameMenu m _ _ _ _ _) = if isBot 1 pbl then state else state{isMoving = (1,B,0):m}
inputReact (EventKey (SpecialKey KeyDown) Up _ _) state@(State _ pbl GameMenu m _ _ _ _ _) = if isBot 1 pbl then state else state{isMoving = filter (\(a,b,_) -> a /= 1 || b /= B) m}
inputReact (EventKey (SpecialKey KeyRight) Down _ _) state@(State _ pbl GameMenu m _ _ _ _ _) = if isBot 1 pbl then state else state{isMoving = (1,D,0):m}
inputReact (EventKey (SpecialKey KeyRight) Up _ _) state@(State _ pbl GameMenu m _ _ _ _ _) = if isBot 1 pbl then state else state{isMoving = filter (\(a,b,_) -> a /= 1 || b /= D) m}
inputReact (EventKey (Char ',') Down _ _) state@(State estado pbl GameMenu _ _ _ _ _ _) = if isBot 1 pbl then state else state{estadoState = jogada 1 (Dispara Canhao) estado}
inputReact (EventKey (Char '.') Down _ _) state@(State estado pbl GameMenu _ _ _ _ _ _) = if isBot 1 pbl then state else state{estadoState = jogada 1 (Dispara Laser) estado}
inputReact (EventKey (Char '-') Down _ _) state@(State estado pbl GameMenu _ _ _ _ _ _) = if isBot 1 pbl then state else state{estadoState = jogada' 1 (Dispara Choque) estado}
inputReact (EventKey (Char 't') Down _ _) state@(State _ pbl GameMenu m _ _ _ _ _) = if isBot 2 pbl then state else state{isMoving = (2,C,0):m}
inputReact (EventKey (Char 't') Up _ _) state@(State _ pbl GameMenu m _ _ _ _ _) = if isBot 2 pbl then state else state{isMoving = filter (\(a,b,_) -> a /= 2 || b /= C) m}
inputReact (EventKey (Char 'f') Down _ _) state@(State _ pbl GameMenu m _ _ _ _ _) = if isBot 2 pbl then state else state{isMoving = (2,E,0):m}
inputReact (EventKey (Char 'f') Up _ _) state@(State _ pbl GameMenu m _ _ _ _ _) = if isBot 2 pbl then state else state{isMoving = filter (\(a,b,_) -> a /= 2 || b /= E) m}
inputReact (EventKey (Char 'g') Down _ _) state@(State _ pbl GameMenu m _ _ _ _ _) = if isBot 2 pbl then state else state{isMoving = (2,B,0):m}
inputReact (EventKey (Char 'g') Up _ _) state@(State _ pbl GameMenu m _ _ _ _ _) = if isBot 2 pbl then state else state{isMoving = filter (\(a,b,_) -> a /= 2 || b /= B) m}
inputReact (EventKey (Char 'h') Down _ _) state@(State _ pbl GameMenu m _ _ _ _ _) = if isBot 2 pbl then state else state{isMoving = (2,D,0):m}
inputReact (EventKey (Char 'h') Up _ _) state@(State _ pbl GameMenu m _ _ _ _ _) = if isBot 2 pbl then state else state{isMoving = filter (\(a,b,_) -> a /= 2 || b /= D) m}
inputReact (EventKey (Char '4') Down _ _) state@(State estado pbl GameMenu _ _ _ _ _ _) = if isBot 2 pbl then state else state{estadoState = jogada 2 (Dispara Canhao) estado}
inputReact (EventKey (Char '5') Down _ _) state@(State estado pbl GameMenu _ _ _ _ _ _) = if isBot 2 pbl then state else state{estadoState = jogada 2 (Dispara Laser) estado}
inputReact (EventKey (Char '6') Down _ _) state@(State estado pbl GameMenu _ _ _ _ _ _) = if isBot 2 pbl then state else state{estadoState = jogada' 2 (Dispara Choque) estado}
inputReact (EventKey (Char 'i') Down _ _) state@(State _ pbl GameMenu m _ _ _ _ _) = if isBot 3 pbl then state else state{isMoving = (3,C,0):m}
inputReact (EventKey (Char 'i') Up _ _) state@(State _ pbl GameMenu m _ _ _ _ _) = if isBot 3 pbl then state else state{isMoving = filter (\(a,b,_) -> a /= 3 || b /= C) m}
inputReact (EventKey (Char 'j') Down _ _) state@(State _ pbl GameMenu m _ _ _ _ _) = if isBot 3 pbl then state else state{isMoving = (3,E,0):m}
inputReact (EventKey (Char 'j') Up _ _) state@(State _ pbl GameMenu m _ _ _ _ _) = if isBot 3 pbl then state else state{isMoving = filter (\(a,b,_) -> a /= 3 || b /= E) m}
inputReact (EventKey (Char 'k') Down _ _) state@(State _ pbl GameMenu m _ _ _ _ _) = if isBot 3 pbl then state else state{isMoving = (3,B,0):m}
inputReact (EventKey (Char 'k') Up _ _) state@(State _ pbl GameMenu m _ _ _ _ _) = if isBot 3 pbl then state else state{isMoving = filter (\(a,b,_) -> a /= 3 || b /= B) m}
inputReact (EventKey (Char 'l') Down _ _) state@(State _ pbl GameMenu m _ _ _ _ _) = if isBot 3 pbl then state else state{isMoving = (3,D,0):m}
inputReact (EventKey (Char 'l') Up _ _) state@(State _ pbl GameMenu m _ _ _ _ _) = if isBot 3 pbl then state else state{isMoving = filter (\(a,b,_) -> a /= 3 || b /= D) m}
inputReact (EventKey (Char '7') Down _ _) state@(State estado pbl GameMenu _ _ _ _ _ _) = if isBot 3 pbl then state else state{estadoState = jogada 3 (Dispara Canhao) estado}
inputReact (EventKey (Char '8') Down _ _) state@(State estado pbl GameMenu _ _ _ _ _ _) = if isBot 3 pbl then state else state{estadoState = jogada 3 (Dispara Laser) estado}
inputReact (EventKey (Char '9') Down _ _) state@(State estado pbl GameMenu _ _ _ _ _ _) = if isBot 3 pbl then state else state{estadoState = jogada' 3 (Dispara Choque) estado}
inputReact (EventKey (SpecialKey KeyRight) Down _ _) state@(State _ _ GameOver _ _ _ _ _ _) = state{selectedOption = VoltarAoMenu}
inputReact (EventKey (SpecialKey KeyLeft) Down _ _) state@(State _ _ GameOver _ _ _ _ _ _) = state{selectedOption = JogarNovamente}
inputReact (EventKey (SpecialKey KeyEnter) Down _ _) state@(State estado pbl GameOver _ _ JogarNovamente _ nm _) = state{currentMenu = GameMenu,estadoState = resetGame estado nm,playersBotsList = resetScore pbl,isMoving = []}
inputReact (EventKey (SpecialKey KeyEnter) Down _ _) state@(State estado pbl GameOver _ _ VoltarAoMenu _ nm _) = state{currentMenu = MainMenu,estadoState = resetGame estado nm,playersBotsList = resetScore pbl, selectedOption = Jogar,isMoving = []}
inputReact (EventKey (SpecialKey KeyEnter) Down _ _) state@(State _ _ MainMenu _ _ Jogar _ _ _) = state{currentMenu = PlayMenu, selectedOption = IniciarJogo}
inputReact (EventKey (SpecialKey KeyDown) Down _ _) state@(State _ _ MainMenu _ _ Jogar _ _ _) = state{selectedOption = Creditos}
inputReact (EventKey (SpecialKey KeyUp) Down _ _) state@(State _ _ MainMenu _ _ Creditos _ _ _) = state{selectedOption = Jogar}
inputReact (EventKey (SpecialKey KeyEnter) Down _ _) state@(State _ _ MainMenu _ _ Creditos _ _ _) = state{currentMenu = CreditsMenu, selectedOption = SairCreditos}
inputReact (EventKey _ Down _ _) state@(State _ _ CreditsMenu _ _ _ _ _ _) = state{currentMenu = MainMenu, selectedOption = Jogar} 
inputReact (EventKey (SpecialKey KeyEnter) Down _ _) state@(State _ pbl PlayMenu _ _ IniciarJogo _ _ _) = state{currentMenu = GameMenu,currentTick = 0,selectedOption = JogarNovamente,playersBotsList = resetScore pbl}
inputReact (EventKey (SpecialKey KeyDown) Down _ _) state@(State _ _ PlayMenu _ _ IniciarJogo _ _ _) = state{selectedOption = AlterarMapa}
inputReact (EventKey (SpecialKey KeyUp) Down _ _) state@(State _ _ PlayMenu _ _ AlterarMapa _ _ _) = state{selectedOption = IniciarJogo}
inputReact (EventKey (SpecialKey KeyDown) Down _ _) state@(State _ _ PlayMenu _ _ AlterarMapa _ _ _) = state{selectedOption = AlterarJogadores}
inputReact (EventKey (SpecialKey KeyUp) Down _ _) state@(State _ _ PlayMenu _ _ AlterarJogadores _ _ _) = state{selectedOption = AlterarMapa}
inputReact (EventKey (SpecialKey KeyEnter) Down _ _) state@(State _ _ PlayMenu _ _ AlterarMapa _ _ _) = state{currentMenu = MapMenu,selectedOption = Mapa1}
inputReact (EventKey (SpecialKey KeyRight) Down _ _) state@(State _ _ MapMenu _ _ option _ _ _) = state{selectedOption = if last rOpt `elem` ['1'..'3'] then read (init rOpt ++ [succ (last rOpt)]) else option}
    where rOpt = show option
inputReact (EventKey (SpecialKey KeyLeft) Down _ _) state@(State _ _ MapMenu _ _ option _ _ _) = state{selectedOption = if last rOpt `elem` ['2'..'4'] then read (init rOpt ++ [pred (last rOpt)]) else option}
    where rOpt = show option
inputReact (EventKey (SpecialKey KeyDown) Down _ _) state@(State _ _ MapMenu _ _ _ _ _ _) = state{selectedOption = MapaP}
inputReact (EventKey (SpecialKey KeyUp) Down _ _) state@(State _ _ MapMenu _ _ MapaP _ _ _) = state{selectedOption = Mapa1}
inputReact (EventKey (SpecialKey KeyEnter) Down _ _) state@(State _ _ MapMenu _ _ MapaP _ _ _) = state{currentMenu = MapEditor}
inputReact (EventKey (SpecialKey KeyEnter) Down _ _) state@(State est@(Estado _ jogadores _) _ MapMenu _ _ option _ _ _) = state{currentMenu = PlayMenu,selectedOption = IniciarJogo,estadoState = est{mapaEstado = novoMapa,jogadoresEstado = colocarJogadores jogadores novoMapa 0}, cleanMap = novoMapa}
    where novoMapa = mapas !! (read [last (show option)] - 1)
inputReact (EventKey (SpecialKey KeyRight) Down _ _) state@(State _ _ MapEditor _ _ _ _ _ insts) = state{mapInsts = insts ++ [Move D]}
inputReact (EventKey (SpecialKey KeyLeft) Down _ _) state@(State _ _ MapEditor _ _ _ _ _ insts) = state{mapInsts = insts ++ [Move E]}
inputReact (EventKey (SpecialKey KeyUp) Down _ _) state@(State _ _ MapEditor _ _ _ _ _ insts) = state{mapInsts = insts ++ [Move C]}
inputReact (EventKey (SpecialKey KeyDown) Down _ _) state@(State _ _ MapEditor _ _ _ _ _ insts) = state{mapInsts = insts ++ [Move B]}
inputReact (EventKey (Char 'r') Down _ _) state@(State _ _ MapEditor _ _ _ _ _ insts) = state{mapInsts = insts ++ [Roda]}
inputReact (EventKey (Char 't') Down _ _) state@(State _ _ MapEditor _ _ _ _ _ insts) = state{mapInsts = insts ++ [MudaTetromino]}
inputReact (EventKey (Char 'p') Down _ _) state@(State _ _ MapEditor _ _ _ _ _ insts) = state{mapInsts = insts ++ [MudaParede]}
inputReact (EventKey (Char 'd') Down _ _) state@(State _ _ MapEditor _ _ _ _ _ insts) = state{mapInsts = insts ++ [Desenha]}
inputReact (EventKey (SpecialKey KeyEnter) Down _ _) state@(State estado _ MapEditor _ _ _ _ _ insts) = state{estadoState = resetGame estado (constroi insts),cleanMap = constroi insts,currentMenu = PlayMenu, selectedOption = IniciarJogo, mapInsts = []}
inputReact (EventKey (SpecialKey KeyEnter) Down _ _) state@(State _ _ PlayMenu _ _ AlterarJogadores _ _ _) = state{currentMenu = PlayerMenu,selectedOption = Player1}
inputReact (EventKey (SpecialKey KeyDown) Down _ _) state@(State _ _ PlayerMenu _ _ Player1 _ _ _) = state{selectedOption = Bot2}
inputReact (EventKey (SpecialKey KeyDown) Down _ _) state@(State _ _ PlayerMenu _ _ Player2 _ _ _) = state{selectedOption = Bot2}
inputReact (EventKey (SpecialKey KeyDown) Down _ _) state@(State _ _ PlayerMenu _ _ Player3 _ _ _) = state{selectedOption = Bot3}
inputReact (EventKey (SpecialKey KeyDown) Down _ _) state@(State _ _ PlayerMenu _ _ Player4 _ _ _) = state{selectedOption = Bot4}
inputReact (EventKey (SpecialKey KeyDown) Down _ _) state@(State _ _ PlayerMenu _ _ _ _ _ _) = state{selectedOption = OKPlayer}
inputReact (EventKey (SpecialKey KeyLeft) Down _ _) state@(State _ _ PlayerMenu _ _ Bot2 _ _ _) = state{selectedOption = Player1}
inputReact (EventKey (SpecialKey KeyEnter) Down _ _) state@(State _ pbl PlayerMenu _ _ Bot2 _ _ _) = state{playersBotsList = switchBot 1 pbl}
inputReact (EventKey (SpecialKey KeyLeft) Down _ _) state@(State _ _ PlayerMenu _ _ Bot3 _ _ _) = state{selectedOption = Bot2}
inputReact (EventKey (SpecialKey KeyEnter) Down _ _) state@(State _ pbl PlayerMenu _ _ Bot3 _ _ _) = state{playersBotsList = switchBot 2 pbl}
inputReact (EventKey (SpecialKey KeyLeft) Down _ _) state@(State _ _ PlayerMenu _ _ Bot4 _ _ _) = state{selectedOption = Bot3}
inputReact (EventKey (SpecialKey KeyEnter) Down _ _) state@(State _ pbl PlayerMenu _ _ Bot4 _ _ _) = state{playersBotsList = switchBot 3 pbl}
inputReact (EventKey (SpecialKey KeyRight) Down _ _) state@(State _ _ PlayerMenu _ _ Bot2 _ _ _) = state{selectedOption = Bot3}
inputReact (EventKey (SpecialKey KeyRight) Down _ _) state@(State _ _ PlayerMenu _ _ Bot3 _ _ _) = state{selectedOption = Bot4}
inputReact (EventKey (SpecialKey KeyUp) Down _ _) state@(State _ _ PlayerMenu _ _ Bot2 _ _ _) = state{selectedOption = Player2}
inputReact (EventKey (SpecialKey KeyUp) Down _ _) state@(State _ _ PlayerMenu _ _ Bot3 _ _ _) = state{selectedOption = Player3}
inputReact (EventKey (SpecialKey KeyUp) Down _ _) state@(State _ _ PlayerMenu _ _ Bot4 _ _ _) = state{selectedOption = Player4}
inputReact (EventKey (SpecialKey KeyEnter) Down _ _) state@(State _ _ PlayerMenu _ _ Player1 _ _ _) = state{selectedOption = Player2}
inputReact (EventKey (SpecialKey KeyEnter) Down _ _) state@(State _ _ PlayerMenu _ _ Player2 _ _ _) = state{selectedOption = Player3}
inputReact (EventKey (SpecialKey KeyEnter) Down _ _) state@(State _ _ PlayerMenu _ _ Player3 _ _ _) = state{selectedOption = Player4}
inputReact (EventKey (SpecialKey KeyEnter) Down _ _) state@(State _ _ PlayerMenu _ _ Player4 _ _ _) = state{selectedOption = OKPlayer}
inputReact (EventKey (SpecialKey KeyUp) Down _ _) state@(State _ _ PlayerMenu _ _ OKPlayer _ _ _) = state{selectedOption = Bot2}
inputReact (EventKey (SpecialKey KeyEnter) Down _ _) state@(State _ _ PlayerMenu _ _ OKPlayer _ _ _) = state{currentMenu = PlayMenu,selectedOption = IniciarJogo}
inputReact (EventKey (SpecialKey KeyRight) Down _ _) state@(State _ pbl PlayerMenu _ _ option _ _ _) | take 4 (show option) == "Play" = state{playersBotsList = atualizaIndiceLista (read [last (show option)] - 1) (pb,(currentN + 1) `mod` 8,points) pbl}
                                                                                                     | otherwise = state
    where (pb,currentN,points) = encontraIndiceLista (read [last (show option)] - 1) pbl
inputReact (EventKey (SpecialKey KeyLeft) Down _ _) state@(State _ pbl PlayerMenu _ _ option _ _ _) | take 4 (show option) == "Play" = state{playersBotsList = atualizaIndiceLista (read [last (show option)] - 1) (pb,(currentN - 1) `mod` 8,points) pbl}
                                                                                                    | otherwise = state
    where (pb,currentN,points) = encontraIndiceLista (read [last (show option)] - 1) pbl
inputReact _ state = state

-- | 'State' inicial do jogo.

initialState :: State
initialState = State (Estado (head mapas) [Jogador (1,1) D 6 3 3,Jogador (1,mapRows - 3) B 6 3 3,Jogador (mapLines - 3,1) C 6 3 3,Jogador (mapLines - 3,mapRows - 3) E 6 3 3] []) [(HumanPlayer,6,0),(BotPlayer,0,0),(BotPlayer,1,0),(BotPlayer,2,0)] MainMenu [] 0 Jogar [] (head mapas) []
    where (mapLines,mapRows) = dimensaoMatriz (head mapas)

-- | Função que desenha o jogo na janela do gloss.

drawState :: ([[Picture]],[[Picture]],[[Picture]],[[Picture]]) -> State -> Picture
drawState pics (State (Estado mapa players shots) plbtlist GameMenu _ ctick _ lasers _ _) = Pictures ([bg] ++ desenhaMapa mapa 0 mapa (head mapPics) ++ desenhaJogadores players 0 mapa plbtlist playerPics ++ desenhaDisparos shotsLas players plbtlist mapa shotsPics ++ desenhaPontos plbtlist ++ [Translate 0 (- fi (snd displayDimension) / 2 + 8) (Scale 0.3 0.3 (Color white (text (show (ctick `div` fps `div` 60) ++ ":" ++ (if seconds < 10 then "0" else "") ++ show seconds))))])
    where (mapPics,playerPics,shotsPics,gUIPics) = pics
          bg = head (gUIPics !! 1)
          seconds = ctick `div` fps `mod` 60
          shotsLas = filter (\disp -> case disp of DisparoLaser{} -> False ; _ -> True) shots ++ map fst lasers
drawState pics state@(State _ pbl GameOver _ _ option _ _ _) = Pictures [drawState pics state{currentMenu = GameMenu},winnerScreen,displayTanks]
    where winnerScreen | option == VoltarAoMenu = head d !! 1
                       | otherwise = head (head d)
          winnerTanks = map (\winn -> Scale 2.5 2.5 (head b !! (let  (_,xT,_) = pbl !! winn in xT))) winners 
          displayTanks | length winners == 1 = Translate 0 18 (head winnerTanks)
                       | otherwise = Pictures [Translate (-80) 18 (head winnerTanks), Translate 80 18 (winnerTanks !! 1)]
          (_,b,_,d) = pics 
          (_,winners) = foldl (\(maxi,ns) (p,q,r) -> if r > maxi then (r,[findTankIndex (p,q,r)]) else if r == maxi then (maxi,findTankIndex (p,q,r):ns) else (maxi,ns)) (0,[]) pbl
          findTankIndex pb = fromJust (pb `elemIndex` pbl)
drawState (_,tanks,_,pics) (State _ plbtlist PlayerMenu _ _ option _ _ _) = Pictures [head (pics !! 1),currentOption,tanques,crosses]
    where pmo1:pmo2:pmo3:pmo4:pmo5:pmo6:pmo7:pmo8:cross:_ = drop 12 (pics !! 1)
          currentOption | option == Player1 = pmo1
                        | option == Player2 = pmo2
                        | option == Player3 = pmo3
                        | option == Player4 = pmo4
                        | option == Bot2 = pmo5
                        | option == Bot3 = pmo6
                        | option == Bot4 = pmo7
                        | option == OKPlayer = pmo8
          tqsTemp = map (\(_,b,_) -> head tanks !! b) plbtlist
          tanques = Pictures [Translate (-480) 75 (sc3 (head tqsTemp)), Translate (-162) 75 (sc3 (tqsTemp !! 1)), Translate 156 75 (sc3 (tqsTemp !! 2)),Translate 476 75 (sc3 (tqsTemp !! 3))]
          sc3 = Scale 2.8 2.8
          crosses = Pictures [Translate (-158) (-124) x1, Translate 161 (-124) x2, Translate 480 (-124) x3]
          [x1,x2,x3] = map (\(a,_,_) -> if a == BotPlayer then cross else Blank) (tail plbtlist)
drawState (_,_,_,pics) (State _ _ MapMenu _ _ option _ _ _) = Pictures [fundo,currentOption,mapasD]
    where fundo = head (pics !! 1)
          mmo1:mmo2:mmo3:mmo4:mmo5:_ = drop 7 (pics !! 1)
          mpa1:mpa2:mpa3:mpa4:_ = pics !! 2
          currentOption | option == Mapa1 = mmo1
                        | option == Mapa2 = mmo2
                        | option == Mapa3 = mmo3
                        | option == Mapa4 = mmo4
                        | option == MapaP = mmo5
          mapasD = Pictures [Translate (-428) 28 mpa1, Translate (-138) 28 mpa2, Translate 160 28 mpa3,Translate 448 28 mpa4]
drawState (mapPics,_,_,gUIpics) (State _ _ MapEditor _ _ _ _ _ insts) = Pictures $ [fundo] ++ [instsEd] ++ desenhaMapa (constroi insts) 0 (constroi insts) (head mapPics) ++ desenhaEditor (instrucoes insts (editorInicial insts)) (mapPics !! 1)
    where fundo:resto = gUIpics !! 1
          instsEd = last resto
drawState (_,_,_,pics) (State _ _ _ _ _ option _ _ _) = Pictures [fundo,
                                                                  currentOption,
                                                                  titulo]
    where fundo:titulo:mmo1:mmo2:pmo1:pmo2:pmo3:resto = pics !! 1
          currentOption | option == Jogar = mmo1
                        | option == Creditos = mmo2
                        | option == IniciarJogo = pmo1
                        | option == AlterarMapa = pmo2
                        | option == AlterarJogadores = pmo3
                        | option == SairCreditos = last $ init resto

-- | Função que permite ao jogo gerir o tempo, isto é, que define a passagem do tempo no jogo.

timeReact :: Float -> State -> State
timeReact _ state@(State estado pbl m movements ctick option las cm insts) | gameOver = state{currentMenu = GameOver,playersBotsList = gameOverPoints estado pbl}
                                                                           | m /= GameMenu = state
                                                                           | otherwise = tick' (State (foldl (\acc (nJ,dir,x) -> if x == 0 then jogada nJ (Movimenta dir) acc else acc) estado movements) pbl m (foldl (\acc (nJ,dir,x) -> (nJ,dir,(x + 1) `mod` 2) : acc) [] movements) (ctick + 1) option las cm insts)
    where gameOver = length (filter (\(Jogador _ _ v _ _) -> v /= 0) (jogadoresEstado estado)) == 1 && m /= GameOver
          tick' (State e pblist b c d g dL mn f) = State (playBots (tick e) pblist 0) (addPoints pblist 0 e) b c d g (foldl (\acc (laser,n) -> if n /= 0 then (laser,n - 1) : acc else acc) [] (foldl (\acc disp -> case disp of DisparoLaser nj pos di -> if DisparoLaser nj pos di `elem` map fst dL then acc else (DisparoLaser nj pos di , 4) : acc;_ -> acc) dL (disparosEstado e))) mn f

-- ** Funções auxiliares da função 'drawState'

-- | Função que desenha o 'Mapa' na janela do gloss.

desenhaMapa :: Mapa -> Int -- ^ Nº da linha atual, na recursão
        -> Mapa -> [Picture] -> [Picture]
desenhaMapa [] _ _ _ = []
desenhaMapa (l:ls) n mapa pics = desenhaLinha l (n,0) mapa pics ++ desenhaMapa ls (n + 1) mapa pics

-- | Função que desenha cada linha de um 'Mapa' na janela do gloss.
--
-- | __NB:__ Esta função é uma função auxiliar da função 'desenhaMapa'.

desenhaLinha :: [Peca] -> (Int,Int) -> Mapa -> [Picture] -> [Picture]
desenhaLinha [] _ _ _ = []
desenhaLinha (p:ps) (n,x) mapa pics = case p of Vazia -> Translate posPecaX posPecaY (sc mapa vazia) : desenhaLinha ps (n,x + 1) mapa pics
                                                Bloco Indestrutivel -> Translate posPecaX posPecaY (sc mapa indestr) : desenhaLinha ps (n,x + 1) mapa pics
                                                Bloco Destrutivel -> Translate posPecaX posPecaY (sc mapa destr) : desenhaLinha ps (n,x + 1) mapa pics
    where vazia:indestr:destr:_ = pics
          posPecaX = (fi x - (fi mapRows/2) + oddAdj) * pieceSide mapa
          posPecaY = ((fi mapLines/2) - fi n) * pieceSide mapa
          (mapLines,mapRows) = dimensaoMatriz mapa 
          oddAdj | odd mapRows = 0.5
                 | otherwise = 0

-- | Função que permite redimensionar uma 'Picture' para a escala do jogo. 

sc :: Mapa -> Picture -> Picture          
sc m = Scale (gameScale m) (gameScale m)

-- | A escala do jogo, com base no tamanho do mapa (quanto maior o mapa, menor a escala).

gameScale :: Mapa -> Float
gameScale m = 18/fromIntegral (max (l + 1) (r - 5))
    where (l,r) = dimensaoMatriz m

-- | O tamanho que cada 'Peca' do 'Mapa' deve ter na janela do gloss.

pieceSide :: Mapa -> Float
pieceSide m = 32 * gameScale m

-- | Função que desenha os 'Jogador'es na janela do gloss.

desenhaJogadores :: [Jogador] -- ^ A lista dos jogadores.
                 -> Int -- ^ O nº do jogador que será desenhado na função 'desenhaJogador'.
                 -> Mapa -- ^ O mapa do jogo.
                 -> [(PlBt,Int,Points)] -- ^ A lista que contém o tanque escolhido por cada jogador.
                 -> [[Picture]] -- ^ As imagens relativas aos jogadores.
                 -> [Picture] -- ^ Os jogadores e as suas informações (vidas,munições,etc) para serem desenhados na janela do gloss.
desenhaJogadores [] _ _ _ _ = []
desenhaJogadores (p:ps) n mapa pbl pics = desenhaJogador p n mapa pbl pics : desenhaJogadores ps (n + 1) mapa pbl pics

-- | Função que desenha um 'Jogador' na janela do gloss.
--
-- __NB:__ Esta função é uma função auxiliar da função 'desenhaJogadores'.

desenhaJogador :: Jogador -> Int -> Mapa -> [(PlBt,Int,Points)] -> [[Picture]] -> Picture
desenhaJogador (Jogador (y,x) d v l s) n mapa plb (tankPics:guiPics:_) = Pictures [Translate ((fi x - fi mapRows/2 + oddAdj) * pieceSide mapa + pieceSide mapa / 2) ((fi mapLines/2 - fi y) * pieceSide mapa - pieceSide mapa / 2) (rotatePic d (sc mapa tank)),
                                                                                   Translate a b thing,
                                                                                   Translate (if a > 0 then a + 35 else a - 35) (b + 30) (Scale 2 2 tank),
                                                                                   Translate (if a > 0 then a - 116 else a + 125) (b + 76) (nums !! v),
                                                                                   Translate (if a > 0 then a - 116 else a + 125) (b + 25) (nums !! l),
                                                                                   Translate (if a > 0 then a - 116 else a + 125) (b - 25) (nums !! s)] 
    where tank | v > 0 = tankPics !! nTank
               | otherwise = Blank
          (_,nTank,_) = plb !! n
          (mapLines,mapRows) = dimensaoMatriz mapa
          (width,height) = displayDimension
          (a,b) = (negateX (fi width / 2 - 105), negateY (fi height / 2 - 150))
          negateX x = if odd n then x else negate x
          negateY y = if n < 2 then y else negate y
          (things,nums) = splitAt 6 guiPics
          thing | isBot n plb = things !! (4 + (n `mod` 2))
                | otherwise = things !! n 
          oddAdj | odd mapRows = 0.5
                 | otherwise = 0

-- | Função que permite rodar uma 'Picture', assumindo que a 'Direcao' original é 'C'

rotatePic :: Direcao -> Picture -> Picture
rotatePic D = Rotate 90
rotatePic B = Rotate 180
rotatePic E = Rotate 270
rotatePic C = id
        
-- | Função que desenha os pontos de cada 'Jogador' na janela do gloss.

desenhaPontos :: [(PlBt,Int,Points)] -> [Picture]
desenhaPontos pbl = [Translate (-490) 84 points0, Translate 440 84 points1, Translate (-490) (-256) points2, Translate 440 (-256) points3]
    where [points0,points1,points2,points3] = map (Scale 0.23 0.3 . (\(_,_,c) -> Text (Tx.unpack (Tx.justifyRight 3 '0' (Tx.pack (show c)))))) pbl

-- | Função que desenha os 'Disparo's na janela do gloss.

desenhaDisparos :: [Disparo] -> [Jogador] -> [(PlBt,Int,Points)] -> Mapa -> [[Picture]] -> [Picture]
desenhaDisparos [] _ _ _ _ = []
desenhaDisparos (s:ss) pl pbl mapa pics = desenhaDisparo s pl pbl mapa pics : desenhaDisparos ss pl pbl mapa pics

-- | Função qu desenha um 'Disparo' na janela do gloss.
--
-- __NB:__ Esta função é uma função auxiliar da função 'desenhaDisparos'.

desenhaDisparo :: Disparo -> [Jogador] -> [(PlBt,Int,Points)] -> Mapa -> [[Picture]] -> Picture
desenhaDisparo (DisparoCanhao n (y,x) d) _ pbl mapa pics = Translate ((fi x - fi mapRows/2 + oddAdj) * pS + pS / 2) ((fi mapLines/2 - fi y) * pS - pS / 2) (rotatePic d (sc mapa canhaoCor))
    where cannons = head pics
          (mapLines,mapRows) = dimensaoMatriz mapa
          pS = pieceSide mapa
          (_,nTank,_) = pbl !! n
          canhaoCor | nTank < 5 = cannons !! nTank
                    | nTank == 5 = cannons !! 3
                    | nTank == 6 = head cannons
                    | otherwise = cannons !! 1
          oddAdj | odd mapRows = 0.5
                 | otherwise = 0
desenhaDisparo las@(DisparoLaser n (y,x) d) pl pbl mapa pics | (y,x) == laserend = Blank
                                                             | otherwise = Pictures [Translate ((fi x - fi mapRows/2 + oddAdj) * pS + pS / 2) ((fi mapLines/2 - fi y) * pS - pS / 2) (rotatePic d (sc mapa laser)),desenhaDisparo (DisparoLaser n (somaVetores (y,x) (direcaoParaVetor d)) d) pl pbl mapa pics]
    where laserpics = init $ last pics
          laser | nTank < 5 = laserpics !! nTank
                | nTank == 5 = laserpics !! 3
                | nTank == 6 = head laserpics
                | otherwise = laserpics !! 1
          (_,nTank,_) = pbl !! n
          laserend = last (bateEmBlocos las mapa)
          (mapLines,mapRows) = dimensaoMatriz mapa
          pS = pieceSide mapa
          oddAdj | odd mapRows = 0.5
                 | otherwise = 0
desenhaDisparo (DisparoChoque n _) players _ mapa pics = Translate ((fi x - fi mapRows/2 + oddAdj) * pS + pS / 2) ((fi mapLines/2 - fi y) * pS - pS / 2) (sc mapa shock)
    where shock = last $ last pics
          (y,x) = posicaoJogador (encontraIndiceLista n players)
          (mapLines,mapRows) = dimensaoMatriz mapa
          pS = pieceSide mapa
          oddAdj | odd mapRows = 0.5
                 | otherwise = 0

-- | Função que desenha o 'Editor' de 'Mapa's na janela do gloss.

desenhaEditor :: Editor -> [Picture] -> [Picture]
desenhaEditor (Editor (y,x) d t p m) pics = foldl (\acc (a,b) -> Translate (posPecaX (x + b)) (posPecaY (y + a)) (sc m (pieceToDraw (a,b))) : acc) [] posicoesTetromino
    where redOL:redBI:redBD:_ = pics
          posPecaX xp = (fi xp - (fi mapRows/2) + oddAdj) * pieceSide m
          posPecaY yp = ((fi mapLines/2) - fi yp) * pieceSide m
          (mapLines,mapRows) = dimensaoMatriz m
          oddAdj | odd mapRows = 0.5
                 | otherwise = 0
          posicoesTetromino = [(c,e) | c <- [0..xMat-1], e <- [0..yMat-1]]
          (yMat,xMat) = dimensaoMatriz $ tetrominoParaMatriz t
          pecaTetromino (a,b) = if encontraPosicaoMatriz (a,b) tetrominoRodado then Bloco p
                                                                               else Vazia
          pieceToDraw (py,px) = case pecaTetromino (py,px) of Vazia -> redOL
                                                              Bloco Indestrutivel -> redBI
                                                              Bloco Destrutivel -> redBD
          tetrominoRodado = case d of C -> tetrominoParaMatriz t
                                      D -> rodaMatriz $ tetrominoParaMatriz t
                                      B -> rodaMatriz . rodaMatriz $ tetrominoParaMatriz t
                                      E -> rodaMatriz . rodaMatriz . rodaMatriz $ tetrominoParaMatriz t

-- | Os mapas que existem no nosso jogo.
    
mapas :: [Mapa]
mapas = [mapa1,mapa2,mapa3,mapa4]

-- | O 1º mapa.
mapa1 :: Mapa
mapa1 = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],
          [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
          [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
          [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
          [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
          [Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],
          [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
          [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
          [Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],
          [Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],
          [Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],
          [Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],
          [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
          [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
          [Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],
          [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
          [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
          [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
          [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
          [Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]
-- | O 2º mapa.
mapa2 :: Mapa
mapa2 = [[Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Destrutivel, Bloco Destrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Destrutivel, Bloco Destrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Destrutivel, Bloco Destrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Destrutivel, Bloco Destrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Destrutivel, Bloco Destrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Vazia, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Vazia, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Vazia, Vazia, Vazia, Bloco Destrutivel, Bloco Destrutivel, Vazia, Vazia, Vazia, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Vazia, Vazia, Vazia, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Vazia, Vazia, Vazia, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Vazia, Vazia, Bloco Destrutivel, Bloco Destrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Destrutivel, Bloco Destrutivel, Vazia, Vazia, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel], [Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Vazia, Vazia, Bloco Destrutivel, Bloco Destrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Destrutivel, Bloco Destrutivel, Vazia, Vazia, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Vazia, Vazia, Vazia, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Vazia, Vazia, Vazia, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Vazia, Vazia, Vazia, Bloco Destrutivel, Bloco Destrutivel, Vazia, Vazia, Vazia, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Vazia, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Vazia, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Destrutivel, Bloco Destrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Destrutivel, Bloco Destrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Destrutivel, Bloco Destrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Destrutivel, Bloco Destrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Destrutivel, Bloco Destrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel]]
-- | O 3º mapa.
mapa3 :: Mapa
mapa3 = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel, Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel, Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel, Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]
-- | O 4º mapa.
mapa4 :: Mapa
mapa4 = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]
-- | O 5º mapa (este mapa foi usado apenas para testes, é igual a um dos mapas do torneio da tarefa 6).
mapa5 :: Mapa
mapa5 = [[Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Vazia, Vazia, Bloco Destrutivel, Vazia, Vazia, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel], [Bloco Indestrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Indestrutivel], [Bloco Indestrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Indestrutivel], [Bloco Indestrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Destrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Indestrutivel], [Bloco Indestrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Destrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Indestrutivel], [Bloco Indestrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Indestrutivel], [Bloco Indestrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Indestrutivel], [Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Vazia, Vazia, Bloco Destrutivel, Vazia, Vazia, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel]]
-- | O 6º mapa (este mapa foi usado apenas para testes, é igual a um dos mapas do torneio da tarefa 6).
mapa6 :: Mapa
mapa6 = [[Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel, Bloco Indestrutivel, Vazia, Vazia, Vazia, Bloco Indestrutivel, Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Bloco Indestrutivel, Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Destrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Destrutivel, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Bloco Destrutivel, Vazia, Bloco Destrutivel, Bloco Destrutivel, Vazia, Vazia, Vazia, Bloco Destrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Bloco Destrutivel, Bloco Destrutivel, Vazia, Bloco Destrutivel, Bloco Destrutivel, Vazia, Vazia, Bloco Destrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Vazia, Bloco Destrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Destrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel], [Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel]]

-- *** Ignorar estas funções, apenas servem para abreviar coisas mais compridas.
-- | fromIntegral
fi :: Int -> Float
fi = fromIntegral
-- | "Tarefa5/Resources/"
t5r :: String
t5r = "Tarefa5/Resources/"