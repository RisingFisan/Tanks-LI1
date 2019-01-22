-- | Este módulo define funções comuns da Tarefa 1 do trabalho prático.
module Tarefa1_2018li1g080 where

import LI11819
import Tarefa0_2018li1g080

-- * Testes

-- | Testes unitários da Tarefa 1.
--
-- Cada teste é uma sequência de 'Instrucoes'.
testesT1 :: [Instrucoes]
testesT1 = [[Move C,MudaParede,Move C,Move B,Desenha,MudaTetromino,Roda,Roda,Desenha,Roda,Roda,Move D,Move D,Move B,Desenha,Roda,Roda,Roda,Move C,Move C,Move E,Desenha,Move D,Move D,MudaTetromino,Roda,Move B,Move D,Move B,Desenha,MudaTetromino,MudaTetromino,MudaTetromino,MudaTetromino,MudaTetromino,Move C,Desenha,Move D,Move D,Move D,Move D,Move E,Desenha,Roda,Roda,Move D,Desenha,Move E,Roda,Move D,Move E,Move E,MudaTetromino,Move D,Move D,Move B,Desenha,Move C,Move C,Desenha,Move B,MudaParede],[]]

-- * Funções principais da Tarefa 1.

-- | Aplica uma 'Instrucao' num 'Editor'.
--
--    * 'Move' - move numa dada 'Direcao'.
--
--    * 'MudaTetromino' - seleciona a 'Peca' seguinte (usar a ordem léxica na estrutura de dados),
--       sem alterar os outros parâmetros.
--
--    * 'MudaParede' - muda o tipo de 'Parede'.
--
--    * 'Desenha' - altera o 'Mapa' para incluir o 'Tetromino' atual, sem alterar os outros parâmetros.
instrucao :: Instrucao -- ^ A 'Instrucao' a aplicar.
          -> Editor    -- ^ O 'Editor' anterior.
          -> Editor    -- ^ O 'Editor' resultante após aplicar a 'Instrucao'.

instrucao (Move d) edit = edit {posicaoEditor = somaVetores (posicaoEditor edit) (direcaoParaVetor d) }

instrucao Roda edit = edit { direcaoEditor = rodaDirecao (direcaoEditor edit) }

instrucao MudaTetromino edit = edit { tetrominoEditor = mudaTetromino (tetrominoEditor edit) }

instrucao MudaParede edit = edit { paredeEditor = mudaParede (paredeEditor edit) }

instrucao Desenha edit = edit { mapaEditor = mudaMapa (mapaEditor edit) (posicaoEditor edit) (direcaoEditor edit, tetrominoEditor edit, paredeEditor edit) }

-- | Aplica uma sequência de 'Instrucoes' num 'Editor'.
--
-- __NB:__ Deve chamar a função 'instrucao'.
instrucoes :: Instrucoes -- ^ As 'Instrucoes' a aplicar.
           -> Editor     -- ^ O 'Editor' anterior.
           -> Editor     -- ^ O 'Editor' resultante após aplicar as 'Instrucoes'.
instrucoes [] edit = edit
instrucoes [inst] edit = instrucao inst edit
instrucoes (inst:insts) edit = instrucoes insts (instrucao inst edit)

-- | Cria um 'Mapa' inicial com 'Parede's nas bordas e o resto vazio.
mapaInicial :: Dimensao -- ^ A 'Dimensao' do 'Mapa' a criar.
            -> Mapa     -- ^ O 'Mapa' resultante com a 'Dimensao' dada.
mapaInicial (y,x) = foldl (\acc (c,d) -> if not (eBordaMatriz (c,d) acc) then atualizaPosicaoMatriz (c,d) Vazia acc else acc) (criaMatriz (y,x) (Bloco Indestrutivel)) [(a,b) | a <- [0..y] , b <- [0..x]]

-- | Cria um 'Editor' inicial.
--
-- __NB:__ Deve chamar as funções 'mapaInicial', 'dimensaoInicial', e 'posicaoInicial'.
editorInicial :: Instrucoes  -- ^ Uma sequência de 'Instrucoes' de forma a poder calcular a  'dimensaoInicial' e a 'posicaoInicial'.
              -> Editor      -- ^ O 'Editor' inicial, usando a 'Peca' 'I' 'Indestrutivel' voltada para 'C'.
editorInicial insts = Editor {posicaoEditor = posicaoInicial insts, 
                              direcaoEditor = C,
                              tetrominoEditor = I, 
                              paredeEditor = Indestrutivel, 
                              mapaEditor = mapaInicial (dimensaoInicial insts)}

-- | Constrói um 'Mapa' dada uma sequência de 'Instrucoes'.
--
-- __NB:__ Deve chamar as funções 'Instrucoes' e 'editorInicial'.
constroi :: Instrucoes -- ^ Uma sequência de 'Instrucoes' dadas a um 'Editor' para construir um 'Mapa'.
         -> Mapa       -- ^ O 'Mapa' resultante.
constroi insts = mapaEditor (instrucoes insts (editorInicial insts))

-- * Funções auxiliares da Tarefa 1

-- | Roda a 'Direcao' do 'Editor' 90º no sentido dos ponteiros do relógio

rodaDirecao :: Direcao -- ^ 'Direcao' antiga do 'Editor'.
            -> Direcao -- ^ 'Direcao' nova do 'Editor'.
rodaDirecao t = case t of E -> C
                          _ -> succ t

-- | Muda o 'Tetromino' entre as várias formas possíveis

mudaTetromino :: Tetromino -- ^ 'Tetromino' antigo do 'Editor'.
              -> Tetromino -- ^ 'Tetromino' novo do 'Editor'.
mudaTetromino t = case t of Z -> I 
                            _ -> succ t

-- | Muda uma 'Parede' para o estado 'Destrutivel' ou para o estado 'Indestrutivel'

mudaParede :: Parede -- ^ 'Parede' antiga do 'Editor'.
           -> Parede -- ^ 'Parede' nova do 'Editor'.
mudaParede Destrutivel = Indestrutivel
mudaParede Indestrutivel = Destrutivel

-- | Altera o 'Mapa' atual para incluir o 'Tetromino' selecionado.

mudaMapa :: Mapa -- ^ 'Mapa' antigo do 'Editor'.
         -> Posicao -- ^ 'Posicao' do 'Editor'.
         -> (Direcao,Tetromino,Parede) -- ^ 'Direcao', 'Tetromino' e 'Parede' do 'Editor'.
         -> Mapa -- ^ 'Mapa' novo do 'Editor'.
mudaMapa mapa1 (yi,xi) (dir,tetromino,parede) = foldl (\mp (y,x) -> atualizaPosicaoMatriz (yi+y,xi+x) (pecaTetromino (y,x)) mp) mapa1 posicoesTetromino
    where posicoesTetromino = [(y,x) | x <- [0..xMat-1], y <- [0..yMat-1]]
          (yMat,xMat) = dimensaoMatriz $ tetrominoParaMatriz tetromino
          pecaTetromino (y,x) = if encontraPosicaoMatriz (y,x) tetrominoRodado then Bloco parede 
                                                                               else encontraPosicaoMatriz (yi+y,xi+x) mapa1
          tetrominoRodado = case dir of C -> tetrominoParaMatriz tetromino
                                        D -> rodaMatriz $ tetrominoParaMatriz tetromino
                                        B -> rodaMatriz . rodaMatriz $ tetrominoParaMatriz tetromino
                                        E -> rodaMatriz . rodaMatriz . rodaMatriz $ tetrominoParaMatriz tetromino
