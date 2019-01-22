-- | Este módulo define funções genéricas sobre vetores e matrizes, que serão úteis na resolução do trabalho prático.
module Tarefa0_2018li1g080 where

import LI11819

-- * Funções não-recursivas.

-- | Um 'Vetor' é uma 'Posicao' em relação à origem.
type Vetor = Posicao
-- ^ <<http://oi64.tinypic.com/mhvk2x.jpg vetor>>

-- ** Funções sobre vetores

-- *** Funções gerais sobre vetores.

-- | Soma dois vetores.
somaVetores :: Vetor -> Vetor -> Vetor
somaVetores (y1, x1) (y2, x2) = (y1 + y2, x1 + x2)

-- | Subtrai dois vetores.
subtraiVetores :: Vetor -> Vetor -> Vetor
subtraiVetores (y1, x1) (y2, x2) = (y1 - y2, x1 - x2)

-- | Multiplica um escalar por um 'Vetor'.
multiplicaVetor :: Int -> Vetor -> Vetor
multiplicaVetor a (y,x) = (a*y, a*x)

-- | Roda um 'Vetor' 90º no sentido dos ponteiros do relógio, alterando a sua direção sem alterar o seu comprimento (distânciaa à origem).
--
-- <<http://oi65.tinypic.com/2j5o268.jpg rodaVetor>>
rodaVetor :: Vetor -> Vetor
rodaVetor (y, x) = (x, -y)

-- | Espelha um 'Vetor' na horizontal (sendo o espelho o eixo vertical).
--
-- <<http://oi63.tinypic.com/jhfx94.jpg inverteVetorH>>
inverteVetorH :: Vetor -> Vetor
inverteVetorH (y, x) = (y, -x)

-- | Espelha um 'Vetor' na vertical (sendo o espelho o eixo horizontal).
--
-- <<http://oi68.tinypic.com/2n7fqxy.jpg inverteVetorV>>
inverteVetorV :: Vetor -> Vetor
inverteVetorV (y, x) = (-y, x)

-- *** Funções do trabalho sobre vetores.

-- | Devolve um 'Vetor' unitário (de comprimento 1) com a 'Direcao' dada.
direcaoParaVetor :: Direcao -> Vetor
direcaoParaVetor dir = case dir of C -> (-1, 0)
                                   B -> (1, 0)
                                   E -> (0, -1)
                                   D -> (0, 1)

-- ** Funções sobre listas

-- *** Funções gerais sobre listas.
--
-- Funções não disponíveis no 'Prelude', mas com grande utilidade.

-- | Verifica se o indice pertence à lista.
eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido i l = length l >= (i + 1) && i >= 0

-- ** Funções sobre matrizes.

-- *** Funções gerais sobre matrizes.

-- | Uma matriz é um conjunto de elementos a duas dimensões.
--
-- Em notação matemática, é geralmente representada por:
--
-- <<https://upload.wikimedia.org/wikipedia/commons/d/d8/Matriz_organizacao.png matriz>>
type Matriz a = [[a]]

-- | Calcula a dimensão de uma matriz.
dimensaoMatriz :: Matriz a -> Dimensao
dimensaoMatriz [] = (0,0)
dimensaoMatriz m = (length m, length $ head m)

-- | Verifica se a posição pertence à matriz.
ePosicaoMatrizValida :: Posicao -> Matriz a -> Bool
ePosicaoMatrizValida (0, 0) ([]:_) = True
ePosicaoMatrizValida (i, j) a = (i < ia) && (j < ja) && (i >= 0) && (j >= 0) where (ia, ja) = dimensaoMatriz a

-- | Verifica se a posição está numa borda da matriz.
eBordaMatriz :: Posicao -> Matriz a -> Bool
eBordaMatriz (0, 0) [] = True
eBordaMatriz (i, j) a = (i == 0 || j == 0 || i == ia - 1 || j == ja - 1) && ePosicaoMatrizValida (i, j) a where (ia, ja) = dimensaoMatriz a

-- *** Funções do trabalho sobre matrizes.

-- | Converte um 'Tetromino' (orientado para cima) numa 'Matriz' de 'Bool'.
--
-- <<http://oi68.tinypic.com/m8elc9.jpg tetrominos>>
tetrominoParaMatriz :: Tetromino -> Matriz Bool
tetrominoParaMatriz I = [[False, True, False, False],[False, True, False, False],[False, True, False, False],[False, True, False, False]]
tetrominoParaMatriz J = [[False, True, False],[False, True, False],[True, True, False]]
tetrominoParaMatriz L = [[False, True, False],[False, True, False],[False, True, True]]
tetrominoParaMatriz O = [[True, True],[True, True]]
tetrominoParaMatriz S = [[False, True, True],[True, True, False],[False, False, False]]
tetrominoParaMatriz T = [[False, False, False],[True, True, True],[False, True, False]]
tetrominoParaMatriz Z = [[True, True, False],[False, True, True],[False, False, False]]

-- * Funções recursivas.

-- ** Funções sobre listas.
--
-- Funções não disponíveis no 'Prelude', mas com grande utilidade.

-- | Devolve o elemento num dado índice de uma lista.
encontraIndiceLista :: Int -> [a] -> a
encontraIndiceLista 0 (x:_) = x
encontraIndiceLista n (_:xs) = encontraIndiceLista (n-1) xs
encontraIndiceLista _ _ = error "Índice inválido!"

-- | Modifica um elemento num dado índice.
--
-- __NB:__ Devolve a própria lista se o elemento não existir.
atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista _ _ [] = []
atualizaIndiceLista 0 a [_] = [a]
atualizaIndiceLista 0 a (_:xs) = a : xs
atualizaIndiceLista n a (x:xs) = x : atualizaIndiceLista (n-1) a xs

-- ** Funções sobre matrizes.

-- | Roda uma 'Matriz' 90º no sentido dos ponteiros do relógio.
--
-- <<http://oi68.tinypic.com/21deluw.jpg rodaMatriz>>
rodaMatriz :: Matriz a -> Matriz a
rodaMatriz [] = []
rodaMatriz ([]:_) = []
rodaMatriz m = reverse [head mat | mat <- m] : rodaMatriz [tail mat | mat <- m]

{-
rodaMatriz' ([]:m) = []
rodaMatriz' m = (reverse (headM m)):rodaMatriz' (tailM m)
    where headM [x] = [head x]
          headM (mat:mats) = (head mat):headM mats
          tailM [x] = [tail x]
          tailM (mat:mats) = (tail mat):tailM mats
-}

-- | Inverte uma 'Matriz' na horizontal.
--
-- <<http://oi64.tinypic.com/iwhm5u.jpg inverteMatrizH>>
inverteMatrizH :: Matriz a -> Matriz a
inverteMatrizH m = [reverse mat | mat <- m]

-- | Inverte uma 'Matriz' na vertical.
--
-- <<http://oi64.tinypic.com/11l563p.jpg inverteMatrizV>>
inverteMatrizV :: Matriz a -> Matriz a
inverteMatrizV = reverse

-- | Cria uma nova 'Matriz' com o mesmo elemento.
criaMatriz :: Dimensao -> a -> Matriz a
criaMatriz (0, _) _ = []
criaMatriz (i,j) x = replicate j x : criaMatriz (i - 1, j) x

-- | Devolve o elemento numa dada 'Posicao' de uma 'Matriz'.
encontraPosicaoMatriz :: Posicao -> Matriz a -> a
encontraPosicaoMatriz (0, j) (m:_) = encontraIndiceLista j m
encontraPosicaoMatriz (i, j) (_:ms) = encontraPosicaoMatriz (i - 1, j) ms
encontraPosicaoMatriz _ _ = error "Posição inválida!"

-- | Modifica um elemento numa dada 'Posicao'
--
-- __NB:__ Devolve a própria 'Matriz' se o elemento não existir.
atualizaPosicaoMatriz :: Posicao -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz _ _ [] = []
atualizaPosicaoMatriz (0, j) x (m:ms) | ePosicaoMatrizValida (0, j) (m:ms) = atualizaIndiceLista j x m : ms
                                      | otherwise = m : ms
atualizaPosicaoMatriz (i, j) x m@(m0:ms) | ePosicaoMatrizValida (i, j) m = m0:atualizaPosicaoMatriz (i - 1, j) x ms
                                         | otherwise = m

