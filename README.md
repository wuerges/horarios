# rhorarios


O programa vem com três casos de testes, na pasta inputs. 
O primeiro é um caso simples, que é obrigado a funcionar.
O segundo também é um caso simples, mas não deve ser possível alocar os horários para esse teste.
O terceiro teste é baseado na carga horária de 2015/2.

Antes de compilar o programa, é necessário, ter o GHC instalado, com alguns pacotes:

$ apt-get install ghc
$ cabal install cabal cabal-install
$ cabal update
$ cabal install fgl parsec lens


Para compilar o programa:

$ ghc -O2 Main.hs -o alocador

Para rodar o programa, basta executá-lo com o teste passado pela entrada padrão.

Exemplo:

$ ./alocador < inputs/teste3.in


A saída do programa é um arquivo CSV com os horários.
Quando o programa não conseguiu alocar horários para todas as disciplinas, imprime na primeira linha

> Allocation Error:

E na segunda linha, a lista de disciplinas que não conseguiu alocar:

> unallocated: (Opt IV, Braulio), (BD II, Denio), (Teoria, Zatesko), (Int. Inf., Raquel)



