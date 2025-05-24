## Mini compilateur langage C-like

Ce readme présentera mon projet de compilateur de langage "similaire" au C vers de l'assembleur MIPS.Je précise que j'ai repris depuis le dossier proj2 que vous avez envoyé sur mattermost. Ce compilateur permet actuellement la génération de code MIPS valide à partir de fichiers test contenant des instructions.
Il manque donc à ce projet les définitions de fonctions,pour qu'il soit complet pour faire une sorte de mini-C.



Mais d'abord pour le tester:

-> dune build main.exe 
-> ./main tests/prog-complet.test > test.s 
-> spim 
-> load "test.s" 
-> run 











