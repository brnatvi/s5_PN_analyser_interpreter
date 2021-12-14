1. Bragina Natalia, gitlab id: bragina, numéro d'étudiant 22015704
Golikova Anna, gitlab id: golikova, numéro d'étudiant 21962377

2. Fonctionnalités : Dans notre projet, les fonctionnalités attendues 
sont implémentées : un help est disponible, ainsi que les fonctions "reprint" (qui reproduit le 
texte du programme demandé) et "eval" qui effectue l'évaluation du programme choisi.

3. Compilation et exécution : La compilation se fait en lançant la commande "make" depuis le terminal; 
4. ensuite on exécute ./run --help (pour voir la liste des options disponibles), 
5. ./run --reprint "chemin du fichier" pour reaffichage, ./run --eval "chemin du fichier".

4. Découpage modulaire : Le projet contient les modules suivants: polish, types, evaluate, prints et syntax. 
Les rôles sont répartis entre les modules comme suit :
polish.ml : contient main et les fonctions du groupe "read" (qui permettent de lire dans un fichier et de 
compter les indentations) et "split" (qui coupent les chaînes de caractères).
types.ml : contient les définitions des types proposés au début du projet et complétés.
evaluate.ml : inclut les fonctions qui évaluent les opérateurs arithmétiques, les expressions et 
les conditions, et ensuite les blocs d'instructions. 
syntax.ml : contient les fonctions qui analysent les données passées en paramètre 
(opérateurs, variables, expressions...) suivies par les fonctions qui construisent un bloc. 
prints.ml : les fonctions d'affichage.

5. Organisation du travail : Nous avons surtout réparti les tâches concernant les fonctionnalités 
6. -reprint et -eval; -eval a été fait plus tard car nous avons rencontré des difficultés au départ liées 
7. à l'interprétation initiale des fichiers polish.

6. 