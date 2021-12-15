1. Bragina Natalia, gitlab id: bragina, numéro d'étudiant 22015704
Golikova Anna, gitlab id: golikova, numéro d'étudiant 21962377

2. Fonctionnalités : Dans notre projet, les fonctionnalités attendues 
sont implémentées : un help est disponible, ainsi que les fonctions "reprint" (qui reproduit le 
texte du programme demandé) et "eval" qui effectue l'évaluation du programme choisi.

3. Compilation et exécution : La compilation se fait en lançant la commande "make" depuis le terminal; 
ensuite on exécute ./run --help (pour voir la liste des options disponibles), 
./run --reprint "chemin du fichier" pour reaffichage, ./run --eval "chemin du fichier".

4. Découpage modulaire : Le projet contient les modules suivants: polish, types, evaluate, prints et syntax. 
Les rôles sont répartis entre les modules comme suit :

polish.ml : contient main et les fonctions du groupe "read" et "split" qui permettent de lire dans le fichier et de 
decouper les lignes du code en chaine de caractères.

types.ml : contient les définitions des types proposés au début du projet et complétés. Nous avons entroduit un nouveau
type "code_lines_t" (qui désigne le code dont les lignes sont numérotées et decoupées en chaines de caractères) car il 
est fréquemment utilisé dans notre programme.

syntax.ml : contient les fonctions qui analysent les sequences de chaines de caractères et les convertissent en 
conditions, expressions etc., suivies par les fonctions qui construisent un bloc du programme. 

evaluate.ml : inclut les fonctions qui évaluent les opérateurs arithmétiques, les expressions et les conditions, 
et ensuite les blocs d'instructions. 

prints.ml : les fonctions d'affichage.

5. Organisation du travail : Nous avons surtout réparti les tâches concernant les fonctionnalités -reprint et -eval. 
Le prétraitement du code s'est effectué pendant le travail sur -reprint et l'organisation modulaire du programme a été 
effectué après l'implémentation de -eval.

6. 