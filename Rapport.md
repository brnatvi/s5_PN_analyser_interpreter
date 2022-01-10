1. Bragina Natalia, gitlab id: bragina, numéro d'étudiant 22015704
Golikova Anna, gitlab id: golikova, numéro d'étudiant 21962377

2. Fonctionnalités : Dans notre projet, les fonctionnalités suivantes 
sont implémentées : un help est disponible, ainsi que les fonctions "reprint" (qui reproduit le texte du programme demandé), "eval" qui effectue l'évaluation du programme choisi, "vars" qui fournit deux listes de variables, "simpl" qui simplifie le programme donné, "sign" qui traite les signes possibles des variables.

3. Compilation et exécution : La compilation se fait en lançant la commande "make" depuis le terminal; 
ensuite on exécute ./run --help (pour voir la liste des options disponibles), 
./run --reprint "chemin du fichier" pour reaffichage, ./run --eval "chemin du fichier",
./run --vars "chemin du fichier", ./run --simpl "chemin du fichier", ./run --sign "chemin du fichier".

4. Découpage modulaire : Le projet contient les modules suivants: polish, types, evaluate, prints, syntax, vars, simpl et sign. 
Les rôles sont répartis entre les modules comme suit :

polish.ml : contient main et les fonctions du groupe "read" et "split" qui permettent de lire dans le fichier et de 
decouper les lignes du code en chaine de caractères.

types.ml : contient les définitions des types proposées au début du projet et complétées. Nous avons entroduit un nouveau type "code_lines_t" (qui désigne le code dont les lignes sont numérotées et decoupées en chaines de caractères) car il est fréquemment utilisé dans notre programme.

syntax.ml : contient les fonctions qui analysent les sequences de chaines de caractères et les convertissent en conditions, expressions etc., suivies par les fonctions qui construisent un bloc du programme. 

evaluate.ml : inclut les fonctions qui évaluent les opérateurs arithmétiques, les expressions et les conditions, et ensuite les blocs d'instructions. 

prints.ml : les fonctions d'affichage.

vars.ml : affiche les noms des variables du fichier .p en question et les répartissant en deux groupes: toutes les variables et les variables auxquelles on a accès avant leur première écriture.

simpl.ml : affiche le programme .p passé en paramètres après sa simplification, c'est-à-dire après avoir fait les opérations arithmétiques possibles et après avoir fait la propagation des constantes. 

sign.ml : considère les signes possibles des variables. Nous avons implémenté -sign dans 
une version où dans "if" et "while" la valuation des variables ne se fait pas; on effectue 
juste la propagation des variables dans les blocs. Mais nous avons écrit l'essentiel des fonctions qui permetteraient cette valuation. Nous avons introduit un nouveau type pour cette valuation: sign_bool qui peut prendre les valeurs Yes | No | May be.

Nous avons ajouté des fichiers .mli pour six modules (prints.ml, syntax.ml, evaluate.ml, 
vars.ml, simpl.ml, sign.ml) pour limiter
l'accès à leur contenu des autres modules et rendre visible juste les fonctions nécessaires ailleurs.

5. Organisation du travail : Au début, nous avons surtout réparti les tâches concernant les fonctionnalités -reprint et -eval. 
Le prétraitement du code s'est effectué pendant le travail sur -reprint et l'organisation modulaire du programme a été 
effectué après l'implémentation de -eval.
Pour la deuxième partie, Natalia Bragina a fait la partie concernant -vars, ensuite nous avons travaillé ensemble sur les fonctionnalités -simpl et -sign.