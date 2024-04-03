# ProjetTDD
Projet de Types de Données, Preuves

## Auteurs
 - LOAËC Jade
 - DA COSTA Yacine

## Compilation et execution des tests
Les tests sont tous présents dans le dossier `./Tests`.

Afin de compiler les fichiers, il suffit de faire la commande `make` dans la console. 

Pour tester la vérification de types : 
```ocaml
    make
    ocaml
    #use "use.ml";;
    open Parser;;
    let f = parse "Tests/fichier_voulu.ml";;;
```

Pour tester la transformation : 
```sh
    make
    ./comp Tests/fichier_voulu.ml
```

## Travail effectué

### Vérification des types : fichier `typing.ml`
 - Fonction `tp_expr` qui renvoie le type d'une expression passée en argument
 - Fonction `tp_var` qui cherche une variable dans l'environnement et renvoie son type 
 - Fonction `tp_prog` qui permet de construire un environnement et d'évaluer le type d'une expression 
 - Toutes les fonctions annnexes necessaires pour faire fonctionner ces autres fonctions

### Transformation de MiniCaml vers MiniPython : `fichier transf.ml`

- Fonction `names_expr` qui renvoie l'ensemble des noms de variables utilisés dans l'expression passée en argument

- Fonction `is_tailrec_expr` qui détermine si une fonction est récursive terminale

- Fonctions `transf_expr`, `transf_fpdefn` et `transf_prog` qui transforment des programmes MiniCaml en MiniPython

### Affichage du code MiniPython : fichier `pprinter.ml`

Par manque de temps, ce fichier là n'a pas été fait par nos soins. Nous utilisons juste la version d'un camarade afin de pouvoir montrer les résultats de la transformation de MiniCaml en MiniPython.

## Difficultés rencontrées

Pour nous, la majeure difficulté a été la compréhension du sujet, afin de savoir comment et part où commencer. Une autre difficulté qui nous a freiné était de bien comprendre les outils et fichiers existants, ce qu'ils représentent, et comment s'en servir.