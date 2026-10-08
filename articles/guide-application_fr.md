# Guide de l'application

`nemetonshiny` est l’application web de la plateforme Néméton : elle
permet d’analyser une forêt sans écrire de code, depuis un navigateur.
Elle ne calcule rien elle-même. Les indicateurs, les familles, le niveau
de précision (NDP), FORDEAD, RECONFORT et la reGénération viennent du
paquet cœur [`nemeton`](https://pobsteta.github.io/nemeton/) ; la
desserte et l’accessibilité, du paquet `foretaccess`. L’application
orchestre ces calculs, enregistre les projets et présente les résultats.

## Installer et lancer

L’installation (dépendances système, chaîne Rust, `pak`) est décrite
dans le [README](https://github.com/pobsteta/nemetonshiny#installation).
Ensuite :

``` r

nemetonshiny::run_app()
```

| Paramètre | Rôle | Défaut |
|----|----|----|
| `language` | langue de départ, `"fr"` ou `"en"` | langue du système |
| `project_dir` | dossier des projets | dossier utilisateur (`~/.local/share/nemeton/projects` sous Linux) |
| `max_parcels` | nombre maximal de parcelles sélectionnables | `30` |
| `tour` | visite guidée au premier lancement | `TRUE` |
| `options` | options Shiny (`port`, `host`, `launch.browser`…) | navigateur ouvert seulement en session interactive |

Sur un serveur :

``` r

nemetonshiny::run_app(tour = FALSE, options = list(port = 3838, host = "0.0.0.0"))
```

La base PostGIS, l’authentification, les clés d’API et les notifications
se règlent par des variables d’environnement, décrites dans le [contrat
public](https://github.com/pobsteta/nemetonshiny/blob/main/CONTRAT.md).

## Parcours type

### 1. Atlas › Sélection : parcelles, projet et unités de gestion

L’onglet **Atlas** est le point de départ. Il a deux sous-onglets,
**Sélection** et **Synthèse** ; on commence par **Sélection**.

1.  Choisissez un **département** puis une **commune** : ses parcelles
    cadastrales s’affichent sur la carte.
2.  **Cliquez les parcelles** à étudier (30 au plus par défaut, voir
    `max_parcels`).
3.  Donnez un **nom** au projet et créez-le. Les projets sont
    enregistrés dans le dossier des projets ; les **projets récents** se
    rouvrent d’un clic. Un projet créé avec une version antérieure à la
    1.0 n’est pas repris : il est marqué « Antérieur à la 1.0 » et ne
    peut qu’être supprimé, puis recréé.
4.  Sous la carte, l’éditeur des **Unités de Gestion Forestières (UGF)**
    regroupe ou découpe les parcelles en unités de gestion : création,
    fusion, découpage au trait ou au polygone, renommage, groupe
    d’aménagement, import ou export du découpage en GeoPackage pour le
    retoucher dans QGIS. Les indicateurs sont calculés **par UGF**.

Le bouton **Lancer tous les calculs** enchaîne les calculs du projet :
les indicateurs, puis les modules qui s’appuient dessus.

### 2. Calcul des indicateurs

Le calcul télécharge les données publiques nécessaires (IGN : cadastre,
BD TOPO, BD Forêt, MNT, LiDAR HD ; INPN ; OSO ; Sentinel-2 s’il est en
cache…) puis calcule les indicateurs des **12 familles**. Une carte de
progression suit les téléchargements et les calculs. Il tourne hors de
la session : on peut continuer à naviguer, et un calcul long n’empêche
pas les autres utilisateurs de travailler.

Sans données terrain, l’application travaille au **NDP 0** (sources
publiques seulement). La confiance associée au niveau de précision est
affichée avec le score.

### 3. Atlas › Synthèse

Le sous-onglet **Synthèse** de l’Atlas rassemble les résultats du
projet.

- **Score global** sur 100 : agrégation des 12 familles par le cœur
  (pondération de Fibonacci selon le NDP). Les scores de famille sont
  **pondérés par la surface des UGF** : une petite unité ne pèse pas
  autant qu’une grande.
- **Radar** des 12 familles et **tableau récapitulatif**.
- **Analyse IA** : synthèse rédigée par un modèle de langage selon un
  **profil d’expert** (propriétaire, gestionnaire, naturaliste, élu…),
  avec les sources documentaires citées si un corpus est configuré. Le
  contenu généré est signalé par la couleur ambre.
- **Commentaires** libres, repris dans le rapport.
- **Exports** : rapport **PDF** (Quarto si disponible, avec une image de
  couverture au choix) et **GeoPackage** des résultats pour un SIG.

### 4. Familles d’indicateurs

Le menu **Familles d’indicateurs** ouvre un onglet par famille (Carbone,
Biodiversité, Eau, Air, Sol, Paysage, Temporel, Risques, Social,
Production, Énergie, Naturalité). Chacun montre la carte des UGF, la
valeur de chaque indicateur, sa méthode et, quand une source manque, la
raison d’une valeur absente. La famille Production présente aussi les
volumes estimés par les tarifs IFN.

### 5. Plan d’actions

Planification des interventions par UGF : actions et années, vue
calendrier et tableau Kanban, génération d’un plan par l’IA, historique
des modifications. Le plan s’échange avec **Marculus** (export du
martelage, réimport des tiges cubées) et entre dans un rapport dédié.

### 6. Terrain accessible

- **Export terrain** : plan d’échantillonnage des placettes (tirage
  spatial équilibré, ordre de visite) et export vers **QField / QGIS**.
- **Import terrain** : ingestion du GeoPackage rapporté du terrain, et
  validation des alertes sanitaires visitées.
- **Accessibilité** et **Desserte** : réseau routier, pistes, places de
  dépôt, propositions de desserte et contrôle d’intégrité du réseau.

### 7. Suivi sanitaire

Surveillance de la santé de la forêt à partir de Sentinel-2. L’onglet a
un sous-onglet par mode :

| Mode | Usage |
|----|----|
| **FAST** | chocs récents (coupe, chablis, incendie) : indices NDMI / NDVI / NBR en fenêtre glissante et en tendance |
| **FORDEAD** | dépérissement progressif des résineux (scolyte, sécheresse) ; nécessite Python \>= 3.11 |
| **RECONFORT** | dépérissement des feuillus (chêne, châtaignier) ; nécessite un environnement de calcul dédié |

Les zones suivies sont créées à partir des UGF du projet. Chaque mode
propose un plan de placettes de validation à visiter. Les traitements
longs peuvent envoyer une notification push (ntfy) en fin de calcul.

### 8. reGénération

Aide au choix des essences de reboisement : sensibilité climatique (R6),
risque de gel tardif (R7) et contexte climatique régional (E-OBS), par
UGF.

## Réglages

- **Langue** : sélecteur FR / EN de la barre de navigation. Le choix
  vaut pour votre session seulement ; on peut aussi ouvrir l’application
  avec `?lang=en`.
- **Configuration** (icône d’engrenage) :
  - *Theia / DATA TERRA* : identifiants pour les données Sentinel-2 et
    les hauteurs de canopée ;
  - *Fournisseur LLM* : Mistral, Anthropic, OpenAI, Ollama… et la clé
    correspondante (les clés peuvent aussi venir de `MISTRAL_API_KEY`,
    `ANTHROPIC_API_KEY`, `OPENAI_API_KEY`) ;
  - *Corpus RAG* : documents de référence cités par l’analyse IA ;
  - *Sources & paramètres* : réglages par projet (coupes rases, îlots de
    chaleur, FAST, accessibilité, production, desserte, ONF,
    reGénération).

  Sur un serveur avec authentification, ces réglages sont réservés aux
  administrateurs.
- **Aide** (icône point d’interrogation) : aide et relance de la visite
  guidée.

## Plusieurs utilisateurs

Avec une authentification OAuth (Keycloak), les droits viennent des
rôles : un rôle d’édition pour modifier, sinon lecture seule. Un projet
ouvert est **verrouillé** pour un seul éditeur ; les autres l’ouvrent en
lecture seule, avec un bandeau. Il n’y a **pas d’isolation** entre
utilisateurs d’une même instance : tout utilisateur authentifié voit
tous les projets. Une instance correspond donc à un collectif de
confiance.

## Sans l’interface

- **API R** :
  [`projets_lister()`](https://pobsteta.github.io/nemetonshiny/reference/projets_lister.md),
  [`projet_etat()`](https://pobsteta.github.io/nemetonshiny/reference/projet_etat.md),
  [`projet_lire()`](https://pobsteta.github.io/nemetonshiny/reference/projet_lire.md),
  [`projet_creer()`](https://pobsteta.github.io/nemetonshiny/reference/projet_creer.md),
  [`parcelles_commune()`](https://pobsteta.github.io/nemetonshiny/reference/parcelles_commune.md),
  [`projet_calculer()`](https://pobsteta.github.io/nemetonshiny/reference/projet_calculer.md),
  [`projet_rapport()`](https://pobsteta.github.io/nemetonshiny/reference/projet_rapport.md),
  [`projet_gpkg()`](https://pobsteta.github.io/nemetonshiny/reference/projet_gpkg.md)
  (voir
  [`?api_hors_interface`](https://pobsteta.github.io/nemetonshiny/reference/api_hors_interface.md)).
  Elles donnent les mêmes chiffres que l’onglet Synthèse.
- **Liens profonds** : `http://<hôte>/?project=<id>&tab=<onglet>` ouvre
  l’application sur un projet et un onglet (`synthesis`, `action_plan`,
  `monitoring`, `famille_carbone`…). Ouverte par une autre page
  (assistant VICTOR), l’application lui signale quand la page est prête.
- **Serveur MCP** (`inst/mcp/server.R`) : un assistant (Claude Code,
  AIGORA) peut lister les projets, lire leur synthèse, lancer un calcul
  en tâche de fond, suivre et annuler ce calcul, générer le rapport ou
  le GeoPackage. Voir `inst/mcp/README.md`.

## Résolution des problèmes

- **Les données ne se téléchargent pas** : vérifier la connexion
  internet ; les services de l’IGN et de l’INPN sont parfois
  indisponibles. L’indicateur concerné reste vide et sa fiche en donne
  la raison.
- **Le PDF ne se génère pas** : installer [Quarto](https://quarto.org)
  et une distribution LaTeX (`xelatex`). Sans Quarto, un PDF simplifié
  est produit.
- **L’analyse IA ne répond pas** : vérifier la clé du fournisseur dans
  la configuration ; avec Mistral, seuls certains modèles sont ouverts
  aux comptes gratuits.
- **Le projet s’ouvre en lecture seule** : un autre utilisateur le
  modifie, ou votre compte n’a pas de rôle d’édition.
- **« Base antérieure à la 1.0.0 »** : la base de suivi sanitaire (ou la
  base PostGIS) a été créée avant la 1.0.0 ; la recréer (nouveau fichier
  SQLite, ou base PostgreSQL vide).
- **P2 vide, C1 « estimé par le NDVI »** : la BD Forêt ne donne pas
  l’âge des peuplements. Sans âge réel, l’indice de station (P2) ne se
  calcule pas et la biomasse (C1) retombe sur le NDVI, sauf avec un
  modèle de canopée LiDAR.

## Voir aussi

- [`?run_app`](https://pobsteta.github.io/nemetonshiny/reference/run_app.md)
  et
  [`?api_hors_interface`](https://pobsteta.github.io/nemetonshiny/reference/api_hors_interface.md)
- [Contrat
  public](https://github.com/pobsteta/nemetonshiny/blob/main/CONTRAT.md)
  : variables d’environnement, format des projets, compatibilité
- [Documentation du cœur `nemeton`](https://pobsteta.github.io/nemeton/)
  : définition et formule des indicateurs
