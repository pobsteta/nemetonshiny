# BRIEF `nemetonshiny` — rendre visibles les sorties de l'onglet Desserte

> **Statut** : ouvert, 2026-08-14.
> **Packages concernés** : `nemetonshiny` (l'essentiel), `foretaccess` (un
> manque bloquant, §4).
> **Nature** : aucune modification du calcul. Trois sorties sur cinq sont
> produites, écrites sur disque, puis jamais montrées.
> **Livré en brief, pas en patch** : `R/mod_desserte.R` était modifié il y a
> trois minutes au moment de la rédaction (session en cours, `main` à
> `0.123.0.9000`, 5 fichiers en travail). Des patches ancrés sur des numéros
> de ligne y pourriraient en quelques minutes. Le brief cite donc des
> **observers et des fonctions**, jamais des lignes.
> **Contexte de lecture** : `nemetonshiny@0.123.0.9000`, `foretaccess 2.3.0`,
> lecture seule.

---

## 1. Le constat

La sidebar droite de l'onglet Desserte porte cinq actions. Elles produisent
toutes un résultat sur disque. **Deux seulement sont visibles sur la carte.**

| Action | Produit | Sur la carte | Dans l'export |
|---|---|---|---|
| Typage du réseau | `typage_<moteur>.gpkg` / `reseau_type` | ⚠️ calque « Réseau typé », **perdu au rechargement** (§2.2) | ❌ |
| Intégrité du réseau | `integrite.rds` (4 scalaires) | ✅ badges (pas de géométrie — normal) | — |
| Optimisation du réseau | `optim.rds` (coût, n routes) | ✅ 2 lignes de texte | — |
| **Complément OSM** | `desserte_osm.gpkg` / `osm_track` | ❌ **rien** | ❌ |
| **Détection de routes** | `desserte_detectee.gpkg` / `desserte_detectee` | ❌ **rien** | ❌ |

Les deux dernières sont les plus coûteuses de l'onglet — la détection demande
plusieurs minutes et jusqu'à 8 Go sur ~1 800 ha, la comparaison OSM dépend du
débit d'Overpass — et ce sont précisément celles dont on ne voit rien. La
sidebar affiche un compteur (« *N* tronçons analysés », « *N* route(s)
détectée(s) ») et une table de classes ; la géométrie, elle, reste dans le
cache du projet :

```
<projet>/cache/desserte/
├── desserte.gpkg            parcelles, desserte_existante, reseau_cree  ← seul exporté
├── typage_<moteur>.gpkg     reseau_type          (carte oui, export non)
├── desserte_osm.gpkg        osm_track            ← QGIS uniquement
└── desserte_detectee.gpkg   desserte_detectee    ← QGIS uniquement
```

Pour voir son propre résultat, l'utilisateur doit connaître ce chemin et ouvrir
QGIS. Ce n'est écrit nulle part dans l'interface.

---

## 2. Ce qui manque, côté app

### 2.1 Deux calques carte

Le gabarit existe déjà et fonctionne : l'observer du calque « Réseau typé »
fait exactement ce qu'il faut — `leafletProxy()` + `clearGroup()`, lecture du
GeoPackage, `st_transform(4326)`, `addPolylines()` coloré par attribut, et le
respect de `input$map_groups` lu sous `isolate()` pour ne pas s'auto-déclencher.
**Copier ce gabarit deux fois**, avec deux constantes de groupe à côté des
existantes (`DESS_GROUPE_PARCELLES`, `DESS_GROUPE_TYPE`, …) :

```r
DESS_GROUPE_OSM      <- "Pistes OSM"
DESS_GROUPE_DETECTEE <- "Routes détectées"
```

et les ajouter au vecteur `overlays` qui alimente `addLayersControl()` — un
groupe peint mais non déclaré n'a pas de case pour l'éteindre, le commentaire
en place le rappelle (régression corrigée en 0.122.6).

**Recommandation : les deux démarrent éteints.** Ce sont des diagnostics, pas
le résultat de l'onglet ; les allumer d'office noierait le réseau conçu sous
des centaines de tronçons OSM. Un `leaflet::hideGroup()` à la création de la
carte suffit — le reste de la logique `shown` continue de respecter le choix de
l'utilisateur ensuite.

**Symbologie proposée**

- *Routes détectées* — couleur par `CLASSE`, trait **tireté** pour dire
  « hypothèse, pas relevé ». Popup : `CLASSE`, `CLASSE_CONF`, `CLASSE_MOTIF`
  (quels critères ont voté) et `OSM_TAGS` quand il est renseigné. `CLASSE_CONF`
  mérite d'être portée à l'écran et pas seulement en moyenne dans la sidebar :
  une classe posée sur deux critères sur six ne vaut pas une classe posée sur
  six.
- *Pistes OSM* — gris-bleu, tireté, label par type OSM. **Lire le §4 avant de
  choisir le libellé du calque** : ce ne sont pas « les pistes absentes de la
  BD TOPO ».

### 2.2 Un chemin de GeoPackage à remonter

Asymétrie à corriger dans `service_desserte.R` : `run_desserte_detection()`
renvoie bien `gpkg_path`, mais `run_desserte_osm()` écrit `desserte_osm.gpkg`
**sans le renvoyer** — sa liste de retour s'arrête à `n_osm`, `resume`,
`corridor_m`, `date_requete`, `foretaccess_version`. Le module devrait
reconstruire le chemin à la main, ce qui dupliquerait la convention de nommage.
Ajouter `gpkg_path` au retour, et le faire ressortir aussi par
`.load_cached_osm()` (il lit `osm.rds`, qui ne porte pas le chemin non plus) —
sinon le calque disparaît au rechargement du projet alors que le fichier est là.

**Le typage, lui, ne survit pas du tout au rechargement** — vérifié :
`service_desserte.R` définit `.load_cached_integrite()`, `.load_cached_optim()`,
`.load_cached_osm()` et `.load_cached_detection()`, mais **il n'existe aucun
`.load_cached_typage()`**. Le module lit `rv_typage()` seul, sans repli sur le
cache, aussi bien pour la table classe/longueur que pour l'observer du calque.
Résultat : on rouvre le projet, `typage_<moteur>.gpkg` est bien là sur le
disque, et l'onglet affiche « Générez d'abord une desserte, puis typez son
réseau » — il faut refaire tourner le typage. À aligner sur les quatre autres
(un `typage.rds` à écrire, un `.load_cached_typage()` à lire).

### 2.3 Un export qui exporte tout

`export_desserte_geopackage()` fait un `file.copy()` de `desserte.gpkg`. Le
bouton « Télécharger le GeoPackage » livre donc `parcelles`,
`desserte_existante`, `reseau_cree` — et rien d'autre. Le typage, la
comparaison OSM et la détection, qui sont le travail le plus long de l'onglet,
n'en sortent jamais.

Fusionner les couches présentes dans un seul fichier au moment du
téléchargement :

```r
export_desserte_geopackage <- function(result, file, cache_dir = NULL) {
  src <- tryCatch(result$gpkg_path, error = function(e) NULL)
  if (is.null(src) || !file.exists(src)) return(invisible(FALSE))
  if (!isTRUE(tryCatch(file.copy(src, file, overwrite = TRUE),
                       error = function(e) FALSE))) return(invisible(FALSE))

  # Couches optionnelles : chacune n'existe que si l'action a tourne.
  # `append = FALSE` remplace la couche ; surtout PAS `delete_dsn`, qui
  # effacerait le fichier qu'on vient de copier.
  extras <- list(
    reseau_type        = file.path(cache_dir, "typage_*.gpkg"),
    osm_track          = file.path(cache_dir, "desserte_osm.gpkg"),
    desserte_detectee  = file.path(cache_dir, "desserte_detectee.gpkg")
  )
  for (lyr in names(extras)) {
    gp <- Sys.glob(extras[[lyr]])[1]
    if (is.na(gp) || !file.exists(gp)) next
    d <- tryCatch(sf::st_read(gp, layer = lyr, quiet = TRUE), error = function(e) NULL)
    if (!inherits(d, "sf") || nrow(d) == 0L) next
    tryCatch(sf::st_write(d, file, layer = lyr, append = FALSE, quiet = TRUE),
             error = function(e) invisible(NULL))
  }
  invisible(TRUE)
}
```

Le `Sys.glob()` sur `typage_*.gpkg` est là parce que le nom porte le moteur
(`typage_glouton.gpkg` / `typage_steiner.gpkg`) ; si les deux existent, prendre
celui du moteur du run courant plutôt que le premier venu.

Point de vigilance : le module appelle aujourd'hui
`export_desserte_geopackage(res, file)` sans `cache_dir`. Le dossier se déduit
de `result$gpkg_path` (`dirname()`), ce qui évite de changer la signature — au
choix.

### 2.4 Dire où sont les fichiers

Indépendamment de tout le reste : afficher le chemin du cache quelque part dans
l'onglet (une ligne `text-muted small` sous le bouton de téléchargement) ferait
gagner un temps réel à qui veut ouvrir les couches dans QGIS. Aujourd'hui rien
ne l'indique.

---

## 3. Ce que la carte ne doit PAS laisser croire

Trois garde-fous déjà présents en texte dans la sidebar, à ne pas perdre au
passage à la carte — une géométrie affirme beaucoup plus qu'un compteur.

1. **La détection sans LiDAR est « nettement moins sûre »** (avertissement du
   cœur, déjà affiché). Une absence de détection ne vaut pas constat d'absence
   de route. Si le calque est allumé sans LiDAR, le rappel doit rester visible.
2. **`CLASSE` seule est trompeuse** — le brief de la spec 028 insiste : une
   classe posée sur peu de critères renseignés doit se voir. D'où `CLASSE_CONF`
   au popup, et pas seulement la moyenne en sidebar. Rappel : les critères
   fossés et NDVI ne sont pas encore fournis par l'app, la confiance est donc
   structurellement plafonnée.
3. **`OSM_TAGS` est une proposition, jamais un téléversement.** Le libellé du
   popup doit le dire, comme le fait déjà l'encart de la sidebar.

---

## 4. Le point dur : la comparaison OSM ne renvoie aucune géométrie

C'est le manque qui empêche de faire proprement le calque le plus attendu.

`foretaccess::comparer_desserte_osm(bdtopo, osm, corridor_m = 15)` renvoie
**trois tables de linéaire** — `osm` (linéaire OSM total et hors corridor, par
type), `bdtopo` (linéaire BD TOPO hors corridor OSM, par classe) et `resume`.
**Aucune géométrie.** Le « hors corridor », c'est-à-dire le gisement à
instruire, n'existe que sous forme de mètres linéaires.

Conséquence directe : ce que l'app écrit dans `desserte_osm.gpkg` n'est **pas**
le résultat de la comparaison, c'est la couche OSM **brute** telle
qu'acquise — tous les tronçons, y compris ceux qui doublonnent la BD TOPO.
Un calque construit dessus s'appellerait « pistes OSM », pas « pistes absentes
de la BD TOPO », et n'apporterait pas grand-chose à l'œil.

Trois options :

| # | Où | Ce que ça donne |
|---|---|---|
| a | App : recalculer la différence localement (corridor + `st_difference`) | Duplique la logique du cœur, avec un `corridor_m` qui peut diverger. **À éviter** |
| b | `foretaccess` : faire renvoyer aussi les géométries hors corridor (`osm_hors_corridor`, `bdtopo_hors_corridor`) | La bonne place. Le calcul est déjà fait, il est jeté |
| c | App, en attendant : afficher la couche OSM brute, libellée honnêtement | Peu de valeur, mais pas mensonger |

**Recommandation : (b), et (c) seulement si (b) tarde.** Le recoupement coûte
104 s pour 3 122 × 544 tronçons (mesure `nemetonshiny` du 2026-08-12) : le
refaire côté app pour récupérer une géométrie que le cœur avait sous la main
serait absurde.

Et quoi qu'il arrive, la documentation du cœur est explicite et doit être
reprise dans l'UI : *« C'est un diagnostic, pas un résultat : un linéaire hors
corridor n'est pas une desserte manquante prouvée. »* Un tronçon hors corridor
peut être un décalage de saisie, une trace erronée ou un chemin non carrossable.

---

## 5. Checklist

**`nemetonshiny`**

- [ ] `DESS_GROUPE_OSM` et `DESS_GROUPE_DETECTEE` + ajout au vecteur `overlays`.
- [ ] Deux observers `leafletProxy()` calqués sur celui de « Réseau typé ».
- [ ] Les deux calques **éteints au départ** (`hideGroup()` à la création).
- [ ] Popup détection : `CLASSE`, `CLASSE_CONF`, `CLASSE_MOTIF`, `OSM_TAGS`.
- [ ] `run_desserte_osm()` renvoie `gpkg_path` ; `osm.rds` le porte ;
      `.load_cached_osm()` le ressort.
- [ ] `.load_cached_typage()` + persistance `typage.rds` : aujourd'hui le
      typage est perdu au rechargement du projet alors que son GeoPackage est
      sur le disque (seul des cinq à n'avoir aucun `.load_cached_*`).
- [ ] `export_desserte_geopackage()` fusionne `reseau_type`, `osm_track` et
      `desserte_detectee` dans le fichier téléchargé.
- [ ] Afficher le chemin du cache du projet dans l'onglet.
- [ ] i18n : libellés des deux calques, en-têtes de popup, rappel « proposition
      OSM, aucun téléversement ». FR + EN.
- [ ] Tests : `test-mod_desserte.R` — l'export contient bien les couches
      optionnelles quand les fichiers existent, et n'échoue pas quand ils
      manquent.

**`foretaccess`** (option (b) du §4)

- [ ] `comparer_desserte_osm()` renvoie aussi `osm_hors_corridor` et
      `bdtopo_hors_corridor` (`sf`), sans changer les trois tables existantes.
- [ ] Documenter que ce sont des **gisements à instruire**, pas des dessertes
      manquantes prouvées — le `@details` actuel dit déjà l'essentiel.
