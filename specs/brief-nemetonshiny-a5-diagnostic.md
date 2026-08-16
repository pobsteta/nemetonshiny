# Brief `nemetonshiny` — A5 : dire *pourquoi* l'indicateur est vide (spec 032, suite)

**Dépôt cible** : `nemetonshiny`. Suite du brief
`specs/032-regulation-thermique-albedo-lst/brief-nemetonshiny.md`, dont le
câblage est livré et fonctionne. Ce brief-ci ne remet rien en cause : il comble
le seul angle mort qui reste, la **lisibilité d'un A5 vide**.

**Origine** : diagnostic du 2026-08-16 sur les caches projet. La question posée
était « pourquoi `cache/layers/lst/` est-il vide ? ». La réponse — *hors
couverture Thermocity* — était correcte et documentée, mais **introuvable depuis
l'application**.

---

## 1. Le constat, mesuré

`theia_lst` (lignée Thermocity) ne couvre que quelques métropoles. Requête STAC
réelle sur les emprises des projets locaux :

| Projet | Emprise | Assets `theia_lst` | Cache |
|---|---|---|---|
| Fordead | Ardennes, rural | **0** | vide |
| ForetAccess | forêt privée, rural | **0** | vide |
| Dabo | Moselle | **8** (ECOSTRESS Strasbourg) | `lst_0505a849e182860a.tif` |

Le comportement est **conforme à la spec** : A5 est source-conditionné, il rend
`NA` hors couverture. Le brief initial l'avait même anticipé (CA « Projet rural :
A5 = NA proprement »).

Ce qui manque, c'est le chemin inverse : **partant de l'interface, rien ne permet
de savoir que c'est normal**. L'utilisateur voit un axe vide sur le radar, une
carte grise, « NA » dans le détail de famille — et, dans le panneau des sources,
un statut qui affirme toujours `lst_active` (« Rafraîchissement urbain :
activé »). Le message est donc, littéralement, *activé mais rien*.

## 2. Les quatre causes, aujourd'hui indistinguables

`build_lst_layer()` (`service_compute.R`) rend `NULL` dans les quatre cas :

| # | Cause | Détectée où, aujourd'hui | Visible par l'utilisateur |
|---|---|---|---|
| 1 | source non activée | `project_lst_enabled()` | oui (panneau sources) |
| 2 | clés `TLD_*` absentes | `theia_api_key_configured()` | oui (`lst_need_theia`) |
| 3 | **AOI hors couverture** (0 asset) | nulle part — `load_theia_source()` échoue sur une liste vide | **non** |
| 4 | erreur réseau / lecture | `tryCatch` → `cli_warn` en console | **non** |

Les cas 3 et 4 sortent par le même `NULL`, et un `cli_warn` en console n'est pas
une interface. Or 3 est **le cas normal** de la majorité des projets forestiers,
et 4 est une vraie panne : les confondre, c'est faire passer une panne pour une
normalité et réciproquement.

## 3. Ce que le cœur fournira

> **Statut : à livrer côté cœur.** Rien de ce qui suit n'est publié à la date de
> ce brief. Ne pas commencer le câblage avant que la version soit annoncée ici.
> Si tu veux commencer sans attendre, le point 3.2 est trivial à contourner
> (colonne absente = comportement actuel), le 3.1 ne l'est pas.

### 3.1 `theia_source_status(source_key, aoi, country = "FR")`

Interroge le catalogue **sans télécharger** et rend une cause nommée :

```r
theia_source_status("theia_lst", aoi)
#> $available   FALSE
#> $reason      "no_asset_over_aoi"
#> $n_assets    0
#> $collection  "thermocity-lst"
```

`reason` ∈ `{"ok", "unknown_source", "no_stac_collection", "no_credentials",
"no_asset_over_aoi", "error"}`. C'est une **clé stable**, pas un message : la
traduction est à toi, la cause est au cœur. L'app n'a ainsi ni à interpréter un
`conditionMessage()`, ni à réimplémenter une requête STAC.

Pourquoi côté cœur : la connaissance « quelle collection, quel service, quelle
authentification » vit dans `inst/datasources/<pays>.json` et `R/theia_stac.R`.
La dupliquer dans l'app la ferait diverger au premier changement de catalogue —
c'est exactement ce qui vient d'arriver à SUFOSAT (cf. §6).

### 3.2 Colonne `a5_status`

`indicateur_a5_rafraichissement()` rendra, à côté de `A5`, une colonne
`a5_status` ∈ `{"calculated", "skipped_no_lst"}` — **même contrat que
`r5_status`** (`R/indicators-deperissement.R`), qui existe depuis la spec 008.

## 4. Le travail côté app

### 4.1 Ne plus jeter le diagnostic

`service_r5.R:86` fait `out$r5_status <- NULL` : la seule colonne qui explique
pourquoi R5 est vide est supprimée juste avant d'arriver à l'interface. C'est le
geste à ne pas reproduire pour A5, et à défaire pour R5 (§7).

Garder la colonne de statut jusqu'à l'UI. Si elle gêne la détection de famille,
la préfixer (`.a5_status`) plutôt que la supprimer — `create_family_index()`
n'apparie que `indicateur_*` / `A[0-9]`.

### 4.2 Enregistrer la cause à l'acquisition

Dans `build_lst_layer()`, avant le `load_theia_source()` : appeler
`nemeton::theia_source_status("theia_lst", aoi_2154)`, et **stocker le `reason`**
(dans `layers$lst_status`, et en cache projet pour l'affichage hors calcul).
Court-circuiter le téléchargement quand `available` est `FALSE` — aujourd'hui on
paie une requête STAC, un échec et un `tryCatch` pour un résultat connu d'avance.

### 4.3 Un statut qui dit l'état réel, pas l'intention

`mod_sources_config.R` : le bloc de statut LST distingue actuellement
« activé » / « inactif » / « Theia non configuré ». Il lui manque le cas qui nous
occupe. Proposition à trois états, avec le compte de scènes :

- ✅ **`lst_status_ok`** — « Couverture disponible (%d scène·s sur l'emprise). »
- ℹ️ **`lst_status_no_coverage`** — « Activé, mais aucune scène LST sur cette
  emprise : Thermocity ne couvre que quelques métropoles. A5 restera vide — ce
  n'est pas une erreur. » (ton neutre : c'est une information, pas un
  avertissement)
- ⚠️ **`lst_status_error`** — « La source LST n'a pas pu être interrogée (%s). »

L'état actuel `lst_active` devient donc conditionnel au `reason`.

### 4.4 Le détail de famille

`mod_family.R` : `make_indicator_leaflet()` rend déjà une carte grise quand
`all(is.na(vals))` (l. 732). Y ajouter, **pour tout indicateur entièrement NA**,
un bandeau explicatif alimenté par le statut quand il existe, et un message
générique sinon (`indicator_all_na`). Le radar n'a besoin de rien : l'axe vide
est déjà géré.

### 4.5 Clés i18n (FR/EN, `utils_i18n.R`)

`lst_status_ok`, `lst_status_no_coverage`, `lst_status_error`,
`a5_skipped_no_lst`, `indicator_all_na`. Aucun littéral dans le code.

## 5. Critères d'acceptation

- [ ] `DESCRIPTION` exige la version cœur qui publie `theia_source_status()`.
- [ ] Projet **rural** (Fordead) : le panneau des sources affiche
      `lst_status_no_coverage`, **aucune** requête de téléchargement n'est
      lancée, et le détail de famille A explique l'axe vide.
- [ ] Projet **couvert** (Dabo) : `lst_status_ok` avec le nombre de scènes ; A5
      calculé comme aujourd'hui — aucune régression.
- [ ] Clés `TLD_*` absentes : `lst_need_theia` inchangé (cas 2 déjà traité).
- [ ] Source injoignable (réseau) : `lst_status_error`, **distinct** du cas
      « hors couverture ».
- [ ] `a5_status` (ou `.a5_status`) survit jusqu'à l'UI ; le radar et
      `create_family_index()` sont inchangés.
- [ ] Tous les textes passent par `i18n$t()`.

## 6. Pourquoi ce brief vaut plus qu'un message d'aide

Le même symptôme — indicateur `NA`, cache vide, aucun message — avait une **tout
autre cause** pour SUFOSAT : l'entrée `sufosat` de `inst/datasources/FR.json`
déclarait ses champs STAC hors de `access`, donc le résolveur refusait la
source ; `build_sufosat_layer()` attrapait l'erreur et rendait `NULL`. T3 valait
`NA` depuis toujours, y compris sur un projet où la source était explicitement
activée. Corrigé côté cœur en **v0.173.1**, avec un garde-fou de schéma.

Deux causes opposées, un seul symptôme, aucun signal : une panne de source est
aujourd'hui **indiscernable** d'une absence légitime de données. C'est ce que ce
brief ferme, et c'est pourquoi la cause doit être **nommée** (clé stable) plutôt
que rédigée au fil de l'eau.

## 7. Extension recommandée (même mécanique, hors périmètre de ce brief)

Les autres indicateurs source-conditionnés souffrent du même angle mort :

- **R5** dépérissement — `r5_status` existe **déjà** au cœur (`calculated`,
  `calculated_reconfort`, `skipped_no_fordead`, `skipped_no_reconfort`,
  `skipped_no_method`) et est jeté par l'app. Le gain est immédiat : il n'y a
  rien à livrer côté cœur, juste à cesser de supprimer la colonne.
- **T3** coupes rases — même besoin de statut que A5, sur `theia_source_status("sufosat", aoi)`.
- **R7** gel tardif — conditionné à SAFRAN/meteoland.

À traiter dans un brief dédié une fois A5 validé : le mécanisme d'affichage (§4.4)
est le même, seule la source du statut change.

## 8. Hors scope

- Chercher une source LST **nationale** (ECOSTRESS brut, Copernicus LST) pour
  étendre A5 hors métropoles — chantier cœur, déjà listé hors scope du brief
  initial.
- Carte de chaleur LST.
- Toute modification du calcul de A5 : le sens, les bornes et le `buffer_m`
  restent ceux de la spec 032.
