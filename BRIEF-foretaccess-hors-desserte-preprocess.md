# BRIEF cœur `foretaccess` — `preprocess()` rejette `hors_desserte`, que `acquire_desserte()` produit désormais par défaut

> Hand-off depuis la session de dev `nemetonshiny`.
> **À traiter dans une session de dev dédiée sur `/home/pascal/dev/foretaccess`** (un repo = une session).
> Repo concerné : `pobsteta/foretaccess`.
> Versions observées : `foretaccess 2.0.0`, `nemetonshiny 0.121.3`, `terra 1.9.34`.
> **L'onglet Accessibilité de l'app est entièrement bloqué** — tous chemins, tous projets. Aucun correctif app ne rétablit le bénéfice visé ; voir §7.

## 1. Symptôme

« Lancer l'analyse » dans l'onglet Accessibilité échoue immédiatement :

```
Échec du prétraitement (MNT / desserte / forêt).
 — Valeur de classe inconnue dans la desserte : "hors_desserte".
 i Valeurs attendues : "route", "piste", "dfci", and "reseau_public".
```

Le préfixe est le libellé i18n de l'app ; le reste vient de `foretaccess`.

## 2. La contradiction interne

`acquire_desserte()` a basculé **`garder_hors_desserte = TRUE` par défaut le 2026-07-30**, délibérément. Sa propre documentation (`acquire_desserte.Rd`) justifie et cadre le choix :

> Conserver les tronçons `hors_desserte` (CL_SVAC = 0) dans la sortie ? **Défaut `TRUE` depuis le 2026-07-30.** Les retirer **coupe le réseau** : mesuré sur l'AOI oracle, leur suppression faisait passer les infractions de connectivité de 15 à 21 à 1600 m de buffer. […] Ils sont donc conservés pour la **topologie**, et exclus du **débardage** par `preprocess()`, qui ne connaît que les classes de `.classes_desserte()`.

Mais `preprocess()` ne les **exclut** pas — il **rejette** :

```
preprocess()  ->  valider_entrees()  ->  .valider_desserte()  ->  cli::cli_abort()
```

```r
.valider_desserte <- function(desserte) {
  ...
  attendues <- .classes_desserte()          # "route" "piste" "dfci" "reseau_public"
  inconnues <- setdiff(unique(valeurs), attendues)
  if (length(inconnues)) {
    cli::cli_abort(c("Valeur{?s} de {.field classe} inconnue{?s} dans la desserte : {.val {inconnues}}.",
                     i = "Valeurs attendues : {.val {attendues}}."))
  }
}
```

Les deux moitiés du changement 2.0.0 sont donc incompatibles : **aucun appelant ne peut consommer le nouveau défaut**. `valider_entrees()` étant exportée, le blocage vaut pour tout code utilisateur, pas seulement pour l'app.

## 3. Reproduction

Sur le cache Accessibilité du projet DABO (`nemetonshiny`, AOI Vosges ~3 000 ha) :

```
desserte brute (cache)              n = 1032   ABORT: classe inconnue "hors_desserte"
  -> sans hors_desserte             n =  712   OK
desserte_corrigee/desserte_corrigee n =  710   ABORT
desserte_corrigee/desserte_origine  n = 1032   ABORT
```

Répartition des classes sur la couche brute :

| classe | n |
|---|---:|
| piste | 434 |
| `hors_desserte` | **320** |
| route | 228 |
| reseau_public | 50 |

La couche corrigée au LiDAR (`qualifier_desserte()`) est touchée aussi : **les deux chemins, NDP 0 et NDP 1, échouent**.

## 4. Le piège : relâcher le validateur ne suffit PAS

À première vue le correctif est trivial — `.rasteriser_desserte()` fait

```r
v$code_classe <- match(as.character(desserte$classe), classes)
```

et `match()` rend `NA` pour une classe inconnue. On pourrait donc croire que `hors_desserte` devient simplement absent du raster, exactement comme la doc le promet, et qu'il suffit d'assouplir `.valider_desserte()`.

**C'est faux, et le mode d'échec est silencieux.**

`terra::rasterize(field = "code_classe", fun = "max", touches = TRUE)` **grave la sentinelle entière `-2147483648`** dans les cellules atteintes par une géométrie dont le champ vaut `NA`, au lieu de les laisser vides. Dans une cellule partagée par un `hors_desserte` et une vraie desserte, la sentinelle **écrase la classe valide**.

Mesuré sur les données réelles de DABO, grille 5 m, en comparant « validateur relâché » contre « filtrage amont » :

| couche | cellules de desserte réelle | **cellules écrasées** | % |
|---|---:|---:|---:|
| `desserte.gpkg` (brute) | 24 259 | **440** | 1,8 % |
| `desserte_corrigee.gpkg` (NDP 1) | 19 957 | **310** | 1,6 % |

Les 440 cellules écrasées correspondent **exactement** au nombre de cellules partagées entre `hors_desserte` et les classes valides, mesuré indépendamment : la perte est donc **systématique**, pas marginale.

Et surtout, ces cellules sont par construction les **nœuds de jonction** — c'est précisément là qu'un sentier rejoint une route. Couper le réseau à ses jonctions est le pire endroit possible : cela fabrique exactement les « composantes orphelines fictives et surfaces déclarées inaccessibles à tort » que le NEWS 2.0.0 dit vouloir éliminer. Un correctif naïf transformerait donc une erreur bruyante en amputation silencieuse du réseau, **dans le sens inverse de l'intention de la bascule**.

> Note de méthode : ce piège est facile à manquer en test. Une première mesure de notre côté a conclu à « 0 cellule écrasée » parce qu'elle testait `is.na()` — or terra rend la sentinelle `-2147483648` comme une **valeur**, pas comme `NA`. Tout test de non-régression sur ce point doit contrôler la sentinelle explicitement, sinon il sera vacant.

## 5. Correctif demandé

**Filtrer, ne pas seulement tolérer.**

1. `.valider_desserte()` accepte `hors_desserte` comme valeur **connue** (au même titre que les 4 classes de débardage), et continue de rejeter tout le reste. Idéalement via un helper dédié — p. ex. `.classes_desserte_connues() <- c(.classes_desserte(), "hors_desserte")` — pour que la liste « classes de débardage » et la liste « classes acceptées en entrée » restent deux notions distinctes et nommées.
2. `.rasteriser_desserte()` **retire explicitement** les tronçons hors débardage **avant** `terra::rasterize()` :
   ```r
   desserte <- desserte[as.character(desserte$classe) %in% classes, ]
   ```
   C'est ce filtrage amont, et lui seul, qui rend la promesse de la doc vraie. Ne pas se reposer sur le `NA` de `match()`.
3. Vérifier les autres consommateurs de la colonne `classe` pour la même hypothèse implicite — `.masque_classe()` compare à un code issu de `match()` et paraît sain, mais `places_depot()`, `qualifier_desserte()` et `flag_dfci()` méritent une relecture sous cet angle.
4. **Documenter le point de consommation** dans l'aide de `acquire_desserte()`. Voir §5bis : le consommateur existe et le bénéfice est réel, mais rien dans la doc ne dit *quelle fonction* exploite ces tronçons, ce qui rend le défaut `TRUE` difficile à défendre pour un lecteur qui n'utilise que `preprocess()`.

## 5bis. Le défaut `TRUE` est justifié — vérifié, ne pas le retirer

La tentation naturelle, face à un paramètre qui ne fait que casser `preprocess()`, est de revenir à `garder_hors_desserte = FALSE`. **Ce serait une régression.**

`verifier_integrite_desserte()` (spec 025) est le consommateur réel : il n'appelle pas `.valider_desserte()`, ne filtre pas par classe, et reçoit donc le réseau complet. Mesuré sur la desserte DABO :

| | avec `hors_desserte` (n = 1032) | sans (n = 712) |
|---|---:|---:|
| **infractions** | **11** | **14** |
| longueur en infraction | 4 107 m | 5 048 m |
| composants | 15 | 15 |
| composants orphelins | 8 | 6 |

Conserver les tronçons **réduit les infractions de 14 à 11** et le linéaire fautif de 941 m, reproduisant la direction annoncée par la doc (15 → 21 sur l'AOI oracle). Le défaut `TRUE` a donc un bénéfice mesurable.

Nuance à ne pas masquer : les **composants orphelins vont dans l'autre sens** (8 avec, 6 sans), à nombre de composants inchangé. Le gain n'est pas uniforme sur toutes les métriques — c'est la métrique de tête qui s'améliore. Cela mériterait peut-être un regard, mais ne remet pas en cause le choix.

**Le paramètre n'est donc pas en cause : c'est `preprocess()` qui doit filtrer.** Revenir à `FALSE` réglerait le blocage en réintroduisant exactement ce que la bascule du 2026-07-30 corrigeait.

Précision utile pour arbitrer : `nemetonshiny` n'appelle **aucune** fonction d'intégrité (ni `verifier_integrite_desserte`, ni `desserte_reseau*`). Du seul point de vue de l'app, le `TRUE` n'apporte rien et ne coûte qu'un blocage — d'où l'impression trompeuse d'un paramètre inutile. C'est un angle mort de l'app, pas un défaut du cœur.

## 6. Tests attendus

- Non-régression du rejet : une classe réellement inconnue (`"totalement_inconnue"`) doit toujours faire échouer `valider_entrees()`.
- Acceptation : une desserte contenant `hors_desserte` passe `valider_entrees()` sans erreur.
- **Anti-écrasement (le test qui compte)** : deux tronçons croisant la même cellule, l'un `route`, l'autre `hors_desserte` → la cellule vaut le code de `route`. **Contrôler la sentinelle `-2147483648`, pas seulement `is.na()`** (cf. note de méthode §4), sinon le test est vacant.
- Invariance : sur une AOI de référence, le raster de desserte produit avec `garder_hors_desserte = TRUE` est **identique** à celui produit avec `FALSE`. C'est la formulation la plus forte : elle vérifie d'un coup le filtrage et l'absence d'effet de bord.
- Bout en bout : `acquire_desserte()` (défauts) → `preprocess()` s'exécute sans erreur.
- Non-régression intégrité : `verifier_integrite_desserte()` continue de recevoir le réseau complet et rend le même décompte d'infractions qu'avant le correctif (le filtrage doit vivre dans `preprocess()`, pas dans `acquire_desserte()`).

## 7. Côté app — pourquoi aucun contournement ne suffit

`nemetonshiny` appelle `foretaccess::preprocess(mnt = mnt, desserte = desserte, foret = foret_mask)` (`R/service_accessibility.R:610`) après `acquire_desserte(aoi_ext, crs = epsg, cache_dir = acq_dir)` (`:368`), **sans** passer `garder_hors_desserte` — donc conformément au contrat documenté. Il n'y a rien à corriger dans sa logique.

Deux contournements existent, tous deux insatisfaisants :

- passer `garder_hors_desserte = FALSE` ;
- filtrer la desserte juste avant `preprocess()`.

Ils sont équivalents du point de vue de `preprocess()` et débloquent l'app, mais **aucun ne restaure le bénéfice topologique** visé par la bascule : seul le cœur peut conserver ces tronçons pour la connectivité *tout en* les excluant du débardage. Un éventuel filtre côté app serait donc un contournement assumé, à retirer dès la correction cœur livrée.

## 8. Points annexes relevés

- **Plancher de version non tenu** : `nemetonshiny/DESCRIPTION` déclare `foretaccess (>= 1.20.0)` alors que 2.0.0 est installé. Avec `Remotes: pobsteta/foretaccess@*release`, l'app tire automatiquement la majeure sans que le plancher ne l'ait jamais validée. À bumper côté app une fois ce correctif publié.
- **Caches à purger** : le NEWS 2.0.0 annonce « Tout cache antérieur est invalide, et tout chiffre publié avec est suspect » (WFS perdant des tronçons sur grandes emprises, RGE ALTI par WMS banni). Les caches Accessibilité antérieurs au 2026-07-31 sont à invalider indépendamment de ce bug. Le sidecar de provenance (spec 027) devrait s'en charger, mais cela n'a pas été vérifié ici.

## 9. Références

- Cache de repro : `~/.local/share/nemeton/projects/20260801_130303_xpdk/cache/accessibility/`
  - `emprise_250m/layers/desserte/desserte.gpkg` — 1032 tronçons, 320 `hors_desserte`
  - `desserte_corrigee.gpkg` — couches `desserte_corrigee` (710) et `desserte_origine` (1032)
- Appel app : `nemetonshiny/R/service_accessibility.R:368` (acquisition) et `:610` (prétraitement)
- Fonctions cœur : `.valider_desserte()`, `valider_entrees()` (exportée), `.rasteriser_desserte()`, `.masque_classe()`, `.classes_desserte()`
