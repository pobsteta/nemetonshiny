# Spec — desserte : les quatre reliquats côté app

> **Statut** : ouvert. Rédigé le 2026-08-12, **§2 réécrit le 2026-08-13** sur mesures. Repo : `nemetonshiny`.
> **Amont** : `foretaccess 2.1.0` a livré le §B du brief consolidé (specs de détection) ; le §A (déclaration de `dessertR`) reste bloqué en amont par l'archivage de `rlas` sur le CRAN.
> **Ce qui a changé** : le §2 n'attend plus le cœur. Ce qu'il attendait est arrivé et **n'a rien changé au résultat** — il est devenu une décision, pas une implémentation.
> **Contexte** : reliquats de la série `v0.121.1` → `v0.121.15` (onglet Desserte, détection, classement).

---

## Ordonnancement

| § | Sujet | Dépend du cœur ? | Faisable maintenant |
|---|---|---|---|
| 1 | Plancher `foretaccess` | ✅ oui (§A bloqué par `rlas`) | non |
| 2 | **Détection : décider de son sort** | ❌ non (le cœur a livré) | **oui — c'est un arbitrage** |
| 3 | `stations` + `ndvi` pour le classement | partiellement | **oui, mais inutile avant §2** |
| 4 | Garde-fou mémoire ALSroads périmé | ❌ non | **oui** |

**Le §2 a changé de nature** (révision du 2026-08-13). Il n'attend plus le cœur :
`foretaccess 2.1.0` a livré ce qu'il demandait, et **cela n'a rien changé**. Ce
n'est plus une tâche d'implémentation, c'est une décision à prendre sur une
fonctionnalité qui coûte une heure et ne rend rien.

Le §4 reste faisable immédiatement et ne dépend de personne. Le §3 ne rapporte
rien tant que la détection rend 0 — et le §2 dit maintenant qu'il ne faut pas
compter dessus.

---

## 1. Bumper le plancher `foretaccess`

### Situation

`DESCRIPTION` déclare aujourd'hui `foretaccess (>= 2.0.1)`, et **ce plancher reste correct** — l'app ne consomme encore aucune API de 2.1.0.

*Mise à jour du 2026-08-13* : des deux correctifs attendus, **un seul est sorti**.

- **§B — livré** dans `foretaccess 2.1.0` : `detecter_desserte(specs = )` accepte quatre formes. Ne devient un motif de bump que si le §2 décide de s'en servir, ce qui n'est pas acquis.
- **§A — NON livré, et pour une raison durable.** `dessertR` n'est toujours pas déclaré en `Suggests` parce qu'il dépend de `rlas`, **archivé sur le CRAN** : la déclaration faisait échouer les quatre jobs de CI du cœur. Le prédicat public `dessertR_disponible()` n'existe donc pas, et nos gardes locaux `requireNamespace("dessertR")` restent nécessaires. Ce n'est pas un retard, c'est bloqué en amont tant que `rlas` ne revient pas.

### À faire

1. `DESCRIPTION` : `foretaccess (>= X.Y.Z)`, la version qui porte les correctifs.
2. Si le cœur expose un prédicat public (`dessertR_disponible()`, demandé au §A.3), remplacer notre garde local par lui — dans `.desserte_integrite()` et `run_desserte_detection()`, qui font tous deux `requireNamespace("dessertR")`.
3. Vérifier que `Remotes: pobsteta/foretaccess@*release` tire bien la nouvelle release (cf. CLAUDE.md, `@*release` suit le tag le plus élevé).

### Critères d'acceptation

- `devtools::check()` passe avec le nouveau plancher.
- Les tests de `test-service_desserte.R` restent verts, **en particulier** celui qui vérifie que `.desserte_integrite()` rend `NULL` plutôt qu'un verdict vide.
- Sur un poste sans `dessertR`, le message affiché reste actionnable.

### Piège

Ne pas bumper « pour suivre ». Le plancher est le **minimum que le code app exige** (CLAUDE.md) — il ne monte que quand l'app consomme réellement une API nouvelle ou dépend d'un correctif.

---

## 2. Détection : quatre variables d'ajustement épuisées, une décision à prendre

> **Révision du 2026-08-13.** La version précédente de ce §2 postulait que la
> détection rendait 0 **parce que** ses bornes venaient d'un autre massif, et que
> la correction attendue du cœur lèverait le blocage. **Cette prémisse est
> réfutée par la mesure.** Le texte qui suit la remplace ; l'ancien est conservé
> dans l'historique git, il n'a pas à être suivi.

### Ce que le cœur a livré, et ce que ça n'a pas changé

`foretaccess 2.1.0` a livré exactement ce que le brief demandait :
`detecter_desserte(specs = )` accepte quatre formes, dont `"auto"` (calibration
sur place) et la sortie plate de `dsr_calibrer_specs()`, et
`specs_depuis_calibration()` expose la conversion. Vérifié : nos 7 canaux
calibrés sont correctement promus en `geomorpho`.

**Le comptage n'a pas bougé d'un tronçon.**

### La mesure — Reconfort, 554 ha, 2 h 52 de calcul

Reconfort a été choisi parce qu'il lève l'objection que ForêtAccess ne pouvait
pas lever : sur 31 ha de forêt privée, « il n'y a peut-être rien à trouver » est
une explication recevable. Ici, 554 ha de forêt communale, **105,7 km de BD TOPO
en référence**, et une signature de micro-relief franchement discriminante :

| MNT | canaux retenus | meilleure AUC |
|---|---:|---:|
| ForêtAccess (31 ha) | 5 / 7 | 0,77 |
| **Reconfort (554 ha)** | **7 / 7**, tous stables | **0,763** (rugosité) |

Sept configurations, un seul fragment de 55 m au total :

| configuration | tronçons | durée | pic RSS |
|---|---:|---:|---:|
| bornes figées, géomorphologie | 0 | 436 s | — |
| calibration locale, géomorphologie | 0 | 410 s | 5,65 Go |
| calibration locale, seuil 0,3 | **1** *(55 m)* | — | — |
| calibration locale, seuils 0,4 / 0,5 / 0,6 | 0 | 1 380 s | — |
| calibration locale, `buffer_ref = 0` | 0 | 203 s | — |
| **+ canal de surface, seuil 0,6** | 0 | 4 018 s | 8,66 Go |
| **+ canal de surface, seuil 0,4** | 0 | 3 357 s | **8,75 Go** |

### Ce que ces mesures écartent

- **Les bornes.** Calibrées localement sur 7 canaux retenus : même résultat.
- **Le seuil.** À 0,3 — *sous* la plage 0,4–0,8 que la spec 026 prescrit — un
  seul linéaire de plus de 30 m survit. L'indice ne décroche pas à un seuil, il
  est uniformément trop bas.
- **Le masquage de la référence.** `buffer_ref = 0` rend 0. Et le corridor de
  15 m ne masque que **8 % de l'emprise** (115 ha sur 1 440) — pas les 22 %
  qu'un calcul sans recouvrement laisserait croire.
- **Le canal de surface.** Il multiplie le coût par 10 et n'ajoute **aucune**
  détection. À noter : le run ForêtAccess de 729 s le passait déjà (4 dalles,
  `avec_lidar` par défaut) et rendait 0 lui aussi.

### Ce qui reste, et qu'on ne peut pas trancher d'ici

Deux hypothèses tiennent encore, et **aucune n'est instrumentable côté app** :

1. **Il n'y a réellement rien à détecter** sur ces massifs — plausible sur une
   forêt communale bien desservie, où ce qui existe est déjà cartographié.
2. **La chaîne a un défaut en amont** de ses réglages, qui empêche l'indice
   d'atteindre des valeurs exploitables quelles que soient les entrées.

Les distinguer demande d'inspecter le raster `p_desserte` lui-même, que
`detecter_desserte()` n'expose pas. Reconstruire `sigma_geo` nous-mêmes
reviendrait à réimplémenter la logique du cœur — exclu (règle 1).

### La décision à prendre

La question n'est plus « comment calibrer » mais **« cette fonctionnalité doit-elle
rester exposée en l'état »**. Elle demande **67 minutes et 8,7 Go** pour un
résultat vide qu'aucun message n'explique. Trois options :

- **(a) Retirer le bouton** de l'onglet Desserte tant que la chaîne n'a pas
  produit un résultat non vide sur au moins une AOI. Le plus honnête vis-à-vis
  de l'utilisateur, et réversible.
- **(b) Le garder en l'assortissant d'un avertissement mesuré** — durée, mémoire,
  et le fait qu'aucune détection n'a été obtenue sur trois massifs. Laisse la
  porte ouverte à un massif où ça marcherait.
- **(c) Passer un brief au cœur** avec ces mesures, en demandant l'exposition du
  raster `p_desserte` (ou de son quantile maximal) pour pouvoir distinguer les
  deux hypothèses ci-dessus. C'est le seul chemin qui *résout* au lieu de
  contourner.

(a) et (c) ne s'excluent pas — c'est probablement la combinaison à retenir.

### Deux défauts app, indépendants du 0, à corriger dans tous les cas

**L'emprise n'est pas découpée.** `run_desserte_detection()` fait
`terra::rast(mnt_path)` sur la mosaïque entière et n'utilise pas l'argument
`emprise` de `detecter_desserte()`. Sur Reconfort : **2 500 ha traités pour
1 440 utiles**. Le NEWS du cœur est explicite — « dégrader la résolution ne fait
pas gagner du temps, cela fait perdre le signal ; la seule variable d'ajustement
est l'emprise ».

**Le garde-fou mémoire modélise la mauvaise grille.** Il annonce **1,96 Go** et
laisse passer un run mesuré à **8,75 Go**. Deux erreurs qui se composent et se
compensent partiellement, ce qui explique qu'elles soient passées inaperçues :

| | garde-fou | réalité |
|---|---|---|
| grille | AOI à 5 m — 0,22 M cellules | mosaïque à 1 m — **25 M cellules** |
| coût par cellule | structures du solveur glouton (`NodeState`, listes de voisins) | canaux raster |

`.desserte_memory_estimate()` est calibré sur le moteur `reseau_desserte`, pas
sur `detecter_desserte`. Le commentaire qui justifie sa réutilisation — « son
estimation est pilotée par la grille, donc elle vaut aussi ici » — est faux.

C'est le même motif que le §4 : **un garde-fou dont le calibrage ne correspond
plus à ce qu'il garde est pire qu'absent.** Celui-ci a laissé passer un job de
8,7 Go sur une machine où 4 Go étaient déjà pris.

### Critères d'acceptation

- La décision (a/b/c) est prise et **écrite**, pas laissée implicite.
- L'emprise est découpée ou `emprise` est passé ; le gain est mesuré.
- Le garde-fou est recalibré sur `detecter_desserte` — ou retiré en disant
  pourquoi. Ses constantes viennent d'une mesure datée, comme celles de
  `.desserte_memory_estimate()`.
- Si la détection reste exposée, un 0 s'accompagne du **pourquoi** : bornes,
  seuil, ou absence effective.

### Données de référence

Reconfort `20260701_204501_ltcp` : 30 parcelles / 554 ha, bbox 3,28 × 3,51 km,
25 dalles LiDAR (MNT 0,5 m, mosaïque 5 × 5 km), référence BD TOPO 435 tronçons /
105,7 km. Bancs de mesure et artefacts conservés hors dépôt (scratchpad de
session) — les chiffres ci-dessus sont reproductibles avec
`dsr_calibrer_specs()` puis `detecter_desserte_balayage()`.

## 3. `stations` et `ndvi` : faire monter la confiance du classement

### Situation

`dsr_classer()` est câblé (`v0.121.14`) et fonctionne, mais nous ne lui passons que `reference` et `parcellaire`. Mesuré sur 151 tronçons de ForetAccess : **33 % de confiance**, 77 tronçons sur 151 en `indetermine`.

Les critères manquants sont déclarés **inconnus** par dessertR, pas supposés — c'est honnête, mais peu utile. Le brief dessertR §2 liste ce qui rend le classement discriminant :

| argument | source | ce qu'il débloque |
|---|---|---|
| `stations` | `dsr_measure()` par tronçon, colonne `troncon` | critère **fossés** |
| `ndvi` | `dsr_ndvi()` sur une ortho IRC via `dsr_ortho_ign()` | sépare **route / piste**, et **conditionne le pare-feu** |
| `tpi` | `dsr_slrm(mnt, fenetres_m = 50)` | **crête → pare-feu**, uniquement avec `ndvi` |

Note : `tpi` n'apparaît pas dans les arguments nommés de `dsr_classer()` en 1.3.0 — la fonction a un `...`, à vérifier avant de l'y passer.

### À faire

1. **`stations`** — appeler `dsr_measure(trace, mnt, ...)` sur les linéaires détectés, avec une colonne `troncon` identifiant chaque tronçon. Le MNT est déjà résolu par le service (LiDAR 0,5 m). C'est le plus simple des trois : aucune donnée nouvelle à acquérir.
2. **`ndvi`** — acquérir une ortho IRC par `dsr_ortho_ign()`, puis `dsr_ndvi()`. Nouvelle acquisition réseau, donc nouveau poste de cache et nouveau risque de latence. **Mesurer avant de câbler.**
3. Afficher le gain : la confiance moyenne doit apparaître avant/après pour que l'utilisateur voie ce que l'ajout apporte.

### Critères d'acceptation

- La confiance moyenne dépasse nettement 33 % sur ForetAccess, et la part d'`indetermine` baisse.
- Le coût de chaque ajout est mesuré séparément et documenté.
- Sans ortho IRC disponible, le classement continue de fonctionner en mode dégradé — sans erreur, et en le disant.

### Priorité

**Basse tant que le §2 n'est pas résolu.** Raffiner le classement d'un ensemble vide n'apporte rien. La séquence est : détection productive, puis classement discriminant.

---

## 4. Garde-fou mémoire calibré sur un chemin disparu

### Situation

*Les fonctions s'appellent `.lidar_memory_estimate()` et `.lidar_memory_check()`* (`R/service_accessibility.R`) — la première rédaction de cette spec les nommait `.acc_estimate_alsroads_memory()`, qui n'existe pas.

Elles estiment la mémoire de la **dérivation MNT par ALSroads**, un chemin que `foretaccess` a retiré en 1.27.0. Vérifié le 2026-08-13 : `.mnt_alsroads` **n'existe plus** dans le namespace de `foretaccess 2.1.0`, et ni `lidR` ni `ALSroads` ne figurent dans ses `Imports`/`Suggests`.

**Le point n'est pas que le garde-fou soit mort — c'est que le risque a changé de nature.** Le NEWS 1.27.0 est explicite : « plus de dérivation automatique d'un MNT 1 m depuis les points sol […] **Fournir un MNT à 1 m ou plus fin**, sans quoi les largeurs sortent `NA` ». Un MNT trop grossier ne provoque donc plus un OOM mais une **dégradation silencieuse** — des largeurs `NA` que rien ne signale. Le garde-fou parle d'un danger disparu et se tait sur celui qui l'a remplacé.

Notre code fournit bien un MNT ≤ 1 m (LiDAR 0,5 m, sinon acquisition à 1 m), donc le cas ne se présente pas aujourd'hui. Il se présentera le jour où le WMS rendra plus grossier.

Signalé sans correction depuis `v0.121.6`.

### À faire

Trois options, par ordre de coût croissant. **Le choix demande une mesure, pas une opinion.**

1. **Mesurer d'abord.** Instrumenter `run_desserte_lidar_correction()` sur deux AOI de tailles différentes (ForetAccess 31 ha / 4 dalles, Dabo 774 ha / 27 dalles) et relever le pic RSS réel. C'est ce qui décide entre les options 2 et 3.
2. **Si le profil dessertR est proche** : garder le garde-fou, recalibrer les constantes, réécrire les commentaires qui parlent d'ALSroads.
3. **Si le profil est très différent, ou si dessertR borne lui-même** : retirer le garde-fou et la dépendance résiduelle à `lidR`, en disant pourquoi.

### Critères d'acceptation

- Plus aucune mention d'ALSroads dans le code actif de `service_accessibility.R` (les mentions historiques en commentaire de NEWS restent).
- Si un garde-fou subsiste, ses constantes sont issues d'une mesure datée, comme celles de `.desserte_memory_estimate()`.
- La correction LiDAR de la desserte fonctionne toujours sur ForetAccess et Dabo.

### Pourquoi le faire même s'il ne bloque rien

Un garde-fou dont le calibrage ne correspond plus à ce qu'il garde est **pire qu'absent** : il donne l'illusion d'une protection. Le même motif a produit deux vrais défauts dans cette série — le garde `lidR`/`ALSroads` qui refusait la correction LiDAR sur toute machine sans ces paquets (corrigé en `v0.121.6`), et le bilan d'intégrité vide qui se lisait « aucune infraction » (`v0.121.11`).

---

## Références

- Brief cœur : `BRIEF-foretaccess-desserte-consolide.md` (§A, §B, §C)
- Brief dessertR : `BRIEF-dessertR-classement-osm-et-cout-terrassement.md` (§2 pour les entrées du classement)
- Code : `R/service_desserte.R` (`run_desserte_detection`, `.desserte_integrite`), `R/service_accessibility.R` (`.acc_estimate_alsroads_memory`), `R/mod_desserte.R` (panneaux)
- Projets d'essai : ForetAccess `20260717_101641_wsfi` (30 parcelles / 31 ha, 4 dalles), Reconfort `20260701_204501_ltcp` (30 parcelles / 554 ha, 25 dalles — **essayé le 2026-08-13, cf. §2**), Dabo `20260801_130303_xpdk` (4 parcelles / 774 ha, 27 dalles)
