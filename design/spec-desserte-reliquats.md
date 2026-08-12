# Spec — desserte : les quatre reliquats côté app

> **Statut** : ouvert, 2026-08-12. Repo : `nemetonshiny`.
> **Amont** : trois des quatre points dépendent de `BRIEF-foretaccess-desserte-consolide.md`, en cours côté cœur. Le §4 est indépendant et faisable tout de suite.
> **Contexte** : reliquats de la série `v0.121.1` → `v0.121.15` (onglet Desserte, détection, classement).

---

## Ordonnancement

| § | Sujet | Dépend du cœur ? | Faisable maintenant |
|---|---|---|---|
| 1 | Plancher `foretaccess` | ✅ oui | non |
| 2 | Rebrancher la détection | ✅ oui (§B du brief) | non |
| 3 | `stations` + `ndvi` pour le classement | partiellement | **oui, mais inutile avant §2** |
| 4 | Garde-fou mémoire ALSroads périmé | ❌ non | **oui** |

**Faire le §4 en premier** : c'est le seul qui ne dépende de personne. Les §1 et §2 s'enchaînent mécaniquement dès la release cœur. Le §3 est le plus gros, et il ne rapporte rien tant que la détection rend 0.

---

## 1. Bumper le plancher `foretaccess`

### Situation

`DESCRIPTION` déclare aujourd'hui `foretaccess (>= 2.0.1)`. Les correctifs demandés au cœur (§A et §B du brief consolidé) sortiront dans une version ultérieure dont l'app dépendra **strictement** : sans le §A, `verifier_integrite_desserte()` peut rendre un bilan vide qui se lit « aucune infraction ».

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

## 2. Rebrancher la détection sur une calibration locale

### Situation

`run_desserte_detection()` appelle `foretaccess::detecter_desserte()` sans `specs`, donc avec `specs_desserte_calibrees()` — des bornes calibrées **sur un autre massif**. Résultat mesuré sur ForetAccess : **0 détection en 729 s**, avec l'avertissement `dessertR` « Bornes mal adaptées à la donnée ».

Nous avons déjà corrigé notre part (`v0.121.15`) : le service prend le MNT LiDAR 0,5 m et non le RGE ALTI 5 m. Chiffré, ce correctif fait passer les canaux retenus de **0/7 à 5/7** (AUC max 0,56 → 0,77). Le signal est donc là ; c'est l'accès à la calibration qui manque.

### À faire — selon la réponse du cœur

Le brief propose trois pistes ; l'implémentation diffère peu.

- **Si `specs = "auto"`** (piste préférée) : passer l'argument, rien d'autre. Le surcoût attendu est celui de `dsr_layers_dtm()` + `dsr_calibrer_specs()`, mesuré à **66 s** sur ForetAccess, à ajouter aux 729 s.
- **Si un convertisseur est exposé** : calibrer nous-mêmes puis convertir. Il faut alors construire la pile de canaux (`dsr_grille_reference()` puis `dsr_layers_dtm()`), ce qui ajoute une dépendance directe à `dessertR` dans notre code — aujourd'hui nous ne l'appelons que via `dsr_classer()`.
- **Si `specs = NULL` suffit** (question §B.6 du brief) : une ligne, aucun surcoût. C'est le cas le plus probable et le plus simple ; ne pas construire plus avant de connaître la réponse.

Dans tous les cas, **remonter l'avertissement de calibration à l'utilisateur**. Il part aujourd'hui dans la console du worker `future` et personne ne le voit. Un « 0 route détectée » sans ce contexte se lit comme un constat d'absence alors que c'est un défaut de bornes.

### Critères d'acceptation

- Sur ForetAccess, la détection rend un résultat **et** l'interface indique si les bornes ont été calibrées localement ou héritées.
- Si elle rend toujours 0, l'interface dit **pourquoi** : bornes inadaptées, ou absence effective.
- Le surcoût de la calibration est mesuré et documenté dans l'en-tête du service, comme les autres.

### Ce qu'on ne sait pas

**Il est possible qu'il n'y ait rien à détecter** sur ces 31 ha de forêt privée. Ne pas traiter un 0 persistant comme un échec du correctif sans avoir cherché une AOI où l'on sait qu'il existe des pistes non cartographiées — Reconfort (30 parcelles, 554 ha, 25 dalles LiDAR) est le meilleur candidat non encore essayé.

---

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

`.acc_estimate_alsroads_memory()` et son garde-fou pré-vol (`R/service_accessibility.R`, ~lignes 279–310) estiment la mémoire de la **dérivation MNT par ALSroads** — un chemin que `foretaccess` a retiré en 1.27.0 au profit de dessertR (« `ALSroads` et `lidR` ne sont plus utilisés du tout »).

Ils dégradent proprement — `requireNamespace("lidR")` absent rend `NULL` — et ne bloquent rien. Mais leur calibrage n'a jamais été revérifié contre le profil mémoire de dessertR, et les commentaires décrivent une mécanique qui n'existe plus.

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
- Projets d'essai : ForetAccess `20260717_101641_wsfi` (30 parcelles / 31 ha, 4 dalles), Reconfort `20260701_204501_ltcp` (30 parcelles / 554 ha, 25 dalles, **non encore essayé**), Dabo `20260801_130303_xpdk` (4 parcelles / 774 ha, 27 dalles)
