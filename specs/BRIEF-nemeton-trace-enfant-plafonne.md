# BRIEF `nemeton` — la sortie de l'enfant plafonné part dans `/dev/null`

> **Statut** : ouvert, 2026-09-03.
> **Dépôt concerné** : `nemeton` — `R/isolate.R`, et le wrapper `mapprod` de
> RECONFORT.
> **Nature** : deux demandes indépendantes. La première est un correctif de
> diagnosticabilité (petit, mécanique). La seconde est une investigation.
> **Contexte** : `nemetonshiny@0.143.15`, incident Couchey du 2026-09-03.

---

## 0. L'incident

Chaîne « Tout calculer » lancée le 2026-09-02 à 20:28 sur le projet Couchey
(zone 49, tuile T31TFN). L'étape RECONFORT a tourné **20 h 19** puis échoué :

```
"run_reconfort_dieback" failed in its capped child process (exit 1).
 — dans nemeton::run_memory_capped("run_reconfort_dieback", args = list(...))
```

**Ce n'est pas un dépassement mémoire**, et c'est vérifiable :

- `exit 1`. Le commentaire de `isolate.R:296` dit qu'un débordement de plafond
  arrive en `-9` ou `-15`, jamais en `1`.
- `memory.events` du cgroup IOTA² : `max 0`, `oom 0`, `oom_kill 0`. Pic relevé
  à 11,52 Go pour un plafond de 12 Go — il est passé près, il n'a pas touché.
- PSI du user slice montée à 0,53 puis redescendue. Aucune tuerie.

L'ingestion, elle, a été **parfaite** : 203/203 scènes (51 reprises du cache,
152 téléchargées), **zéro échec**.

## 1. Demande A — ne plus jeter la sortie de l'enfant

### Le problème

`R/isolate.R:283-284` :

```r
std <- if (quiet) NULL else ""
px <- processx::process$new(cmd$command, cmd$args, stdout = std, stderr = std)
```

`""` en `processx` signifie « hériter du parent ». L'intention est bonne : en
usage console, l'utilisateur voit le sous-processus travailler.

Mais côté app, le parent est un worker `future` multisession, lancé par
`parallelly` avec — relevé dans sa ligne de commande réelle sur ce poste —
`OUT=/dev/null`. **La sortie de l'enfant plafonné est donc écrite dans
`/dev/null`.** Le traceback Python d'IOTA², le message d'erreur R du wrapper,
tout ce qui expliquerait `exit 1` : perdu à la source.

C'est le cœur du problème : après 20 h de calcul, il ne restait **aucun**
message d'erreur. J'ai dû reconstituer le diagnostic depuis les fichiers
qu'IOTA² laisse derrière lui (`IOTA2_tasks_status.txt`, `logs.zip`,
l'inventaire de `classif/` et `final/`).

### Ce que je demande

Que `run_memory_capped()` puisse **capturer** la sortie dans un fichier plutôt
que de la laisser suivre le parent. Par exemple un argument `log_path = NULL` :

- `NULL` → comportement actuel, strictement inchangé (`""` / héritage) ;
- un chemin → `stdout = log_path, stderr = log_path` (processx accepte un
  chemin, et `"2>&1"` pour fusionner), le fichier étant conservé quel que soit
  le sort de l'enfant.

Et que le message d'échec construit par `.capped_failure_message()` **cite ce
chemin** quand il est défini, pour que l'utilisateur et l'app sachent où
regarder.

Je ne demande pas de changer le défaut. Un `log_path` optionnel me suffit :
l'app le posera dans le répertoire projet, à côté du NDJSON.

### Pourquoi je ne peux pas le contourner côté app

Trois pistes explorées, aucune ne tient :

- **`quiet = TRUE`** donne `std <- NULL`, c'est-à-dire *jeter* la sortie —
  l'inverse de ce qu'on veut.
- **Rediriger le worker `future`** : `OUT=/dev/null` est posé par `parallelly`
  au démarrage du worker, pas par nous ; on ne contrôle pas cet argument.
- **`sink()` dans le worker** ne capture pas la sortie d'un *sous-processus*.

Il n'y a pas d'angle applicatif. C'est pour ça que je viens vous voir.

### Ce que l'app a fait de son côté, en attendant

`nemetonshiny@0.143.15` **archive** le NDJSON de progression au lieu de
l'effacer sur les chemins d'échec (FAST, FORDEAD, RECONFORT) :
`…/data/reconfort_progress.ndjson.failed-<horodatage>`, cinq archives gardées
au maximum. Ça sauve la trace *structurée* (une ligne par item, une par phase).

Ça ne remplace pas le message d'erreur : le NDJSON dit **jusqu'où** on est
allé, jamais **pourquoi** ça s'est arrêté.

## 2. Demande B — IOTA² s'arrête après `classification`, sans `final/`

### Ce qui est sur le disque

Dans `…/output_zone_49/results/iota2_results_classif_labels-z49-S2_2025/` :

`IOTA2_tasks_status.txt` liste **huit tâches, toutes `done`** :

```
preprocessing_T31TFN · common_mask_T31TFN · validity_raster_T31TFN
tiles_envelopes · region_generation · vector_form_T31TFN
merge_samples_T31TFN · classification_T31TFN_model_1_seed_0_2
```

Et la classification a bien produit ses rasters :

```
classif/Classif_T31TFN_model_1_seed_0_SUBREGION_2.tif
classif/PROBAMAP_T31TFN_model_1_seed_0_SUBREGION_2.tif
classif/T31TFN_model_1_confidence_seed_0_SUBREGION_2.tif
```

**Mais `final/` est vide** — seul un `TMP/` créé à 16:29 et jamais rempli.
Aucune mosaïque, aucun produit final. Les logs (`logs.zip`, écrit à 16:46)
s'arrêtent proprement à 16:45 sur le découpage des échantillons, **sans une
seule ligne d'erreur** : `grep -ri "error\|traceback\|exception"` sur tous les
`.out` ne remonte rien.

L'échec R est survenu vers 16:47–16:48, après l'écriture de `logs.zip`.

### La question

Le suffixe `SUBREGION_2` sur les trois rasters attire l'œil : la liste des
tâches ne contient qu'**une** tâche de classification, `…_seed_0_2`. S'il
manque les autres sous-régions, ou l'étape de fusion qui les assemble, IOTA²
s'arrêterait « avec succès » sur un travail partiel — et le wrapper R
chercherait ensuite dans `final/` un produit qui n'a jamais été demandé.

Je ne sais pas si c'est ça. C'est une hypothèse tirée des noms de fichiers,
pas une conclusion, et je n'ai pas le message d'erreur qui trancherait — voir
la demande A.

Ce que je peux affirmer : **la chaîne IOTA² se termine sans produire de
classification finale, et sans le dire dans ses logs.**

## 3. Ce qu'il ne faut PAS conclure

- **Que RECONFORT est cassé.** L'ingestion des 203 scènes a été parfaite, et
  c'était la première fois — les quatre tentatives précédentes plafonnaient
  entre 82 et 109 items. La v0.143.12 (adoption des zones) et la v0.143.10
  (enfant plafonné) tiennent toutes les deux.
- **Que c'est la mémoire.** Trois mesures indépendantes disent le contraire
  (section 0).
- **Qu'il faut relancer l'ingestion.** Les 203 scènes sont en cache avec leurs
  marqueurs `.done` ; un relancement repart directement en `mapprod`.

## 4. Une remarque sur la composition des plafonds

Relevé en passant, sans demande attachée. Pendant `mapprod`, il y avait **deux**
scopes plafonnés frères, chacun à 12 Go :

| Scope | Posé par | Usage observé |
|---|---|---|
| `nemeton-run_reconfort_dieback-…` | l'app (v0.143.10) | 0,97 Go |
| `run-r459be71b…` (`conda run … Iota2.py`) | le cœur, `.reconfort_cap_memory()` | 7,7 – 11,5 Go |

Le pire cas cumulé est donc **24 Go**, plus le scope de la session (~8 Go),
sur une machine de 31 Go. Les plafonds bornent chacun un débordement, mais
rien ne borne leur **somme**. Ça tient aujourd'hui parce que le R parent reste
sous 1 Go pendant que Python travaille — c'est une propriété de la séquence,
pas une garantie.

Je ne demande rien : sur ce poste ça passe, et je n'ai pas de proposition
propre. Je le signale parce que c'est le genre de chose qu'on préfère avoir lue
avant de la découvrir.

## 5. Pour reproduire

Projet Couchey, `20260828_140251_hwuy`, zone 49, tuile T31TFN, S2 2025. Le
cache d'ingestion est complet, donc un `run_reconfort_dieback()` repart
directement sur `mapprod` — l'aller-retour est de quelques minutes, pas de
20 heures.
