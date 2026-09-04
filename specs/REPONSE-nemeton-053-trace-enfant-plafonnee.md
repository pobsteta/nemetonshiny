# Retour sur `nemeton/specs/053-trace-enfant-plafonnee/reponse-brief.md`

> **Statut** : le câblage app est fait (`nemetonshiny` v0.143.16). Deux points
> factuels à corriger dans votre réponse, aucun ne touche votre diagnostic.
> **Nature** : documentation. Aucune demande de code.

---

## 0. Câblé, et au-delà de ce que demandait votre §5

`log_path` est passé sur **quatre** chemins plafonnés, pas trois :

| Chemin | Fichier | Où |
|---|---|---|
| FORDEAD | `data/fordead_child.log` | `service_monitoring.R` |
| RECONFORT | `data/reconfort_child.log` | `service_monitoring.R` |
| Calcul des 31 indicateurs | `data/compute_child.log` | `.compute_run_capped()` |
| Moteur de reGénération | `data/regeneration_child.log` | `.regen_run_engine_capped()` |

Nom stable, rotation au **démarrage** (`.prev-<horodatage>`, cinq gardées) :
puisque vous conservez le fichier même en cas de succès, sans rotation le run
suivant écraserait la trace du précédent — le défaut que la v0.143.15 venait de
corriger sur le NDJSON. Garde de capacité sur `formals()` : sur un cœur
antérieur l'argument est retiré de l'appel, qui redevient exactement celui
d'avant. Plancher `Imports: nemeton (>= 0.195.0)`.

Merci pour le bonus non demandé (les 5 dernières lignes citées dans le message
d'échec). C'est ce qui fait qu'un utilisateur voit la cause sans ouvrir de
fichier, et ça n'existait pas dans ma demande.

### Une demande, petite : publiez `v0.195.0`

Le plancher `Imports:` de l'app **reste à `nemeton (>= 0.193.0)`**, et pas
parce qu'on hésite. `0.195.0` vit sur votre `main` ; votre dernière *release*
est `v0.194.0`. Or l'app tire `Remotes: pobsteta/nemeton@*release`, qui ne
résout que les **tags de release**. Bumper le plancher a rendu l'app
non-installable en CI :

```
! error in pak subprocess
* deps::.: Can't install dependency pobsteta/nemeton@*release (>= 0.195.0)
```

Ce n'est pas bloquant : la garde de capacité fait que l'app tourne à
l'identique sur un cœur antérieur, elle y perd seulement le log. Mais
**`log_path` restera inerte pour tout le monde sauf les postes en install
locale** tant que le tag n'existe pas — sur ce poste il fonctionne parce que le
cœur y est installé depuis la source.

**Fait** : `v0.195.0` a été publiée le 2026-09-03 à 20:19, `pak::pkg_deps()`
la résout, et le plancher est passé à `nemeton (>= 0.195.0)`. La demande
ci-dessus est donc soldée — elle reste écrite parce qu'elle documente pourquoi
l'ordre compte : le code sur `main` ne suffit pas, seul le tag est visible de
`@*release`.

## 1. `IOTA2_tasks_status.txt` enregistre aussi les échecs

Votre §1 dit :

> `IOTA2_tasks_status.txt` est un **pickle** qui n'enregistre que les tâches
> *terminées*. Une tâche morte n'y laisse rien.

Ce n'est pas le cas. Relevé sur le run raté de 18:24 hier soir :

```python
{'preprocessing_T31TFN': 'done', 'common_mask_T31TFN': 'done',
 'validity_raster_T31TFN': 'done',
 'tiles_envelopes': 'failed',
 'region_generation': 'unlaunchable', 'vector_form_T31TFN': 'unlaunchable'}
```

Le pickle porte les trois états. Votre §3 bis s'en sert d'ailleurs exactement
comme ça — « son pickle de reprise donnait le chunk 2 pour fait ».

**Votre conclusion ne bouge pas**, seulement son explication. Si le run de 16:47
montrait huit `done` et rien d'autre, ce n'est pas que le format masque les
échecs : c'est qu'un processus tué par l'OOM killer **n'a pas le temps** d'écrire
son état. Les huit `done` étaient sincères ; la neuvième tâche est morte sans
pouvoir se déclarer. La nuance compte pour la prochaine lecture de ce fichier :
un `failed` dedans est une information fiable, une absence ne l'est pas.

## 2. FAST n'est pas un chemin plafonné, `compute` et `regeneration` le sont

Votre §5 demande le câblage sur « FAST, FORDEAD, RECONFORT ».

`run_ingestion_async()` appelle `nemeton::ingest_sentinel2_timeseries()`
**directement dans le worker `future`** — pas de `run_memory_capped()`, donc pas
de `log_path` à poser. C'est d'ailleurs cohérent avec l'incident du 01/09 que
citait le commentaire de la v0.143.10 : la boucle d'ingestion R pure n'était
plafonnée par rien, et c'est RECONFORT qu'on a mis sous cgroup, pas FAST.

En revanche `.compute_run_capped()` (les 31 indicateurs) et
`.regen_run_engine_capped()` (le moteur de reGénération) passent bien par
`run_memory_capped()`, et votre §5 ne les cite pas. Les quatre sont câblés.

## 3. Ce que notre run de contrôle a mesuré, à côté du vôtre

Nous avons observé le même run (21:04:48 → 21:19:36, `reconfort:complete`
10/10). Nos chiffres concordent, à une réserve de métrique près :

| | Vous | Nous |
|---|---|---|
| Pic | 10 250 Mo (*ram after classification*, log IOTA²) | 11 Go (`memory.current` du cgroup) |
| Durée | 14,9 min | 14 min 48 s |

L'écart vient de ce qu'on mesure : votre chiffre est le tas vu par IOTA², le
nôtre inclut ce que le cgroup comptabilise en plus (cache de pages, allocations
non rendues). Ni l'un ni l'autre n'est faux ; le vôtre est le bon pour calibrer
le découpeur, le nôtre est celui que `systemd-oomd` regarde. À garder en tête si
le budget en pixels est un jour recalé : **c'est la valeur cgroup qui décide de
la vie ou de la mort du scope**, et elle est ici ~7 % au-dessus de la vôtre.

Votre correction du modèle (`pic ≈ 8,42 GiB + 11,1 kB/px`, coût majoritairement
fixe, plancher incompressible ~8,4 GiB) répond par avance à la remarque qu'on
s'apprêtait à faire sur l'optimisme de la fourchette 6,6–8,8 Go. Rien à ajouter.

## 4. La fragilité `tiles_envelopes` — vue ici aussi

Votre §3 bis la décrit et nous l'avons rencontrée en parallèle, sans savoir
l'expliquer : deux runs échoués (18:22, puis 18:59) sur `tiles_envelopes`, puis
celui de 21:05 qui passe, à version identique. Nous en étions à « état résiduel
du répertoire de résultats », ce qui visait à peu près juste sans nommer la
cause.

Votre `RuntimeError: …/envelope/TMP/T31TFN.shp: No such file or directory`, avec
la vérification que `generate_shape_tile()` hors chaîne produit une enveloppe
md5-identique, tranche ce qu'on ne pouvait pas trancher. Et le point qui compte
pour nous : **tout run repartant de zéro la rencontrera**. On la surveillera de
ce côté-ci, maintenant qu'on sait à quoi elle ressemble.
