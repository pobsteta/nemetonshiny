# BRIEF `nemeton` — ERA5 : émettre un événement par MOIS, et savoir reprendre

**Dépôt cible** : `/home/pascal/dev/nemeton` (cœur). **Session dédiée requise** —
`nemetonshiny` ne peut rien corriger ici : tout le chemin est côté cœur. L'app a
déjà posé sa moitié du contrat (cf. §5), il ne reste que le cœur.

**Origine** : diagnostic du 2026-08-26 sur le projet **Fordead**. L'utilisateur
signale « le moteur microclimf de reGénération bloque ». Il ne bloque pas : il
télécharge, en silence, pendant une heure quarante par année.

---

## 1. Mesures

Processus plafonné (`run_memory_capped`), PID 20136, observé pendant 3 h 04 :

| Signal | Valeur | Lecture |
|--------|--------|---------|
| `%CPU` | 32,5 % en continu | travaille, jamais en attente morte |
| `VmRSS` | 1,06 Go | très loin du plafond |
| `/proc/<pid>/fd` | `.nc` ERA5 successifs | I/O réseau + netCDF, pas un calcul figé |
| `engine_status.json` | figé à `microclimf_canicule`, `year=2022`, `i=1`, `n=1` | **une seule écriture en 1 h 40** |

Déroulé reconstruit à partir des `mtime` du cache
`cache/regeneration/microclimf/` :

| Phase | Fenêtre | Durée |
|-------|---------|-------|
| PAI (LiDAR HD) | 18:02 → 18:54 | 52 min, `pai.tif` 14 Mo |
| ERA5 **2020** (année moyenne) | 19:02 → 20:38 | **1 h 36**, 12 requêtes mensuelles |
| ERA5 **2022** (année canicule) | 20:39 → … | mois 2/12 à 20:58 |

**~8 min par mois**, 12 mois par année, 2 années : **~3 h 15 de téléchargement
CDS pour un run**, dont l'app ne rapporte rien.

## 2. Chaîne fautive

```
.rsen_moyenne_categorie(annees, emit=, category=)      R/regen_engines.R:457
  emit(list(current = "regen_expo:era5", year=, i=k, n=length(annees)))   <-- UN emit
  .rsen_traiter_annee(annees[[k]], ...)
    .rsen_forcage_era5(lon, lat, annee, cache_dir)     R/regen_engines.R:347
      req <- mcera5::build_era5_request(..., by_month = TRUE)   ==> LISTE de 12
      .rsen_era5_with_retry(function() mcera5::request_era5(req, out_path=))
                                                        <-- 12 telechargements, 0 emit
```

`emit` est appelé **une fois par année**, avant la descente. Avec une seule année
par catégorie (le cas nominal : `year_moyenne` + `year_canicule`), cela donne
`i=1`, `n=1` — un compteur exact, qui n'apprend rien, suivi de 1 h 36 de silence.

`mcera5::request_era5()` boucle en interne (`for (req in 1:length(request))`) et
n'accepte aucun callback : le cœur ne peut pas s'insérer sans découper la boucle
lui-même.

## 3. Ce qui est demandé — B1 : un événement par mois

Émettre `regen_expo:era5_mois` avant chaque requête mensuelle :

```r
list(current = "regen_expo:era5_mois",
     category = <"moyenne" | "canicule">,
     year     = <annee>,
     mois_i   = <k>,        # 1..12
     mois_n   = <length(req)>)
```

`category` doit redescendre jusqu'à `.rsen_forcage_era5()` (aujourd'hui elle
s'arrête à `.rsen_moyenne_categorie()`), sinon l'app ne saura pas si elle affiche
l'année moyenne ou l'année canicule. Le plus simple est de faire passer `emit` et
`category` en arguments de `.rsen_traiter_annee()` puis de `.rsen_forcage_era5()`.

Pas de compteur d'années dans cet événement : `.rsen_forcage_era5()` ne connaît
pas le rang de l'année, et « 2022 — mois 3/12 » dit déjà tout ce que « (1/1) »
prétendait dire. L'app gère l'absence (cf. §5).

## 4. Ce qui vient avec — B2 : la reprise après interruption (bug réel)

En découpant la boucle, on tombe sur un piège qu'il faut corriger dans le même
geste, sous peine d'aggraver la situation.

`mcera5::request_era5()` **refuse** de re-télécharger : si le `.zip` cible existe
déjà et que `overwrite = FALSE` (le défaut), il `stop()` — et sur une *série*
(`length(request) > 1`), le message est
`"Filename already exists within requested out_path in request N of request series."`

Conséquence aujourd'hui : **un run tué au mois 7 rend l'année entière
irrécupérable**. Au run suivant, `.rsen_era5_combined()` ne trouve pas de combiné
(les mensuels s'appellent `era5_2020_2020_7.nc`, ils ne matchent pas `_2020\.nc$`),
donc le cœur redemande les 12 mois, et `request_era5()` échoue immédiatement sur
le mois 1 dont le `.zip` est encore là. `.rsen_era5_with_retry()` brûle alors ses
3 tentatives sur une erreur qui ne peut pas guérir. Les `.zip` **ne sont pas
supprimés** après extraction (vérifié : `era5_2020_2020_1.zip` et `.nc`
cohabitent dans le cache Fordead).

Forme proposée — la boucle mois par mois règle B1 et B2 d'un coup :

```r
# `combine_netcdf()` est EXPORTE par mcera5 0.4.0 (verifie).
.rsen_era5_telecharger <- function(req, cache_dir, annee, emit = NULL,
                                   category = NA) {
  n <- length(req)
  nc <- function(k) file.path(cache_dir, sub("\\.zip$", ".nc", req[[k]]$target))
  for (k in seq_len(n)) {
    if (!is.null(emit)) emit(list(current = "regen_expo:era5_mois",
                                  category = category, year = annee,
                                  mois_i = k, mois_n = n))
    if (file.exists(nc(k))) next            # B2 : mois deja acquis -> on passe
    .rsen_era5_with_retry(function()
      mcera5::request_era5(req[k], out_path = cache_dir, overwrite = TRUE))
  }
  mcera5::combine_netcdf(
    filenames     = vapply(seq_len(n), nc, ""),
    combined_name = file.path(cache_dir, sprintf("era5_%d_%d.nc", annee, annee)))
}
```

Deux points vérifiés sur `mcera5` 0.4.0, à ne pas re-deviner :

- `req[k]` (liste de longueur 1) et non `req[[k]]` : `request_era5()` attend une
  **liste** de requêtes et itère dessus.
- le nom du combiné. `request_era5()` le calcule par `shared_substring()` sur les
  12 cibles puis coupe le `_` final : avec `outfile_name = "era5_2020"`, les
  mensuels sont `era5_2020_2020_<mois>.nc`, le préfixe commun `era5_2020_2020_`,
  donc le combiné est **`era5_2020_2020.nc`** (double année). C'est exactement ce
  que `.rsen_era5_combined()` cherche via `_%d\.nc$`, et ce qu'on observe dans le
  cache. Reproduire ce nom **à l'identique**, sinon le cache ne se relit jamais —
  c'est déjà la panne qu'a corrigée le commentaire de `.rsen_era5_combined()`.

## 5. Contrat côté app — déjà posé, rien à faire

`nemetonshiny` mappe déjà l'événement (`R/service_regeneration.R`, `on_prog`) :

```r
"regen_expo:era5_mois" =
  .regen_write_phase(out_dir, paste0("microclimf_", p$category %||% "moyenne"),
                     list(year = p$year, mois_i = p$mois_i, mois_n = p$mois_n)),
```

et l'affiche (`.regen_micro_lbl()`) sous la forme
**« Microclimat — étés canicule 2022 — mois 3/12 »**. Chaque morceau est optionnel :
tant que le cœur n'émet pas `regen_expo:era5_mois`, la branche est morte et rien
ne change. Livré app **v0.141.0** (`nemetonshiny@b7aff412`).

L'app a aussi cessé de jeter un `engine_status.json` vieux de plus de 2 min : elle
garde la dernière phase connue et date son silence (« — dernier signe de vie il y
a 27 min »). Ce garde-fou **reste utile après B1** — il couvre les autres phases
muettes — mais il ne remplace pas le compteur : dater un silence n'est pas
mesurer un avancement.

## 6. Critères d'acceptation

- **CA-1** — un run microclimf sur 2 années émet **24** `regen_expo:era5_mois`,
  avec `category` correcte (`moyenne` puis `canicule`) et `mois_i` de 1 à 12.
- **CA-2** — le combiné produit par la boucle porte **le même nom** que celui de
  `request_era5(combine = TRUE)` (`era5_<annee>_<annee>.nc`), et
  `.rsen_era5_combined()` le retrouve au run suivant : **aucun re-téléchargement**
  sur un cache complet. Test possible sans réseau : poser 12 `.nc` factices et
  vérifier le nom du combiné + le court-circuit.
- **CA-3** — un cache **partiel** (mois 1 à 6 présents, `.zip` compris) ne
  re-télécharge que les mois 7 à 12 et n'échoue plus sur
  `"Filename already exists"`. C'est le bug B2 ; sans test dessus il reviendra.
- **CA-4** — `emit = NULL` reste un no-op : le cœur doit tourner hors app.
- **CA-5** — les évènements `regen_expo:era5` par année sont **conservés** (l'app
  s'en sert encore, et ntfy en fait un message par année, cf.
  `regen_ntfy_era5`) ; `regen_expo:era5_mois` s'ajoute, ne remplace pas.

## 7. Hors périmètre

- La **durée** des requêtes CDS (~8 min/mois) : c'est la file d'attente de
  Copernicus, pas le cœur. Ce brief rend l'attente lisible, il ne la raccourcit
  pas.
- Le choix ERA5 vs SAFRAN : `cfg$forcing` ne pilote que BILJOU
  (`load_biljou_forcing(source=)`) ; le chemin microclimf passe **toujours** par
  ERA5/mcera5. Ce n'est pas une anomalie — vérifié à l'occasion de ce diagnostic
  parce que le cache d'un projet en `forcing = "safran"` est plein de `era5_*.nc`,
  ce qui se lit comme un bug et n'en est pas un. À documenter éventuellement,
  pas à changer.

## 8. NEWS / PLAN

Entrée `Fixed` (B2 est une correction de bug) + `Changed` (B1), et une ligne au
journal `PLAN.md` mentionnant le brief et l'app consommatrice
(`nemetonshiny@b7aff412`, v0.141.0).
