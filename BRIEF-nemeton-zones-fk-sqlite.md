# BRIEF cœur nemeton — build_project_monitoring_zones() : FOREIGN KEY constraint failed (backend SQLite)

> Hand-off depuis la session de dev `nemetonshiny`. **À traiter dans une
> session de dev dédiée sur `/home/pascal/dev/nemeton`** (règle 12 : pas
> de modif cœur depuis la session app). Repo concerné :
> `pobsteta/nemeton` (cœur métier). Aucune modif `nemetonshiny` requise
> par ce fix.

## Contexte

Onglet **Suivi sanitaire** → bouton **« Générer les zones de suivi »**.
Côté app (`nemetonshiny`), le clic appelle simplement :

``` r

nemeton::build_project_monitoring_zones(
  con,
  project_name = project$metadata$name %||% project$id,
  project_uuid = project$id,
  ugf          = ugf_sf,
  bdforet      = bdforet_sf
  # strata = c("tot","feu","res","mix"), replace = TRUE (defaults)
)
```

L’app ne fait qu’ouvrir la connexion
([`nemeton::db_connect`](https://pobsteta.github.io/nemeton/reference/db_connect.html)),
migrer
([`nemeton::db_migrate`](https://pobsteta.github.io/nemeton/reference/db_migrate.html))
puis déléguer. Le schéma `monitoring_zone` + ses tables enfants et leurs
contraintes FK sont **définis par le cœur** (`db_migrate()`), pas par
l’app (les seules migrations SQL de `nemetonshiny/inst/sql/` concernent
le schéma PostGIS projet/parcelles/UG, sans rapport avec la base
monitoring).

## Symptôme

Sur **Windows en local** (donc backend **SQLite** — aucune var
`NEMETON_DB_*` / `POSTGRESQL_ADDON_*` définie, fallback
`<projet>/data/monitoring.sqlite`), la génération des zones échoue avec
:

    Échec de la génération des zones : FOREIGN KEY constraint failed

`FOREIGN KEY constraint failed` est l’erreur runtime **RSQLite**.
[`nemeton::db_connect()`](https://pobsteta.github.io/nemeton/reference/db_connect.html)
active `PRAGMA foreign_keys = ON` sur SQLite, donc les contraintes FK
sont réellement vérifiées (contrairement au défaut SQLite qui les
ignore).

## Reproduction / cause confirmée

Confirmé par l’utilisateur :

- **Sur une base vierge** (après suppression de `monitoring.sqlite`), la
  génération réussit.
- **Au re-build** (re-clic sur un projet dont la base contient déjà des
  lignes enfants : alertes / observations / liaison placettes), l’erreur
  apparaît.

→ `build_project_monitoring_zones()` est en upsert **`replace = TRUE`**
: il **supprime** les zones existantes du projet (`project_uuid`) puis
les recrée. Sur SQLite avec FK actives, le **DELETE des zones parentes
est bloqué** par les lignes enfants qui référencent encore
`monitoring_zone.id` **sans `ON DELETE CASCADE`**.

(Scénario alternatif à écarter pendant l’investigation : insertion d’une
ligne enfant référençant un `zone_id` pas encore committé/visible —
moins probable vu que la 1re génération réussit, mais à vérifier dans
l’ordre des INSERT.)

Sur PostgreSQL le problème ne se manifeste pas de la même façon (zones
fraîches côté serveur, sémantique transactionnelle/cascade différente),
d’où l’effet « ne casse qu’en local Windows ».

## Pistes de fix (au choix de l’auteur cœur)

1.  **`ON DELETE CASCADE`** sur les FK des tables enfants pointant
    `monitoring_zone.id` (alerte, observation, liaison placettes…), via
    une nouvelle migration de schéma monitoring **+ migration de bascule
    pour les bases existantes** (SQLite ne supporte pas
    `ALTER ... ADD CONSTRAINT` → recréer la table avec la nouvelle FK et
    recopier les données, pattern
    `CREATE new / INSERT SELECT / DROP / RENAME`).
2.  **Suppression explicite des lignes enfants avant les zones**, dans
    la même transaction que l’upsert
    (`DELETE FROM alert WHERE zone_id IN (…)`, etc., puis
    `DELETE FROM monitoring_zone`).
3.  **`PRAGMA defer_foreign_keys = ON`** (SQLite) le temps de la
    transaction DELETE/INSERT de l’upsert, pour que les FK soient
    vérifiées au COMMIT et non à chaque statement (utile si l’ordre
    interne est correct au COMMIT mais transitoirement invalide).

Recommandation : option 1 (cascade) si la suppression d’une zone doit
logiquement emporter ses alertes/observations ; sinon option 2 si on
veut un nettoyage explicite et contrôlé.

## Tests attendus (cœur)

- Cas SQLite : **re-build** d’un projet dont les zones portent déjà des
  alertes + observations + placettes →
  `build_project_monitoring_zones()` réussit et les lignes enfants
  orphelines sont gérées conformément à l’option retenue (supprimées en
  cascade, ou re-rattachées).
- Vérifier l’idempotence : 2 appels consécutifs donnent le même nombre
  de zones (`_tot/_feu/_res/_mix`) sans erreur FK.
- Vérifier que `PRAGMA foreign_keys` reste `ON` après l’upsert (pas
  d’effet de bord si option 3 retenue).

## À mettre à jour côté `nemeton`

- `NEWS.md` (entrée datée, type `fix:`).
- `PLAN.md` (journal + commit cœur).
- Bump cycle dev / release patch cœur selon la convention `nemeton`.

## Côté `nemetonshiny` — rien à faire pour le fond

Le wiring app (`R/mod_monitoring.R`, observer du bouton `register` /
`register_inline`) est correct. Une fois la release cœur publiée,
`Remotes: pobsteta/nemeton@*release` la tirera automatiquement (pas de
bump `Remotes:`). Bumper le plancher `Imports: nemeton (>= X.Y.Z)` côté
app **uniquement** si l’app doit exiger cette version corrigée comme
minimum strict.

Option cosmétique app envisageable (non bloquante, hors de ce brief) :
message d’erreur plus explicite quand `conditionMessage` contient «
FOREIGN KEY » — non retenue pour l’instant.
