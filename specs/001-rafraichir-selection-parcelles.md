# Spec 001-app — Rafraîchir l'onglet Sélection quand les parcelles changent

**Statut** : proposée, 2026-08-20 · **Portée** : `nemetonshiny` seul, aucune
action cœur · **Version cible** : 0.130.7

## 1. Le problème

Depuis la v0.130.6, la coche « Supprimer les parcelles hors forêt publique »
retire réellement des parcelles du projet : de `$parcels`, de `$tenements`, et
de `parcels.gpkg`. `app_state$current_project$parcels` est mis à jour dans la
foulée.

**L'onglet Sélection continue pourtant d'afficher les parcelles supprimées.**

La raison est que cet onglet ne lit pas `app_state$current_project`. Sa carte
(`mod_map`) tient son propre état — `rv$selected_ids` et la couche de parcelles
dessinée — alimenté par un signal unique, `app_state$restore_project`, que
`mod_home` pose **au chargement d'un projet** (`mod_home.R:543`) :

```r
app_state$restore_project <- list(
  commune_code = , department_code = , parcels = , geometry = ,
  selected_ids = project$parcels$id,
  timestamp = Sys.time()      # force la réactivité
)
```

`mod_map` compare ce `timestamp` au dernier traité (`rv$last_restore_timestamp`)
pour ne pas rejouer deux fois la même restauration.

L'utilisateur voit donc, après une purge, une Sélection qui ment : des parcelles
y figurent, sélectionnées, alors qu'elles ne sont plus dans le projet.

## 2. Pourquoi ne pas simplement reposter `restore_project`

C'est la solution évidente, et elle a deux défauts qui la disqualifient.

**Elle réveille `mod_search`.** Ce module écoute le même signal
(`mod_search.R:395`) et peut relancer une requête vers `geo.api.gouv.fr` pour
retrouver la commune. Reposter le signal depuis Carte UGF déclencherait un
appel réseau pour un rafraîchissement purement local.

**Elle exige des données que Carte UGF n'a pas.** `commune_code` et
`department_code` sont dérivés des parcelles par une logique propre à
`mod_home`. Les reconstruire dans `mod_ug` dupliquerait ce code ; les oublier
produirait un signal incomplet, donc une restauration partielle.

`restore_project` répond à la question « charge ce projet ». Notre question est
plus étroite : « les parcelles ont changé, redessine-les ». Un signal qui dit
davantage que nécessaire finit par déclencher davantage que nécessaire.

## 3. La proposition — un signal étroit

Ajouter `app_state$parcels_changed`, posé par tout module qui modifie
`projet$parcels`, écouté par `mod_map` seul.

```r
app_state$parcels_changed <- list(
  parcels   = projet$parcels,   # sf des parcelles APRÈS modification
  timestamp = Sys.time()        # force la réactivité, comme restore_project
)
```

### Ce que `mod_map` en fait

1. Redessine la couche de parcelles à partir du `sf` reçu.
2. **Restreint** `rv$selected_ids` aux parcelles encore présentes
   (`intersect()`), sans jamais en ajouter : le signal annonce une
   modification, pas une nouvelle sélection.
3. Ne touche ni au fond de carte, ni au zoom, ni à la commune : rien de tout
   cela n'a changé.

### Ce qu'il n'en fait pas

- Aucun appel réseau.
- Aucune écriture disque : la persistance est faite par l'émetteur.
- Aucun effet si `parcels` est absent ou vide — un signal mal formé ne doit pas
  vider la carte.

### Garde d'idempotence

Même mécanisme que `restore_project` : `mod_map` mémorise le dernier
`timestamp` traité et ignore un signal déjà vu. Sans cela, toute invalidation
de `app_state` rejouerait le redessin.

## 4. Émetteurs

| Module | Quand |
|---|---|
| `mod_ug` | après une purge ONF qui a retiré des parcelles (`with_parcels = TRUE`) |

Un seul aujourd'hui. Le signal est conçu pour que d'autres puissent s'y
brancher — toute action future qui retire ou ajoute des parcelles — sans que
`mod_map` ait à les connaître.

## 5. Critères d'acceptation

- [ ] Après une purge ayant retiré des parcelles, l'onglet Sélection ne les
      affiche plus.
- [ ] Les parcelles conservées **restent sélectionnées** : on ne repart pas
      d'une sélection vide.
- [ ] `mod_search` n'émet **aucune** requête réseau du fait de ce signal.
- [ ] Sans purge (coche décochée), aucun signal n'est posé et rien ne bouge.
- [ ] Un signal au `timestamp` déjà traité est ignoré.
- [ ] Un signal sans `parcels`, ou avec 0 ligne, laisse la carte intacte.

## 6. Hors scope

- Rafraîchir la Sélection après d'**autres** modifications du projet (ajout de
  parcelles depuis un import, édition manuelle) : le signal les accueillera,
  mais aucun émetteur n'est ajouté ici.
- Synchroniser en sens inverse (Sélection → Carte UGF) : la sélection
  cartographique sert à **construire** un projet, pas à le refléter.
