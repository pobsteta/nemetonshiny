# BRIEF `nemeton` — entrée `PLAN.md` pour `nemetonshiny v0.122.0`

> **Un seul dépôt à ouvrir : `pobsteta/nemeton`.** Rien à modifier dans le code
> cœur — cette entrée est de la tenue de journal.
> Rédigé depuis la session `nemetonshiny`, qui n'a pas le droit d'écrire dans le
> repo cœur (règle 12 du `CLAUDE.md` app).

---

## Ce qu'il faut porter

Une entrée datée au journal du `PLAN.md`, dans le chantier **E-accessibilité /
desserte**. Elle clôt un défaut, elle n'ouvre ni ne coche d'épaississement.

**Aucune case à cocher.** Ce n'est pas une livraison de sous-chantier : c'est un
correctif de régression sur une fonctionnalité déjà livrée. Ne rien clore.

---

## Texte proposé pour le journal

> **2026-08-13 — `nemetonshiny@b1a7d41a` (v0.122.0, cycle dev `0.122.0.9000`)**
> — *Desserte corrigée : l'invariant BD TOPO rétabli, complément OSM câblé.*
>
> L'app passait `retirer_disparues = TRUE` à
> `foretaccess::qualifier_desserte()` — un opt-in que le cœur laisse à `FALSE`.
> La correction LiDAR **supprimait donc des tronçons déclarés** : 280 sur 373
> sur ForêtAccess (84 % du linéaire, **une `route` sur deux**), 322 sur 1 032
> sur Dabo. Cette couche amputée remplaçait la BD TOPO en entrée de
> `preprocess()`, donc de **tous les moteurs**, dès que « utiliser la desserte
> corrigée » était cochée : les surfaces hors desserte étaient surestimées
> d'autant.
>
> La cause est une erreur d'interprétation côté app. `dsr_etat()` définit
> `hors_route` comme « **les deux conductivités faibles** », c'est-à-dire
> *aucun signal* — et avertit que l'état « n'est réellement interprétable que
> le long d'un tracé retenu par le pathfinder ». Une plateforme routière laisse
> une empreinte dans le terrain pendant des décennies : l'absence de signal
> désigne un échec de mesure bien plus souvent qu'une route effacée. L'app en
> avait fait un verdict d'existence.
>
> **Règle posée, non contournable** : la desserte corrigée conserve
> l'intégralité de la BD TOPO, s'enrichit d'OSM, qualifie l'ensemble et rend le
> tout. La qualification **renseigne** (état, largeur, géométrie recalée) ; elle
> ne **décide** pas de l'existence. Un garde-fou refuse la correction si la
> sortie compte moins de tronçons que l'entrée.
>
> Complément OSM (`.desserte_complement_osm()`) conforme au contrat du cœur
> (« Source *complémentaire* de la BD TOPO, jamais substitutive ») : seule la
> portion hors d'un corridor de 15 m est ajoutée, à partir de 30 m.
> Best-effort — Overpass bridé rend la BD TOPO intacte, et l'UI le dit.
>
> **Validé bout-en-bout sur ForêtAccess** : 373/373 tronçons BD TOPO conservés,
> répartition par classe **identique** à l'entrée (44 `hors_desserte`,
> 254 `piste`, 7 `reseau_public`, 68 `route`), + 28 tronçons OSM (3,61 km, plus
> court 33,8 m). 25,9 min, pic 6,3 Go. Suite app : 0 échec, 10 774 tests.
>
> Les états mesurés sur les 401 tronçons disent pourquoi le retrait était faux :
> **213 `abandonnee` + 95 `hors_route`**, soit 77 % du réseau, sur une emprise
> portant 68 `route` de la BD TOPO. Ces états ne sont pas un inventaire.

---

## Deux remontées pour `foretaccess` (dépôt distinct)

À ne PAS mettre dans le `PLAN.md` cœur — elles concernent `pobsteta/foretaccess`.
Elles sont listées ici pour ne pas se perdre.

1. **Gabarits `cli` non interpolés.** Avec `retirer_disparues = FALSE` — le
   **défaut** du paquet — le message de fin de `qualifier_desserte()` sort ses
   accolades brutes :

   ```
   357/401 tronçons relocalisés et mesurés, 357 largeurs renseignées,
   {n_retire} disparu{?s} retiré{?s}, {n_inapte} inapte{?s} grumier retiré{?s}.
   ```

   Bénin à l'affichage, mais révélateur : le chemin `FALSE` est moins exercé que
   le chemin `TRUE`, alors que c'est lui le défaut.

2. **`retirer_disparues = TRUE` mérite un avertissement.** Le paramètre est
   documenté « opt-in », ce qui est correct, mais rien ne prévient qu'il peut
   retirer la majorité d'un réseau. Le même raisonnement a conduit `foretaccess`
   2.1.0 à faire avertir `pondere_cout = FALSE` : un défaut silencieux qui
   change massivement la sortie finit par être pris pour un résultat. Un
   avertissement quand la proportion retirée dépasse un seuil (le tiers ?)
   aurait signalé ce défaut en une session au lieu de plusieurs.

---

## Ce que ce correctif ne règle pas

- **La détection rend toujours 0.** Mesuré le 2026-08-13 sur Reconfort
  (554 ha, 7/7 canaux retenus, AUC rugosité 0,763) : 0 tronçon en sept
  configurations — bornes figées, calibration locale, seuils 0,3 à 0,6,
  `buffer_ref = 0`, et avec canal de surface (4 018 s, 8,75 Go). La troisième
  couleur de la légende du comparateur (« ajouté par détection ») reste donc
  sans emploi. Voir `design/spec-desserte-reliquats.md` §2, dont la prémisse est
  **réfutée** par ces mesures et reste à réécrire.
- **Les tronçons OSM entrent avec `largeur = NA`.** Si un moteur s'appuie sur la
  largeur, ils seront traités comme non renseignés.
- **Le garde-fou mémoire de la détection modélise la mauvaise grille** : il
  annonce 1,96 Go pour un run mesuré à 8,75 Go (solveur glouton sur l'AOI à 5 m,
  au lieu de canaux raster sur la mosaïque à 1 m). Non corrigé.

---

## Références

- App : `nemetonshiny@b1a7d41a` (merge), `c4265a9b` (`chore(release): v0.122.0`),
  release GitHub `v0.122.0` publiée le 2026-08-13.
- Code : `R/service_accessibility.R` (`.desserte_complement_osm`,
  `.osm_highway_vers_classe`, `run_desserte_lidar_correction`),
  `R/mod_accessibility.R` (légende par source).
- Cœur consommé : `foretaccess 2.1.0`, `dessertR 1.3.0.9000`.
- Données d'essai : ForêtAccess `20260717_101641_wsfi`, Dabo
  `20260801_130303_xpdk`, Reconfort `20260701_204501_ltcp`.
