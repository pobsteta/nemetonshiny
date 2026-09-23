# BRIEF `nemeton` — `PLAN.md` : douze livraisons app (0.143.17 → 0.143.28) et quatre écarts à refermer

> **Statut** : ouvert, 2026-09-23.
> **Dépôt concerné** : `nemeton` uniquement — `PLAN.md` racine.
> **Nature** : documentation seule. **Aucun code cœur, aucune release cœur.**
> **Contexte** : `nemetonshiny@0.143.28` (`main` = `3869ffd8`), cœur plancher
> `nemeton (>= 0.197.0)`, `foretaccess (>= 2.4.0)`.
>
> Le brief précédent (`BRIEF-nemeton-plan-md-0.143.14-0.143.16.md`) est soldé :
> 0.143.14 et 0.143.16 sont au journal (lignes 3917 et 3768 au 2026-09-23).
> Le dernier numéro app consigné est donc **0.143.16** ; rien de 0.143.17 à
> 0.143.28 n'y figure.

---

## 1. Table « Écarts ouverts » (l. 26 sq.) — quatre lignes à passer en « refermé »

Relu en lecture seule sur `nemetonshiny@3869ffd8`. Déplacer ces lignes dans la
seconde table (« Refermé par / Ce qui a été relu ») :

| # | Refermé par | Ce qui a été relu |
|---|---|---|
| 7 | app **v0.142.0** (`819ab31c`, plancher `>= 0.192.0` en `50043030`) | `mod_family.R` pose l'icône « fiche » à côté du « i » ; `app_config.R:165-204` lit `doc_url` / `doc_lang` depuis `indicator_labels()`. Rien de codé en dur : ajouter une fiche reste un geste 100 % cœur |
| 8 | app **v0.143.25** (`383a110c`) | `R/migrate.R:149` — `INDICATOR_SENSE_VERSION <- 3L`. `ensure_indicator_sense_current()` compare au marqueur `indicator_sense_version` des métadonnées projet et appelle `invalidate_indicators()` **une seule fois** à la première ouverture après montée de version (marqueur posé même sans rien à invalider, pour ne pas rejouer). Le message cite « v3 : inversion de L1… » |
| 10 | app **v0.143.25** (même commit) | Même mécanisme, même palier v3 (« borne 200 ans sur T1 ») |
| 11 | app **v0.143.25** (même commit) | Même mécanisme, même palier v3 (« E1/E2 alignés sur P1 ») |

Plancher : `DESCRIPTION:15` exige `nemeton (>= 0.197.0)`.

**Écart n° 6 (B4/L3)** : **accusé de réception** côté app. Aucun écran ne classe,
ne compare ni ne moyenne B4/L3 entre projets — l'app n'a pas de vue
inter-projets. Le point 4 du brief (B02 absente de l'espace k-means, remplacée
par `ID`) **n'est pas encore instruit** : laisser l'écart ouvert sur ce seul
point, reformulé « question B02 en attente côté app ».

**Écart n° 3** (profil en travers, validation terrain) : inchangé.

Mettre à jour la phrase d'en-tête : « Six écarts » → **« Deux écarts (n° 3, n° 6 résiduel) »**.

## 2. Journal — où coller

En tête de journal, **au-dessus** de `### 2026-09-18 — La réserve reprise…`
(l. 3535). Ordre antéchronologique.

## 3. Les entrées

| Release | Merge `main` | Date | Contenu |
|---|---|---|---|
| v0.143.28 | `3869ffd8` | 2026-09-19 | Tour guidé : l'étape « Plan d'action » s'ancre sur la carte « Tableau des actions » (plus sur la sidebar 793 px dans une fenêtre de 900). Boucle driver.js ↔ `ResizeObserver` bslib : 326 `resize` en 6,4 s → 0 |
| v0.143.27 | `8cb4abfc` | 2026-09-19 | Tour guidé : n'entre plus dans les onglets restreints ; ancres statiques |
| v0.143.26 | `ce38faa4` | 2026-09-19 | Tour guidé : ne meurt plus à la bascule d'onglet ; cadre la carte entière |
| v0.143.25 | `59603c58` | 2026-09-18 | **Consomme cœur v0.197.0 (spec 048)** : invalidation unique L1/T1/E1/E2 (écarts 8/10/11). + recadrage carte cadastrale au retour d'onglet ; puces du message d'invalidation (`cli_warn` au lieu de `cli_alert_warning`) |
| v0.143.24 | `596cdf16` | 2026-09-18 | Consomme le verdict « CHM suspect » du cœur ; `R/service_python.R` — registre d'interpréteurs + runner isolé |
| v0.143.23 | `31327652` | 2026-09-17 | Plancher `nemeton >= 0.196.0` : la garde `formals()` sur l'arrêt RECONFORT tombe |
| v0.143.22 | `5d5863d5` | 2026-09-17 | Chaîne Santé : moteur annulé ≠ « ok » ; rejets du curseur de chaîne explicités ; état de la chaîne persisté sur disque |
| v0.143.21 | `048fe55d` | 2026-09-14 | **RECONFORT s'arrête pour de vrai** (arrêt coopératif, brief `briefs/vers-nemeton/2026-09-14-reconfort-cancel-path.md` → cœur v0.196.0) ; garde de version temporaire, plancher non bumpé à ce stade |
| v0.143.20 | `9ccffa37` | 2026-09-14 | Retrait de la copie morte de `mod_home_ui` dans `app_ui.R` |
| v0.143.19 | `0fc7a5e1` | 2026-09-14 | `cancel_computation` devient LE signal d'arrêt ; bandeau S2 fantôme après annulation ; fond de carte dans le bouton « couches » ; étapes Santé nommées par moteur |
| v0.143.18 | `09b399ee` | 2026-09-14 | LLM : modèle Mistral par défaut hors palier « premier » + repli automatique sur refus de palier/quota |
| v0.143.17 | `8de94ed0` | 2026-09-04 | « Tableau des actions » contient enfin les actions ; « Tout calculer » cède l'emphase pleine ; deux tests chrono fiabilisés |

Toutes taguées par `release.yml`. Cycle dev courant : `0.143.28.9000`.

Suggestion de regroupement (une entrée par thème plutôt que douze) :

- `### 2026-09-18 — App v0.143.25 : spec 048 consommée, écarts 8/10/11 refermés`
- `### 2026-09-14 → 09-17 — App v0.143.19 → v0.143.23 : un arrêt est un arrêt (RECONFORT, chaîne Santé)`
- `### 2026-09-19 — App v0.143.26 → v0.143.28 : le tour guidé tient debout` — retenir la règle consignée dans `service_tour.R` : **une ancre de tour ne doit pas remplir la fenêtre**
- `v0.143.17`, `v0.143.18`, `v0.143.20`, `v0.143.24` en une ligne chacune

## 4. Section « Brief émis — Onglet Desserte » (l. 88)

L'écart n° 2 (`osm_hors_corridor` / `bdtopo_hors_corridor`) est refermé
(foretaccess 2.4.0 + app v0.127.0). Le titre peut passer en **« Chantier CLOS — »**.
Reste seulement l'écart n° 3 (validation terrain), déjà suivi dans la table.
