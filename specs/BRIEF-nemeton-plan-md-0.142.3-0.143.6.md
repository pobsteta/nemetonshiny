# BRIEF `nemeton` — `PLAN.md` partagé : huit livraisons app (0.142.3 → 0.143.6)

> **Statut** : ouvert, 2026-08-31.
> **Dépôt concerné** : `nemeton` uniquement — le `PLAN.md` à sa racine est la
> source unique de vérité partagée avec `nemetonshiny` (ADR-009).
> **Nature** : documentation seule. **Aucun code cœur, aucune release cœur**
> requis par ces huit livraisons (§5). Deux signaux à instruire en §4.
> **Contexte de lecture** : `nemetonshiny@0.143.6.9000`, `main` à `592dfaa9`,
> dernière release **v0.143.6** taguée par `release.yml`.

---

## 0. Où insérer, exactement

La règle 12 du `CLAUDE.md` de `nemetonshiny` interdit d'**écrire** dans
`/home/pascal/dev/nemeton`, mais autorise la **lecture**. J'ai donc ouvert
`PLAN.md` et je peux être précis, contrairement aux briefs `PLAN.md`
antérieurs :

- Le journal commence à `PLAN.md:3520` (`## Journal`), entrées **du plus récent
  au plus ancien**, titres au format `### YYYY-MM-DD — …`.
- La première entrée actuelle est `PLAN.md:3522` —
  *2026-08-28 — v0.192.2 : `resolve_project_chm()` ne voyait pas les CHM
  Open-Canopy*.
- La dernière entrée app est `PLAN.md:3586` —
  *2026-08-28 — App `nemetonshiny` v0.142.2*.

**Coller le §2 entre la ligne 3520 (`## Journal`) et la ligne 3522**, donc en
tête de journal : les huit livraisons vont du 28/08 au 31/08 et sont toutes
postérieures ou contemporaines de v0.192.2.

Les numéros de ligne datent du 2026-08-31 ; si le journal a bougé depuis,
c'est le repère textuel qui fait foi, pas le numéro.

## 1. Les huit livraisons

Toutes **releasées et taguées** (tags posés automatiquement par `release.yml`
depuis `main`) — la règle « ne jamais clore un chantier sans release poussée »
est satisfaite dans les huit cas.

| Release | Commit du tag (`main`) | Commit fonctionnel | Date | Bump |
|---|---|---|---|---|
| v0.142.3 | `652f3db9` | `ce1f3b51` + `42009f67` | 2026-08-28 | patch |
| **v0.143.0** | `6825b959` | `4c0cb16a` | 2026-08-28 | **minor — jalon** |
| v0.143.1 | `e6c51b99` | `81a0353a` | 2026-08-29 | patch |
| v0.143.2 | `109140fc` | `5798d879` | 2026-08-29 | patch |
| v0.143.3 | `3d677895` | `d0525703` | 2026-08-30 | patch |
| v0.143.4 | `f095c6b4` | `f0caff21` | 2026-08-30 | patch |
| v0.143.5 | `ecaa30bd` | `7e10de2f` | 2026-08-30 | patch |
| v0.143.6 | `f477e960` | `29fb357e` | 2026-08-31 | patch |

Cycle dev repris après chaque release ; cycle courant `0.143.6.9000`
(`592dfaa9`).

## 2. Texte exact de l'entrée de journal à coller

```markdown
### 2026-08-28 → 08-31 — App `nemetonshiny` : « Tout calculer » (v0.143.0) et les six runs qui l'ont durcie (v0.142.3 → v0.143.6)

**Le jalon.** Un seul bouton, dans la sidebar de l'onglet *Sélection*, enchaîne
les seize calculs de l'application puis les deux générations IA. Une modale
demande le périmètre (étapes cochables) et le **profil de l'analyste** parmi les
quinze profils experts, appliqué à toutes les générations IA de la chaîne. Un
panneau suit l'avancement ; un rapport final donne par étape son issue et sa
durée. Une étape en échec **n'interrompt pas** la chaîne, et le rapport
distingue `réussie` / `échec` / `sautée` / `annulée` — « trois sautées faute de
configuration » ne doit pas se lire « trois en échec ».

L'ordre encode ce qui alimente quoi : indicateurs (1) → accessibilité et
correction LiDAR (2-3) → desserte, typage, intégrité (4-6) → reGénération,
années E-OBS d'abord car elles **déterminent** ce que le gel et le moteur
consomment (7-11) → Santé, surveillance rapide avant FORDEAD et RECONFORT qui
lisent le cache Sentinel-2 qu'elle remplit (12-14) → perspective IA (15) → plan
d'actions, bâti sur les commentaires que (15) vient d'écrire (16). La création
des zones de suivi s'y est ajoutée en v0.143.2, en position 12 : les trois
moteurs Santé exigent tous un `zone_id`. **Dix-sept étapes** au total.

**Architecture — l'orchestrateur ne lance aucun moteur.** Il poste
`app_state$pipeline_request` ; le module propriétaire répond sur
`app_state$pipeline_answer`. Chaque moteur est un `ExtendedTask` dont les
arguments viennent des inputs de son onglet ; un orchestrateur qui les
appellerait directement redupliquerait tout et divergerait dès qu'un onglet
gagne une option. Le corps de chaque observer de bouton est extrait en fonction
locale, appelée par le bouton **et** par la chaîne. Aucun bloc dupliqué.
**Aucune fonction cœur nouvelle n'est requise** : la chaîne rejoue les chemins
d'appel existants.

**Ce que six runs réels sur Couchey ont appris** — chaque release ci-dessous est
née d'un run, pas d'une relecture.

| Release | Ce que le run a montré |
|---|---|
| v0.143.1 | La chaîne restait bloquée sur « Indicateurs / En cours » : la réponse était posée depuis `poll_fn`, un callback `later::later()` donc **hors contexte réactif**, où lire un `reactiveVal` lève `Operation not allowed without an active reactive context`. Second défaut trouvé en vérifiant si le premier était isolé : dans les six autres modules les lectures étaient légales mais **abonnaient** l'observer de statut à la mémoire de requête — un `success` résiduel aurait rapporté une étape réussie **avant que le moteur ne redémarre**. Toutes les lectures sont isolées ; un test de source refuse toute lecture non isolée. |
| v0.143.2 | « Perspective IA : Réussie » en **1 seconde**, pour ce qui demande treize appels LLM : le `tryCatch` rendait `NULL` mais la fonction continuait jusqu'à `invisible(TRUE)`. Un faux positif silencieux est pire qu'un échec. C'est aussi ce qui expliquait le Plan d'actions sauté juste après. Au passage : les 12 commentaires de famille n'étaient pas générés (la chaîne suivait un switch décoché par défaut). |
| v0.143.3 | Santé sautée alors que les quatre zones venaient d'être créées en 9 s — la garde interrogeait `input$zone_id`, alimenté par `updateSelectInput()`, qui ne remonte au serveur **qu'après un aller-retour client**. **Troisième occurrence du même piège** dans cette chaîne (après les années E-OBS et `use_corrected`, tous deux pourtant commentés). Les gardes lisent désormais les zones **en base**. |
| v0.143.4 | Le panneau de progression liste dix-sept étapes et repoussait le reste de la sidebar hors de l'écran : la section devient repliable comme ses voisines. |
| v0.143.5 | « Échec du typage » affiché **en rouge** sur le meilleur résultat possible : 0 route nouvelle, 17 056 tronçons existants, **76 parcelles desservies sur 76**, coût glouton 0. Le réseau existant dessert déjà tout ; `foretaccess::vectoriser_reseau()` travaille sur les routes *nouvelles* et abandonne quand il n'y en a aucune (cf. §4.2 du brief). Cas désormais distingué **avant** l'appel, compté *Sautée*. Idem pour microclimf, qui annonçait « structure de végétation manquante » quand c'était la grille LiDAR (MNT/MNH non téléchargés) qui manquait. |
| v0.143.6 | Le rapport affichait « Erreur » **sans jamais dire laquelle** — alors que `task$result()` re-lève l'erreur du worker, seul endroit où son message existe encore. Sur des moteurs qui tournent 13 h 40 (ingest FAST) et 4 h 10 (FORDEAD), c'était la seule information exploitable sans tout relancer. `pipeline_task_error()` l'extrait sur les sept réponses concernées, couvre aussi l'échec rendu **par valeur** (`list(status = "error", reason =, detail =)`), et tronque les tracebacks Python de FORDEAD à 300 caractères. |

**Le mode de défaillance à connaître, pour tout module futur.** Tout chemin de
code qui a reconnu une requête **DOIT** répondre. Un module qui se tait bloque
la chaîne sur son étape, sans rien afficher. Les gardes internes (pas de zone
monitoring, pas de clé API, lecture seule) faisaient exactement cela. Un test
vérifie que **chaque étape déclarée a un écouteur** dans le module annoncé.

**Aussi livré** (v0.142.3) : la carte UGF restait vide au premier passage sur
son sous-onglet — `output$ug_map` était la seule des six cartes leaflet de l'app
à rester suspendue onglet caché, et leaflet **jette silencieusement** les
`leafletProxy()` adressés à une carte absente du DOM.

Hors chaîne, volontairement : optimisation, OSM et détection (panneaux d'analyse
annexes de la desserte), RVT et pré-build CVAT (préparation d'annotation).

Suite app : 13 325 PASS, 0 FAIL (7 skips). Plancher `Imports: nemeton (>= 0.192.0)` inchangé.
Cycle dev : `nemetonshiny@592dfaa9` (`0.143.6.9000`).
```

> Total de la suite app mesuré le 2026-08-31 sur `592dfaa9` :
> `[ FAIL 0 | WARN 100 | SKIP 7 | PASS 13325 ]`.

## 3. Cases à cocher : aucune, et c'est une information

J'ai cherché dans `PLAN.md` un chantier qui porterait cette chaîne
(`grep -n "Tout calculer\|pipeline_request\|orchestrat"`). **Il n'y en a pas** :
les seules occurrences d'« orchestration » concernent RECONFORT L2b.3
(`PLAN.md:2866`), les moteurs de reGénération (`:3070`, `:6188`, `:6305`) et
l'orchestrateur FORDEAD E6.c.1 (`:3457`) — tous côté cœur, sans rapport.

C'est cohérent : « Tout calculer » est une capacité **purement applicative**,
qui ne consomme aucune API cœur nouvelle. Deux options, à trancher côté cœur :

1. **Journal seul** (mon choix par défaut) — coller le §2, ne rien cocher.
2. **Ouvrir une ligne d'épaississement app** si vous estimez qu'un jalon de ce
   niveau (un bouton qui pilote les seize calculs, dix-sept étapes, protocole
   requête/réponse inter-modules) mérite d'exister dans la table de suivi. Dans
   ce cas la ligne appartient à la **Partie B — app `nemetonshiny`**
   (`PLAN.md:686`), pas à la Partie A.

Ne pas forcer la correspondance avec un chantier existant : aucun ne décrit ceci.

## 4. Deux signaux qui concernent le cœur — à instruire, pas à corriger à l'aveugle

Ni l'un ni l'autre n'est une demande de correctif ferme. Ce sont deux
observations que seule l'app pouvait faire, et qu'elle ne peut pas trancher.

### 4.1 — FAST et FORDEAD lèvent **après** avoir produit leur travail utile

Troisième run Couchey, 2026-08-30. Les deux moteurs Santé ont échoué —
surveillance rapide après **13 h 40**, FORDEAD après **4 h 10**. Inspection du
projet après coup :

| | |
|---|---|
| `ingest_run.json` | statut **done**, **183 scènes** |
| Table `alert`, zone `couchey_tot` | **51 alertes `fordead_dieback`** |

Autrement dit : les deux pipelines ont fait leur travail, l'ont persisté, **puis
ont levé**. L'échec est postérieur au résultat utile. Sa cause reste inconnue —
c'est précisément ce que le correctif v0.143.6 permettra de savoir au prochain
run, puisque le message du worker remonte désormais au rapport.

**Ce qui est demandé côté cœur : rien pour l'instant.** Je signale seulement
qu'un chemin de sortie de `ingest_sentinel2_timeseries()` et de
`run_fordead_dieback()` lève après persistance, sur un run réel de plusieurs
heures. Si vous connaissez un candidat (nettoyage de staging, fermeture de
connexion, `unlink()` d'un cache encore ouvert, garde-fou post-persist), c'est
le moment de le regarder. Sinon, attendre le prochain run et le message exact —
je le transmettrai.

### 4.2 — `vectoriser_reseau()` traite « zéro route nouvelle » comme une panne

Dépôt **`foretaccess`**, pas `nemeton` — je le consigne ici pour mémoire, la
décision d'ouvrir un brief séparé vous revient.

Quand le réseau existant dessert déjà toutes les UGF (Couchey : 76/76, coût
glouton 0), `reseau$lignes` est vide et `foretaccess::vectoriser_reseau()`
abandonne. Le **meilleur résultat possible** est donc indiscernable d'un échec
pour l'appelant. L'app le distingue désormais **avant** l'appel, mais le
contournement est chez elle ; rendre un objet typé vide plutôt qu'abandonner
serait plus juste au niveau du paquet.

Non urgent : le contournement app est en place et testé.

## 5. Ce qui ne change pas

- **Aucun fichier R du cœur touché** par ces huit livraisons.
- **Aucune release cœur** requise, aucun bump de cycle dev cœur.
- **Plancher `Imports: nemeton (>= 0.192.0)` inchangé** — la chaîne ne consomme
  que des API déjà exposées.
- `Remotes: pobsteta/nemeton@*release` inchangé.
- Aucune spec cœur à amender : l'ordre des dix-sept étapes est une décision
  applicative, documentée dans le NEWS app et dans l'entrée §2.

## 6. À vérifier côté cœur avant de coller

1. Le total de la suite app (`<N>` du §2) — fourni par la session app.
2. Que l'entrée v0.192.2 (`PLAN.md:3522`) reste bien **sous** le bloc collé :
   les dates 08-28 → 08-31 le placent au-dessus, mais 0.142.3 est du 28/08 comme
   v0.192.2, et les deux se citent mutuellement (le brief
   `BRIEF-nemeton-resolve-chm-opencanopy.md` a été rédigé en v0.142.3 et corrigé
   le jour même en v0.192.2).
3. Que le §3 a été tranché explicitement (journal seul, ou ligne Partie B).
