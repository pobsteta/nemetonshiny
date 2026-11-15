# Brief `nemetonmcp` — contrat adopté, jeton de confirmation, erreurs `isError`, annotations, noms dictés

> **Déposé le 2026-11-10** par la session `nemetonshiny` (mission 013, `pilotage/spec.md` § 7).
> **Décrit ce qui est livré sur `main` de nemetonshiny** (`348ff3df`), pas ce qui est prévu.
> Rien n'a été écrit dans `nemetonmcp` par l'émetteur (lecture seule).
> **Portée** : `r/R/outils.R`, `r/R/utils.R`, `r/R/plan.R`, doc et déploiement. Tout est à faire de votre côté.

Sources livrées (PR fusionnées dans nemetonshiny) :

| Sujet | PR | Commits clés | Tests |
|---|---|---|---|
| Contrat des objets internes déclaré (mission 001) | #253 | `46eee94b` | `tests/testthat/test-contrat_nemetonmcp.R` |
| Erreurs `isError`, classées, bilingues (003) | #256 | `86138435`, `7ff648aa`, `3eb6d441` | `test-service_mcp_erreurs.R`, `test-service_mcp_erreurs_classes.R` |
| Annotations des 10 outils (004) | #257 | `1b6930c3` | `test-service_mcp_annotations.R` |
| Noms dictés tolérants (007) | #258 | `b99be25e`, `0427664a`, `97bd8b6a`, `a848a9a9` | `test-service_mcp_noms_dictes.R`, `test-service_mcp_noms_dictes_locale.R`, `test-service_mcp_rapprocher_voisin.R` |
| Jeton de confirmation (010, **CASSANT**) | #259 | `bade93a8`, `20c38ae2`, `f690888b` | `test-service_mcp_jeton.R`, `test-service_mcp_confirmation.R`, `test-service_mcp_projet_nomme.R` |
| `resume_projet` complet (008) | #260 | `1ea3289b` | `test-service_mcp_resume.R` |
| Jeton sur disque (016) | #261 | `348ff3df` | `test-service_mcp_jeton_disque.R` |

Décisions : `pilotage/adr/0001` (contrat), `0002` (jeton), `0004` (outils longs, pas encore livré).
Référence des réponses : `inst/mcp/README.md`. Référence du contrat : `CONTRAT.md` § 6 bis.

---

## 1. Le contrat : 35 objets internes (ADR 0001)

`CONTRAT.md` § 6 bis déclare **35 objets internes** que vous lisez par `getFromNamespace` (31 avant
la mission 003/010). Les renommer ou changer leurs arguments est désormais un changement cassant côté
nemetonshiny (section « Changements cassants » de `NEWS.md`, plus un brief pour vous).
`test-contrat_nemetonmcp.R` verrouille présence et `formals()` (dans l'ordre) et vérifie que `CONTRAT.md`
égale la liste lue dans votre code.

Les 4 objets **ajoutés** (au contrat avant d'être appelés par vous ; exception datée du 2026-11-10 dans ce
test, variable `pas_encore`) :

- `.mcp_vers_outil(x)` ;
- `.mcp_garde_ecriture(id, call)` ;
- `.mcp_jeton_emettre(outil, projet, lien, apercu)` ;
- `.mcp_jeton_consommer(jeton, outil, projet, lien, garde)`.

Vos 31 appels `.ns("…")` actuels (`grep -ohP '\.ns\("[^"]*"\)' r/R/*.R | sort -u`, relevé du 2026-11-10)
sont les 31 premiers objets du contrat. **À faire** : n'appeler rien d'autre ; quand vous appelez les 4
nouveaux, prévenez-nous.

**L'exception `pas_encore` est tout-ou-rien** : le test fait
`expect_setequal(appelles, setdiff(contrat, pas_encore))`. Dès que vous appelez **un seul** des 4 objets, il
échoue, puisque cet objet est alors dans `appelles` et dans `pas_encore`. Deux façons de ne pas casser la
CI de nemetonshiny : soit **livrer les 4 appels ensemble** (une seule demande de retrait de l'exception),
soit **prévenir nemetonshiny à chaque objet** pour qu'on le retire de `pas_encore` un par un, avant votre
merge.

**Contrôle sauté en CI** : le test « nemetonmcp n'appelle aucun objet hors contrat » lit
`/home/pascal/dev/nemetonmcp/r/R` (chemin en dur, surchargeable par `NEMETONMCP_R_DIR`) et **est sauté**
si le dossier est absent, donc en CI. Il ne protège que sur le poste de Pascal : une dérive de votre côté
n'est pas vue par la CI de nemetonshiny. Un test équivalent chez vous (vos appels `.ns()` face à la
section de `CONTRAT.md`) comblerait ce trou.

## 2. Erreurs : `isError`, classées, bilingues (mission 003, #256)

Forme unique de la chaîne JSON d'erreur de nemetonshiny :

```
{"ok": false, "classe", "code", "langue", "erreur", "action", "candidats"}
```

- `classe` : classe R complète (`nemetonshiny_projet_introuvable`) ; `code` : nom court (`projet_introuvable`).
- `erreur` : FR par défaut ; `langue` (`fr`/`en`) n'est acceptée que par `resume_projet` et `generer_rapport`.
- `action` : **une phrase traduite** (FR ou EN selon `langue`) donnant la suite à proposer, rendue par
  `i18n$t("mcp_act_<clé>")` (`R/service_mcp.R:85`) ; ce n'est **pas** une clé. Les clients **décident sur
  `code`** et lisent `action` seulement.
- `candidats` : seulement pour `projet_ambigu`, au format `nom (id)`.
- Classes non répertoriées : `nemetonshiny_erreur_inconnue` ou `nemetonshiny_erreur`.

Côté MCP, `mcp_tools()` rend l'erreur en `ellmer::ContentToolResult(error = …)`, que mcptools marque
`isError: true` (une erreur R levée donnerait une erreur JSON-RPC -32603 ; une chaîne ordinaire passerait
pour un succès : lecture de mcptools 1.0.3 par l'orchestrateur, 2026-11-09). Les fonctions `mcp_*` gardent
leur contrat de chaîne JSON ; seule la chaîne d'erreur porte l'attribut `mcp_erreur = TRUE`.

**Ce qui reste faux chez vous** : vous retirez `etat_calcul` et `exporter_gpkg` de `mcp_tools()` pour les
remplacer par `etat_calcul_vue` et `exporter_gpkg_vue` (`r/R/outils.R`). Ces deux fonctions rendent
`texte` tel quel : leurs erreurs sortent **sans `isError`**. À faire :

1. Dans `etat_calcul_vue` et `exporter_gpkg_vue`, appeler `.ns(".mcp_vers_outil")(...)` (au contrat)
   sur la chaîne rendue : en cas d'erreur, `.ns(".mcp_vers_outil")(texte)` (la chaîne de
   `mcp_etat_calcul` / `mcp_exporter_gpkg` porte déjà l'attribut `mcp_erreur`) ; en cas de succès, il
   rend la chaîne telle quelle. Gardez `texte` d'origine pour le renvoi (vous ne passez par
   `jsonlite::fromJSON` que pour lire, c'est bon : ne reconstruisez pas la chaîne d'erreur).
2. **Ne pas utiliser `.mcp_marquer`** : il n'est pas au contrat et échoue hors de nemetonshiny (il cherche
   `.mcp_vers_outil` dans l'espace de noms de la fonction enveloppée).
3. Aligner **votre** `.mcp_call` (`r/R/utils.R`) sur la forme ci-dessus. Il rend aujourd'hui
   `{ok, erreur, classe, candidats}` : il manque `code`, `langue`, `action`, et il ne pose ni l'attribut
   `mcp_erreur` ni donc `isError` pour vos propres outils (`creer_projet`, `plan_actions`,
   `ajouter_action`, `vue_atlas`, …). Appliquez `.ns(".mcp_vers_outil")` aussi à ceux-là. Vos classes
   `nemetonmcp_*` n'ont ni `code` ni `action` : définissez un `code` court par classe. Pour `action`
   (**décision de Pascal**), nemetonmcp écrit **ses propres phrases** dans son propre i18n ; seuls les
   **`code`** sont alignés sur la liste de nemetonshiny quand le cas existe déjà (`projet_introuvable`,
   `projet_ambigu`, `projet_verrouille`, `calcul_en_cours`, …). **Ne lisez pas `TRANSLATIONS`** de
   nemetonshiny : il est hors contrat (§ 6 bis) et peut changer sans préavis. AIGORA et VICTOR décident
   sur `code`.

## 3. Annotations (mission 004, #257)

Chaque outil de `mcp_tools()` porte `read_only_hint`, `destructive_hint`, `idempotent_hint` : lecture
seule pour `lister_projets`, `resume_projet`, `etat_calcul`, `url_app` ; destructifs `appliquer_ugf` et
`croiser_onf` ; non idempotent `lancer_calcul` seul (arbitrage de Pascal, 2026-11-10).

**Écart constaté chez vous** : dans `outils_mcp()`, pour `lister_projets`, `resume_projet`, `url_app` vous
**remplacez** l'annotation (`o@annotations <- .lecture_seule()`, soit `read_only_hint = TRUE` seul) : les
`destructive_hint = FALSE` et `idempotent_hint = TRUE` de nemetonshiny sont perdus. `etat_calcul_vue` repart
aussi avec la seule lecture seule ; `exporter_gpkg_vue` sans annotation (chez nous : `read_only_hint =
FALSE`, `destructive_hint = FALSE`, `idempotent_hint = TRUE`). Vos outils d'écriture de plan
(`ajouter_action`, `modifier_action`, `supprimer_action`) n'ont aucune annotation. À faire : ne pas
écraser une annotation déjà posée ; annoter explicitement vos propres outils (les écritures sur projet
existant : `destructive_hint = TRUE`). Non vérifié : ce que le client reçoit pour une annotation partielle.

## 4. Jeton de confirmation (mission 010, #259 — CASSANT)

`appliquer_ugf` et `croiser_onf` n'écrivent plus au premier appel (ADR 0002 ; arbitrage 4 de Pascal :
« il faut confirmer dès qu'on touche à la base de données postgres/postgis »). Périmètre tranché le
2026-11-09 : toute modification d'un **projet existant**, sur disque ou en base ; lecture, lancement de
calcul et exports restent libres.

Protocole exact :

1. **1er appel** (sans `jeton`) : tous les contrôles (gardes, fichier, croisement ONF), **rien n'est écrit**.
   Réponse : erreur `confirmation_requise`, avec `jeton`, `apercu`, `expire_a` (ISO 8601, UTC), `projet`
   (identifiant) et `projet_nom` (nom résolu, cité aussi dans le message). Elle sort en `isError: true`.
   - `appliquer_ugf` : `apercu` = `n_ugf_remplacees`, `n_ugf_nouvelles` ;
   - `croiser_onf` : `apercu` = `ecartees` (IDU), `n_ecartees`, `n_retenues`.
2. **2e appel** : les **mêmes arguments** + `jeton` → l'écriture a lieu.
3. Le jeton est **à usage unique, valable 10 minutes** (constante `.MCP_JETON_DUREE`, provisoire jusqu'à
   l'essai VICTOR), **gardé sur disque** hors du dossier du projet (mission 016, voir ci-dessous).
4. Refus : `jeton_invalide` (inconnu, déjà utilisé, périmé, autre outil, autre projet, autres paramètres :
   fichier ou son md5, `remplacer`, réglages ONF résolus dont `purger`, parcelles du projet changées). Il
   faut redemander un aperçu.
5. Un jeton connu présenté est consommé même refusé, **sauf** si le projet est introuvable ou ambigu (ou si
   les métadonnées ONF sont illisibles) : ces erreurs sont levées avant la lecture du jeton, **il reste
   valable**.
6. Ordre à la confirmation, **état livré sur main** : `.mcp_jeton_consommer` retire le jeton, puis rejoue
   la garde (`.mcp_garde_ecriture` : calcul en cours, verrou, projet ancien), puis compare ; donc un refus
   `calcul_en_cours` ou `projet_verrouille` au 2e appel **détruit le jeton** : il faut redemander un aperçu.
   Puis fichier relu / croisement recalculé, puis écriture. **À venir, non livré** (décision de Pascal,
   2026-11-11, mission 012) : la garde passera **avant** la consommation, un refus de garde ne
   consommera plus le jeton.

**Barrière, pas preuve** : un agent qui enchaîne seul les deux appels contourne la garde. Seule
l'*elicitation* MCP mettrait vraiment un humain dans la boucle ; mcptools 1.0.3 ne la fournit pas.

**À faire chez vous** (vos propres écritures sur projet existant) :

- Appliquer le même protocole à `ajouter_action`, `modifier_action`, `supprimer_action`
  (`r/R/plan.R`, qui appellent `save_action_plan`) : ce sont des modifications d'un projet existant.
  Outils du contrat : `.mcp_jeton_emettre(outil, projet, lien, apercu)` au 1er appel (après gardes et
  contrôles métier, avant toute écriture ; il lève lui-même `confirmation_requise`) et
  `.mcp_jeton_consommer(jeton, outil, projet, lien)` au 2e appel, avant d'écrire. `lien` est ce que la
  confirmation doit retrouver inchangé (comparé par `identical()`) : pour une action, son identifiant et
  son contenu. Ajouter un argument `jeton` (optionnel) à chacun de ces outils.
- Remplacer votre `.refuser_si_verrouille` (`plan.R`) par `.mcp_garde_ecriture()` : il couvre aussi « calcul
  en cours » et « projet ancien », pas seulement le verrou. **Au 2e appel, ne l'appelez PAS vous-même avant
  `.mcp_jeton_consommer`** : celui-ci rejoue déjà la garde (paramètre `garde`, défaut
  `.mcp_garde_ecriture`), comme `mcp_appliquer_ugf` ; une garde de plus avant lui diverge de l'ordre de
  nemetonshiny, et encore plus quand la mission 012 inversera l'ordre. Au **1er appel**, appelez
  `.mcp_garde_ecriture` puis `.mcp_jeton_emettre`.
- `creer_projet` crée un projet neuf : hors périmètre du jeton.
- Un 2e appel rejoué après succès renvoie `jeton_invalide` : c'est voulu.

**Le jeton survit au redémarrage du serveur** (mission 016, PR #261, `348ff3df`, tests
`test-service_mcp_jeton_disque.R`, `setup-service_mcp_jetons.R`). Le registre n'est plus en mémoire : un
fichier `<jeton>.rds` par jeton dans `NEMETON_MCP_JETONS_DIR` (défaut
`tools::R_user_dir("nemetonshiny", "data")/mcp/jetons`), hors du dossier du projet, répertoire `0700`,
fichiers `0600`, écriture atomique. La suppression du fichier fait foi : l'usage unique tient **même entre
deux serveurs simultanés**. Il ne contient que le jeton, l'outil, l'id du projet, l'empreinte et
l'échéance. Le contrat client (forme de `confirmation_requise`, 10 min, empreinte) est inchangé ; les deux
objets du contrat gardent leurs arguments (`CONTRAT.md` mis à jour).

Conséquences pour vous :

- **Déploiement** : le connecteur distant tourne sous `User=nemeton` avec `ProtectSystem=strict`,
  `ProtectHome=read-only` et `ReadWritePaths=/srv/nemeton` (`deploiement/nemetonmcp.service`). Le défaut
  (`tools::R_user_dir("nemetonshiny", "data")`, sous le home) n'y est **pas inscriptible** : l'émission d'un
  jeton échouerait (erreur de registre, rien d'écrit). **Posez `NEMETON_MCP_JETONS_DIR` sous `/srv/nemeton`**
  (ex. `/srv/nemeton/mcp/jetons`) dans `deploiement/nemetonmcp.env.exemple`. Le connecteur distant n'a
  **pas** à partager ses jetons avec VICTOR et AIGORA (qui tournent sous l'utilisateur de Pascal) : seuls
  les clients qui se passent un jeton d'une tâche à la suivante partagent le répertoire, donc
  l'utilisateur et la variable (modes 0700/0600 : un autre utilisateur ne voit pas les jetons).
- **Chemins de fichier absolus** : pour `appliquer_ugf`, le chemin du fichier fait partie de l'empreinte.
  Un chemin **relatif** la rend dépendante du répertoire de travail : si la 2e tâche part d'un autre
  répertoire, le jeton est refusé (`jeton_invalide`). Vos outils et vos descriptions doivent demander des
  chemins absolus aux clients.
- Registre impossible à créer ou à écrire : erreur classée `erreur`, rien d'écrit.
- Vos propres écritures de plan (`.mcp_jeton_emettre` / `.mcp_jeton_consommer`) en profitent sans rien
  changer.

## 5. Noms de projets dictés (mission 007, #258)

`.mcp_resolve_project` rapproche un nom approximatif du mot-clé de la commune du projet (« Forêt communale
de Couchey (ONF) » → `couchey`) : accents et casse ignorés (par points de code, indépendant de la locale,
y compris `LC_ALL=C`), « Forêt [communale] de/du/des/d' » retiré des deux côtés, égalité ou suite de mots
entiers en tête ou distance d'édition ≤ 2 (seuil **provisoire**, plancher de 4 lettres des deux côtés).
Une égalité exacte l'emporte, sauf si un autre projet est à distance ≤ 1 (alors `projet_ambigu` avec les
candidats). Fixture : `tests/testthat/fixtures/noms_dictes.csv` (33 lignes **simulées**, pas de vraies
dictées).

Vous l'obtenez sans rien changer : `.resoudre_projet` (`r/R/projet.R`) appelle déjà
`.ns(".mcp_resolve_project")`. **À faire** : vérifier qu'aucun outil ne résout un nom par une autre voie
et ne pas dupliquer ce rapprochement (votre `.normaliser`, `utils.R`, sert aux communes, pas aux projets).

**Lancement du serveur** (décision de Pascal, 2026-11-10) : lancer `nemetonmcp::serveur_mcp()` et
`serveur_mcp_distant()` avec **`TZ=Europe/Paris`** et une **locale UTF-8** (`C.UTF-8`, repli
`en_US.UTF-8`). `inst/mcp/server.R` de nemetonshiny force les deux au lancement (`TZ = "Europe/Paris"`
depuis la mission 008) ; mais vous lancez le serveur par **votre** `serveur_mcp()`, qui ne passe pas par ce
fichier : je n'ai trouvé aucun `Sys.setlocale` ni `TZ` dans `nemetonmcp/r/R`, et
`deploiement/nemetonmcp.env.exemple` pose `LANG=C.UTF-8` mais pas `TZ`. À faire : poser `TZ` dans ce
fichier et dans la doc de lancement ; forcer `TZ` et la locale au démarrage de `serveur_mcp()`. Sans
`TZ`, les dates de `resume_projet` ci-dessous seraient lues dans le fuseau du poste.

**`resume_projet` complet** (mission 008, PR #260, `1ea3289b`, `test-service_mcp_resume.R`) : l'outil rend
tout l'écran Synthèse. Champs ajoutés : `description` ; `created_at`, `updated_at`, `calcule_le` en
**ISO 8601 avec fuseau Europe/Paris** (`calcule_le` : `completed_at` > `fin_a` du job `termine` > date de
`indicators.parquet` > `null`) ; `chm_suspect`, `chm_suspect_max` ; par famille `ndp` et `n_indicateurs`
(présents, à défaut prévus). Changements de forme (notés « Changements cassants » dans `NEWS.md`) :
`updated_at` change de **format** (heure naïve → ISO 8601 avec fuseau) et le `score` des familles passe à
**2 décimales**. Vous n'avez rien à faire pour les servir (vous reprenez l'outil tel quel) ; à vérifier :
que vos vues (`fiche_projet`, `vue_atlas`) ne relisent pas l'ancien format de `updated_at`.

## 6. Noms d'outils dédoublonnés

`outils_mcp()` assemble les 10 outils de nemetonshiny et les vôtres, en retirant à la main deux noms
(`etat_calcul`, `exporter_gpkg`). Une liste manuelle casse en silence le jour où nemetonshiny ajoute un
outil du même nom qu'un des vôtres (deux outils homonymes : comportement client indéfini). À faire :
dédoublonner **par nom** (le vôtre l'emporte) et ajouter un test d'unicité des noms de `outils_mcp()`.
Les 10 noms de nemetonshiny, figés par `test-service_mcp_annotations.R` : `lister_projets`,
`resume_projet`, `lancer_calcul`, `etat_calcul`, `annuler_calcul`, `generer_rapport`, `exporter_gpkg`,
`appliquer_ugf`, `croiser_onf`, `url_app`. Tout ajout de notre côté est annoncé en `NEWS.md` et dans
`CONTRAT.md`.

## 7. Renommage `nemetonclaude` → `nemetonmcp`

Chez vous, les restes relevés le 2026-11-10 sont voulus (`README.md` section migration, `NEWS.md`,
compatibilité `nemetonclaude_conflit` dans `tests/vues.test.mjs` et `clients/claude/vues/plan/index.html`,
`specs/ARCHITECTURE-nemeton-llm.md`). **À vérifier** : cette compatibilité a-t-elle une date de retrait ?
Le renommage est en revanche **incomplet chez vos clients** (voir le brief `vers-aigora` :
`nemetonclaude::serveur_mcp()` dans `register_mcp.py` et `CLAUDE.md`, lien `nemeton-vues` cassé). Ne
retirez pas un alias de compatibilité avant que ces clients aient suivi.

## 8. À venir (annonce, **non livré**) : outils longs détachés (mission 012, ADR 0004)

La mission 012 prévoit de détacher `croiser_onf` (le 2e appel, avec jeton, rendrait aussitôt un `job_id`)
et `generer_rapport`, suivis par `etat_calcul(projet)` ; **un seul job par projet** (un second lancement
rend `calcul_en_cours`). Rien de cela n'est sur `main` : ne rien coder dessus. Un brief de livraison suivra ;
prévoyez seulement que `etat_calcul_vue` aura plus d'un type de job à décrire.

## Ce que cette session n'a pas pu vérifier

- Le jeton de bout en bout entre deux `claude -p` réels (la mission 016 le teste en R, pas via VICTOR) ; le droit d'écriture de `NEMETON_MCP_JETONS_DIR` sous l'unité systemd (§ 4).
- Le rendu client d'une annotation partielle (§ 3).
- Si votre CI exécute un contrôle du contrat (§ 1).

## Retour attendu

Un brief de réponse dans `vers-nemetonshiny/` à la fin de la livraison, avec le SHA de `nemetonmcp`.
