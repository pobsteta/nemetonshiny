# BRIEF `nemeton` — `PLAN.md` : trois livraisons app (0.143.11 → 0.143.13)

> **Statut** : ouvert, 2026-09-02.
> **Dépôt concerné** : `nemeton` uniquement — journal du `PLAN.md` racine.
> **Nature** : documentation seule. **Aucun code cœur, aucune release cœur.**
> **Contexte** : `nemetonshiny@0.143.13.9000`, `main` à `7886555d`.
> Journal cœur à jour jusqu'à l'entrée *App v0.143.10* — les trois suivantes
> manquent.

---

## 0. Où coller

En tête de journal, **au-dessus** de `### 2026-09-02 — v0.194.0 : le plafond
mémoire passe à 40 %`. Les trois entrées sont du 01/09 (soirée) et du 02/09 ;
les coller dans l'ordre ci-dessous les laisse en ordre antéchronologique
correct entre elles.

## 1. Les trois livraisons

| Release | Commit `main` | Date | Suite app |
|---|---|---|---|
| v0.143.11 | `52a5327a` | 2026-09-01 | 13 367 PASS / 0 FAIL |
| v0.143.12 | `28625639` | 2026-09-02 | 13 374 PASS / 0 FAIL |
| v0.143.13 | `7886555d` | 2026-09-02 | 13 392 PASS / 0 FAIL |

Toutes taguées par `release.yml`. Cycle dev courant `0.143.13.9000`.

## 2. Texte à coller

```markdown
### 2026-09-02 — App `nemetonshiny` v0.143.13 : le moteur tournait sur 2018 / 2022 sans le dire

Projet **Lajoux**, où la détection E-OBS est indisponible : l'étape s'affiche en
*Sautée* avec la bonne consigne (« saisir les années manuellement »), puis le gel
R7 et le moteur de reGénération s'affichent en **vert** — sans que rien n'indique
sur quelles années ils ont tourné.

`annees_pipeline()` n'est rempli que si l'étape E-OBS **réussit**. Sautée, il
reste `NULL` et le `%||%` des deux lanceurs retombe sur les champs du
formulaire, dont les valeurs d'usine sont `2018` et `2022`. Un run pouvait donc
décrire une année moyenne et une canicule sans rapport avec le climat du site, et
être rapporté réussi. C'est le mode de défaillance annoncé à la livraison de la
chaîne (v0.143.0) pour le cas « E-OBS réussit » ; le cas « E-OBS est sauté »
n'avait rien prévu.

Les deux étapes consommatrices enregistrent désormais les années qu'elles vont
utiliser **et leur provenance** ; le repli est nommé dans le rapport, avec ses
valeurs. Pas de blocage : quelqu'un a pu saisir ses années exprès, et un résultat
sur des années choisies vaut mieux qu'un refus — ce qui manquait n'était pas un
garde-fou, c'était la visibilité. `detectees` n'est vrai que si les **deux**
années viennent d'E-OBS, une seule suffirait à laisser passer un repli silencieux
sur l'autre.

Suite app : 13 392 PASS, 0 FAIL. `nemetonshiny@7886555d`.

### 2026-09-02 — App `nemetonshiny` v0.143.12 : une garde qui se sabotait à son premier usage

La v0.143.11 devait empêcher la recréation des zones de suivi. Au premier
lancement qui l'a suivie, sur Couchey, elle les a recréées quand même (ids
49-52), et les **106 marqueurs de reprise** consolidés à la main sont repartis en
orphelins.

La cause est dans la garde. `.zones_a_jour()` exigeait le fichier de clé — or ce
fichier n'est écrit qu'**après** un enregistrement réussi. Au premier run il
n'existe pas encore : la garde répondait « périmées » sur des zones parfaitement
valides. Une fois par projet, le correctif détruisait exactement ce qu'il
protégeait.

Des zones qui existent, portent une strate `_tot` et n'ont pas de clé sont
désormais **adoptées** : on écrit la clé des sources courantes et on ne recrée
rien, ce qui amorce la clé pour les runs suivants. Risque assumé et écrit dans le
code — adopter une géométrie périmée si les UGF ont changé — borné par le fait
que l'ancien comportement recréait les zones à *chaque* run (donc des zones
présentes viennent du run précédent) et que le bouton de l'onglet réenregistre
toujours.

**Le motif, qui dépasse ce correctif** : une garde ne doit pas dépendre d'un
artefact que seule l'action qu'elle évite sait produire. Le test initial
vérifiait justement que l'absence de clé rendait `FALSE`, en croyant verrouiller
la prudence — il verrouillait le défaut. Écrire le premier test dans le sens
« projet neuf » plutôt que « clé déjà là » l'aurait montré.

Suite app : 13 374 PASS, 0 FAIL. `nemetonshiny@28625639`.

### 2026-09-01 — App `nemetonshiny` v0.143.11 : deux caches rejoués pour rien

Parti d'une question de l'utilisateur — « pourquoi le contrôle d'intégrité est-il
rejoué en entier ? » — qui a mené à un défaut plus coûteux que celui qu'elle
visait.

**Contrôle d'intégrité de la desserte** : 51 min sur Couchey (17 056 tronçons), à
chaque lancement de la chaîne, réseau inchangé. Le résultat était pourtant sur le
disque (`integrite.rds`), relu — mais seulement pour réafficher le panneau à la
réouverture du projet. Le lancement ne le consultait jamais, et son lecteur ne
prenait **aucune clé** : il savait répondre « le fichier existe », pas « il est
encore valable ». Contraste avec `.load_cached_desserte(project_path, params)`,
qui compare les paramètres. Une clé de fraîcheur (mtime + taille du GeoPackage du
réseau et de l'AOI) en sidecar comble le trou ; le bouton de l'onglet relance
toujours.

**Zones de suivi** : `build_project_monitoring_zones()` a `replace = TRUE` par
défaut, et ce `replace` supprime puis réinsère — les identifiants changent à
chaque appel. Tout ce qui est indexé dessus devenait orphelin. Mesuré sur
Couchey, un seul projet, trois runs : `output_zone_37` (106 marqueurs, 3,5 Go),
`_41` (81, 2,8 Go), `_45` (2, 1,4 Go) — **6,3 Go sous des zones qui n'existent
plus**, vérifié en base. Le disque n'est pas le pire : les marqueurs
d'idempotence de l'ingestion RECONFORT vivent sous ce répertoire, donc un nouvel
identifiant = un répertoire vide = **tout re-téléchargé**. La reprise après arrêt
était déjà écrite côté cœur ; c'est le renouvellement des identifiants qui
l'annulait, run après run.

Suite app : 13 367 PASS, 0 FAIL. `nemetonshiny@52a5327a`.
```

## 3. Rien à cocher

Aucun chantier `PLAN.md` ne porte ces sujets (vérifié en v0.142.3 pour la chaîne
« Tout calculer », inchangé depuis). Journal seul.

## 4. Ce qui te concerne peut-être, côté cœur

**`build_project_monitoring_zones(replace = TRUE)`.** L'app contourne désormais
le renouvellement d'identifiants en ne rappelant pas la fonction. Le
contournement tient, et je ne demande rien. Mais si le cœur voulait un jour un
**vrai** upsert — conserver l'id d'une zone `(project_uuid, name)` existante et
ne mettre à jour que sa géométrie — la garde applicative deviendrait inutile, et
aucun cache indexé sur `zone_id` ne pourrait plus être orphelin par
construction. C'est le seul endroit où le problème disparaîtrait plutôt que
d'être évité.

**Le plafond mémoire.** J'ai vu passer ta v0.194.0 (40 %). Sur ce poste,
`NEMETON_MEMORY_MAX=12G` est posé dans le `.Renviron` **du projet** — 40 % de
31 Go donne 12,4 Go, donc la surcharge est désormais très proche de ta politique
et légèrement plus stricte. Elle n'a plus d'effet notable ; je la laisse, elle ne
nuit pas. Pour mémoire, c'est ce fichier-là qu'il faut lire, pas `~/.Renviron`.

## 5. À vérifier avant de coller

1. Que les trois entrées se placent bien au-dessus de *v0.194.0* et restent
   antéchronologiques entre elles.
2. Qu'aucune de ces trois versions n'est déjà consignée (le journal s'arrêtait à
   *App v0.143.10* au 2026-09-02 08:50).
