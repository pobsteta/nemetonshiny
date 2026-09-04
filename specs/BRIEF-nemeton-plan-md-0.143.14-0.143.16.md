# BRIEF `nemeton` — `PLAN.md` : trois livraisons app (0.143.14 → 0.143.16)

> **Statut** : ouvert, 2026-09-04.
> **Dépôt concerné** : `nemeton` uniquement — journal du `PLAN.md` racine.
> **Nature** : documentation seule. **Aucun code cœur, aucune release cœur.**
> **Contexte** : `nemetonshiny@0.143.16`, `main` à jour.
>
> **Ce brief a rétréci.** Il couvrait 0.143.11 → 0.143.15 ; les entrées
> **0.143.11, 0.143.12 et 0.143.13 sont désormais dans votre journal**
> (lignes 3726, 3649, 3621 au 2026-09-04) et ont été retirées d'ici. Restent
> les trois qui manquent.

---

## 0. Où coller

En tête de journal, **au-dessus** de `### 2026-09-03 — v0.195.0 : le run de
20 h qui est mort sans un mot`. Les trois entrées sont du 02/09 (soir) et du
03/09 ; dans l'ordre ci-dessous elles restent antéchronologiques entre elles.

## 1. Les trois livraisons

| Release | Commit `main` | Date | Suite app |
|---|---|---|---|
| v0.143.14 | `7efe5211` | 2026-09-02 | 0 FAIL / 0 ERROR / 17 SKIP |
| v0.143.15 | `4e760fd2` | 2026-09-03 | 0 FAIL / 0 ERROR / 17 SKIP |
| v0.143.16 | *(cf. PR #174)* | 2026-09-03 | 0 FAIL / 0 ERROR / 17 SKIP |

Toutes taguées par `release.yml`.

## 2. Texte à coller

```markdown
### 2026-09-03 — App `nemetonshiny` v0.143.16 : le log de l'enfant plafonné, et RECONFORT qui va enfin au bout

Complément de la v0.143.15, et dernière pièce du diagnostic Couchey. `log_path`
(votre v0.195.0) est passé sur les **quatre** chemins plafonnés de l'app —
FORDEAD, RECONFORT, le calcul des 31 indicateurs et le moteur de reGénération —
vers `data/<pipeline>_child.log`. Nom stable, rotation au **démarrage**
(`.prev-<horodatage>`, cinq gardées) : puisque le cœur conserve le fichier même
en cas de succès, sans rotation le run suivant écraserait la trace du précédent.

Garde de capacité sur `formals()` : sur un cœur antérieur l'argument est retiré
de l'appel. Le plancher `Imports:` avait été bumpé à `0.195.0` avant que le tag
n'existe, ce qui a rendu l'app non-installable en CI (`@*release` ne résout que
les tags) ; remis à `0.193.0`, puis rebumpé à `0.195.0` une fois la release
publiée.

**Et le run est passé.** Couchey, en échec depuis le 31 août, est allé au bout
le 03/09 à 21:19 : 10/10 phases, 14 min 48 s, les trois rasters masqués 2025.
Trois verrous devaient tomber ensemble — l'adoption des zones (v0.143.12) qui a
rendu les 203 scènes reprises du cache, la levée du blocage CNES, et votre
redécoupage en 7 chunks. Les deux correctifs de traçabilité n'ont pas fait
passer le run ; ils ont rendu les trois causes visibles.

Suite app : 0 FAIL, 0 ERROR, 17 SKIP.

### 2026-09-03 — App `nemetonshiny` v0.143.15 : la trace d'un run en échec était effacée au moment de servir

Projet **Couchey**. RECONFORT échoue après **20 h 19** (`exit 1`), et il ne
reste rien pour comprendre : ni le NDJSON de progression, ni le message de
l'enfant. Le diagnostic a dû être reconstitué depuis les fichiers d'IOTA².

`.cleanup_progress_file()` était appelé à l'identique sur les trois sorties —
succès, annulation **et erreur** — et supprimait le `.json` comme le `.ndjson`.
Le NDJSON est pourtant la seule trace structurée d'un run : une ligne par item,
une par phase. Sur le chemin d'erreur, on détruisait la preuve à la seconde où
elle devenait utile. Les chemins d'échec (FAST, FORDEAD, RECONFORT) archivent
désormais en `<fichier>.failed-<horodatage>`, cinq archives par fichier de base.

C'est ce constat qui a produit le brief `053-trace-enfant-plafonnee`, puisque le
NDJSON dit **jusqu'où** on est allé et jamais **pourquoi** ça s'est arrêté.

Deux mesures de ce run qui méritent le journal :

- **L'ingestion RECONFORT a été parfaite** : 203/203 scènes (51 reprises du
  cache, 152 téléchargées), zéro échec. Première fois sur ce projet — les
  quatre tentatives précédentes plafonnaient entre 82 et 109 items.
- **Ce n'était pas la mémoire de l'enfant applicatif** : `exit 1`,
  `memory.events` du scope externe à `max 0 / oom 0`, pic 0,97 Go. C'est le
  scope *interne* qui a débordé, et c'est vous qui l'avez trouvé.

Suite app : 0 FAIL, 0 ERROR, 17 SKIP. `nemetonshiny@4e760fd2`.

### 2026-09-02 — App `nemetonshiny` v0.143.14 : « Tout calculer » — grisage, toast, et un bloc qui ne répète plus son bouton

Trois retouches de la section de lancement enchaîné (sidebar Sélection).

L'entête du bloc et le bouton juste en dessous portaient le **même texte**,
« Tout calculer », à six pixels d'écart. Le bloc s'appelle désormais
« Tableau des actions » : le bloc nomme ce qu'il regroupe, le bouton nomme
l'action.

Le bouton **se grise pendant la chaîne** et redevient cliquable à la clôture,
avec un toast persistant « Tous les calculs en cours... » en bas à droite. Une
garde serveur double le grisage : un clic resté en vol au démarrage écrasait le
run en cours, dont les réponses étaient ensuite rejetées sur le `run_id` — une
chaîne orpheline tournant sans rien piloter.

Le point de conception : grisage et toast sont pilotés par l'**état du run**,
pas par le clic sur « Tout calculer ». Ce clic-là n'ouvre que la modale de
sélection des étapes ; griser dès l'ouverture aurait laissé un bouton mort à
qui annule la modale. La libération vit dans `.cloturer()`, passage obligé des
**deux** sorties (fin naturelle et arrêt manuel).

Suite app : 0 FAIL, 0 ERROR, 17 SKIP. `nemetonshiny@7efe5211`.
```

## 3. Rien à cocher

Aucun chantier `PLAN.md` ne porte ces sujets. Journal seul.

## 4. À vérifier avant de coller

1. Que les trois entrées se placent bien au-dessus de *v0.195.0* et restent
   antéchronologiques entre elles.
2. Qu'aucune n'est déjà consignée — au 2026-09-04 votre journal s'arrêtait à
   *App v0.143.13* (ligne 3621).
