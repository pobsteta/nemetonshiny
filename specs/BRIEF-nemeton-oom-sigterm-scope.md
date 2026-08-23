# BRIEF `nemeton` — un OOM sous cgroup se présente en `-15`, pas en `-9`

> **Statut** : ouvert, 2026-08-23.
> **Dépôt concerné** : `nemeton` uniquement.
> **Fichier** : `R/isolate.R`, `run_memory_capped()`, branche d'échec (≈ l. 254-266).
> **Nature** : correctif de diagnostic. Le comportement ne change pas — seul le
> message change, et c'est tout l'enjeu.

---

## Le symptôme

Calcul des 31 indicateurs sur Couchey lancé depuis l'app le 2026-08-22. Après
3 h 20 de CPU, l'utilisateur voit :

```
"start_computation" failed in its capped child process (exit -15).
✖ ExtendedTask failed
```

Le journal système, à la même minute, dit la vérité :

```
Aug 23 00:12:16  run-r11dc2ffb…scope: A process of this unit has been killed by the OOM killer.
Aug 23 00:12:17  run-r11dc2ffb…scope: Failed with result 'oom-kill'.
```

C'était un dépassement du plafond de 10 Go. Le message qui l'aurait dit existe,
il est **juste au-dessus** dans le même `if` — et il n'a pas été atteint.

## Pourquoi `-15` et non `-9`

```r
# R/isolate.R
if (as.integer(st) %in% c(-9L, 137L)) {
  # "ran out of memory and was killed (ceiling: …)"
}
cli::cli_abort("{.val {fun}} failed in its capped child process (exit {st}).")
```

Le commentaire au-dessus de ce test est exact sur le principe — SIGKILL, `-9`
côté `processx`, `137` côté shell — mais il décrit **le processus tué**, pas
celui que `processx` observe.

Sous `systemd-run --scope`, l'arbre est :

```
processx  →  systemd-run (client)  →  [scope transitoire]  →  Rscript
                    ↑                                            ↑
          ce que px$get_exit_status() lit          ce que l'OOM killer tue
```

L'OOM killer frappe le **Rscript**, à l'intérieur du scope (SIGKILL). systemd
constate `oom-kill`, démonte le scope, et le **client** `systemd-run` se termine
sur **SIGTERM** — `-15`. `processx` ne voit jamais le `-9`.

Autrement dit : **le cas nominal du plafond, sur le chemin nominal (systemd-run
disponible), ne passe jamais par la branche mémoire.** Le `-9` n'est atteint que
dans le mode dégradé sans cgroup, où l'OOM killer global frappe directement le
processus lancé.

## Le correctif

Élargir la reconnaissance :

```r
if (as.integer(st) %in% c(-9L, 137L, -15L, 143L)) {
```

En pesant le risque d'amalgame : un `-15` peut aussi venir d'un
`systemctl stop`, d'un arrêt de session ou d'un `kill` manuel. Deux façons de
rester honnête :

1. **Formulation prudente** — dire que le processus a été tué et que le plafond
   en est la cause *habituelle*, plutôt que d'affirmer l'OOM. C'est la voie
   prise côté app dans `.compute_error_message()` (v0.133.1), justement parce
   que l'app ne peut pas faire mieux depuis un code de sortie.
2. **Constater plutôt qu'inférer** — le scope connaît son propre sort.
   `systemd-run --scope` accepte `--unit=` : en nommant l'unité, un
   `systemctl --user show <unit> -p Result` après coup rend `oom-kill` sans
   ambiguïté. Plus de code, mais un diagnostic **certain** au lieu d'une
   heuristique — et le cœur est le seul endroit d'où c'est possible, puisque
   c'est lui qui construit la commande.

La 1 suffit à supprimer le symptôme. La 2 est ce qui ferme le sujet pour de bon ;
à arbitrer selon ce que coûte un `--unit` généré côté `.reconfort_cap_memory()`.

## Ce que le message devrait contenir dans tous les cas

Le message mémoire actuel est bon et n'a pas besoin d'être réécrit — il nomme le
plafond, `memory_max`, `NEMETON_MEMORY_MAX` et `options(nemeton.memory_max=)`, et
il rappelle que la session a été épargnée. Il suffit qu'il soit **atteint**.

Point mineur au passage : le code de sortie brut (`exit -15`) n'apprend rien à
un utilisateur. S'il doit rester dans le message générique, il gagnerait à être
accompagné de sa lecture (« tué par le signal 15 »), voire réservé aux logs.

## Vérification

| Contrôle | Attendu |
|---|---|
| Provoquer un dépassement avec `systemd-run` disponible | message **mémoire**, pas le générique |
| Idem sans cgroup (`.reconfort_systemd_run()` → `NULL`) | message mémoire (chemin `-9`, inchangé) |
| Interruption volontaire du scope (`systemctl --user stop`) | ne pas prétendre à un OOM — cf. option 1 ou 2 |
| Le plafond nommé dans le message | celui réellement appliqué (50 % de `MemTotal` depuis v0.183.0) |

## Contexte

Incident complet et cause racine dans le brief frère destiné à
`opencanopynemeton` : `specs/BRIEF-opencanopy-pct-veg-values.md` — deux
`values()` sur un raster de 418 M cellules, ≈ 4,7 Go de vecteurs R pour un
pourcentage affiché dans un `message()`. Le présent brief ne traite que le
**diagnostic** : sans lui, la prochaine occurrence coûtera à nouveau une soirée
avant qu'on pense à ouvrir `journalctl`.
