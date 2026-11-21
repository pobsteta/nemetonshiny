# nemetonshiny 2026.11.2.9010 (2026-11-21)

> **Non publié.** Mission 040.

## Corrections

- **Typage de la desserte : un réseau sans route à créer finit « rien à typer » même sans volume P1.** Dans `run_desserte_typage()`, la lecture du réseau et le test `lignes` vide (`status = "empty"`, `desserte_typage_rien_a_typer`) passent désormais **avant** le bloc volume (préparation IFN, accès disque, comblement). Nouvel ordre : paquets, fichier réseau, parcelles, paramètres, **réseau vide**, préparation IFN / volume (`no_volume`), essence (`no_species`), vectorisation. Sur un parc urbain hors BD Forêt (« Parc de la colombière »), l'étape est ignorée au lieu d'échouer en `no_volume`.
- **Lancement enchaîné : le rapport du typage dit le vrai motif.** L'étape `desserte_typage` affichait toujours le libellé générique `desserte_typage_failed`. Un helper commun (`pipeline_reason_message()`, `R/service_pipeline.R`) construit le message à partir de `reason` (puis `detail`) ; il sert au rapport, à la notification de l'onglet et à `pipeline_engine_outcome()`.

