-- Migration 006 — valeur normalisée [0;100] à côté de la valeur brute
--
-- Un indicateur répond à deux questions : « combien ? » dans son unité propre,
-- et « où cela situe-t-il cette UGF ? » sur une échelle commune. Le schéma ne
-- portait que la première, si bien que tout consommateur voulant comparer
-- devait renormaliser lui-même — et le faisait à sa façon.
--
-- La normalisation est celle du cœur (`normalize_indicator()`), par indicateur
-- et à bornes ABSOLUES : deux projets restent comparables. Un min-max ferait de
-- chaque projet son propre étalon.
--
-- Idempotent : `ADD COLUMN IF NOT EXISTS` se rejoue sans effet.

ALTER TABLE nemeton.indicators ADD COLUMN IF NOT EXISTS indicateur_b1_protection_norm NUMERIC;
ALTER TABLE nemeton.indicators ADD COLUMN IF NOT EXISTS indicateur_b2_structure_norm NUMERIC;
ALTER TABLE nemeton.indicators ADD COLUMN IF NOT EXISTS indicateur_b3_connectivite_norm NUMERIC;
ALTER TABLE nemeton.indicators ADD COLUMN IF NOT EXISTS indicateur_c1_biomasse_norm NUMERIC;
ALTER TABLE nemeton.indicators ADD COLUMN IF NOT EXISTS indicateur_c2_ndvi_norm NUMERIC;
ALTER TABLE nemeton.indicators ADD COLUMN IF NOT EXISTS indicateur_w1_reseau_norm NUMERIC;
ALTER TABLE nemeton.indicators ADD COLUMN IF NOT EXISTS indicateur_w2_zones_humides_norm NUMERIC;
ALTER TABLE nemeton.indicators ADD COLUMN IF NOT EXISTS indicateur_w3_humidite_norm NUMERIC;
ALTER TABLE nemeton.indicators ADD COLUMN IF NOT EXISTS indicateur_a1_couverture_norm NUMERIC;
ALTER TABLE nemeton.indicators ADD COLUMN IF NOT EXISTS indicateur_a2_qualite_air_norm NUMERIC;
ALTER TABLE nemeton.indicators ADD COLUMN IF NOT EXISTS indicateur_f1_fertilite_norm NUMERIC;
ALTER TABLE nemeton.indicators ADD COLUMN IF NOT EXISTS indicateur_f2_erosion_norm NUMERIC;
ALTER TABLE nemeton.indicators ADD COLUMN IF NOT EXISTS indicateur_l1_sylvosphere_norm NUMERIC;
ALTER TABLE nemeton.indicators ADD COLUMN IF NOT EXISTS indicateur_l2_fragmentation_norm NUMERIC;
ALTER TABLE nemeton.indicators ADD COLUMN IF NOT EXISTS indicateur_t1_anciennete_norm NUMERIC;
ALTER TABLE nemeton.indicators ADD COLUMN IF NOT EXISTS indicateur_t2_changement_norm NUMERIC;
ALTER TABLE nemeton.indicators ADD COLUMN IF NOT EXISTS indicateur_r1_feu_norm NUMERIC;
ALTER TABLE nemeton.indicators ADD COLUMN IF NOT EXISTS indicateur_r2_tempete_norm NUMERIC;
ALTER TABLE nemeton.indicators ADD COLUMN IF NOT EXISTS indicateur_r3_secheresse_norm NUMERIC;
ALTER TABLE nemeton.indicators ADD COLUMN IF NOT EXISTS indicateur_r4_abroutissement_norm NUMERIC;
ALTER TABLE nemeton.indicators ADD COLUMN IF NOT EXISTS indicateur_s1_routes_norm NUMERIC;
ALTER TABLE nemeton.indicators ADD COLUMN IF NOT EXISTS indicateur_s2_bati_norm NUMERIC;
ALTER TABLE nemeton.indicators ADD COLUMN IF NOT EXISTS indicateur_s3_population_norm NUMERIC;
ALTER TABLE nemeton.indicators ADD COLUMN IF NOT EXISTS indicateur_p1_volume_norm NUMERIC;
ALTER TABLE nemeton.indicators ADD COLUMN IF NOT EXISTS indicateur_p2_station_norm NUMERIC;
ALTER TABLE nemeton.indicators ADD COLUMN IF NOT EXISTS indicateur_p3_qualite_bois_norm NUMERIC;
ALTER TABLE nemeton.indicators ADD COLUMN IF NOT EXISTS indicateur_e1_bois_energie_norm NUMERIC;
ALTER TABLE nemeton.indicators ADD COLUMN IF NOT EXISTS indicateur_e2_evitement_norm NUMERIC;
ALTER TABLE nemeton.indicators ADD COLUMN IF NOT EXISTS indicateur_n1_distance_norm NUMERIC;
ALTER TABLE nemeton.indicators ADD COLUMN IF NOT EXISTS indicateur_n2_continuite_norm NUMERIC;
ALTER TABLE nemeton.indicators ADD COLUMN IF NOT EXISTS indicateur_n3_naturalite_norm NUMERIC;
