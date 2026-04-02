-- Migration: renommer les colonnes indicators vers les cles NMT du glossaire BMAD
-- A executer une seule fois sur la base Clever Cloud

-- Supprimer l'ancienne table et recreer
DROP TABLE IF EXISTS nemeton.indicators CASCADE;

CREATE TABLE IF NOT EXISTS nemeton.indicators (
  id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  parcel_id UUID REFERENCES nemeton.parcels(id) ON DELETE CASCADE,
  project_id UUID REFERENCES nemeton.projects(id) ON DELETE CASCADE,
  -- 31 indicateurs (cles NMT du glossaire BMAD)
  indicateur_b1_protection NUMERIC,
  indicateur_b2_structure NUMERIC,
  indicateur_b3_connectivite NUMERIC,
  indicateur_c1_biomasse NUMERIC,
  indicateur_c2_ndvi NUMERIC,
  indicateur_w1_reseau NUMERIC,
  indicateur_w2_zones_humides NUMERIC,
  indicateur_w3_humidite NUMERIC,
  indicateur_a1_couverture NUMERIC,
  indicateur_a2_qualite_air NUMERIC,
  indicateur_f1_fertilite NUMERIC,
  indicateur_f2_erosion NUMERIC,
  indicateur_l1_sylvosphere NUMERIC,
  indicateur_l2_fragmentation NUMERIC,
  indicateur_t1_anciennete NUMERIC,
  indicateur_t2_changement NUMERIC,
  indicateur_r1_feu NUMERIC,
  indicateur_r2_tempete NUMERIC,
  indicateur_r3_secheresse NUMERIC,
  indicateur_r4_abroutissement NUMERIC,
  indicateur_s1_routes NUMERIC,
  indicateur_s2_bati NUMERIC,
  indicateur_s3_population NUMERIC,
  indicateur_p1_volume NUMERIC,
  indicateur_p2_station NUMERIC,
  indicateur_p3_qualite_bois NUMERIC,
  indicateur_e1_bois_energie NUMERIC,
  indicateur_e2_evitement NUMERIC,
  indicateur_n1_distance NUMERIC,
  indicateur_n2_continuite NUMERIC,
  indicateur_n3_naturalite NUMERIC,
  -- 12 familles (cles NMT)
  famille_biodiversite NUMERIC,
  famille_carbone NUMERIC,
  famille_eau NUMERIC,
  famille_air NUMERIC,
  famille_sol NUMERIC,
  famille_paysage NUMERIC,
  famille_temporel NUMERIC,
  famille_risque NUMERIC,
  famille_social NUMERIC,
  famille_production NUMERIC,
  famille_energie NUMERIC,
  famille_naturalite NUMERIC,
  -- Metadata
  computed_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_indicators_project ON nemeton.indicators(project_id);
CREATE INDEX IF NOT EXISTS idx_indicators_parcel ON nemeton.indicators(parcel_id);
