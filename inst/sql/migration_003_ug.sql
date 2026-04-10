-- Migration 003: Add UG (Unités de Gestion / Management Units) support
-- ADR: UG as the operational forestry unit, distinct from cadastral parcels.
-- This migration adds tables for atoms, UGs, and UG-level indicators.

-- ============================================================
-- Table: UGs (Management Units)
-- ============================================================
CREATE TABLE IF NOT EXISTS nemeton.ugs (
  id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  project_id UUID REFERENCES nemeton.projects(id) ON DELETE CASCADE,
  label TEXT NOT NULL,
  groupe TEXT,  -- groupe d'aménagement (TSF, REGT, HSN, etc.)
  created_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_ugs_project ON nemeton.ugs(project_id);

-- ============================================================
-- Table: Atomes (smallest geometric subdivision)
-- ============================================================
CREATE TABLE IF NOT EXISTS nemeton.atomes (
  id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  project_id UUID REFERENCES nemeton.projects(id) ON DELETE CASCADE,
  parent_parcel_id UUID REFERENCES nemeton.parcels(id) ON DELETE CASCADE,
  ug_id UUID REFERENCES nemeton.ugs(id) ON DELETE SET NULL,
  label TEXT,
  geometry GEOMETRY(MultiPolygon, 3035),  -- ETRS89/LAEA (same as parcels)
  surface_m2 NUMERIC,
  created_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_atomes_project ON nemeton.atomes(project_id);
CREATE INDEX IF NOT EXISTS idx_atomes_ug ON nemeton.atomes(ug_id);
CREATE INDEX IF NOT EXISTS idx_atomes_parcel ON nemeton.atomes(parent_parcel_id);
CREATE INDEX IF NOT EXISTS idx_atomes_geometry ON nemeton.atomes USING GIST(geometry);

-- ============================================================
-- Table: UG-level indicators (aggregated from parcel-level)
-- ============================================================
CREATE TABLE IF NOT EXISTS nemeton.indicators_ug (
  id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  ug_id UUID REFERENCES nemeton.ugs(id) ON DELETE CASCADE,
  project_id UUID REFERENCES nemeton.projects(id) ON DELETE CASCADE,
  -- 31 indicateurs (same columns as nemeton.indicators)
  -- B - Biodiversite
  indicateur_b1_protection NUMERIC,
  indicateur_b2_structure NUMERIC,
  indicateur_b3_connectivite NUMERIC,
  -- C - Carbone & Vitalite
  indicateur_c1_biomasse NUMERIC,
  indicateur_c2_ndvi NUMERIC,
  -- W - Eau & Regulation
  indicateur_w1_reseau NUMERIC,
  indicateur_w2_zones_humides NUMERIC,
  indicateur_w3_humidite NUMERIC,
  -- A - Air & Microclimat
  indicateur_a1_couverture NUMERIC,
  indicateur_a2_qualite_air NUMERIC,
  -- F - Fertilite des sols
  indicateur_f1_fertilite NUMERIC,
  indicateur_f2_erosion NUMERIC,
  -- L - Paysage
  indicateur_l1_sylvosphere NUMERIC,
  indicateur_l2_fragmentation NUMERIC,
  -- T - Dynamique temporelle
  indicateur_t1_anciennete NUMERIC,
  indicateur_t2_changement NUMERIC,
  -- R - Risques & Resilience
  indicateur_r1_feu NUMERIC,
  indicateur_r2_tempete NUMERIC,
  indicateur_r3_secheresse NUMERIC,
  indicateur_r4_abroutissement NUMERIC,
  -- S - Social & Usages
  indicateur_s1_routes NUMERIC,
  indicateur_s2_bati NUMERIC,
  indicateur_s3_population NUMERIC,
  -- P - Production & Economie
  indicateur_p1_volume NUMERIC,
  indicateur_p2_station NUMERIC,
  indicateur_p3_qualite_bois NUMERIC,
  -- E - Energie & Climat
  indicateur_e1_bois_energie NUMERIC,
  indicateur_e2_evitement NUMERIC,
  -- N - Naturalite
  indicateur_n1_distance NUMERIC,
  indicateur_n2_continuite NUMERIC,
  indicateur_n3_naturalite NUMERIC,
  -- 12 familles (scores normalises 0-100)
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

CREATE INDEX IF NOT EXISTS idx_indicators_ug_project ON nemeton.indicators_ug(project_id);
CREATE INDEX IF NOT EXISTS idx_indicators_ug_ug ON nemeton.indicators_ug(ug_id);
