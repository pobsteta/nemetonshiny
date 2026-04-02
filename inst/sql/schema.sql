-- Nemeton PostgreSQL/PostGIS Schema
-- ADR-002: niveau plateforme (multi-utilisateurs, requetes spatiales)

-- Extensions
CREATE EXTENSION IF NOT EXISTS postgis;
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

-- ============================================================
-- Schema nemeton
-- ============================================================
CREATE SCHEMA IF NOT EXISTS nemeton;

-- ============================================================
-- Users & Roles
-- ============================================================
CREATE TABLE IF NOT EXISTS nemeton.users (
  id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  external_id TEXT UNIQUE,           -- ID du fournisseur OAuth (sub claim)
  provider TEXT DEFAULT 'local',     -- keycloak, google, github, agentconnect
  email TEXT UNIQUE,
  name TEXT NOT NULL,
  preferred_language TEXT DEFAULT 'fr',
  profile_key TEXT DEFAULT 'profil_citoyen',  -- cle NMT du profil acteur
  created_at TIMESTAMPTZ DEFAULT NOW(),
  updated_at TIMESTAMPTZ DEFAULT NOW(),
  last_login_at TIMESTAMPTZ
);

CREATE TABLE IF NOT EXISTS nemeton.user_roles (
  user_id UUID REFERENCES nemeton.users(id) ON DELETE CASCADE,
  role TEXT NOT NULL,                -- admin, gestionnaire, lecteur
  granted_at TIMESTAMPTZ DEFAULT NOW(),
  PRIMARY KEY (user_id, role)
);

-- ============================================================
-- Projects
-- ============================================================
CREATE TABLE IF NOT EXISTS nemeton.projects (
  id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  project_id TEXT UNIQUE NOT NULL,   -- ex: 20260327_144122_ilgd
  owner_id UUID REFERENCES nemeton.users(id),
  name TEXT NOT NULL,
  description TEXT,
  status TEXT DEFAULT 'draft',       -- draft, downloading, computing, completed, error
  ndp_level INTEGER DEFAULT 0,
  commune_code TEXT,
  commune_name TEXT,
  department_code TEXT,
  created_at TIMESTAMPTZ DEFAULT NOW(),
  updated_at TIMESTAMPTZ DEFAULT NOW(),
  metadata JSONB DEFAULT '{}'::jsonb
);

CREATE INDEX IF NOT EXISTS idx_projects_owner ON nemeton.projects(owner_id);
CREATE INDEX IF NOT EXISTS idx_projects_commune ON nemeton.projects(commune_code);
CREATE INDEX IF NOT EXISTS idx_projects_department ON nemeton.projects(department_code);

-- ============================================================
-- Parcels (geometries spatiales)
-- ============================================================
CREATE TABLE IF NOT EXISTS nemeton.parcels (
  id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  project_id UUID REFERENCES nemeton.projects(id) ON DELETE CASCADE,
  nemeton_id TEXT,                   -- identifiant interne nemeton
  geo_parcelle TEXT,                 -- reference cadastrale
  section TEXT,
  numero TEXT,
  contenance NUMERIC,
  geometry GEOMETRY(MultiPolygon, 3035),  -- ETRS89/LAEA paneuropeen (ADR-008)
  created_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_parcels_project ON nemeton.parcels(project_id);
CREATE INDEX IF NOT EXISTS idx_parcels_geometry ON nemeton.parcels USING GIST(geometry);

-- ============================================================
-- Indicators (31 indicateurs par parcelle)
-- ============================================================
CREATE TABLE IF NOT EXISTS nemeton.indicators (
  id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  parcel_id UUID REFERENCES nemeton.parcels(id) ON DELETE CASCADE,
  project_id UUID REFERENCES nemeton.projects(id) ON DELETE CASCADE,
  -- 31 indicateurs (cles NMT du glossaire BMAD)
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

CREATE INDEX IF NOT EXISTS idx_indicators_project ON nemeton.indicators(project_id);
CREATE INDEX IF NOT EXISTS idx_indicators_parcel ON nemeton.indicators(parcel_id);

-- ============================================================
-- Comments (perspectives IA)
-- ============================================================
CREATE TABLE IF NOT EXISTS nemeton.comments (
  id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  project_id UUID REFERENCES nemeton.projects(id) ON DELETE CASCADE,
  type TEXT NOT NULL,                -- synthesis, family_B, family_C, etc.
  profile_key TEXT,                  -- profil acteur utilise
  content TEXT,
  language TEXT DEFAULT 'fr',
  created_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_comments_project ON nemeton.comments(project_id);
