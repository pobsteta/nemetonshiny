-- Migration: EPSG:2154 → EPSG:3035 (ADR-008 paneuropeen)
-- Reprojette les geometries existantes de Lambert-93 vers ETRS89/LAEA

-- Reprojeter les parcelles existantes
ALTER TABLE nemeton.parcels
  ALTER COLUMN geometry TYPE GEOMETRY(MultiPolygon, 3035)
  USING ST_Transform(geometry, 3035);

-- Mettre a jour l'index spatial
REINDEX INDEX nemeton.idx_parcels_geometry;
