-- Migration 004 — reGénération climate states (spec 027 §7, L6)
-- Versioned per-UGF climate-vulnerability states (archival: earlier versions
-- kept for change tracking over time, §6A). §7 attributes stored as JSONB so
-- the schema is stable regardless of which engines ran.

CREATE TABLE IF NOT EXISTS nemeton.regeneration_states (
  id           BIGSERIAL PRIMARY KEY,
  project_id   TEXT        NOT NULL,
  version      INTEGER     NOT NULL,
  generated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  ug_id        TEXT,
  payload      JSONB       NOT NULL
);

CREATE INDEX IF NOT EXISTS idx_regeneration_states_project
  ON nemeton.regeneration_states (project_id, version);
