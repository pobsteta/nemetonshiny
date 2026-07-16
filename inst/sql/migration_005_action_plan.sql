-- Migration 005 — action plan states (multi-utilisateurs)
-- Versioned per-action snapshots of the action plan (archival: earlier versions
-- kept for change tracking / multi-user history). Mirrors migration_004
-- (regeneration_states). Each action is stored as a JSONB payload so the schema
-- is stable regardless of the action fields. The plan also lives on disk
-- (action_plan.json); this table is the shared server-side history.

CREATE TABLE IF NOT EXISTS nemeton.action_plan_states (
  id           BIGSERIAL   PRIMARY KEY,
  project_id   TEXT        NOT NULL,
  version      INTEGER     NOT NULL,
  generated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  horizon      INTEGER,
  ug_id        TEXT,
  action_id    TEXT,
  payload      JSONB       NOT NULL
);

CREATE INDEX IF NOT EXISTS idx_action_plan_states_project
  ON nemeton.action_plan_states (project_id, version);
