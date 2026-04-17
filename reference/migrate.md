# Project Migration: v1 to v2 (UG support)

Migrates projects from schema v1 (parcel-only) to v2 (with UG support).
Creates default tenements and UGs where 1 parcel = 1 tenement = 1 UG.

Migration is transparent: existing indicator values are preserved
exactly. The UG layer is additive — it sits on top of the unchanged
parcel data.
